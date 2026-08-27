"""
Incorporate MapBiomas national annual land-cover maps into the CropGrids/FAOSTAT
back-cast time series (pred_<crop>_<year>.tif), wherever a national MapBiomas
platform resolves that crop as its own class.

WHY THIS SCRIPT EXISTS
-----------------------
The main pipeline currently derives every crop-year raster (data/05_crop_types/CG/
time_series/pred_<crop>_<year>.tif) by rescaling the single circa-2020 CROPGRIDS
spatial pattern with a national FAOSTAT multiplier (see
Scripts/I_01_02_2_Compile_CROPGRIDS_FAOSTAT-timeseries.py). That assumes a crop's
spatial distribution within a country never changes -- demonstrably false (see the
Brazil soybean sensitivity analysis: national impact error up to -19% and hotspot
overlap as low as 56% at 20 years from the baseline).

Several MapBiomas national platforms map specific crops directly, every year, from
Landsat -- real ground truth, not a rescaled prior. Where that exists, this script
REPLACES the back-cast values with the MapBiomas-derived values, restricted to that
country's territory and to the years both datasets cover. Everywhere else (other
countries, crops MapBiomas doesn't resolve, years outside MapBiomas' range), the
existing back-cast is left untouched.

VERIFIED CLASS CODES (checked directly against each platform's legend PDF, Aug 2026)
--------------------------------------------------------------------------------
Brazil      (Collection 10, 1985-2024): soybean=39, sugarcane=20, rice=40, cotton=62,
            other_temporary_crop=41, coffee=46, citrus=47, oil_palm=35, forest_plantation=9
Bolivia     (Collection 3,  1985-2023): soybean=39
Peru        (Collection 3,  1985-2023*): oil_palm=35, rice=40
Colombia    (Collection 3.0,1985-2024*): oil_palm=35, banana=74 (banana confirmed
            from the official Collection 3.0 codes PDF, Oct 2025 -- "3.3 Platano y
            banano (beta)"=74, same code as Ecuador. NOT present in Collection 2.0,
            so only Collection-3.0-covered years should be patched for banana.)
Ecuador     (Collection 3.0,1985-2024): banana=74
Indonesia   (Collection 4,  2000-2022*): rice_paddy=40, oil_palm=35, pulpwood_plantation=9
Argentina, Chile: no crop-specific classes in any collection -- not included.

Note the code reuse across the network (39=soybean, 35=oil palm, 40=rice, 9=
plantation) is a MapBiomas convention, verified per-country from each legend PDF, not assumed.

* = end-year taken from general platform descriptions, not the legend PDF itself.
    CONFIRM against the actual collection you download before running.

UNVERIFIED / NEEDS YOUR CHECK BEFORE RUNNING
---------------------------------------------
2. CROPGRIDS_NAME below must match the exact string used in your own
   data/05_crop_types/CG/croptypekey.csv / pred_<name>_<year>.tif filenames.
   "soybean", "oilpalm", "citrusnes" are confirmed from your treecrops list; the
   others (sugarcane, rice, cotton, coffee, banana) are standard CROPGRIDS/
   Monfreda names but not individually re-verified against your file -- check
   the actual filenames in data/05_crop_types/CG/time_series/ before running.
3. Every MAPBIOMAS_RASTER_TEMPLATE path below is a placeholder. Fill in the real
   path once you've downloaded each country's annual coverage rasters (same
   format as the Brazil ones you already have: one GeoTIFF per year, single
   band, pixel value = MapBiomas class code, EPSG:4326).
4. This script assumes the north-south flip bug in
   I_01_02_2_Compile_CROPGRIDS_FAOSTAT-timeseries.py is already fixed and
   pred_<crop>_<year>.tif has already been regenerated correctly -- it reads
   whatever is currently in time_series/ as the baseline to patch.

WHAT IT DOES
------------
For every crop grouped in CONFIG:
  1. For every year present in BOTH the crop's back-cast series (2000-2023) AND
     a contributing country's MapBiomas coverage:
     a. Aggregate that country's MapBiomas raster (mask = pixel == class_code)
        from its native ~30 m resolution up to the CropGrids 0.05 degree grid,
        producing area-in-hectares per cell (latitude-weighted pixel area,
        streamed in blocks so multi-GB rasters never fully load into memory).
     b. Rasterize that country's polygon (Natural Earth 110m) onto the same
        0.05 degree grid, to know exactly which cells to touch.
     c. Overwrite pred_<crop>_<year>.tif inside that mask with the MapBiomas
        values (legitimate zeros included -- MapBiomas absence of the class
        means the crop truly wasn't there that year). Cells outside every
        contributing country's mask are left as the existing back-cast.
  2. Writes the patched raster to a new file (pred_<crop>_<year>_mapbiomas.tif)
     rather than overwriting the original in place, so the back-cast-only
     version stays available for comparison. Change OVERWRITE_IN_PLACE below
     if you'd rather replace the originals directly.

Designed to run unattended on your own machine/cluster -- resumable via a
per-(crop,year) completion marker, so re-running after an interruption just
skips finished outputs.
"""

import os
import time
import numpy as np
import rasterio
import geopandas as gpd
from rasterio.features import rasterize
from rasterio.windows import Window
from affine import Affine

# ----------------------------------------------------------------------------
# PATHS -- adjust to your remote environment
# ----------------------------------------------------------------------------
PROJECT_ROOT = "h:\\02_Projekte\\03_Intensification-fragmentation-CFs"  # run from the Scripts/ directory, or set an absolute path
CG_TIME_SERIES_DIR = os.path.join(PROJECT_ROOT, "data/05_crop_types/CG/time_series")
COUNTRY_SHP_PATH = "H:/02_Projekte/allgemein_biodiversity_impact/02_data/country_shp/ne_110m_admin_0_countries.shp"
# ^ same file used elsewhere in the pipeline (H:/02_Projekte/allgemein_biodiversity_impact/02_data/country_shp/)
MAP_BIOMAS_PATH = os.path.join(PROJECT_ROOT, "data/01_raw/mapbiomas")  
MAP_BIOMAS_PATH_BRA = "H:/02_Projekte/02_Fragmentation/data/01_raw/mapbiomas"
OUT_DIR = os.path.join(PROJECT_ROOT, "data/05_crop_types/CG/time_series_mapbiomas_patched")
CHECKPOINT_DIR = os.path.join(PROJECT_ROOT, "data/05_crop_types/CG/_mapbiomas_patch_checkpoints")
os.makedirs(OUT_DIR, exist_ok=True)
os.makedirs(CHECKPOINT_DIR, exist_ok=True)

OVERWRITE_IN_PLACE = False  # True = write back into CG_TIME_SERIES_DIR directly

# ----------------------------------------------------------------------------
# CROPGRIDS GLOBAL GRID (must match pred_<crop>_<year>.tif exactly)
# ----------------------------------------------------------------------------
CG_ORIGIN_LON = -180.0
CG_ORIGIN_LAT = 90.0
CG_CELL = 0.05
BACKCAST_YEAR_RANGE = range(2024, 2025)  # years your FAOSTAT back-cast covers

BLOCK_ROWS = 2048  # MapBiomas raster streaming block size (rows). Lower if RAM-constrained.

# ----------------------------------------------------------------------------
# CONFIG -- grouped by CROPGRIDS crop name. Each entry lists every country that
# resolves this crop as its own MapBiomas class.
#
# FILL IN: mapbiomas_raster_template (use {year} as placeholder), and confirm
# years_available against the actual collection you download.
# ----------------------------------------------------------------------------
CONFIG = {
    "soybean": [
        dict(country="Brazil", class_code=39,
             mapbiomas_raster_template=os.path.join(MAP_BIOMAS_PATH_BRA, "brazil", "brazil_coverage_{year}.tif"),
             years_available=range(2000, 2025)),  # already have this one -- see Brazil case study
        dict(country="Bolivia", class_code=39,
             mapbiomas_raster_template=os.path.join(MAP_BIOMAS_PATH, "bolivia", "bolivia_coverage_{year}.tif"),
             years_available=range(2000, 2025)),  # VERIFY end year against Bolivia Collection 3 download
    ],
    "sugarcane": [
        dict(country="Brazil", class_code=20,
             mapbiomas_raster_template=os.path.join(MAP_BIOMAS_PATH_BRA, "brazil", "brazil_coverage_{year}.tif"),
             years_available=range(2000, 2025)),
    ],
    "rice": [
        dict(country="Brazil", class_code=40,
             mapbiomas_raster_template=os.path.join(MAP_BIOMAS_PATH_BRA, "brazil", "brazil_coverage_{year}.tif"),
             years_available=range(2000, 2025)),
        dict(country="Peru", class_code=40,
             mapbiomas_raster_template=os.path.join(MAP_BIOMAS_PATH, "peru", "peru_coverage_{year}.tif"),
             years_available=range(2000, 2025)),  # VERIFY end year
        dict(country="Indonesia", class_code=40,
             mapbiomas_raster_template=os.path.join(MAP_BIOMAS_PATH, "indonesia", "indonesia_coverage_{year}.tif"),
             years_available=range(2000, 2025)),  # Collection 2.0 = 2000-2022; VERIFY
        dict(country="Colombia", class_code=40,
                     mapbiomas_raster_template=os.path.join(MAP_BIOMAS_PATH, "colombia", "colombia_coverage_{year}.tif"),
                     years_available=range(2000, 2025)),
    ],
    "cotton": [
        dict(country="Brazil", class_code=62,
             mapbiomas_raster_template=os.path.join(MAP_BIOMAS_PATH_BRA, "brazil", "brazil_coverage_{year}.tif"),
             years_available=range(2000, 2025)),
    ],
    "coffee": [
        dict(country="Brazil", class_code=46,
             mapbiomas_raster_template=os.path.join(MAP_BIOMAS_PATH_BRA, "brazil", "brazil_coverage_{year}.tif"),
             years_available=range(2000, 2025)),
    ],
    "citrusnes": [  # CROPGRIDS name for citrus -- confirmed present in your treecrops list
        dict(country="Brazil", class_code=47,
             mapbiomas_raster_template=os.path.join(MAP_BIOMAS_PATH_BRA, "brazil", "brazil_coverage_{year}.tif"),
             years_available=range(2000, 2025)),
    ],
    "oilpalm": [  # CROPGRIDS name -- confirmed present in your treecrops list
        dict(country="Brazil", class_code=35,
             mapbiomas_raster_template=os.path.join(MAP_BIOMAS_PATH_BRA, "brazil", "brazil_coverage_{year}.tif"),
             years_available=range(2000, 2025)),
        dict(country="Peru", class_code=35,
             mapbiomas_raster_template=os.path.join(MAP_BIOMAS_PATH, "peru", "peru_coverage_{year}.tif"),
             years_available=range(2000, 2025)),  # VERIFY end year
        dict(country="Colombia", class_code=35,
             mapbiomas_raster_template=os.path.join(MAP_BIOMAS_PATH, "colombia", "colombia_coverage_{year}.tif"),
             years_available=range(2000, 2023)),  # VERIFY end year (Collection 2.0 = 1985-2022)
        dict(country="Indonesia", class_code=35,
             mapbiomas_raster_template=os.path.join(MAP_BIOMAS_PATH, "indonesia", "indonesia_coverage_{year}.tif"),
             years_available=range(2000, 2023)),  # VERIFY end year
    ],
    "banana": [  # CROPGRIDS name -- standard Monfreda naming, cross-check your croptypekey.csv
        dict(country="Ecuador", class_code=74,
             mapbiomas_raster_template=os.path.join(MAP_BIOMAS_PATH, "ecuador", "ecuador_coverage_{year}.tif"),
             years_available=range(2000, 2025)),
        # Colombia: confirmed in the official Collection 3.0 codes PDF
        # (colombia.mapbiomas.org, Oct 2025) -- "3.3 Platano y banano (beta)" = 74,
        # the same code as Ecuador. 
        dict(country="Colombia", class_code=74,
             mapbiomas_raster_template=os.path.join(MAP_BIOMAS_PATH, "colombia", "colombia_coverage_{year}.tif"),
             years_available=range(2000, 2025)),  # VERIFY: Collection 3.0 year range specifically
    ],
    # "forest_plantation" / pulpwood: Indonesia's "Pulpwood Plantation" (code 9) is
    # industrial fast-growing tree plantation for pulp/paper (mainly Acacia and
    # Eucalyptus in Sumatra/Kalimantan) -- the same MapBiomas class 9 used for
    # Brazil's "Forest Plantation". CROPGRIDS doesn't carry a matching crop-specific
    # layer (industrial pulpwood isn't one of the 173 CROPGRIDS crops), so there's
    # nothing to patch here directly -- flagging for awareness, not action.
}


# ----------------------------------------------------------------------------
# CORE FUNCTIONS
# ----------------------------------------------------------------------------

def pixel_area_ha(lat_deg, px_w_deg, px_h_deg):
    """Latitude-weighted pixel area in hectares (accounts for meridian convergence)."""
    lat_rad = np.radians(lat_deg)
    width_m = px_w_deg * 111320.0 * np.cos(lat_rad)
    height_m = px_h_deg * 110540.0
    return (width_m * height_m) / 10000.0


def aggregate_mapbiomas_class_to_cg_grid(raster_path, class_code, block_rows=BLOCK_ROWS, log=print):
    """
    Stream a MapBiomas national coverage raster and aggregate pixels == class_code
    onto the CropGrids 0.05-degree grid, returning area in hectares per cell.

    Returns: (area_ha [np.float32 2D], row0, col0, n_target_rows, n_target_cols)
    where (row0, col0) locate this sub-grid within the global 3600x7200 CropGrids array.
    """
    with rasterio.open(raster_path) as src:
        n_rows, n_cols = src.shape
        transform = src.transform
        px_w = transform.a
        px_h = -transform.e
        bounds = src.bounds

        col0 = int(np.floor((bounds.left - CG_ORIGIN_LON) / CG_CELL))
        col1 = int(np.floor((bounds.right - CG_ORIGIN_LON) / CG_CELL))
        row0 = int(np.floor((CG_ORIGIN_LAT - bounds.top) / CG_CELL))
        row1 = int(np.floor((CG_ORIGIN_LAT - bounds.bottom) / CG_CELL))
        n_target_rows = row1 - row0 + 1
        n_target_cols = col1 - col0 + 1

        count = np.zeros((n_target_rows, n_target_cols), dtype=np.int64)

        col_indices = np.arange(n_cols)
        lon_centers = transform.c + (col_indices + 0.5) * px_w
        # Define target_col, that is the center of the pixel in the target grid, for each column in the source raster
        target_col = np.floor((lon_centers - CG_ORIGIN_LON) / CG_CELL).astype(np.int64) - col0
        target_col = np.clip(target_col, 0, n_target_cols - 1)

        # To save memory, the raster is streamed in blocks of rows, and the counts are accumulated into the count array.
        n_blocks = (n_rows + block_rows - 1) // block_rows
        for bi in range(n_blocks):
            r_start = bi * block_rows
            r_end = min(r_start + block_rows, n_rows)
            window = Window(0, r_start, n_cols, r_end - r_start)
            block = src.read(1, window=window)

            row_indices = np.arange(r_start, r_end)
            lat_centers = transform.f + (row_indices + 0.5) * transform.e
            target_row = np.floor((CG_ORIGIN_LAT - lat_centers) / CG_CELL).astype(np.int64) - row0
            target_row = np.clip(target_row, 0, n_target_rows - 1)

            mask = (block == class_code)
            if mask.any():
                rows_2d, cols_2d = np.nonzero(mask)
                tgt_r = target_row[rows_2d]
                tgt_c = target_col[cols_2d]
                lin = tgt_r * n_target_cols + tgt_c
                counts = np.bincount(lin, minlength=n_target_rows * n_target_cols)
                count += counts.reshape(n_target_rows, n_target_cols)

            if bi % 20 == 0:
                log(f"    block {bi}/{n_blocks}")

        target_row_idx = np.arange(n_target_rows)
        target_row_lat = CG_ORIGIN_LAT - (row0 + target_row_idx + 0.5) * CG_CELL
        area_per_pixel = pixel_area_ha(target_row_lat, px_w, px_h)
        area_ha = (count * area_per_pixel[:, None]).astype(np.float32)

    return area_ha, row0, col0, n_target_rows, n_target_cols


_country_gdf_cache = None


def get_country_grid_mask(country_name, row0, col0, nr, nc):
    """Rasterize one country's polygon onto the sub-grid defined by row0/col0/nr/nc."""
    global _country_gdf_cache
    if _country_gdf_cache is None:
        _country_gdf_cache = gpd.read_file(COUNTRY_SHP_PATH)
    gdf = _country_gdf_cache
    sub = gdf[gdf["GEOUNIT"] == country_name]
    if len(sub) == 0:
        raise ValueError(f"Country '{country_name}' not found in {COUNTRY_SHP_PATH} (GEOUNIT field)")

    transform = Affine(CG_CELL, 0, CG_ORIGIN_LON + col0 * CG_CELL,
                        0, -CG_CELL, CG_ORIGIN_LAT - row0 * CG_CELL)
    mask = rasterize(((geom, 1) for geom in sub.geometry),
                      out_shape=(nr, nc), transform=transform, fill=0, dtype="uint8")
    return mask.astype(bool)


def patch_one_crop_year(crop_name, year, country_configs, log=print):
    """Patch pred_<crop>_<year>.tif with MapBiomas data from every contributing
    country that covers this year. Returns True if the file was written."""
    src_path = os.path.join(CG_TIME_SERIES_DIR, f"pred_{crop_name}_{year}.tif")
    if not os.path.exists(src_path):
        log(f"  [skip] {src_path} does not exist")
        return False

    applicable = [c for c in country_configs if year in c["years_available"]]
    if not applicable:
        return False  # nothing to patch for this crop/year

    with rasterio.open(src_path) as src:
        full = src.read(1)
        profile = src.profile

    touched = False
    for c in applicable:
        raster_path = c["mapbiomas_raster_template"].format(year=year)
        if not os.path.exists(raster_path):
            log(f"  [warn] missing MapBiomas raster for {c['country']} {year}: {raster_path}")
            continue

        log(f"  {crop_name} {year}: aggregating {c['country']} (class {c['class_code']})")
        area_ha, row0, col0, nr, nc = aggregate_mapbiomas_class_to_cg_grid(
            raster_path, c["class_code"], log=log)
        country_mask = get_country_grid_mask(c["country"], row0, col0, nr, nc)

        sub = full[row0:row0 + nr, col0:col0 + nc]
        sub[country_mask] = area_ha[country_mask]
        full[row0:row0 + nr, col0:col0 + nc] = sub
        touched = True

    if not touched:
        return False

    out_path = src_path if OVERWRITE_IN_PLACE else os.path.join(
        OUT_DIR, f"pred_{crop_name}_{year}_mapbiomas.tif")
    with rasterio.open(out_path, "w", **profile) as dst:
        dst.write(full, 1)
    log(f"  -> wrote {out_path}")
    return True


def main():
    def log(msg):
        line = f"[{time.strftime('%H:%M:%S')}] {msg}"
        print(line, flush=True)

    for crop_name, country_configs in CONFIG.items():
        #print(f"=== Processing crop: {crop_name} ===")
        years = sorted(set(BACKCAST_YEAR_RANGE) & set().union(*(c["years_available"] for c in country_configs)))
        for year in years:
            marker = os.path.join(CHECKPOINT_DIR, f"{crop_name}_{year}.done")
            if os.path.exists(marker):
                continue
            log(f"=== {crop_name} {year} ===")
            try:
                patch_one_crop_year(crop_name, year, country_configs, log=log)
            except Exception as e:
                log(f"  [ERROR] {crop_name} {year}: {e}")
                continue
            open(marker, "w").close()

    log("=== ALL DONE ===")


if __name__ == "__main__":
    main()

