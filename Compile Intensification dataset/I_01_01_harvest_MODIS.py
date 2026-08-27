"""
Stage A: Download MCD12Q2 (Land Cover Dynamics, NumCycles only) and MCD12Q1
(Land Cover Type, LC_Type1) tiles, and mosaic each into a global raster at
ORIGINAL MODIS resolution (500m, native Sinusoidal projection/grid, no
interpolation - each tile is placed at its exact pixel location):

    land_cover_type/native_500m/{lc_year}.tif   -- full LC_Type1 classification
    numcycles_cropland/native_500m/{year}.tif   -- NumCycles, non-cropland
                                                    pixels (LC_Type1 != 12)
                                                    set to nodata

The land-cover mosaic is keyed by lc_year and reused across MCD12Q2 years
that map to the same lc_year (see LC_YEAR_OVERRIDE, e.g. 2024 reuses the
2023 land cover since MCD12Q1 stops at 2023) - if it already exists on disk,
MCD12Q1 is not re-downloaded; the cropland mask is instead read back from
the persisted native-resolution mosaic.

Stage B (last step): reproject the cropland-masked NumCycles native mosaic
to the LUH2 grid (resolution/extent taken from a reference LUH2 .nc file)
using area-weighted averaging over the underlying 500m cropland pixels
(GDAL Resampling.average, which ignores nodata / non-cropland pixels), then
round to the nearest integer and save as the final yearly output:

    OUTPUT_DIR/{year}.nc   -- variable NumCycles, on the LUH2 lat/lon grid

MCD12Q2 data availability: 2001 - 2024
MCD12Q1 data availability: 2001 - 2023 (2023 land cover reused for 2024, see LC_YEAR_OVERRIDE)
"""

import os
from pathlib import Path
from datetime import datetime

import earthaccess
import numpy as np
import xarray as xr
import rasterio
from rasterio.crs import CRS
from rasterio.transform import Affine
from rasterio.windows import Window
from rasterio.warp import reproject, Resampling
from pyhdf.SD import SD, SDC


START_YEAR = 2001
END_YEAR = 2024

# hard code
OUTPUT_DIR = "/dss/dssfs04/lwp-dss-0002/pn46de/pn46de-dss-0000/Data_Vroni/MODIS/land_cover/nc"
HDF_DIR = "/dss/dssfs04/lwp-dss-0002/pn46de/pn46de-dss-0000/Data_Vroni/MODIS/land_cover/hdf"
LC_HDF_DIR = "/dss/dssfs04/lwp-dss-0002/pn46de/pn46de-dss-0000/Data_Vroni/MODIS/land_cover_type/hdf"
LC_NATIVE_DIR = "/dss/dssfs04/lwp-dss-0002/pn46de/pn46de-dss-0000/Data_Vroni/MODIS/land_cover_type/native_500m"
NUMCYCLES_NATIVE_DIR = "/dss/dssfs04/lwp-dss-0002/pn46de/pn46de-dss-0000/Data_Vroni/MODIS/numcycles_cropland/native_500m"
KEEP_HDF = False
KEEP_LC_HDF = False

# TODO: confirm this path on the server - same reference file used in
# step0e_ndvi_cropping_intensity.py to define the LUH2 target grid.
LUH2_REFERENCE_PATH = "/dss/dssfs04/lwp-dss-0002/pn46de/pn46de-dss-0000/Data_Vroni/reference_raster.tif"

SHORT_NAME = "MCD12Q2"
VERSION = "061"
FILL_VALUE = 32767
NUMCYCLES_VAR = "NumCycles"

LC_SHORT_NAME = "MCD12Q1"
LC_VERSION = "061"
LC_VARIABLE = "LC_Type1"       # IGBP classification
LC_FILL_VALUE = 255            # MCD12Q1 unclassified/fill value
CROPLAND_CLASS = 12            # IGBP class 12 = Croplands
# MCD12Q1 is only produced through 2023; reuse 2023 land cover to mask 2024 MCD12Q2.
LC_YEAR_OVERRIDE = {2024: 2023}

TILES = None  # e.g. ["h08v05", "h09v05"]

# MODIS Sinusoidal grid geometry (native 500m tiles, no reprojection in Stage A)
EARTH_R = 6371007.181       # meters
TILE_SIZE = 1111950.0       # ~10 deg tile in meters
GLOBAL_XMIN = -20015109.354
GLOBAL_YMAX = 10007554.677
NPIX = 2400                 # pixels per tile (500m product)
PIXEL_SIZE = TILE_SIZE / NPIX
N_TILES_H = 36               # tile columns (h = 0..35)
N_TILES_V = 18               # tile rows (v = 0..17)
GLOBAL_WIDTH = N_TILES_H * NPIX
GLOBAL_HEIGHT = N_TILES_V * NPIX

MODIS_SINU_CRS = CRS.from_proj4(
    f"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a={EARTH_R} +b={EARTH_R} +units=m +no_defs"
)
GLOBAL_TRANSFORM = Affine(PIXEL_SIZE, 0.0, GLOBAL_XMIN, 0.0, -PIXEL_SIZE, GLOBAL_YMAX)

def parse_tile_id(filename):
    """Extract tile ID (e.g. 'h08v05') from filename."""
    parts = filename.split(".")
    for part in parts:
        if part.startswith("h") and "v" in part and len(part) == 6:
            return part
    return None


def tile_window(tile_id):
    """Pixel window (col_off, row_off, width, height) of a tile within the
    global MODIS Sinusoidal grid - exact placement, no interpolation."""
    h = int(tile_id[1:3])
    v = int(tile_id[4:6])
    return Window(col_off=h * NPIX, row_off=v * NPIX, width=NPIX, height=NPIX)


# HDF Reading
def read_numcycles(filepath):
    """Read the NumCycles SDS from a single MCD12Q2 HDF tile."""
    try:
        hdf = SD(str(filepath), SDC.READ)
        datasets = hdf.datasets()
        if NUMCYCLES_VAR not in datasets:
            del hdf
            return None
        sds = hdf.select(NUMCYCLES_VAR)
        arr = sds[:].astype(np.int16)
        del sds
        del hdf
        return arr
    except Exception as e:
        print(f"  [WARNING] Failed to read {filepath}: {e}")
        return None


def read_lc_type1(filepath):
    """Read the raw LC_Type1 SDS from a single MCD12Q1 HDF tile."""
    try:
        hdf = SD(str(filepath), SDC.READ)
        sds = hdf.select(LC_VARIABLE)
        lc = sds[:].astype(np.int16)
        del sds
        del hdf
        return lc
    except Exception as e:
        print(f"  [WARNING] Failed to read land cover {filepath}: {e}")
        return None


def search_and_download(short_name, version, year, hdf_dir, tiles_filter=None):
    """Search and download all granules for a product/year onto disk.

    Returns a sorted list of downloaded .hdf file paths (empty list if none).
    """
    print(f"  Searching {short_name} for {year}...")
    results = earthaccess.search_data(
        short_name=short_name,
        version=version,
        temporal=(f"{year}-01-01", f"{year}-12-31"),
    )

    if not results:
        print(f"  [WARNING] No granules found for {short_name} {year}")
        return []

    print(f"  Found {len(results)} {short_name} granule(s)")

    if tiles_filter is not None:
        filtered = []
        for r in results:
            links = r.data_links()
            name = os.path.basename(links[0]) if links else ""
            tid = parse_tile_id(name)
            if tid in tiles_filter:
                filtered.append(r)
        results = filtered
        print(f"  After tile filter: {len(results)} {short_name} granule(s)")

    if not results:
        return []

    hdf_dir.mkdir(parents=True, exist_ok=True)

    print(f"  Downloading {len(results)} {short_name} file(s)...")
    downloaded = earthaccess.download(results, str(hdf_dir))

    if not downloaded:
        print(f"  [WARNING] Download failed for {short_name} {year}")
        return []

    print(f"  Downloaded {len(downloaded)} {short_name} file(s)")
    return sorted(Path(hdf_dir).glob("*.hdf"))


def cleanup_hdf_dir(hdf_dir):
    for fpath in hdf_dir.glob("*.hdf"):
        fpath.unlink()
    for fpath in hdf_dir.glob("*.hdf.xml"):
        fpath.unlink()
    try:
        hdf_dir.rmdir()
    except OSError:
        pass


def create_native_raster(path, nodata):
    """Create an empty (sparse, nodata-filled) global MODIS-Sinusoidal
    GeoTIFF at 500m to be filled tile-by-tile with windowed writes."""
    path.parent.mkdir(parents=True, exist_ok=True)
    profile = {
        "driver": "GTiff",
        "dtype": "int16",
        "nodata": nodata,
        "width": GLOBAL_WIDTH,
        "height": GLOBAL_HEIGHT,
        "count": 1,
        "crs": MODIS_SINU_CRS,
        "transform": GLOBAL_TRANSFORM,
        "tiled": True,
        "blockxsize": NPIX,
        "blockysize": NPIX,
        "compress": "deflate",
        "predictor": 2,
        "sparse_ok": True,
        "BIGTIFF": "YES",
    }
    with rasterio.open(path, "w", **profile):
        pass


def read_tile_window(raster_path, tile_id):
    """Read a single tile's window back out of a persisted native mosaic."""
    with rasterio.open(raster_path) as src:
        return src.read(1, window=tile_window(tile_id))
      
# Stage A: build native-resolution (500m) mosaics
def build_native_mosaics(year, lc_year, hdf_files, lc_hdf_files):
    """Mosaic MCD12Q2 NumCycles (cropland-masked) and MCD12Q1 LC_Type1 into
    global 500m GeoTIFFs at their native MODIS Sinusoidal grid. Returns the
    path to the cropland-masked NumCycles native mosaic for `year`."""
    lc_native_path = Path(LC_NATIVE_DIR) / f"{lc_year}.tif"
    numcycles_native_path = Path(NUMCYCLES_NATIVE_DIR) / f"{year}.tif"

    if numcycles_native_path.exists():
        print(f"  [SKIP] Native NumCycles mosaic already exists: {numcycles_native_path}")
        return numcycles_native_path

    lc_native_exists = lc_native_path.exists()
    if lc_native_exists:
        print(f"  Reusing existing land-cover native mosaic: {lc_native_path}")
    else:
        if not lc_hdf_files:
            print(f"  [WARNING] No {LC_SHORT_NAME} tiles available to build land-cover mosaic for {lc_year}")
            return None
        create_native_raster(lc_native_path, LC_FILL_VALUE)

    lc_lookup = {}
    for fpath in lc_hdf_files:
        tid = parse_tile_id(fpath.name)
        if tid:
            lc_lookup[tid] = fpath

    create_native_raster(numcycles_native_path, FILL_VALUE)

    lc_dst = None if lc_native_exists else rasterio.open(lc_native_path, "r+")
    numcycles_dst = rasterio.open(numcycles_native_path, "r+")

    n_tiles = len(hdf_files)
    n_skipped_no_lc = 0
    try:
        for i, fpath in enumerate(hdf_files):
            tile_id = parse_tile_id(fpath.name)
            if tile_id is None:
                continue

            if (i + 1) % 50 == 0 or i == 0:
                print(f"  Mosaicking tile {i+1}/{n_tiles}: {tile_id}")

            # Get the cropland mask for this tile, building the land-cover
            # mosaic on the fly if it doesn't exist yet.
            if lc_native_exists:
                lc_arr = read_tile_window(lc_native_path, tile_id)
            else:
                lc_fpath = lc_lookup.get(tile_id)
                if lc_fpath is None:
                    n_skipped_no_lc += 1
                    continue
                lc_arr = read_lc_type1(lc_fpath)
                if lc_arr is None:
                    continue
                lc_dst.write(lc_arr, 1, window=tile_window(tile_id))

            cropland_mask = lc_arr == CROPLAND_CLASS

            numcycles_arr = read_numcycles(fpath)
            if numcycles_arr is None:
                continue

            valid = (numcycles_arr != FILL_VALUE) & cropland_mask
            numcycles_masked = np.where(valid, numcycles_arr, FILL_VALUE).astype(np.int16)
            numcycles_dst.write(numcycles_masked, 1, window=tile_window(tile_id))
    finally:
        if lc_dst is not None:
            lc_dst.close()
        numcycles_dst.close()

    if n_skipped_no_lc:
        print(f"  [WARNING] {n_skipped_no_lc} tile(s) skipped: no matching land-cover tile")

    print(f"  Native-resolution mosaicking complete.")
    return numcycles_native_path
  
# Stage B: resample the cropland-masked NumCycles native mosaic to LUH2 grid
def read_luh2_grid(luh2_path):
    """Read the LUH2 target grid from a reference GeoTIFF."""
    with rasterio.open(luh2_path) as src:
        lon = src.xy(0, np.arange(src.width), offset="center")[0]
        lat = src.xy(np.arange(src.height), 0, offset="center")[1]
        return src.transform, (src.height, src.width), src.crs,lon, lat 


def resample_numcycles_to_luh2(numcycles_native_path, dst_transform, dst_crs,dst_shape):
    """Reproject the native-resolution cropland-masked NumCycles mosaic to
    the LUH2 grid as the area-weighted average of the underlying 500m
    cropland pixels (non-cropland/invalid pixels are nodata and excluded)."""
    dst = np.full(dst_shape, float(FILL_VALUE), dtype=np.float64)
    with rasterio.open(numcycles_native_path) as src:
        reproject(
            source=rasterio.band(src, 1),
            destination=dst,
            src_transform=src.transform,
            src_crs=src.crs,
            src_nodata=FILL_VALUE,
            dst_transform=dst_transform,
            dst_crs=dst_crs,
            dst_nodata=float(FILL_VALUE),
            resampling=Resampling.average,
        )
        
    rounded = np.rint(dst)
    rounded[dst == FILL_VALUE] = FILL_VALUE
    return rounded.astype(np.int16)
  
  
# Main Processing
def process_year(year, luh2_lat, luh2_lon, luh2_transform, luh2_shape):
    output_path = Path(OUTPUT_DIR) / f"{year}.nc"

    if output_path.exists():
        print(f"[SKIP] {output_path} already exists.")
        return

    print(f"\n{'='*60}")
    print(f"Processing year {year}")
    print(f"{'='*60}")

    year_hdf_dir = Path(HDF_DIR) / str(year)
    hdf_files = search_and_download(SHORT_NAME, VERSION, year, year_hdf_dir, TILES)
    if not hdf_files:
        print(f"  [WARNING] No {SHORT_NAME} HDF files found for {year}")
        return

    lc_year = LC_YEAR_OVERRIDE.get(year, year)
    lc_native_path = Path(LC_NATIVE_DIR) / f"{lc_year}.tif"
    year_lc_hdf_dir = Path(LC_HDF_DIR) / str(lc_year)

    lc_hdf_files = []
    if not lc_native_path.exists():
        lc_hdf_files = search_and_download(LC_SHORT_NAME, LC_VERSION, lc_year, year_lc_hdf_dir, TILES)
        if not lc_hdf_files:
            print(f"  [WARNING] No {LC_SHORT_NAME} land-cover tiles found for {lc_year}; "
                  f"cannot build cropland mask, skipping year {year}.")
            return

    numcycles_native_path = build_native_mosaics(year, lc_year, hdf_files, lc_hdf_files)
    if numcycles_native_path is None:
        return

    print("  Resampling cropland-masked NumCycles to LUH2 grid...")
    numcycles_grid = resample_numcycles_to_luh2(numcycles_native_path, luh2_transform, luh2_crs, luh2_shape)

    # replace fill value by 0
    numcycles_grid_zero = numcycles_grid.copy()
    numcycles_grid_zero[numcycles_grid_zero == 32767] = 0

    ds = xr.Dataset(
        {
            "NumCycles": (
                ["lat", "lon"],
                numcycles_grid_zero,
                {
                    "long_name": "Number of valid vegetation cycles, cropland-masked area-weighted mean rounded to nearest integer",
                    "units": "count",
                    "original_name": "NumCycles",
                },
            ),
        },
        coords={
            "lat": ("lat", luh2_lat, {"units": "degrees_north", "long_name": "Latitude"}),
            "lon": ("lon", luh2_lon, {"units": "degrees_east", "long_name": "Longitude"}),
        },
        attrs={
            "title": f"MCD12Q2 NumCycles (MODIS), cropland-masked, resampled to LUH2 grid - {year}",
            "source": "NASA MODIS MCD12Q2.061, masked with MCD12Q1.061 LC_Type1 (IGBP)",
            "product": "Land Cover Dynamics Yearly L3 Global 500m SIN Grid (NumCycles only)",
            "cropland_mask": f"MCD12Q1.061 LC_Type1 == {CROPLAND_CLASS} (Croplands), year {lc_year}",
            "native_resolution_source": str(numcycles_native_path),
            "spatial_resolution": "LUH2 grid (reprojected from native 500m sinusoidal)",
            "temporal_resolution": "yearly",
            "year": year,
            "projection": "WGS84 geographic (EPSG:4326)",
            "reproject_method": "GDAL Resampling.average of native 500m cropland pixels per LUH2 cell",
            "created": datetime.now().isoformat(),
        },
    )

    encoding = {
        "NumCycles": {
            "dtype": "int16",
            "zlib": True,
            "complevel": 4,
            "_FillValue": np.int16(0),
        }
    }

    Path(OUTPUT_DIR).mkdir(parents=True, exist_ok=True)
    ds.to_netcdf(output_path, encoding=encoding)
    file_size = os.path.getsize(output_path) / 1e6
    print(f"  Saved: {output_path}  ({file_size:.1f} MB)")
    ds.close()
    del ds

    if not KEEP_HDF:
        cleanup_hdf_dir(year_hdf_dir)
    if not KEEP_LC_HDF and lc_hdf_files:
        cleanup_hdf_dir(year_lc_hdf_dir)


if __name__ == "__main__":
    print("Logging in to NASA Earthdata...")
    earthaccess.login()
    print("Login successful.\n")

    print(f"Reading LUH2 target grid from: {LUH2_REFERENCE_PATH}")
    luh2_transform, luh2_shape, luh2_crs, luh2_lon, luh2_lat = read_luh2_grid(LUH2_REFERENCE_PATH)

    print(f"Product:    {SHORT_NAME} v{VERSION} (NumCycles only), masked with "
          f"{LC_SHORT_NAME} v{LC_VERSION} (LC_Type1 == {CROPLAND_CLASS})")
    print(f"Years:      {START_YEAR} - {END_YEAR}")
    print(f"Tiles:      {'ALL' if TILES is None else TILES}")
    print(f"Native mosaics: {NUMCYCLES_NATIVE_DIR} , {LC_NATIVE_DIR}  (500m, MODIS Sinusoidal)")
    print(f"Final output:   {OUTPUT_DIR}/{{year}}.nc  (LUH2 grid, shape={luh2_shape})")
    print()

    for year in range(START_YEAR, END_YEAR + 1):
        try:
            process_year(year, luh2_lat, luh2_lon, luh2_transform, luh2_shape)
        except Exception as e:
            print(f"[ERROR] Failed to process year {year}: {e}")
            import traceback
            traceback.print_exc()
            continue

    print("\n" + "=" * 60)
    print("All done!")
    print(f"Output files in: {OUTPUT_DIR}")
    print("=" * 60)
