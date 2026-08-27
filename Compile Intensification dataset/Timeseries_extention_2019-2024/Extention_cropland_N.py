"""
Extend Adalibieke et al. (2023, Scientific Data) gridded cropland N-application
grid from 2020 to 2024.

Method: same country-scaling idea as the pasture N extension
(pasture_N_extention.py) - hold the spatial pattern of the base year (2020,
the last year covered by Adalibieke et al.) fixed per pixel, and scale each
country's pixels by a national change_prop = value(year) / value(2020) taken
from FAOSTAT. Applied at Adalibieke's NATIVE resolution (per-crop rasters under
Adalbieke_2020_N_application/), not the LUH2-resampled 0.25 deg version, so
the country mask respects real (fine-resolution) borders. The extended native
layers are then resampled to the LUH2 grid using sum-aggregation, matching
exactly how the existing 2020 LUH2 layer (N_app_tot_2020_LUH2.tif) was itself
built (I_01_01_fertilizer_crop_intensity.R: project(..., method="sum")).

Not crop-specific: this collapses the 20 per-crop native rasters into one
total gridded N-application layer for 2020 (matching how the project's own
pipeline already sums them for the intensity classification step) and scales
that total. Extending each crop / fertilizer-type / placement separately
would require FUBC/IFASTAT data that isn't available/current past ~2018-2020.

Variable -> FAOSTAT proxy:
  total cropland N application -> FAOSTAT "Nutrient nitrogen N (total)" /
                                   "Agricultural Use" (Inputs: Fertilizers by
                                   Nutrient domain).

Caveat: this FAOSTAT series reflects synthetic (inorganic) fertilizer use.
Adalibieke et al.'s total N also includes manure and crop-residue N, which do
not necessarily track synthetic-fertilizer trends 1:1 (see how
pasture_N_extention.py scales nfer_pas_* and nmanure_*_pas separately with
different proxies). If manure/residue trends turn out to matter for your
results, FAOSTAT's "Cropland Nutrient Budget" domain (synthetic fertilizer,
manure applied, crop residues, atmospheric deposition, biological fixation
reported separately) would be a closer match, but currently only overlaps
the same 1961-2020 window as Adalibieke et al. itself, so it doesn't extend
the time series any further.

FAOSTAT coverage: as of the August 2026 download, "Agricultural Use" is
populated through 2024 for ~234/243 areas (barely less complete than
2018-2023), so no forward-fill was needed for the checked snapshot. The
forward-fill fallback below is kept in case a re-download has gaps.

Requires rasterio >= 1.2 (GDAL >= 3.1) for Resampling.sum.
"""
import glob
import os

import geopandas as gpd
import h5py
import numpy as np
import pandas as pd
import rasterio
from rasterio.enums import Resampling
from rasterio.features import rasterize
from rasterio.warp import reproject

PATH_PROJECT = "H:/02_Projekte/03_Intensification-fragmentation-CFs"
PATH_FAOSTAT = f"{PATH_PROJECT}/data/01_raw/Inputs_FertilizersNutrient_E_All_Data/Inputs_FertilizersNutrient_E_All_Data_NOFLAG.csv"
PATH_COUNTRY_SHP = "H:/02_Projekte/allgemein_biodiversity_impact/02_data/country_shp/ne_110m_admin_0_countries.shp"

# Per-crop native harvested area (ha), 1961-2020, used to convert the N_app_*_glob_*.tif
# rate rasters (kg/ha) into per-pixel N-application AMOUNT (kg) before summing across
# crops - see sum_native_crop_rasters() below for why this multiplication is required.
PATH_HARVEST_H5 = "D:/03_Intensification-fragmentation-CFs/data/01_raw/Adalibieke_fertilizer_crops/Harvested_area_1961-2020.h5"

# 20 per-crop N-application rasters at Adalibieke's native resolution (~5
# arc-min), 2020, plus the already-reprojected LUH2 total - all confirmed to
# live in the same folder (verified via directory listing, not the
# 01_raw/Adalibieke_fertilizer_crops path guessed earlier - that one only
# has native totals through 2019, no 2020).
PATH_NATIVE_CROP_DIR = "D:/03_Intensification-fragmentation-CFs/data/02_resampled/LUH2/Adalibieke_fertilizer_crops/Adalbieke_2020_N_application"

# Existing 2020 total, already reprojected to the LUH2 grid - used only as
# the target-grid reference for resampling (shape/transform/crs), and as a
# sanity check that resampling the native sum ourselves reproduces it.
PATH_LUH2_2020_REFERENCE = f"{PATH_NATIVE_CROP_DIR}/N_app_tot_2020_LUH2.tif"

OUT_DIR = f"{PATH_PROJECT}/data/02_b_extention_datasets"

ITEM = "Nutrient nitrogen N (total)"
ELEMENT = "Agricultural Use"
BASE_YEAR = 2020
TARGET_YEARS = range(2021, 2025)

# Glob pattern that matches only the 20 per-crop native files in
# PATH_NATIVE_CROP_DIR, excluding N_app_tot_2020_LUH2.tif itself (which
# lives in the same folder but is already on the coarser LUH2 grid).
NATIVE_CROP_GLOB = f"N_app_*_glob_{BASE_YEAR}.tif"

# used later to describe native and luh2 target grids (shape, transform, crs) for reprojection/resampling
def get_grid(raster_path):
    with rasterio.open(raster_path) as src:
        return {
            "shape": (src.height, src.width),
            "transform": src.transform,
            "crs": src.crs,
        }


def sum_native_crop_rasters(crop_dir, pattern=NATIVE_CROP_GLOB):
    """Sum every per-crop raster in crop_dir into one native-resolution total-N layer.
    Uses `pattern` (default: N_app_*_glob_<BASE_YEAR>.tif) rather than a bare *.tif so
    it doesn't pick up N_app_tot_2020_LUH2.tif, which lives in the same folder but is
    already on the coarser LUH2 grid (different shape - would break the sum)."""
    files = sorted(glob.glob(os.path.join(crop_dir, pattern)))
    if not files:
        raise FileNotFoundError(f"No files matching {pattern!r} found in {crop_dir}")
    print(f"Summing {len(files)} native crop rasters from {crop_dir}:")
    for f in files:
        print(f"  {os.path.basename(f)}")

    with rasterio.open(files[0]) as src0:
        total = np.zeros((src0.height, src0.width), dtype="float64")
        profile = src0.profile
        grid = {"shape": (src0.height, src0.width), "transform": src0.transform, "crs": src0.crs}

    for f in files:
        with rasterio.open(f) as src:
            arr = src.read(1, masked=True).filled(0).astype("float64")
            if arr.shape != total.shape:
                raise ValueError(f"{f} has shape {arr.shape}, expected {total.shape} - grids don't match")
            total += arr

    return total.astype("float32"), profile, grid


def build_country_raster(grid, cache_tag):
    """Rasterize the project's standard country shapefile onto `grid` (cached per cache_tag)."""
    raster_path = os.path.join(OUT_DIR, f"country_raster_cropland_N_{cache_tag}.tif")
    lookup_path = os.path.join(OUT_DIR, f"country_lookup_cropland_N_{cache_tag}.csv")
    if os.path.exists(raster_path) and os.path.exists(lookup_path):
        with rasterio.open(raster_path) as src:
            country_codes = src.read(1)
        lookup = pd.read_csv(lookup_path, dtype={"un_m49": str})
        return country_codes, lookup

    countries = gpd.read_file(PATH_COUNTRY_SHP)
    countries["country_code"] = np.arange(1, len(countries) + 1)

    shapes = list(zip(countries.geometry, countries["country_code"]))
    country_codes = rasterize(
        shapes, out_shape=grid["shape"], transform=grid["transform"], fill=0, dtype="int32"
    )

    with rasterio.open(
        raster_path, "w", driver="GTiff",
        height=grid["shape"][0], width=grid["shape"][1], count=1,
        dtype="int32", crs=grid["crs"], transform=grid["transform"],
    ) as dst:
        dst.write(country_codes, 1)

    lookup = countries[["country_code", "NAME", "ISO_A3", "UN_A3"]].rename(
        columns={"NAME": "name", "ISO_A3": "iso3", "UN_A3": "un_m49"}
    )
    lookup["un_m49"] = lookup["un_m49"].astype(str)
    lookup.to_csv(lookup_path, index=False)
    return country_codes, lookup


def load_faostat_wide(path):
    """Filter to N/Agricultural Use, drop regional aggregates (Area Code >= 5000),
    and return a tidy (un_m49 x year) wide table plus the raw filtered frame."""
    df = pd.read_csv(path, encoding="latin-1")
    df = df[(df["Item"] == ITEM) & (df["Element"] == ELEMENT) & (df["Area Code"] < 5000)].copy()

    df["un_m49"] = (
        df["Area Code (M49)"].astype(str).str.lstrip("'").str.strip().str.zfill(3)
    )

    year_cols = [c for c in df.columns if c.startswith("Y") and c[1:].isdigit()]
    long = df.melt(id_vars=["un_m49", "Area"], value_vars=year_cols,
                    var_name="year", value_name="value")
    long["year"] = long["year"].str[1:].astype(int)
    long["value"] = pd.to_numeric(long["value"], errors="coerce")

    wide = long.pivot_table(index="un_m49", columns="year", values="value", aggfunc="sum")
    return wide, df[["un_m49", "Area"]].drop_duplicates()


def change_prop_by_year(wide, lookup, base_year=BASE_YEAR, target_years=TARGET_YEARS):
    """From a (un_m49 x year) value table, build {year: {country_code: change_prop}}
    plus a tidy long-format table for inspection/export."""
    if base_year not in wide.columns:
        raise ValueError(f"Base year {base_year} missing from FAOSTAT table")
    base = wide[base_year]
    available_years = sorted(wide.columns)

    props_by_year = {}
    records = []
    for year in target_years:
        use_year = year if year in wide.columns else max(y for y in available_years if y <= year)
        if use_year != year:
            print(f"  note: FAOSTAT N data missing for {year}, forward-filling from {use_year}")

        prop = (wide[use_year] / base).replace([np.inf, -np.inf], np.nan)
        prop = prop.where(base > 0, other=1.0)  # no 2020 baseline use -> hold constant
        prop = prop.fillna(1.0)

        merged = lookup.merge(prop.rename("change_prop"), left_on="un_m49", right_index=True, how="left")
        merged["change_prop"] = merged["change_prop"].fillna(1.0)
        merged["year"] = year
        merged["source_year_used"] = use_year
        records.append(merged[["un_m49", "name", "year", "source_year_used", "change_prop", "country_code"]])

        props_by_year[year] = dict(zip(merged["country_code"], merged["change_prop"]))

    ratio_table = pd.concat(records, ignore_index=True)
    return props_by_year, ratio_table


def fertilizer_change_prop(country_lookup):
    """country_lookup must contain columns country_code + un_m49 (from build_country_raster)."""
    wide, _ = load_faostat_wide(PATH_FAOSTAT)
    return change_prop_by_year(wide, country_lookup)


def apply_change_prop(base_layer, country_codes, factors_for_year):
    factor_grid = np.ones_like(base_layer, dtype="float32")
    for code, factor in factors_for_year.items():
        if code == 0:
            continue
        factor_grid[country_codes == code] = factor
    return base_layer * factor_grid


def resample_sum(src_array, src_grid, dst_grid):
    """Reproject src_array onto dst_grid using sum-aggregation, mirroring
    terra::project(..., method="sum") used to build the LUH2 base layer."""
    dst_array = np.zeros(dst_grid["shape"], dtype="float32")
    reproject(
        source=src_array,
        destination=dst_array,
        src_transform=src_grid["transform"],
        src_crs=src_grid["crs"],
        dst_transform=dst_grid["transform"],
        dst_crs=dst_grid["crs"],
        resampling=Resampling.sum,
    )
    return dst_array


def sanity_check_2020(native_total_2020, native_grid, luh2_grid):
    """Resample our own native-2020 sum to the LUH2 grid and compare its
    global total against the existing N_app_tot_2020_LUH2.tif, as a check
    that the native crop-summing + resampling method used here reproduces
    the project's own 2020 layer before trusting it for 2021-2024."""
    resampled = resample_sum(native_total_2020, native_grid, luh2_grid)
    with rasterio.open(PATH_LUH2_2020_REFERENCE) as src:
        existing = src.read(1, masked=True).filled(0).astype("float32")

    total_ours = float(np.nansum(resampled))
    total_existing = float(np.nansum(existing))
    pct_diff = 100 * (total_ours - total_existing) / total_existing if total_existing else float("nan")
    print(f"Sanity check vs. existing 2020 LUH2 layer:")
    print(f"  our resampled-native-sum total : {total_ours:,.0f}")
    print(f"  existing N_app_tot_2020_LUH2   : {total_existing:,.0f}")
    print(f"  difference                     : {pct_diff:+.2f}%")
    if abs(pct_diff) > 2:
        print("  WARNING: >2% difference - check units/nodata handling before trusting 2021-2024 output.")


if __name__ == "__main__":
    os.makedirs(OUT_DIR, exist_ok=True)

    for p in (PATH_NATIVE_CROP_DIR, PATH_LUH2_2020_REFERENCE, PATH_COUNTRY_SHP):
        if not os.path.exists(p):
            raise FileNotFoundError(f"Expected path not found: {p}")

    # 1) native-resolution 2020 total (sum of the 20 crop rasters)
    native_total_2020, native_profile, native_grid = sum_native_crop_rasters(PATH_NATIVE_CROP_DIR)

    # 2) country mask at native resolution (accurate borders)
    country_codes, lookup = build_country_raster(native_grid, cache_tag="native")

    # 3) FAOSTAT change ratios, 2021-2024 vs. 2020
    print("Filtering FAOSTAT and computing per-country change ratios (2021-2024 vs. 2020)...")
    fert_props, ratio_table = fertilizer_change_prop(lookup)
    ratio_csv_path = os.path.join(OUT_DIR, "cropland_N_change_prop_2021_2024.csv")
    ratio_table.to_csv(ratio_csv_path, index=False)
    print(f"Saved change-ratio table: {ratio_csv_path}")

    # 4) sanity check: does resampling our native 2020 sum reproduce the existing LUH2 2020 layer?
    luh2_grid = get_grid(PATH_LUH2_2020_REFERENCE)
    sanity_check_2020(native_total_2020, native_grid, luh2_grid)

    # 5) extend at native resolution, then resample each year to the LUH2 grid
    with rasterio.open(PATH_LUH2_2020_REFERENCE) as ref:
        luh2_profile = ref.profile

    for year in TARGET_YEARS:
        native_extended = apply_change_prop(native_total_2020, country_codes, fert_props[year])

        native_out = os.path.join(OUT_DIR, f"N_total_native_{year}.tif")
        prof = native_profile.copy()
        prof.update(dtype="float32", compress="lzw", count=1)
        with rasterio.open(native_out, "w", **prof) as dst:
            dst.write(native_extended, 1)

        luh2_extended = resample_sum(native_extended, native_grid, luh2_grid)
        luh2_out = os.path.join(OUT_DIR, f"N_total_LUH2_{year}.tif")
        prof = luh2_profile.copy()
        prof.update(dtype="float32", compress="lzw", count=1)
        with rasterio.open(luh2_out, "w", **prof) as dst:
            dst.write(luh2_extended, 1)

        print(f"{year}: saved {native_out} and {luh2_out}")
