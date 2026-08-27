"""
Extend Tian et al. (2022, ESSD) pasture and rangeland N-input grids from 2019 to 2024.

Holds the 2019 spatial pattern fixed per pixel and scales each country by a national
change_prop = value(year) / value(2019) taken from FAOSTAT. Mirrors the country-scaling
approach used for the CROPGRIDS timeseries extension
(I_01_02_2_Compile_CROPGRIDS_FAOSTAT-timeseries.py).

Variable -> FAOSTAT proxy:
  nfer_pas_nh4, nfer_pas_no3   -> total agricultural N fertilizer use (FAOSTAT RFN,
                                   "Nutrient nitrogen N (total)" / "Agricultural Use").
                                   Only the relative year-to-year change is used, not
                                   pasture's absolute share of fertilizer.
  nmanure_app_pas               -> FAOSTAT "Manure applied to soils (N content)"
  nmanure_dep_pas + nmanure_dep_range
                                 -> jointly mass-balanced against FAOSTAT "Manure left
                                    on pasture (N content)", see below.
  (all manure proxies summed over Cattle, Buffalo, Sheep, Goats)

Manure deposition (grazing): Tian et al. grid this from a single national FAO total,
split within each grid cell by assuming pasture deposition intensity is twice that of
rangeland (one national total, two land classes). Scaling both variables by the usual
FAOSTAT[year]/FAOSTAT[2019] ratio would preserve the 2:1 split only relatively, since
it assumes Tian's 2019 grid already sums to FAOSTAT's 2019 value per country, which
doesn't hold exactly (different country-boundary source, FAOSTAT revisions since
Tian's 2022 vintage). 
Instead the change_prop for these two variables is
FAOSTAT[year] / (Tian's 2019 grid sum of pasture+rangeland, per country), so the
extended country totals match FAOSTAT directly. The same factor is applied to both
variables to keep the 2:1 spatial split intact.

FAOSTAT manure data (https://www.fao.org/faostat/en/#data/EMN) ends at 2023, so 2024 uses the 2023 change_prop.
"""
import os
import numpy as np
import pandas as pd
import netCDF4 as nc
import rasterio
from rasterio.transform import from_origin
from rasterio.features import rasterize
import geopandas as gpd

PATH_TIAN = "D:/03_Intensification-fragmentation-CFs/data/01_raw/Tian_pasture_fertilizer"
PATH_FERTILIZER_FAOSTAT = "H:/02_Projekte/04_Intensification_TS_expansion/data/trainings_data/01_raw/FAOSTAT_fertilization.csv"
PATH_MANURE_FAOSTAT = "H:/02_Projekte/04_Intensification_TS_expansion/data/trainings_data/01_raw/Environment_LivestockManure_E_All_Data/Environment_LivestockManure_E_All_Data_NOFLAG.csv"
PATH_COUNTRY_SHP = "H:/02_Projekte/allgemein_biodiversity_impact/02_data/country_shp/ne_110m_admin_0_countries.shp"

OUT_DIR = os.path.join(PATH_TIAN, "extended_2020-2024")

BASE_YEAR = 2019
TARGET_YEARS = range(2020, 2025)
GRAZERS = ["Cattle", "Buffalo", "Sheep", "Goats"]

MANURE_ELEMENT_MAP = {
    "nmanure_app_pas": "Manure applied to soils (N content)",
    "nmanure_dep_pas": "Manure left on pasture (N content)",
}

# variable -> (nc filename, nc variable name, first year covered by the nc file's time axis)
TIAN_VARS = {
    "nfer_pas_nh4": ("nfer_pas_nh4.nc", "nfer_pas_nh4", 1961),
    "nfer_pas_no3": ("nfer_pas_no3.nc", "nfer_pas_no3", 1961),
    "nmanure_app_pas": ("nmanure_app_pas.nc", "nmanure_app_pas", 1860),
    "nmanure_dep_pas": ("nmanure_dep_pas.nc", "nmanure_dep_pas", 1860),
    "nmanure_dep_range": ("nmanure_dep_range/nmanure_dep_range.nc", "nmanure_dep_range", 1860),
}

GRID_SHAPE = (2160, 4320)  # (lat, lon) at Tian's native 5 arc-min resolution
GRID_TRANSFORM = from_origin(-180, 90, 1 / 12, 1 / 12)
GRID_CRS = "EPSG:4326"


def build_country_raster():
    """Rasterize the standard country shapefile onto Tian's native grid."""
    raster_path = os.path.join(PATH_TIAN, "country_raster_tian.tif")
    lookup_path = os.path.join(PATH_TIAN, "country_lookup_tian.csv")
    if os.path.exists(raster_path) and os.path.exists(lookup_path):
        with rasterio.open(raster_path) as src:
            country_codes = src.read(1)
        lookup = pd.read_csv(lookup_path, dtype={"un_m49": str})
        return country_codes, lookup

    countries = gpd.read_file(PATH_COUNTRY_SHP)
    countries["country_code"] = np.arange(1, len(countries) + 1)

    shapes = list(zip(countries.geometry, countries["country_code"]))
    country_codes = rasterize(
        shapes, out_shape=GRID_SHAPE, transform=GRID_TRANSFORM, fill=0, dtype="int32"
    )

    with rasterio.open(
        raster_path, "w", driver="GTiff",
        height=GRID_SHAPE[0], width=GRID_SHAPE[1], count=1,
        dtype="int32", crs=GRID_CRS, transform=GRID_TRANSFORM,
    ) as dst:
        dst.write(country_codes, 1)

    lookup = countries[["country_code", "NAME", "ISO_A3", "UN_A3"]].rename(
        columns={"NAME": "name", "ISO_A3": "iso3", "UN_A3": "un_m49"}
    )
    lookup["un_m49"] = lookup["un_m49"].astype(str)
    lookup.to_csv(lookup_path, index=False)
    return country_codes, lookup


def change_prop_by_year(wide, lookup):
    """From a (un_m49 x year) value table, build {year: {country_code: change_prop}}."""
    if BASE_YEAR not in wide.columns:
        raise ValueError(f"Base year {BASE_YEAR} missing from FAOSTAT table")
    base = wide[BASE_YEAR]
    available_years = sorted(wide.columns)

    props_by_year = {}
    for year in TARGET_YEARS:
        use_year = year if year in wide.columns else max(y for y in available_years if y <= year)
        if use_year != year:
            print(f"  note: FAOSTAT data missing for {year}, forward-filling from {use_year}")

        prop = (wide[use_year] / base).replace([np.inf, -np.inf], np.nan)
        prop = prop.where(base > 0, other=1.0)  # no baseline use -> hold constant
        prop = prop.fillna(1.0)

        merged = lookup.merge(prop.rename("change_prop"), left_on="un_m49", right_index=True, how="left")
        merged["change_prop"] = merged["change_prop"].fillna(1.0)
        props_by_year[year] = dict(zip(merged["country_code"], merged["change_prop"]))
    return props_by_year


def fertilizer_change_prop(lookup):
    df = pd.read_csv(PATH_FERTILIZER_FAOSTAT, encoding="latin-1")
    df = df[(df["Item"] == "Nutrient nitrogen N (total)") & (df["Element"] == "Agricultural Use")]
    df["un_m49"] = df["Area Code (M49)"].astype(str).str.zfill(3)

    wide = df.pivot_table(index="un_m49", columns="Year", values="Value", aggfunc="sum")
    return change_prop_by_year(wide, lookup)


def manure_wide_kg(manure_df, element):
    """(un_m49 x year) FAOSTAT table for `element`, summed over GRAZERS. Values are kg N."""
    df = manure_df[(manure_df["Item"].isin(GRAZERS)) & (manure_df["Element"] == element)]
    year_cols = [c for c in df.columns if c.startswith("Y") and c[1:].isdigit()]

    long = df.melt(id_vars=["un_m49"], value_vars=year_cols, var_name="year", value_name="value")
    long["year"] = long["year"].str[1:].astype(int)
    long = long.groupby(["un_m49", "year"], as_index=False)["value"].sum()

    return long.pivot(index="un_m49", columns="year", values="value")


def manure_change_prop(manure_df, element, lookup):
    wide = manure_wide_kg(manure_df, element)
    return change_prop_by_year(wide, lookup)


def zonal_country_sums(grid, country_codes):
    flat_codes = country_codes.ravel().astype(np.int64)
    sums = np.bincount(flat_codes, weights=grid.ravel().astype("float64"))
    return {code: sums[code] for code in np.unique(flat_codes) if code != 0}


def mass_balanced_change_prop(wide_kg, lookup, base_totals_kg_by_code):
    """Like change_prop_by_year, but scales to FAOSTAT[year] / (Tian's 2019 N-
    total per country) instead of FAOSTAT[year] / FAOSTAT[2019], so the extended
    country sum matches the FAOSTAT total directly (see module docstring)."""
    available_years = sorted(wide_kg.columns)

    props_by_year = {}
    for year in TARGET_YEARS:
        use_year = year if year in wide_kg.columns else max(y for y in available_years if y <= year)
        if use_year != year:
            print(f"  note: FAOSTAT data missing for {year}, forward-filling from {use_year}")

        merged = lookup.merge(wide_kg[use_year].rename("faostat_target_kg"), left_on="un_m49", right_index=True, how="left")
        merged["tian_base_kg"] = merged["country_code"].map(base_totals_kg_by_code)

        prop = merged["faostat_target_kg"] / merged["tian_base_kg"]
        prop = prop.replace([np.inf, -np.inf], np.nan)
        prop = prop.where(merged["tian_base_kg"] > 0, other=1.0)  # no Tian baseline -> hold constant
        merged["change_prop"] = prop.fillna(1.0)  # no FAOSTAT figure -> hold constant

        props_by_year[year] = dict(zip(merged["country_code"], merged["change_prop"]))
    return props_by_year


def load_base_layer(var_name):
    """Load the BASE_YEAR layer with non-grazing/ocean cells zeroed out.

    The Tian .nc files store those cells as literal NaN rather than a masked
    _FillValue, so np.ma.filled alone is a no-op - mask NaN/inf explicitly first.
    """
    nc_file, nc_var, time_origin = TIAN_VARS[var_name]
    ds = nc.Dataset(os.path.join(PATH_TIAN, nc_file))
    base_layer = ds.variables[nc_var][BASE_YEAR - time_origin, :, :]
    ds.close()
    base_layer = np.ma.filled(np.ma.masked_invalid(base_layer), 0.0).astype("float32")
    return base_layer


def extend_variable(var_name, country_codes, props_by_year, base_layer=None):
    print(f"Processing {var_name}")
    if base_layer is None:
        base_layer = load_base_layer(var_name)

    for year in TARGET_YEARS:
        out_path = os.path.join(OUT_DIR, f"{var_name}_{year}.tif")
        if os.path.exists(out_path):
            continue

        factor_grid = np.ones_like(base_layer, dtype="float32")
        for code, factor in props_by_year[year].items():
            if code == 0:
                continue
            factor_grid[country_codes == code] = factor

        with rasterio.open(
            out_path, "w", driver="GTiff",
            height=GRID_SHAPE[0], width=GRID_SHAPE[1], count=1,
            dtype="float32", crs=GRID_CRS, transform=GRID_TRANSFORM,
            compress="lzw",
        ) as dst:
            dst.write(base_layer * factor_grid, 1)
        print(f"  saved {out_path}")


if __name__ == "__main__":
    os.makedirs(OUT_DIR, exist_ok=True)
    country_codes, lookup = build_country_raster()

    fert_props = fertilizer_change_prop(lookup)

    manure_df = pd.read_csv(PATH_MANURE_FAOSTAT, encoding="latin-1")
    manure_df["un_m49"] = manure_df["Area Code (M49)"].astype(str).str.lstrip("'").str.zfill(3)
    manure_app_props = manure_change_prop(manure_df, MANURE_ELEMENT_MAP["nmanure_app_pas"], lookup)

    # manure deposition: mass-balance pasture + rangeland jointly against FAOSTAT
    dep_pas_base = load_base_layer("nmanure_dep_pas")
    dep_range_base = load_base_layer("nmanure_dep_range")
    dep_base_total_kg = zonal_country_sums(dep_pas_base + dep_range_base, country_codes)
    dep_base_total_kg = {code: total / 1000.0 for code, total in dep_base_total_kg.items()}  # g N -> kg N

    manure_dep_wide_kg = manure_wide_kg(manure_df, MANURE_ELEMENT_MAP["nmanure_dep_pas"])
    manure_dep_props = mass_balanced_change_prop(manure_dep_wide_kg, lookup, dep_base_total_kg)

    extend_variable("nfer_pas_nh4", country_codes, fert_props)
    extend_variable("nfer_pas_no3", country_codes, fert_props)
    extend_variable("nmanure_app_pas", country_codes, manure_app_props)
    extend_variable("nmanure_dep_pas", country_codes, manure_dep_props, base_layer=dep_pas_base)
    extend_variable("nmanure_dep_range", country_codes, manure_dep_props, base_layer=dep_range_base)