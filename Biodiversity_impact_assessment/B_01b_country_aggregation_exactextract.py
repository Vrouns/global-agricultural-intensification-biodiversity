## @file
# Aggregates biodiversity impact rasters (produced by B_01_biodiversity_impact_assesment_LUH2.py)
# to country totals using fractional-coverage zonal statistics (exactextract),
# replacing the country_raster.tif hard-classification previously used in calculate_impact_csv().
# Run this after the impact rasters (*_impact_{year}.tif) already exist - it does not recompute them.

import os
import geopandas as gpd
from exactextract import exact_extract
os.chdir("H:/02_Projekte/03_Intensification-fragmentation-CFs/")
dataset_used = "LUH2_GCB2025_rev_sensitivity"
shpcountries_path = "H:/02_Projekte/allgemein_biodiversity_impact/02_data/country_shp/ne_110m_admin_0_countries.shp"
out_path = f"output/biodiversity_impact_assessment/{dataset_used}/"

lu_types = ["crops", "plantations"]#, "pasture", "rangeland", "abandoned"]
start_year = 2000
end_year = 2024

shpcountries = gpd.read_file(shpcountries_path)

for lu_type in lu_types:
    print(f"Processing land use type: {lu_type}")
    for year in range(start_year, end_year + 1):
        raster_path = f"{out_path}{lu_type}/{lu_type}_impact_{year}.tif"
        if not os.path.exists(raster_path):
            print(f"  Skipping missing raster: {raster_path}")
            continue

        result = exact_extract(
            raster_path, shpcountries, ["sum"],
            include_cols=["GEOUNIT"], output="pandas"
        )

        # one row per country with band_1_sum/band_2_sum/band_3_sum (intensity 1/2/3)
        # -> long format matching the previous CSV: country, impact_sum, intensity
        impact_df = result.melt(id_vars="GEOUNIT", var_name="band", value_name="impact_sum")
        # abandoned has only one band, so handle differently: 
        if lu_type == "abandoned":
            impact_df["intensity"] = 1
        else:
            impact_df["intensity"] = impact_df["band"].str.extract(r"band_(\d+)_sum").astype(int)
        impact_df = (
            impact_df.rename(columns={"GEOUNIT": "country"})[["country", "impact_sum", "intensity"]]
            .sort_values(["country", "intensity"])
            .reset_index(drop=True)
        )

        output_csv_filename = f"{out_path}{lu_type}/{lu_type}_impact_{year}.csv"
        impact_df.to_csv(output_csv_filename, mode="w", header=True, index=False)
        print(f"  Wrote {output_csv_filename}")
