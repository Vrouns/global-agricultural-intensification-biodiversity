import rasterio
import numpy as np
import geopandas as gpd
import pandas as pd
import os
import netCDF4

# 1. Load change factor table
df = pd.read_csv("../data/05_crop_types/CG/FAOSTAT/FAOSTAT_codes/FAOSTAT_change_2000-2023.csv")

# 2. Load country code raster
with rasterio.open("../data/05_crop_types/CG/country_raster_CG.tif") as src:
    country_codes = src.read(1)
    code_profile = src.profile 

# Create output folder
os.makedirs("../data/05_crop_types/CG/time_series", exist_ok=True)

# 3. Create a blank raster for writing the metadata
with rasterio.open("../data/05_crop_types/CG/blanko_raster.tif") as src:
    blank_r = src.read()
    base_profile = src.profile
    transform = src.transform


# Get list of crop layers (e.g., order of crops = layer 0: wheat, 1: maize, etc.)
crop_layers = list(df["CROPGRIDS"].unique())  # You may need a fixed order list here

# Loop over years
for year in df["year"].unique():
    print(f"Processing year: {year}")
    
    # Get subset for this year
    year_df = df[df["year"] == year]
    
    
    for crop in crop_layers:
        out_path = f"../data/05_crop_types/CG/time_series/pred_{crop}_{year}.tif"
    
        # Skip if file already exists
        if os.path.exists(out_path):
            continue 
        if crop == "other": 
            continue
        if crop == "yautia":
            continue
        crop_df = year_df[year_df["CROPGRIDS"] == crop]
        base_path = f"../data/01_raw/CROPGRID/CROPGRIDSv1.08_NC_maps/CROPGRIDSv1.08_{crop}.nc"
        base_layer = netCDF4.Dataset(base_path)
        base_area = base_layer['croparea'][:]  # Safely load array
        base_layer.close()
        adjusted_layer = base_area.copy()

        for _, row in crop_df.iterrows():
            code = row["country_code"]
            factor = row["change_prop"]
            mask = country_codes == code
            adjusted_layer[mask] = base_area[mask] * factor
         # Save adjusted stack
        out_profile = base_profile.copy()

        with rasterio.open(out_path, "w", **out_profile) as dst:
            dst.write(adjusted_layer,1)


import xarray as xr

ds = xr.open_dataset(base_path)
base_area = ds['croparea'].values