## @file 
# This script calculates biodiversity impacts based on CFs and ecoregions 
# use this after you have compiled the intensity datasets (seperately for each land use class) 

import geopandas as gpd
import rasterio
import numpy as np
import pandas as pd
from rasterio.mask import mask
import os
from shapely.geometry import box
from rasterio.features import geometry_mask
from joblib import Parallel, delayed
from scipy.ndimage import labeled_comprehension
from datetime import datetime
import csv

# Define paths to stative variables
dataset_used = "LUH2_GCB2025"
ecoreg_path = "H:/02_Projekte/allgemein_biodiversity_impact/02_data/ecoregions/wwf_terr_ecos.shp"
CF_path = "../literature/Scherer-et-al_2023/CF_domain.csv"
country_path = f"../data/04_bia_inputs/LUH2/country_raster.tif"
shpcountries_path = "H:/02_Projekte/allgemein_biodiversity_impact/02_data/country_shp/ne_110m_admin_0_countries.shp"
now = datetime.now().isoformat()
status_log_file = f"../data/03_intensity/{dataset_used}/status_log/status_log.csv"  
out_path = f"../output/biodiversity_impact_assessment/LUH2_GCB2025_rev_sensitivity/"


# Load CF data 
CFs = pd.read_csv(CF_path, sep=";", header=0).fillna(0)
ecoreg = gpd.read_file(ecoreg_path)
country_raster  = rasterio.open(country_path).read(1)
shpcountries = gpd.read_file(shpcountries_path)

# Prepare CFs for classification: CF_uni is a subset of CFs to get habitat IDs (e.g. cropland intense)
# Step 1: Create the initial subset
CF_uni = CFs[['habitat_id', 'habitat']].drop_duplicates().copy()
CF_uni['habitat'] = CF_uni['habitat'].fillna('').astype(str)
CF_uni['land_use'] = np.select([
    CF_uni['habitat'].str.contains("Cropland", case=False),
    CF_uni['habitat'].str.contains("Managed_forest", case=False),
    CF_uni['habitat'].str.contains("Pasture", case=False),
    CF_uni['habitat'].str.contains("Plantation", case=False),
    CF_uni['habitat'].str.contains("Urban", case=False)
], ['crops', 'forest', 'pasture', 'plantations', 'builtup'], default=None)
CF_uni['intensity'] = np.select([
    CF_uni['habitat'].str.contains("Intense", case=False),
    CF_uni['habitat'].str.contains("Light", case=False),
    CF_uni['habitat'].str.contains("Minimal", case=False)
], [3, 2, 1], default=None)
CF_uni = CF_uni.dropna(subset=['land_use', 'intensity'])

# Preload CF rasters, created in rasterize_CFs script
def load_CF_rasters(lu_type):
    """
    Load CF rasters for a given land use type and stack them into a 3D NumPy array.

    Parameters:
        lu_type (str): The land use type (e.g., "crops", "forest", etc.).

    Returns:
        np.ndarray: A 3D NumPy array where each layer corresponds to an intensity (1, 2, 3).
    """
    CF_rasters = []
    for intensity in [1, 2, 3]:
        # Get the habitat ID for the given land use type and intensity
        habitat_id = int(
            CF_uni.loc[
                (CF_uni['land_use'] == lu_type) & (CF_uni['intensity'] == intensity),
                'habitat_id'
            ].values[0]
        )
        
        # Load the CF raster and append it to the list
        CF_raster_path = f"../data/04_bia_inputs/LUH2/CF_raster/habitat_{habitat_id}.tif"
        with rasterio.open(CF_raster_path) as src:
            CF_rasters.append(src.read(1))

    # Stack the rasters into a 3D array and return
    return np.nan_to_num(np.stack(CF_rasters, axis=0), nan = 0)


## Function to calculate the biodiversity impact raster
def calculate_impact_raster(lu_type, year, CF_stack):

    """
    Calculates the biodiversity impact raster for a given land-use type and year
    and writes it to disk as a GeoTIFF.

    Parameters:
        lu_type (str): Land use type. Must be one of ["crops", "plantations", "pasture", "builtup", "forest", "rangeland"].
        year (int): Year of the data.
        CF_stack (np.ndarray): CF raster stack for intensity levels.

    Outputs:
        Writes a GeoTIFF file with impact data.
    """

    chunk_path = f"{chunk_folder}{lu_type}_intensity_{year}.tif"
    assert os.path.exists(chunk_path), f"Input file not found: {chunk_path}"

    valid_lu_types = ["crops", "plantations", "pasture", "builtup", "forest","rangeland"]
    assert lu_type in valid_lu_types, f"Invalid land use type: {lu_type}. Must be one of {valid_lu_types}"
    with rasterio.open(chunk_path) as src2:
        intensity_low = src2.read(1)
        intensity_med = src2.read(2)
        intensity_high = src2.read(3)
        profile = src2.profile



    # Vectorized calculation for all intensities
    intensity_stack = np.stack([intensity_low,intensity_med,intensity_high], axis=0)
    intensity_stack = np.nan_to_num(intensity_stack, nan=0)


    impact_stack = intensity_stack *  CF_stack

    # Define output path
    output_dir = os.path.join(out_path, lu_type)
    os.makedirs(output_dir, exist_ok=True)
    output_tif_filename = f"{output_dir}/{lu_type}_impact_{year}.tif"
    profile.update(count=impact_stack.shape[0], compress='deflate')

    with rasterio.open(output_tif_filename, 'w', **profile) as dst:
        i = 0
        for i in range(impact_stack.shape[0]):
            dst.write(impact_stack[i], i + 1)  # Write each intensity's impact to a separate band

    print(f"Created impact raster {output_tif_filename}")


## Function to calculate the per-country impact CSV from an existing impact raster
def calculate_impact_csv(lu_type, year, country_raster):

    """
    Reads the already-computed impact raster for a given land-use type and year,
    aggregates impact per country, and writes a CSV file (no impact recalculation).

    Parameters:
        lu_type (str): Land use type. Must be one of ["crops", "plantations", "pasture", "builtup", "forest", "rangeland"].
        year (int): Year of the data.
        country_raster (np.ndarray): Raster with country IDs.
    """

    output_tif_filename = f"{out_path}/{lu_type}/{lu_type}_impact_{year}.tif"
    assert os.path.exists(output_tif_filename), f"Impact raster not found: {output_tif_filename}"

    with rasterio.open(output_tif_filename) as src:
        impact_stack = src.read()  # shape: (intensity, H, W)

    print(f" impact_stack created for {lu_type} in {year}")






# Main execution
lu_types = [ "crops", "plantations"]
start_year = 2000
end_year = 2019
years = range(start_year, end_year+1)

for lu_type in lu_types:
    print(f"Processing land use type: {lu_type}")
    lu_type_CF = lu_type
    if lu_type == "rangeland":
        lu_type_CF = "pasture"  # Adjust for naming consistency
    CF_stack = load_CF_rasters(lu_type_CF) # always the same for every year
    chunk_folder = f"../data/03_intensity/{dataset_used}/{lu_type}_first_submission/"
    for year in years:
        print(year)
        calculate_impact_raster(lu_type, year, CF_stack)
        calculate_impact_csv(lu_type, year, country_raster)


