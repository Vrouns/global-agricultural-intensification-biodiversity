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
dataset_used = "LUH2"
ecoreg_path = "H:/02_Projekte/02_LUC biodiversity paper/02_data/ecoregions/wwf_terr_ecos.shp"
CF_path = "../literature/Scherer-et-al_2023/CF_domain.csv"
country_path = f"../data/04_bia_inputs/{dataset_used}/country_raster.tif"
shpcountries_path = "H:/02_Projekte/02_LUC biodiversity paper/02_data/country_shp/ne_110m_admin_0_countries.shp"
now = datetime.now().isoformat()
status_log_file = f"../data/03_intensity/{dataset_used}/status_log/status_log.csv"  
out_path = f"../output/biodiversity_impact_assessment/{dataset_used}/"


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
        CF_raster_path = f"../data/04_bia_inputs/{dataset_used}/CF_raster/habitat_{habitat_id}.tif"
        with rasterio.open(CF_raster_path) as src:
            CF_rasters.append(src.read(1))

    # Stack the rasters into a 3D array and return
    return np.nan_to_num(np.stack(CF_rasters, axis=0), nan = 0)


## Function to calculate biodiversity impact
def calculate_biodiversity_impact(lu_type, year, CF_stack, country_raster):

    """
    Calculates biodiversity impact for a given land-use type and year.

    Parameters:
        lu_type (str): Land use type. Must be one of ["crops", "plantations", "pasture", "builtup", "forest"].
        year (int): Year of the data.
        cell_areas (np.ndarray): Array of cell areas in square meters.
        CF_stack (np.ndarray): CF raster stack for intensity levels.
        country_raster (np.ndarray): Raster with country IDs.
        profile (dict): Metadata for output raster files.

    Outputs:
        Writes 
        1. a GeoTIFF file with impact data and 
        2. a CSV file with per-country impacts.
    """

    chunk_path = f"{chunk_folder}{lu_type}_intensity_{year}.tif"
    assert os.path.exists(chunk_path), f"Input file not found: {chunk_path}"

    valid_lu_types = ["crops", "plantations", "pasture", "builtup", "forest"]
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

    # Define output paths
    output_tif_filename = f"{out_path}/{lu_type}/{lu_type}_impact_{year}.tif"
    output_csv_filename = f"{out_path}/{lu_type}/{lu_type}_impact_{year}.csv"
    profile.update(count=impact_stack.shape[0], compress='deflate')
    
    with rasterio.open(output_tif_filename, 'w', **profile) as dst:
        i = 0
        for i in range(impact_stack.shape[0]):
            dst.write(impact_stack[i], i + 1)  # Write each intensity's impact to a separate band
    
    country_impacts = []

    # Calculate impact per country

    # Use advanced indexing to group pixel indices by country
    country_ids = np.unique(country_raster[country_raster > 0]) # Get unique country IDs (excluding 0)

    # Calculate impact sums for each country and intensity in a vectorized manner
    impact_per_country = np.array([
    impact_stack[:, country_raster == country_id].sum(axis=1)
    for country_id in country_ids])     

    # Create a DataFrame in one go
    country_names = shpcountries.GEOUNIT # Adjust index for 1-based country IDs
    data = {
        "country": np.repeat(country_names, 3),  # Each country repeated for the 3 intensity levels
        "impact_sum": impact_per_country.flatten(),  # Flatten impact values by intensity
        "intensity": np.tile([1, 2, 3], len(country_names))
    }

    impact_df = pd.DataFrame(data)
    impact_df.to_csv(output_csv_filename, mode='w', header=True, index=False)
    print(f"Created new CSV with impact {output_csv_filename}")

    filename = f"{lu_type}_intensity_{year}.tif"

    # Log status with timestamp
    # Ensure log file exists with header
    if not os.path.exists(status_log_file):
        with open(status_log_file, "w", newline="") as log:
            writer = csv.writer(log)
            writer.writerow(["filename", "area_status", "bia_status", "area_status_timestamp", "bia_status_timestamp"])

    # Bestehende Zeilen laden
    rows = []
    updated = False

    with open(status_log_file, "r", newline="") as log:
        reader = csv.DictReader(log)
        for row in reader:
            if row["filename"] == filename:
                row["bia_status"] = "processed"
                row["bia_status_timestamp"] = now
                updated = True
            rows.append(row)

    # Falls nicht gefunden → neue Zeile hinzufügen
    if not updated:
        rows.append({
            "filename": filename,
            "area_status": "open",
            "bia_status": "processed",
            "area_status_timestamp": now,
            "bia_status_timestamp": now
        })

    # Datei überschreiben
    with open(status_log_file, "w", newline="") as log:
        writer = csv.DictWriter(log, fieldnames=[
            "filename", "area_status", "bia_status",
            "area_status_timestamp", "bia_status_timestamp"
        ])
        writer.writeheader()
        writer.writerows(rows)




# Main execution
lu_types = [ "crops","plantations"]
start_year = 2000
end_year = 2019
years = range(start_year, end_year+1)

for lu_type in lu_types:
    print(f"Processing land use type: {lu_type}")
    CF_stack = load_CF_rasters(lu_type) # always the same for every year
    chunk_folder = f"../data/03_intensity/{dataset_used}/{lu_type}/"
    for year in years:
        print(year)
        calculate_biodiversity_impact(lu_type, year, CF_stack, country_raster)


