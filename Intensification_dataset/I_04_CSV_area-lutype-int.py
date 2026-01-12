import rasterio 
import geopandas as gpd 
import numpy as np
import pandas as pd
import os
import glob
from concurrent.futures import ProcessPoolExecutor
from datetime import datetime
import csv


# intensity per country 
country_shapes = gpd.read_file("H:/02_Projekte/02_LUC biodiversity paper/02_data/country_shp/ne_110m_admin_0_countries.shp")
# area_file = "../data/04_bia_inputs/area_in_m2.tif"
country_path = "../data/04_bia_inputs/LUH2/country_raster.tif"
country_raster  = rasterio.open(country_path).read(1)
#area_path = "../data/04_bia_inputs/area_m2_sdpt/"
#cell_areas = rasterio.open(area_file).read(1)



def calculate_lu_intensity_per_country (input_folder, output_folder, status_log_file,country_raster,country_shapes): 
    """
    Calculate land-use intensity per country and save results to a CSV.
    
    Args:
        input_folder (str): Path to the folder containing the intensity files.
        output_folder (str): Path to the folder where CSV outputs will be saved.
        processed_folder (str): Path to the folder where processed intensity files will be moved.
        cell_areas (np.array): 2D array with the area of each raster cell.
        country_raster (np.array): 2D array where each cell has a country ID.
        country_shapes (pd.DataFrame): DataFrame with country metadata (columns: GEOUNIT, SOV_A3).
    """

        # Ensure output and processed folders exist
    os.makedirs(output_folder, exist_ok=True)

    # Process each file in the input folder
    for filename in os.listdir(input_folder):

        if filename.endswith(".tif"):  # Only process GeoTIFF files
            file_path = os.path.join(input_folder, filename)
        try:
        # Extract information from the filename (e.g., land-use type and year)
            parts = filename.split("_")  # Example: "crops_intensity_2000.tif"
            if len(parts) > 4:
                raise (f"Skipping file with unexpected format: {filename}")
            

            lu_type = parts[0]  # Land-use type
            year = parts[2].split(".")[0]  # Year (removing ".tif")

            # Read intensity raster
            with rasterio.open(file_path) as src2:
                intensity_low = src2.read(1)
                intensity_med = src2.read(2)
                intensity_high = src2.read(3)
                profile = src2.profile

            # Vectorized calculation for all intensities
            intensity_stack = np.stack([intensity_low,intensity_med,intensity_high], axis=0)
            intensity_stack = np.nan_to_num(intensity_stack, nan=0)

        # Use advanced indexing to group pixel indices by country
            country_ids = np.unique(country_raster[country_raster > 0]) # Get unique country IDs (excluding 0)

            # Calculate intensity for each country and intensity 

            impact_per_country = np.zeros((len(country_ids), 3), dtype=np.float32)  # Shape: (countries x intensities)

    # loop over countries
            for idx, country_id in enumerate(country_ids):
                country_mask = country_raster == country_id  # shape: (H, W)
                for b in range(intensity_stack.shape[0]):  # Loop over bands
                    impact_per_country[idx, b] = intensity_stack[b][country_mask].sum()


        # Create a DataFrame in one go
            country_names = country_shapes.GEOUNIT # Adjust index for 1-based country IDs
            SOV_A3 = country_shapes.SOV_A3
            data = {
                    "GEOUNIT": np.repeat(country_names, 3),  # Each country repeated for the 3 intensity levels
                    "SOV": np.repeat(SOV_A3, 3),
                    "area_ha": impact_per_country.flatten(),  # Flatten impact values by intensity
                    "intensity": np.tile([1, 2, 3], len(country_names))}

            impact_df = pd.DataFrame(data)

            output_csv_filename = os.path.join(output_folder, f"{lu_type}_intensity_{year}.csv")

            impact_df.to_csv(output_csv_filename, mode='w', header=True, index=False)
            print(f"Processed: {filename} -> {output_csv_filename}")

            # Log status with timestamp
            # Ensure log file exists with header
            if not os.path.exists(status_log_file):
                with open(status_log_file, "w", newline="") as log:
                    writer = csv.writer(log)
                    writer.writerow(["filename", "area_status", "bia_status", "area_status_timestamp", "bia_status_timestamp"])

            # Load existing log entries
            existing_files = set()
            with open(status_log_file, "r", newline="") as log:
                reader = csv.DictReader(log)
                for row in reader:
                    existing_files.add(row["filename"])

            # Append if new
            if filename not in existing_files:
                now = datetime.now().isoformat()
                with open(status_log_file, "a", newline="") as log:
                    writer = csv.writer(log)
                    writer.writerow([filename, "processed", "open", now, now])

        except Exception as e:
            print(f"Error processing {filename}: {e}")
            with open(status_log_file, "a") as log:
                log.write(f"{filename},error,{datetime.now().isoformat()},{str(e)}\n")

input_folder = "../data/03_intensity/LUH2/crops/" # folder where the unprocessed area data is stored 
output_folder = "../output/area_intensity/LUH2/CG/"
status_log_file = "../data/03_intensity/LUH2/status_log/status_log.csv"  
calculate_lu_intensity_per_country(input_folder, output_folder, status_log_file,country_raster,country_shapes)


# same for plantations
input_folder = "../data/03_intensity/LUH2/plantations/" # folder where the unprocessed area data is stored 
calculate_lu_intensity_per_country(input_folder, output_folder, status_log_file,country_raster,country_shapes)
