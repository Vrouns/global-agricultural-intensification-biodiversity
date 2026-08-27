import numpy as np
import rasterio
import os

# Define the range of years
start_year = 2000
end_year = 2019
years = list(range(start_year, end_year + 1))
# years = [2005,2010,2020]
os.chdir("h:\\02_Projekte\\03_Intensification-fragmentation-CFs\\")
# Paths, modify as needed
data_path = "data/03_intensity/LUH2_GCB2025/crops/variables_class" # contains fertilizer, irrigation and intensity layer classified
resampled_path = "data/02_resampled/LUH2_GCB2025/" # contains LUH2 data, Cropgrids data of treecrops
out_path = "data/03_intensity/LUH2_GCB2025/crops_first_submission/sensitivity_analysis/" # output path for intensity layers
if not os.path.exists(out_path):
    os.makedirs(out_path)

# Load the raster data for fertilizer, water, and harvest
irr_path = f"{data_path}/irr_class/wf_blue_classified_2000-2024.tif" 
fert_path = f"{data_path}/fertilizer_class/classified_fertilizer_intensity_2000-2024.tif"
harvest_path = f"{resampled_path}/numcycles" # directory containing one .nc file per year, e.g. 2001.nc

# for sensitivity analysis use these paths
# irr_path = f"{data_path}/irr_class/wf_blue_classified_area_thresholds_2015.tif"  # Use the correct path for the irrigation raster
# irr_path = f"{data_path}/irr_class/wf_blue_classified_2015_quantiles.tif"  # Use the correct path for the irrigation raster
# fert_path = f"{data_path}/fertilizer_class/classified_fertilizer_intensity_rep_2015_quantiles.tif"  # Use the correct path for the fertilizer raster


def classification (year, start_year, fert_path, irr_path, harvest_path,out_file): 
    """
    Classify cropping intensity based on precomputed raster layers for a specific year.

    Classification rules:
    - Intensity 3: If water, fertilizer, or harvest is equal to 3
    - Intensity 2: If water or fertilizer is equal to 2
    - Intensity 1: All other cases and If Hilda layer value is 22
    - Masking: Retain only pixels where Hilda layer value is 22
    """
    

    year_idx = (year - start_year )+1 # rasterio starts counting at 1 
    year_idx_irr = year_idx
    year_harvest = year
    if start_year == 2000: 
        year_idx_harvest = year_idx-1
    if year == 2000: 
        year_harvest = 2001

    # Read the water, fertilizer, harvest, and hilda layers for the current year
    with rasterio.open(irr_path) as water_layer: 
        water_data = water_layer.read(year_idx_irr)
    with rasterio.open(fert_path) as fertilizer_layer: 
        fertilizer_data = fertilizer_layer.read(year_idx)
    harvest_file = f"{harvest_path}/{year_harvest}.nc"
    with rasterio.open(harvest_file) as harvest_layer: 
        harvest_data = harvest_layer.read(1)

    LUH2_crop_minus_CG_path = f"{resampled_path}/Cropgrid/LUH2_crops_after_CG_{year}.tif"


    with rasterio.open(LUH2_crop_minus_CG_path) as LUH2_src:
        luh2 = LUH2_src.read(1)
        metadata = LUH2_src.meta
        metadata.update(dtype=rasterio.uint8, count=1, nodata = 0,compress='deflate')
        profile = LUH2_src.profile
 

    # Replace NaN or NoData values with 0 
    water_data = np.nan_to_num(water_data, nan=0)
    fertilizer_data = np.nan_to_num(fertilizer_data, nan=0)
    harvest_data = np.nan_to_num(harvest_data, nan=0)


    # Initialize an empty intensity layer with the same shape as the water layer
    intensity_layer = np.ones_like(luh2, dtype=np.uint8)
    # per default everything has the intensity of 1. 

    # Apply the classification rules

    intensity_layer[(water_data == 2) | 
                (fertilizer_data == 2)] = 2 # medium 
    intensity_layer[(water_data == 3) | 
                    (fertilizer_data == 3) | 
                    (harvest_data == 3)] = 3 # high intensity 
    
    # Create masks for intensity classes
    low_mask = intensity_layer == 1
    med_mask = intensity_layer == 2
    high_mask = intensity_layer == 3

    # Multiply LUH2 by masks to get area per intensity
    low_area = np.where(low_mask, luh2, 0)
    med_area = np.where(med_mask, luh2, 0)
    high_area = np.where(high_mask, luh2, 0)

    bands = [low_area, med_area, high_area]
    bands_array = np.stack(bands)  # shape: (3, height, width)

    profile.update(count=3)  # 3 bands
    metadata.update(count=3)


    # Write the output intensity layer to a new raster file

    with rasterio.open(out_file, 'w', **profile) as dst:
        dst.write(bands_array)
    
    print(f"{year} intensity layer calculated")
    return bands_array


# Process each year
for year in years:
    # Define the output file path
    out_file = os.path.join(out_path, f"crops_intensity_{year}.tif")
    classification(year, start_year, fert_path = fert_path, irr_path=irr_path, 
                   harvest_path=harvest_path, out_file = out_file)
                   

