
## @file 
# This script calculates biodiversity impacts of abandonned land based on CFs and ecoregions 
# use this after you have compiled the intensity datasets (seperately for each land use class) 

import geopandas as gpd
import rasterio
import numpy as np
import pandas as pd
from rasterio.mask import mask
import netCDF4
import os
from shapely.geometry import box
from rasterio.features import geometry_mask
from joblib import Parallel, delayed
from scipy.ndimage import labeled_comprehension


# Define paths to stative variables
CF_path = "../literature/Scherer-et-al_2023/CF_domain.csv"
country_path = "../data/04_bia_inputs/LUH2/country_raster.tif"
area_path = "../data/03_intensity/LUH2/area_ha.tif"
shpcountries_path = "H:/02_Projekte/02_LUC biodiversity paper/02_data/country_shp/ne_110m_admin_0_countries.shp"


# Load CF data 
CFs = pd.read_csv(CF_path, sep=";", header=0).fillna(0)
cell_areas = rasterio.open(area_path).read(1)
country_raster  = rasterio.open(country_path).read(1)
shpcountries = gpd.read_file(shpcountries_path)
with rasterio.open(area_path) as src:
        profile = src.profile


# Load the CF raster
# For now, we use the CFs of pasture minimal 
CF_raster_path = "../data/04_bia_inputs/LUH2/CF_raster/habitat_9.tif"
with rasterio.open(CF_raster_path) as src:
    CF_raster = np.nan_to_num((src.read(1)), nan = 0)



## Function to calculate biodiversity impact
def calculate_biodiversity_impact_abandonned(year, lu_type, cell_areas, country_raster, profile,calc_type,CF_raster = None):
    ''' 
    Calculates biodiversity impact for abandand land and a given year 
    Parameters: 
    year (int): Year of the data.
    cell_areas (np.ndarray): Array of cell areas in square meters.
    CF_stack (np.ndarray): CF stack for intensity levels.
    country_raster (np.ndarray): Raster with country IDs.
    profile (dict): Metadata for output raster files.
    calc_type: can be biodiversity or area
    '''
    
    assert calc_type == "biodiversity" or calc_type == "area" , 'type must be biodiversity or area'

    
    LUH2_path = f"../data/01_raw/LUH2_data/states.nc"

    assert os.path.exists(LUH2_path), f"Chunk file not found: {LUH2_path}"
    LUH2_file = netCDF4.Dataset(LUH2_path)
    
    year_idx = year - 850
    if lu_type == "abandoned": 
        sec_land = LUH2_file.variables['secdn'][year_idx,:,:]
        sec_land = np.nan_to_num(sec_land, nan=0)
        LUH2_file.close()
    if lu_type == "rangeland":
        sec_land = LUH2_file.variables['range'][year_idx,:,:]
        sec_land = np.nan_to_num(sec_land, nan=0)
        LUH2_file.close()


    if calc_type == "biodiversity":
        assert CF_raster is not None, "CF_raster must be provided for biodiversity calculations"    
        impact_stack = sec_land * cell_areas * CF_raster

        # Define output paths
        out_path = "../output/biodiversity_impact_assessment/LUH2/"
        output_tif_filename = f"{out_path}{lu_type}/{lu_type}_impact_{year}.tif"
        output_csv_filename = f"{out_path}{lu_type}/{lu_type}_impact_{year}.csv"

        with rasterio.open(output_tif_filename, 'w', **profile) as dst:
            dst.write(impact_stack,1)  # Write each intensity's impact to a separate band

    
    elif calc_type == "area": 
        impact_stack = sec_land * cell_areas

        # Define output paths
        out_path = "../output/area_intensity/LUH2/CG/" 

        output_csv_filename = f"{out_path}{lu_type}_intensity_{year}.csv"
        output_tif_filename = f"../data/03_intensity/LUH2/{lu_type}/{lu_type}_intensity_{year}.tif"
        with rasterio.open(output_tif_filename, 'w', **profile) as dst:
            dst.write(impact_stack,1)  # Write each intensity's impact to a separate band

    
    
    country_impacts = []

    # Calculate impact per country

    # Use advanced indexing to group pixel indices by country
    country_ids = np.unique(country_raster[country_raster > 0]) # Get unique country IDs (excluding 0)

    # Calculate impact sums for each country and intensity in a vectorized manner
    impact_per_country = np.array([
    impact_stack[country_raster == country_id].sum()
    for country_id in country_ids])     

    # Create a DataFrame in one go
    country_names = shpcountries.GEOUNIT # Adjust index for 1-based country IDs
    country_SOV = shpcountries.SOV_A3
    data = {
        "country": country_names,  # Each country repeated for the 3 intensity levels
        "SOV": country_SOV,
        "impact_sum": impact_per_country  # Flatten impact values by intensity
    }

    impact_df = pd.DataFrame(data)
    if calc_type == "area":
         impact_df = impact_df.rename(columns={"country":"GEOUNIT", "SOV": "SOV","impact_sum": "area_ha"})


    # Convert the list of dictionaries to a pandas DataFrame
      # Append to the CSV if it exists, otherwise create a new one
    
    if os.path.exists(output_csv_filename):
        impact_df.to_csv(output_csv_filename, mode='a', header=False, index=False)
        print(f"Appended cumulative impact for to {output_csv_filename}")
    else:
        impact_df.to_csv(output_csv_filename, mode='w', header=True, index=False)
        print(f"Created new CSV with cumulative impact {output_csv_filename}")


# Main execution
start_year = 2000
end_year = 2019
years = range(start_year, end_year + 1)

for year in years:
    print(year)
    calculate_biodiversity_impact_abandonned(year,"abandoned", cell_areas, country_raster, profile,calc_type="area")
    #calculate_biodiversity_impact_abandonned(year,"rangeland", cell_areas, country_raster, profile,calc_type="biodiversity", CF_raster=CF_raster)




