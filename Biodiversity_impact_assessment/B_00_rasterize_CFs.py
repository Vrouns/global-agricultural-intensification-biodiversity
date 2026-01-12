# Create one CF raster for impact assessment (only once required)
import rasterio
from rasterio import features 
import geopandas as gpd
import numpy as np 
import pandas as pd 
import os

# Define paths to stative variables
ecoreg_path = "H:/02_Projekte/02_LUC biodiversity paper/02_data/ecoregions/wwf_terr_ecos.shp"
CF_path = "../literature/Scherer-et-al_2023/CF_domain.csv"
output_folder = "../data/04_bia_inputs/LUH2"



# Reference raster for resolution and extent
template_raster_path = "../data/03_intensity/LUH2/area_ha.tif"  # A reference raster for resolution and extent
with rasterio.open(template_raster_path) as template_raster:
    out_meta = template_raster.meta  # Metadata (profile) of the raster
    raster_shape = template_raster.shape  # Shape of the raster (height, width)
    transform = template_raster.transform  # Transformation matrix
    crs = template_raster.crs

###### CF raster 

# Load CF data and replace NAs
CFs = pd.read_csv(CF_path, sep=";", header=0).fillna(0)

# Load ecoregion and country shapefiles
ecoreg = gpd.read_file(ecoreg_path)
ecoreg = ecoreg[ecoreg['ECO_ID'] != -9999]
ecoreg = ecoreg[ecoreg['ECO_ID'] != -9998]

# Loop through unique habitat_ids in the CF table
for habitat_id in CFs['habitat_id'].unique():
    # Filter polygons for the current habitat_id
    CF_select = CFs.loc[CFs['habitat_id'] == habitat_id, ['eco_id', 'CF_occ_avg_glo']]

    ecoreg['CF_occ_avg_glo'] = ecoreg['ECO_ID'].map(CF_select.set_index('eco_id')['CF_occ_avg_glo'])
    # Rasterize the CF values for the current habitat_id
    rasterized = features.rasterize(
        [(geom, value) for geom, value in zip(ecoreg.geometry, ecoreg['CF_occ_avg_glo'])],
        out_shape=raster_shape,
        transform=transform,
        fill=0,  # Default value for no data
        dtype='float32'
    )
    
    # Save the raster to the output folder
    name_round = int(habitat_id)
    output_path = os.path.join(output_folder, f"habitat_{name_round}.tif")
    with rasterio.open(output_path, 'w', **out_meta,compress="lzw") as dst:
        dst.write(rasterized, 1)
    
    print(f"Raster for habitat_id {habitat_id} saved to {output_path}")




### Rasterize country shapefile
# Load the shapefile with geopandas
shpcountries = gpd.read_file('H:/02_Projekte/02_LUC biodiversity paper/02_data/country_shp/ne_110m_admin_0_countries.shp')
print(shpcountries.columns)
# Step 1: Create a mapping of country codes to unique numeric IDs
country_code_to_id = {code: idx + 1 for idx, code in enumerate(shpcountries['GU_A3'].unique())}

# Step 2: Map the country codes in GU_A3 to the corresponding numeric ID
shpcountries['country_id'] = shpcountries['GU_A3'].map(country_code_to_id)

# Create an empty raster (initialize with zeros)
country_raster = np.zeros(raster_shape, dtype=np.uint32)
shapes = []

with rasterio.open(f"{output_folder}country_raster.tif", 'w', driver='GTiff', 
                   count=1, dtype='uint32', width=raster_shape[1], 
                   height=raster_shape[0],
                   crs=crs, transform=transform,
                   compress="lzw") as dst:
    
    # Create a generator of (geometry, country_id) pairs
    shapes = ((geom, value) for geom, value in zip(shpcountries.geometry, shpcountries.country_id))
    
    # Rasterize the geometries
    features.rasterize(
        shapes=shapes,  # List of (geometry, country_id) pairs
        out=country_raster,  # The output array (country_raster)
        transform=transform,  # The affine transform
        fill=0,  # Fill no data areas with 0
        dtype='uint32'  # Data type for the raster
    )
    
    # Write the rasterized data to the output file
    dst.write(country_raster, 1)  # Write to the first band