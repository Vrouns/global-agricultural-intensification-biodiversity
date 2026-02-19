# combine biodiversity impact raster to get one total raster to plot with impacts
import rasterio
import numpy as np
import os

lu_types = ["crops","pasture","plantations"]
data_path = "../output/biodiversity_impact_assessment/LUH2"
output_path = "../output/biodiversity_impact_assessment/LUH2/combined"


def sum_impact_one_year(lu_types, data_path, year, output_path): 
    '''
    This function builds the sum of all biodiversity impact rasters to be able to plot impact as map
    Variables: 
    data_path:   contains biodiversity impact rasters per landuse type and year 
    year:        is the year to process 
    output_path: is the directory where to safe the data'''
    
    # load rasters 
    raster_paths = []

    for lu in lu_types:
        lu_folder = os.path.join(data_path, lu)
        for f in os.listdir(lu_folder):
            if f.endswith(f"{year}.tif"):
                raster_paths.append(os.path.join(lu_folder, f))
    # Open all rasters
    rasters = [rasterio.open(path) for path in raster_paths]

    # Ensure all rasters have the same dimensions and number of layers
    layer_counts = [raster.count for raster in rasters]
    widths = [raster.width for raster in rasters]
    heights = [raster.height for raster in rasters]

    assert len(set(layer_counts)) == 1, "Rasters must have the same number of layers"
    assert len(set(widths)) == 1, "Rasters must have the same width"
    assert len(set(heights)) == 1, "Rasters must have the same height"

    # Use metadata from the first raster for the output
    meta = rasters[0].meta
    meta.update(dtype=rasterio.float32)

    # Create output raster
    output_file = f"{output_path}/impact_cpp_{year}.tif"
    # Set count to 1 for output
    meta["count"] = 1

    # Initialize accumulator
    summed = None

    # Loop through all rasters
    for raster in rasters:
        for i in range(1, raster.count + 1):
            layer = raster.read(i)
            if summed is None:
                summed = np.zeros_like(layer, dtype=np.float32)
            summed += layer

    # Write the result
    with rasterio.open(output_file, 'w', **meta) as dst:
        dst.write(summed.astype(rasterio.float32), 1)

    # Close all input rasters
    for raster in rasters:
        raster.close()

    print(f"Summed raster saved to {output_path}")


for year in [2005,2010,2020]:
    sum_impact_one_year(lu_types,data_path, year, output_path)