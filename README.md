# global-agricultural-intensification-biodiversity
Code to quantify global biodiversity impacts of agricultural land-use intensification using crop-resolved, spatio-temporal data (2000–2019).

---

## Table of Contents

1. I Compile Land-use Intensity Dataset
   - Crops
   - Pasture
   - Treecrops / Plantations
   - Aggregated CSV Output
2. B Biodiversity Impact Assessment
   - Prepare Characterization Factors
   - Biodiversity Impact Calculation
   - Crop-type–specific Impacts
3. R Results - Analysis and Visualization
4. How to Open the Data
5. [Shiny Data visualizer](https://veronikaschlosser.shinyapps.io/biodiv/) 
---

## I) Compile Land-use Intensity Dataset 

### I.1 Crops

1. **Preprocess Auxiliary Datasets for intensity classification** (output .tif per category and year)
   - **01_01_fertilizer_crop_intensity.R**
     - Aggregate N-Fertilization rate per pixel for all crops and sum
     - Resampling from 0.0833° to 0.25° based on weighted sum
     - Classification based on thresholds (kgN/ha)
   - **01_01_harvest_intensity_crops.R**
     - Resampling harvest frequency data 250 m → LUH2 resolution
   - **01_01_irrigation_crop_intensity.R**
     - Resampling results to 0.25°
     - Classification based on global quartile thresholds

2. **Identify treecrops from cropgrids and backcast**
   - **01_02_1_backcasting-Cropgrids.Rmd**
     - Create table with proportional differences between 2020 and timeseries using FAOSTAT data (output CSV) per country 
   - **01_02_2_Compile_CROPGRIDS_FAOSTAT-timeseries.py**
     - Use CropGrids 2020 as baseline
     - Multiply each crop area by scaling values from FAOSTAT diff table
     - Output: .tif per crop type and year, gridcells include area of crop
   - **I_01_02_3_comb-LUH2-CG.R**
     - Seperate treecrops (=plantations) and crops, sum to have one layer each
     - Resample to LUH2 resolution
     - Apply scaling if croparea + treecrop exceeds LUH2
     - Output: .tif for crops and treecrops per year

3. **Classification to intensity levels**
   - **01_03_Combine_Classified_crops.py**
     - Read auxiliary datasets from 01_01 (fertilizer, harvest, irrigation)
     - Read remaining crop area from 01_02_3 (cropland = LUH2(cropland) - plantation area)
     - Classify each pixel according to rules
     - Special case: year 2000 uses 2001 harvest intensity
     - Output: .tif for crops and treecrops per year with intensity classification

### I.2 Pasture

- **02_pasture_intensity_LUH2.R**
  - Read NH4, HO3, N_manure, N_mand, N_rangeland datasets from Tian et al. 
  - Sum and resample to LUH2 resolution (0.25°)
  - Classify using thresholds

### I.3 Treecrops / Plantations

- **03_plantation_intensity.R**
  - Calculate plantation intensity based on treecrop layer from 01_02_3 and coverage per pixel

### I.4 Aggregated Intensity Dataset CSV

- **I_4_CSV_area-lutype-int.py**
  - Compile CSV files for per lu_type for year, country, intensity, area (ha)

---

## B) Biodiversity Impact Assessment

### Prepare datasets

- **B_rasterize_CFs.py**
  - Creates raster from CFs (Scherer et al.)
  - Creates raster from country shapefile matching LUH2 resolution

### B.1 Calculate Biodiversity Impacts

- **B_01_Biodiversity-impact-assessment_LUH2.py**
  - Conduct biodiversity impact assessment using land-use dataset and CFs

### B.2 Combine with crop type information

- **B_02_croptype_impacts.py**
  - Assign biodiversity impacts and area CSV files to crop types
  - Scale the total area per intensity and crop (so that it does not exceed the total cropland / plantation area from LUH2)

---

## R - Analyze Results and Plotting

- **R_00**: Combine land-use intensity data into one file for plotting
  - Raster (.nc files, available at zenodo)
  - csv file, available in SI_table.xlsx total_data
- **R_01**: Combine biodiversity impact rasters into one file
   - Produce .tif file for one year in order to plot a single impact map
   - Compile single CSV file with all outputs per country
- **R_02**: Data analysis and plotting
   - Data analysis: Get an idea about hotspot regions and crop types etc.
   - Prepare data for plotting (Figures and data visualizer)
   - R_02_Fig Create plots

---

## IV. How to Open the Data

**Python (xarray)**
```python
import xarray as xr
ds = xr.open_dataset("land_use_intensity_area.nc")
print(ds)
```
**R (terra)**
```r
library(terra)
r <- rast("land_use_intensity_area.nc")
r
```
**R (stars, recommended for multidimensional data)**
```r
library(stars)
s <- read_stars("land_use_intensity_area.nc")
print(s)
```

---

## V. Interactive data visualization

An interactive Shiny application for exploring the results is available here:

[https://](https://veronikaschlosser.shinyapps.io/biodiv/)

The app allows exploration of land-use intensity, crop-specific areas, and biodiversity impacts across space and time.

## Notes

- All spatial data aligned to **LUH2 grid (0.25°)**
- Areas stored as sub-pixel areas (ha)
- Biodiversity impacts in PDF·m²·yr
- Scripts provided for reproducibility and transparency

---

## Recommended Citation

Please cite the accompanying paper and Zenodo DOIs for datasets and code.
