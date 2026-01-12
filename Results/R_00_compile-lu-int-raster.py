# This script combines the intensity data into one single net CDF file 
# Contains data per pixel for 3 land-use types (cropland, plantation, pasture),
#  3 intensity levels (low, medium high) 
# for each year (2000-2019)
# --> in total 3*3*20 layers

# 1st part: 
# area data (per pixel)

# 2nd Part of script: 
# biodiversity impact (per pixel)

import rasterio
import numpy as np
import xarray as xr
from pathlib import Path

years = range(2000, 2020)

land_uses = {
    "crops": 0,
    "plantations": 1,
    "pasture": 2,
    "rangeland": 3,
    "abandoned": 4,
}

intensities = {
    "low": 0,
    "medium": 1,
    "high": 2,
    "not_applicable":3
}

# create a sample raster 
data_dir = Path("../data/03_intensity/LUH2/crops")

sample = rasterio.open(data_dir / "crops_intensity_2000.tif")
lat = sample.xy(np.arange(sample.height), np.zeros(sample.height))[1]
lon = sample.xy(np.zeros(sample.width), np.arange(sample.width))[0]

# allocate array
# shape: time x landuse x intensity x lat x long
area = np.zeros(
    (
        len(years),
        len(land_uses),
        len(intensities),
        sample.height,
        sample.width,
    ),
    dtype="float32",
)

# Loop over landuse types 
# lu_names are crops, plantations, pasture
for t, year in enumerate(years):
    for lu_name, lu_idx in land_uses.items():
        data_dir_int = Path("../data/03_intensity/LUH2")

        fname = data_dir_int / f"{lu_name}/{lu_name}_intensity_{year}.tif"
        with rasterio.open(fname) as src:
            assert src.count == 1 or src.count == 3
        # case 1: lu data have intensity level
            if lu_name in ["cropland", "plantation", "pasture"]:
                    for int_name, int_idx in intensities.items():
                        if int_name == "not_applicable":
                            continue
                        area[t, lu_idx, int_idx, :, :] = src.read(int_idx + 1)
            # case 2: rangeland and abandoned --> no assigned intensity level 
            else:  # rangeland, abandoned
                    area[t, lu_idx, intensities["not_applicable"], :, :] = src.read(1)

            

# Built net cdf 
ds = xr.Dataset(
    data_vars={
        "area": (
            ["time", "land_use", "intensity", "lat", "lon"],
            area,
            {"units": "ha", "long_name": "Sub-pixel area per land-use type, intensity, and year"},
        )
    },
    coords={
        "time": list(years),
        "land_use": list(land_uses.values()),
        "intensity": list(intensities.values()),
        "lat": lat,
        "lon": lon,
    },
    attrs={
        "title": "Land-use intensification area",
        "Conventions": "CF-1.8" # convention so that data is readable
    },
)

ds["land_use"].attrs = {
    "long_name": "Land-use type",
    "flag_values": [0, 1, 2,3,4],
    "flag_meanings": "cropland plantation pasture rangeland abandoned",
}

ds["intensity"].attrs = {
    "long_name": "Land-use intensity",
    "flag_values": [0, 1, 2,3 ],
    "flag_meanings": "low medium high not applicable",
}

# also global documentation

ds.attrs["land_use_definition"] = "0=cropland; 1=plantation; 2=pasture, 3= rangeland, 4=abandoned/secondary"
ds.attrs["intensity_definition"] = "0=low; 1=medium; 2=high, 3=not applicable"

ds["area"].attrs["description"] = (
    "This variable represents the area (ha) of each land-use type (cropland, plantation, pasture, rangeland, abandoned, flagged as 0,1,2) "
    "within a pixel for each intensity class (low, medium, high,not applicable (rangeland, abandoned) flagged as 0,1,2,3) "
    "and each year (2000-2019). Pixels may contain multiple land-use types"
    "but only one intensity class per land use type. Values are sub-pixel areas in ha."
    "land use area is based on the LUH2 dataset (Hurtt et al. 2020)"
)
ds.attrs["title"] = "Land-use intensification area (sub-pixel)"
ds.attrs["summary"] = (
    "This NetCDF dataset provides sub-pixel areas (ha) for cropland, plantation, "
    "and pasture, broken down by intensity class (low, medium, high) and year "
    "(2000-2019). It is suitable for spatial analyses, LCA, "
    "and biodiversity impact assessments."
)
ds.attrs["Conventions"] = "CF-1.8"

# store with compression
ds.to_netcdf(
    "../output/zenodo/land_use_intensity_area.nc",
    encoding={"area": {"zlib": True, "complevel": 4}},
)

###########################################
### 
# Repeat the same but for biodiversity impact 
###

# allocate array
# shape: time x landuse x intensity x lat x long
biodiv_impact = np.zeros(
    (
        len(years),
        len(land_uses),
        len(intensities),
        sample.height,
        sample.width,
    ),
    dtype="float32",
)

# Loop over landuse types 
# lu_names are crops, plantations, pasture
for t, year in enumerate(years):
    for lu_name, lu_idx in land_uses.items():
        data_dir_int = Path("../output/biodiversity_impact_assessment/LUH2")

        fname = data_dir_int / f"{lu_name}/{lu_name}_impact_{year}.tif"
        with rasterio.open(fname) as src:

            if lu_name in ["cropland", "plantation", "pasture"]:
                        for int_name, int_idx in intensities.items():
                            if int_name == "not_applicable":
                                continue
                            biodiv_impact[t, lu_idx, int_idx, :, :] = src.read(int_idx + 1)
            elif lu_name == "rangeland":
                 int_idx = 0 # rangeland only light intensity 
                 biodiv_impact[t, lu_idx, int_idx, :, :] = src.read(int_idx + 1)
                  
                # case 2: rangeland and abandoned --> no assigned intensity level 
            else:  # abandoned
                 biodiv_impact[t, lu_idx, intensities["not_applicable"], :, :] = src.read(1)

biodiv_impact = biodiv_impact * 1e4

# Built net cdf 
ds = xr.Dataset(
    data_vars={
        "biodiversity_impact": (
            ["time", "land_use", "intensity", "lat", "lon"],
            biodiv_impact,
            {"units": "ha", "long_name": "Sub-pixel biodiversity impact per land-use type, intensity, and year"},
        )
    },
    coords={
        "time": list(years),
        "land_use": list(land_uses.values()),
        "intensity": list(intensities.values()),
        "lat": lat,
        "lon": lon,
    },
    attrs={
        "title": "Land-use intensification area",
        "Conventions": "CF-1.8" # convention so that data is readable
    },
)

ds["land_use"].attrs = {
    "long_name": "Land-use type",
    "flag_values": [0, 1, 2],
    "flag_meanings": "cropland plantation pasture rangeland, abandoned land",
}

ds["intensity"].attrs = {
    "long_name": "Land-use intensity",
    "flag_values": [0, 1, 2],
    "flag_meanings": "low medium high not applicable",
}

# also global documentation

ds.attrs["land_use_definition"] = "0=cropland; 1=plantation; 2=pasture, 3=rangeland, 4=abandoned land"
ds.attrs["intensity_definition"] = "0=light; 1=medium; 2=high, 3=not applicable"

ds["biodiversity_impact"].attrs["description"] = (
    "Biodiversity impact (PDFglo/m2/yr) of each land-use type (cropland, plantation, pasture,rangeland,abandoned, flagged as 0,1,2) "
    "within a pixel for each intensity class (low, medium, high, flagged as 0,1,2) "
    "and each year (2000-2019). Pixels may contain multiple land-use types"
    "note: For rangeland and abandoned land, the biodiversity characterization factor corresponds to low-intensity pasture."
    "land use area is based on the LUH2 dataset (Hurtt et al. 2020), CFs are based on Scherer et al. 2023"
)
ds.attrs["title"] = "Land-use intensification biodiversity impact (PDFglo) (sub-pixel)"
ds.attrs["summary"] = (
    "This NetCDF dataset provides sub-pixel biodiversity impact for cropland, plantation, "
    "pasture, rangeland and abandoned land, broken down by intensity class (low, medium, high) and year "
    "(2000-2019)."
)
ds.attrs["Conventions"] = "CF-1.8"

# store with compression
ds.to_netcdf(
    "../output/zenodo/land_use_intensity_biodiversity_impact.nc",
    encoding={"biodiversity_impact": {"zlib": True, "complevel": 4}},
)

ds
