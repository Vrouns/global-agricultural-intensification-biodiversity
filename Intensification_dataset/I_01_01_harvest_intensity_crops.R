library(ggplot2)
library(tidyterra)
library(dplyr)
library(RStoolbox)
library(terra)

# Reproject Harvest intensity file from Liu et al. to match LUH2 resolution 

## Read LUH2 dataset
path_LUH2 <- "./data/01_raw/LUH2_data/states.nc"
LUH2_rast <- rast(path_LUH2)

year <- 2015
LUH2_start_year <- 850
LUH2_layer_index <- year - LUH2_start_year + 1

# C3 crop layer
c3_crops <- grep("^c3ann_", names(LUH2_rast), value = TRUE)[LUH2_layer_index]
 
# Subset the raster
c3_2015 <- LUH2_rast[[c3_crops]]

ref <- c3_2015
# Liu et al. 2021 data ----------------------------------------------------
# Harvest intensity = HI 

# path_HI <- "./data/01_raw/Liu_Cropping_intensity/"
path_HI <- "D:/03_Intensification-fragmentation-CFs/data/01_raw/Liu_Cropping_intensity/"
files_HI <- list.files(path_HI, pattern = "GCI_2.*\\.tif$", full.names =T)
HI_all <- rast(files_HI)

# adjust start and end year
start_year <- 2000 
end_year <- 2020

start_ind <- start_year-2000 # dataset starts at 2001
end_ind <- end_year - start_year + start_ind
HI <- HI_all[[c(start_ind:end_ind)]] # rast only years 2005 to 2019
names(HI) <- basename(files_HI)


# resample HI_crop to HILDA resolution 
# out_path setting 
out_path <- "./data/03_intensity/LUH2/0_intermediate/crops/"
HI_rep <- project(HI, ref, method = "average", 
                  filename = paste0(out_path, "HI_rep_",start_year,"-",end_year,"_LUH2.tif"),
                  overwrite = T) 

