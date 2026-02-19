# fertilizer crop intensity: 
# Fertilizer input data from Adalibieke et al. --> different N-Applications
# Harvest data from Adalibieke et al.
# Multiplies Fertilizer with Harvest to estimate N input per pixel 
# Divide then N-input/pixel by pixel size to get the mean input per ha. 
# Notes: At the beginning, I just took the N/harvested ha from origin, but values seemed way too high 
# then: Multiplied by the harvested area and divided by the cropland size --> still way too high (around 2000 kg/ha)
# only logical values appeared when dividing by the total pixel size --> stick to this. 

# Compile intensity dataset using thresholds from Overmars et al. 2014  
# Output of this script: a layer of fertilizer intensity per pixel in LUH2 resolution 
library(terra)
library(purrr)

fert_path <- "data/01_raw/Adalibieke_fertilizer_crops/"
harvest <- rast(paste0(fert_path,"Harvested_area_1961-2020.h5"))
final_path <- "data/03_intensity/LUH2/0_intermediate/crops/Adalibieke_fertilizer_crops/"
out_path_rep <- "data/02_resampled/LUH2/"
out_path <- "./data/03_intensity/LUH2-GCB2025/0_intermediate/crops/"
fert_files <- list.files(final_path, pattern = "N_app_tot.*\\.tif$", full.names = TRUE) # if only specific years add indices[1:5]
path_LUH2_updated <- "../04_Intensification_TS_expansion/data/LUH_update_states4.nc"


# write functions ---------------------------------------------------------

#' This function fixes the downloaded raster, as it was flipped without crs and extent
#' extent and projection are given in the description file from A. 
#' 
#' @param r raw data raster after download
#' @param extent extent of raster
#' @param projection projection of raster 
#' @return the fixed raster 
#' 
# 1. Raster was downloaded in a somehow flipped position without crs and extend
fix_raster <- function(r, extent, projection) {
  r <- t(r) # transpose raster, somehow it was in the wrong direction
  crs(r) <- projection # assign projection
  ext(r) <- extent # assign extent
  return(r)
}


#' N-application rate per crop N application rate
#' 
#' @param year year to process 
#' @param cropname crop to process 
#' @param fert_path path where raw data is stored 
#' @param final_path path where N_applicatio raster will be stored
#' @return a raster with N application in kg/ha per pixel 
N_calc <- function(year, crop_name, fert_path = fert_path, final_path) {
  # transposed harvest data of the year needed.
  ind_year <- year - 1960
  N_ind_year <- seq(ind_year, 1560, 60)
  file_path <- paste0(fert_path,
                      "N_application_rate_",
                      crop_name,
                      "_1961-2020.h5")
  print(file_path)
  croprast <- rast(file_path)[[N_ind_year]]
  croprast <- fix_raster(croprast, projection = proj_hilda, extent = ext_hilda)
  N_app_tot_year <- sum(croprast, na.rm = T) # sum of different N-applications
  writeRaster(
    N_app_tot_year,
    paste0(final_path, "N_app_", crop_name, "_glob_", year, ".tif"),
    overwrite = T
  )
  return(N_app_tot_year)
}


#' Calculate total N application rate with harvest file
#' 
#' @param year year to process
#' @param harvest_file_year harvest file for the year (is in the harvested area folder)
#' @param
N_app_calc <- function(year, harvest_file_year,
                       writeRaster = F,
                       final_path = NA, 
                       dataset = NA) {
  ind_year <- year - 1960
  N_app_list <- list()
  cropnames <- gsub(paste0("_", ind_year), "", names(harvest_file_year))
  
  # loop over cropnames
  for (c in seq_along(cropnames)) {
    cur_crop <- cropnames[c]
    crop_key <- paste0(cur_crop, "_", ind_year)
    
    if (cur_crop == "Others crops") {
      crop_key <- paste0("Others_crops", "_", ind_year)
    } # problems with _
    if (cur_crop == "Sunflower") {
      crop_key <- paste0("sunflower", "_", ind_year)
    } # problems with capital letters
    
    if (!crop_key %in% names(harvest_file_year)) {
      stop(paste("Harvest data not found for", crop_key))
    }
    
    # Get harvest data for the current crop
    harvest_data_crop <- harvest_file_year[[crop_key]]
    
    # Check if N_amounts contains the current crop
    if (!cur_crop %in% names(N_amounts)) {
      stop(paste("N_amounts not found for crop:", cur_crop))
    }
    
    N_app_list[[c]] <- N_amounts[[cur_crop]] * harvest_data_crop
  }
  
  N_app_tot <- reduce(N_app_list, ~ sum(.x, .y, na.rm = TRUE)) # sum up whole rasterlist to only have N amount!
  if (writeRaster) {
    writeRaster(N_app_tot,
                paste0(final_path, "/N_app_tot_", year,"_", dataset,".tif"),
                overwrite = T)
  }
  return(N_app_tot)
  
}


# loop over years ---------------------------------------------------------

if (!dir.exists(final_path)){dir.create(final_path)}

start_year = 2000
end_year = 2020

# LUH2 dataset for reference
path_LUH2 <- "./data/01_raw/LUH2_data/states.nc"
LUH2_rast <- rast(path_LUH2)

year <- 2015
LUH2_start_year <- 850
LUH2_layer_index <- year - LUH2_start_year + 1

# C3 crop layer
c3_crops <- grep("^c3ann_", names(LUH2_rast), value = TRUE)[LUH2_layer_index]

# Subset the raster
c3_2015 <- LUH2_rast[[c3_crops]]

### chose ref dataset (either hilda or LUH2)
# ref <- hilda_15
ref <- c3_2015
dataset <- "LUH2"
year = 2020

for (year in c(start_year:end_year)) {
  N_app_path <- paste0( "./data/02_resampled/LUH2/Adalibieke_fertilizer_crops/Adalbieke_", year, "_N_application/")
  
  # crop to casestudy extent
  # hilda_crop <- crop(hilda_15, study_area)
  # filter out crops
  # hilda_crop_filter <- hilda_crop == 22
  
  #ggplot()+
  #  geom_spatraster(data = as.factor(hilda_crop_filter))
  
  proj_hilda <- crs(ref) 
  ext_hilda <- ext(ref)
  # Fertilized area
  # calculate N input from Adalbieke et al.
  
  # prepare Fertilizer data --------------------------------------------------
  
  
  # only focus on on year data:
  ind_year <- year - 1960
  indices_year <- seq(ind_year, 1255, 60) # 60 years, z.B.2015 = 1960+55 --> index 55
  
  # transpose the raster as it is stored the wrong way round and assign projection and extent
  harvest_year <- fix_raster(harvest[[indices_year]], extent = ext_hilda, projection = proj_hilda)
  # plot(harvest_2015[[1]])
  
  # Calculate N-application rate per year
  # create path if necessary
  if (!dir.exists(N_app_path)){dir.create(N_app_path)}
  
  cropnames <- gsub(paste0("_", ind_year), "", names(harvest_year))
  
  lapply(cropnames, function(crop_name) {
    N_calc(
      year = year,
      crop_name = crop_name,
      fert_path = fert_path,
      final_path = N_app_path
    )
  })
  
  # once calculated, multiply with harvest data
  N_files <- list.files(N_app_path, pattern = ".tif$", full.names = T)
  N_amounts <- lapply(N_files, rast)
  names(N_amounts) <- cropnames
  
  # replace dataset here 
  N_app_calc(
    year = year,
    harvest_file_year = harvest_year,
    writeRaster = T,
    final_path = final_path, 
    dataset = dataset
  )
  
}


# Intensity dataset compilation -------------------------------------------
# Thresholds for fertilizer data according to Overmars et al.--------
N_app_year_crop_files <- list.files(paste0(out_path_rep,"Adalibieke_fertilizer_crops/Adalbieke_",year,"_N_application"), full.names=T)
N_app_year_crop <- rast(N_app_year_crop_files)
N_app_sum <- sum(N_app_year_crop, na.rm=T)
boxplot(N_app_sum)


# read in data
fert_data <- rast(fert_files)
LUH2_update <- rast(path_LUH2_updated)
years <- 2000:2020
LUH2_start_year <- 850

# calculate cropland area in ha 
# crops_stack <- c(lapply(years, function(y) {
#   
#   idx <- y - LUH2_start_year + 1
#   
#   C3a   <- LUH2_update[[grep("^c3ann_", names(LUH2_update))[idx]]]
#   C4a   <- LUH2_update[[grep("^c4ann_", names(LUH2_update))[idx]]]
#   C3p   <- LUH2_update[[grep("^c3per_", names(LUH2_update))[idx]]]
#   C4p   <- LUH2_update[[grep("^c4per_", names(LUH2_update))[idx]]]
#   C3nfx <- LUH2_update[[grep("^c3nfx_", names(LUH2_update))[idx]]]
#   
#   C3a + C4a + C3p + C4p + C3nfx
# }))
# crops_stack <- rast(crops_stack)
# names(crops_stack) <- paste0("crops_tot_", years)
# # calculate cropland area in ha 
# crops_area <- crops_stack*cellSize(crops_stack$crops_tot_2000, unit = "ha")
# # set zero cropland to NA
# crops_area[crops_area == 0] <- NA

# adjust filenames 
names(fert_data) <- basename(fert_files)

# reproject to LUH2 resolution summing up the fertilizer input 
fert_rep <- project(fert_data, crops_stack$crops_tot_2000, method = "sum")

# # divide the reprojected raster by the cellsizes to obtain amount of N-fertilizer per ha
# 
# # get fertilizer input per ha cropland
# fert_ha_crop <- fert_rep / crops_area
# boxplot(fert_ha_crop) # --> values are way too high!!! 

# fertilizer area
fert_ha <- fert_rep / cellSize(fert_rep, unit = "ha")
plot(fert_ha_cellsize$N_app_tot_2000_LUH2.tif>2000)
boxplot(fert_ha$N_app_tot_2000_LUH2.tif)

# set thresholds to define intensity level per pixel
# low 0-50, medium 50-150, high >150 kg / ha

class_matrix <- matrix(c(-Inf, 50, 1, # Low
                         50, 150, 2, # Medium
                         150, Inf, 3),
                       # High
                       ncol = 3,
                       byrow = TRUE)

# classify whole raster
fertilizer_classified <- classify(
  fert_ha,
  class_matrix,
  include.lowest = T,
  filename = paste0(out_path, "classified_fertilizer_intensity_rep_", start_year, "-", end_year,".tif"),
  overwrite = T
)
            