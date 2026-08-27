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

final_path <- "data/02_resampled/LUH2_GCB2025_first_submission/Adalibieke_fertilizer_crops"
fert_files <- list.files(final_path, pattern = "N_app_tot.*\\.tif$", full.names = TRUE) # if only specific years add indices[1:5]

out_path_rep <- "data/02_resampled/LUH2/" # where reprojected total raster is located

# outpath for classified fertilizer  raster
out_path <- "data/03_intensity/LUH2_GCB2025/crops/variables_class/fertilizer_class/"
path_LUH2_updated <- "../04_Intensification_TS_expansion/data/LUH_update_states4.nc"


harvest <- rast(paste0(fert_path,"Harvested_area_1961-2020.h5"))

# for extentions
ext_path <- "data/02_b_extention_datasets/Nitrogen/"
ext_files <- list.files(ext_path, pattern = "LUH2.*\\.tif$", full.names = TRUE)
ext_rast <- rast(ext_files)

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


# Step 2: Calculate total N-application for crops -----------------------------

if (!dir.exists(final_path)){dir.create(final_path)}

start_year = 2000
end_year = 2020

# LUH2 dataset for reference
path_LUH2 <- "data/01_raw/LUH2_data/states.nc"
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


# Step 3: Intensity definition -------------------------------------------
# Thresholds for fertilizer data according to Overmars et al.--------
N_app_year_crop_files <- list.files(paste0(out_path_rep,"Adalibieke_fertilizer_crops/Adalbieke_",year,"_N_application"), full.names=T)
N_app_year_crop <- rast(N_app_year_crop_files)
N_app_sum <- sum(N_app_year_crop, na.rm=T)
boxplot(N_app_sum)



# read in data
fert_data <- rast(fert_files)
# adjust filenames 
names(fert_data) <- basename(fert_files)


LUH2_update <- rast(path_LUH2_updated)
years <- 2000:2020
years_ext <- 2021:2024
LUH2_start_year <- 850

# define a sample res
sample_ras <- LUH2_update$c3ann_1

# remove NA from fert_data 
fert_data <- ifel(is.na(fert_data), 0, fert_data)
# reproject to LUH2 resolution summing up the fertilizer input 
fert_rep <- project(fert_data, sample_ras, method = "sum")


# fertilizer area
# divide by cellsize, since dividing it by cropland area returns irrealistic high values 
# (tested in advance)
fert_ha <- fert_rep / cellSize(fert_rep, unit = "ha")
plot(fert_ha_ext$N_total_LUH2_2021>150)
boxplot(fert_ha$N_total_LUH2_2021)

# set thresholds to define intensity level per pixel
# Baseline: low 0-50, medium 50-150, high >150 kg / ha

#Threshold definition 
low_threshold <- 50 # upper boundary of low-definition
med_threshold <- 150 # upper boundary of med-definition
class_matrix <- matrix(c(0, low_threshold, 1,  # Low
                         low_threshold, med_threshold, 2, # Medium
                         med_threshold, Inf, 3),   # High
                       ncol=3, byrow=TRUE)

# classify whole raster
fertilizer_classified <- classify(
  fert_ha,
  class_matrix,
  include.lowest = T,
  filename = paste0(out_path, "classified_fertilizer_intensity_rep_", start_year, "-", end_year,".tif"),
  overwrite = T
)

#######
# extention
# for extention 
fert_data_ext <- rast(ext_files)
# for checking remove all other layers, keep last one 
fert_data_ext <- ifel(is.na(fert_data_ext), 0, fert_data_ext)
fert_rep_ext <- project(fert_data_ext, sample_ras, method = "sum")
fert_ha_ext <- fert_rep_ext / cellSize(fert_rep_ext, unit = "ha")


# classify extention
fertilizer_classified <- classify(
  fert_ha_ext,
  class_matrix,
  include.lowest = T,
  filename = paste0(out_path, "classified_fertilizer_intensity_rep_2021-2024.tif"),
  overwrite = T
)

# combine both 

first_half <- rast("data/03_intensity/LUH2_GCB2025/crops_first_submission/variables_class/fertilizer_class/classified_fertilizer_intensity_rep_2000-2020.tif")
extention<- rast("data/03_intensity/LUH2_GCB2025/crops/variables_class/fertilizer_class/classified_fertilizer_intensity_rep_2021-2024.tif")

comb <- c(first_half, extention)

# change names 
names(comb) <- c(paste0("int_N_app_", 2000:2024))
# write raster 
writeRaster(comb,"data/03_intensity/LUH2_GCB2025/crops/variables_class/fertilizer_class/classified_fertilizer_intensity_2000-2024.tif",
            overwrite=T)

# Step 4: Sensitivity Analysis --------------------------------------------

# First sensitivity analysis was to change thresholds in step 3 (=conservative scenario)
low_threshold <- 25 # upper boundary of low-definition
med_threshold <- 100 # upper boundary of med-definition
class_matrix <- matrix(c(0, low_threshold, 1,  # Low
                         low_threshold, med_threshold, 2, # Medium
                         med_threshold, Inf, 3),   # High
                       ncol=3, byrow=TRUE)

# classify whole raster
fertilizer_classified_con <- classify(
  fert_ha,
  class_matrix,
  include.lowest = T,
  filename = paste0(out_path, "classified_fertilizer_intensity_rep_", start_year, "-", end_year,"25_100.tif"),
  overwrite = T
)
fertilizer_classified_con <- rast(paste0(out_path, "classified_fertilizer_intensity_rep_", start_year, "-2020_25_100.tif"))
hist(values(fertilizer_classified_con$N_app_tot_2015.tif))

# second: use Quantiles as in Scherer et al. 2023
# (only for 2015 as showcase)
fert_2015_val <- values(fert_ha$N_app_tot_2015.tif, na.rm=T)
# filter out >0 values 
fert_2015_val <- fert_2015_val[!is.na(fert_2015_val) & fert_2015_val > 0]
min(fert_2015_val)

fert_quantile <- quantile(fert_2015_val, na.rm = T)
round(fert_quantile)
# Include min and max ranges explicitly
class_matrix <- matrix(c(-Inf, fert_quantile[2], 1,  # Low
                         fert_quantile[2], fert_quantile[3], 2, # Medium
                         fert_quantile[3], Inf, 3),   # High
                       ncol=3, byrow=TRUE)

fertilizer_classified <- classify(
  fert_ha$N_app_tot_2015.tif,
  class_matrix,
  include.lowest = T,
  filename = paste0(out_path, "classified_fertilizer_intensity_rep_2015_quantiles.tif"),
  overwrite = T
)

hist(values(fertilizer_classified))

# third: same quantile approach as above, but quantiles and fertilizer application (kg/ha)
# are derived on the original (native) resolution raster, i.e. before reprojecting/summing
# to LUH2 resolution. The classified raster is only resampled to LUH2 resolution afterwards,
# using modal resampling so each LUH2 cell gets the majority class of its underlying pixels
# (only for 2015 as showcase)

# fertilizer application at native resolution (analogous to fert_ha, but before project())
fert_ha_orig <- fert_data / cellSize(fert_data, unit = "ha")

fert_2015_val_orig <- values(fert_ha_orig$N_app_tot_2015.tif, na.rm = T)
# filter out >0 values
fert_2015_val_orig <- fert_2015_val_orig[!is.na(fert_2015_val_orig) & fert_2015_val_orig > 0]

fert_quantile_orig <- quantile(fert_2015_val_orig, na.rm = T)
round(fert_quantile_orig)
# Include min and max ranges explicitly
class_matrix <- matrix(c(-Inf, fert_quantile_orig[2], 1,  # Low
                         fert_quantile_orig[2], fert_quantile_orig[3], 2, # Medium
                         fert_quantile_orig[3], Inf, 3),   # High
                       ncol=3, byrow=TRUE)

# classify at native resolution
fertilizer_classified_orig <- classify(
  fert_ha_orig$N_app_tot_2015.tif,
  class_matrix,
  include.lowest = T,
  filename = paste0(out_path, "classified_fertilizer_intensity_orig_2015_quantiles.tif"),
  overwrite = T
)

# resample the classified (categorical) raster to LUH2 resolution using modal resampling
fertilizer_classified_orig_rep <- resample(
  fertilizer_classified_orig,
  sample_ras,
  method = "average",
  overwrite = T
)
fertilizer_classified_orig_rep_round <- round(fertilizer_classified_orig_rep,digits=0)
hist(values(fertilizer_classified_orig_rep_round))

writeRaster(fertilizer_classified_orig_rep_round, paste0(out_path, "classified_fertilizer_intensity_rep_2015_quantiles_avg.tif"))
