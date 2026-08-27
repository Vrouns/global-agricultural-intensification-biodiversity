### This script happens after the projection of croptypes (script I_01_02_2_Compile_CROPGRIDS_FAOSTAT-timeseries.py)
# and the correction using mapbiomas
## 1.) Resample rep_croptypes to match LUH2 
## 2.) Substract treecrops of CG_pred from LUH2 to have crop layer
## 3.) Write also resampled CG_pred treecrops layer 

library(terra)

### Predicted croptypes Cropgrid
# Ocean/Water: flagged as “-1

out_path <- "data/02_resampled/LUH2_GCB2025/Cropgrid_mapbiomas_patched/"
if (!dir.exists(out_path)) {dir.create(out_path)}
croptype_path <- "data/05_crop_types/CG/time_series/"
mapbiomas_path <- "data/05_crop_types/CG/time_series_mapbiomas_patched/"

# Define treecrops
treecrops <- c("abaca", "agave", "almond", "apple", "apricot", "areca", "avocado", 
               "banana", "carob", "cashew", "cashewapple", "cherry", "chestnut", 
               "citrusnes", "coconut", "cocoa", "coffee", "date", "fig", "fruitnes", 
               "grape", "grapefruitetc", "hazelnut", "karite", "kiwi", "kolanut", 
               "lemonlime", "mango", "oilpalm","olive", "orange", "papaya", "peachetc", "pear", 
               "persimmon", "pimento", "pineapple", "pistachio", "plantain", "plum", 
               "quince", "rasberry", "rubber", "sourcherry", "stonefruitnes", 
               "tea", "tung", "vanilla", "walnut")


## LUH2 dataset

path_LUH2 <- "../04_Intensification_TS_expansion/data/LUH_update_states4.nc"
LUH2_rast <- rast(path_LUH2)
LUH2_start_year <- 850

###### 
# calculate treecrops


for (year in c(2000:2024)){
 message("Now processing year ", year)
  
  crop_files <- list.files(croptype_path, pattern = paste0(year,".tif$"), full.names = T)
  crop_files<- crop_files[crop_files!="data/01_raw/CROPGRID/CROPGRIDSv1.08_NC_maps/Countries_2018.nc"]

  # prefer the MapBiomas-patched file where a crop has one for this year;
  # fall back to the original back-cast (pred_<crop>_<year>.tif) otherwise
  crop_files_patched <- list.files(mapbiomas_path, pattern = paste0(year,"_mapbiomas.tif$"), full.names = T)
  patched_as_orig_basename <- sub("_mapbiomas\\.tif$", ".tif", basename(crop_files_patched))
  crop_files <- crop_files[!(basename(crop_files) %in% patched_as_orig_basename)]
  crop_files <- c(crop_files, crop_files_patched)

  treecrops_selected <- crop_files[grepl(paste0(treecrops, collapse =
                                                  "|"), crop_files)]
  
  # rast 
  treecrops_rast <- rast(treecrops_selected)
  
  # set ocean / water (-1) to na 
  treecrops_rast[treecrops_rast<0] <- NA
  
  # Extract treecrops in one single layer (to discriminate between plantation and crops for impact assessment)
  tot_treecrops_area <- sum(treecrops_rast)
  
  # Resampling --------------------------------------------------------------
  # resample Cropgrid raster to LUH2 resolution

  
  ### 
  # resample and store plantation layer directly? 
  
  LUH2_layer_index <- year - LUH2_start_year + 1
  
  # select crop layers from LUH2 (perannual and annual C3 and C4 crops)
  crop_layers_LUH2 <- c(paste0("c3ann_",LUH2_layer_index),paste0("c4ann_",LUH2_layer_index),
                        paste0("c3per_",LUH2_layer_index),paste0("c4per_",LUH2_layer_index), 
                        paste0("c3nfx_",LUH2_layer_index))
  
  # Subset the rasters
  crops_LUH2 <- LUH2_rast[[crop_layers_LUH2]]
  crops_LUH2_sum <- sum(crops_LUH2, na.rm = T)
  
  # unit of croparea is in percentage per gridcell --> convert this to ha
  LUH2_crop_ha <- crops_LUH2_sum*cellSize(crops_LUH2_sum, unit = "ha")
  
  # resample cropgrid to match LUH2 resolution 
  # unit of cells is area of treecrop per cell 
  CG_treecrops_resampled <- project(tot_treecrops_area, LUH2_crop_ha, method = "sum",
                                    threads=TRUE)
  
  # adapt total sum of treecrops and crops (should not exceed the LUH2 dataset)
  # thus read crops 
  crop_files_crops <- crop_files[!(crop_files%in%treecrops_selected)]
  
  crop_types_stack <- rast(crop_files_crops)
  
  # set ocean / water (-1) to na 
  crop_types_stack[crop_types_stack<0] <- NA
  
  # Extract crops in one single layer (to discriminate between plantation and crops for impact assessment)
  crop_area<- sum(crop_types_stack, na.rm=T)
  
  CG_crop_area_resampled <- project(crop_area, LUH2_crop_ha, method = "sum",
                                    threads=TRUE)

  # make sure that the overall sum of treecrops and crops does not exceed LUH2 layer
  scale_to_LUH2 <- function(crops, treecrops, LUH2) {
    crop_sum <- sum(c(crops, treecrops), na.rm = TRUE)
    
    # scale both crops and treecrops so that total matches LUH2
    scaling_factor <- LUH2 / crop_sum
    crops_scaled   <- crops * scaling_factor
    tc_scaled      <- treecrops * scaling_factor 
    
    return(list(crops_scaled = crops_scaled, 
                treecrops_scaled = tc_scaled))
  }
  
  # Apply scaling 
  crops_r_cor <- scale_to_LUH2(CG_crop_area_resampled,CG_treecrops_resampled, LUH2_crop_ha)
  CG_crop_area_final <- crops_r_cor[[1]]
  CG_treecrops_final<- crops_r_cor[[2]]


  # write treecrops as a raster to use this later as plantation file 
  writeRaster(CG_treecrops_final, paste0(out_path,"treecrops_CG_",year,".tif")
              , overwrite =T)
  
  #save remaining area  
  writeRaster(CG_crop_area_final, paste0(out_path,"LUH2_crops_after_CG_",year,".tif")
              , overwrite =T)
  gc()
}

#####

# for the sensitivity analysis extend first submission TS to 2024
####
out_path <- "data/02_resampled/LUH2_GCB2025/Cropgrid/"
if (!dir.exists(out_path)) {dir.create(out_path)}
croptype_path <- "data/05_crop_types/CG/time_series/"
mapbiomas_path <- "data/05_crop_types/CG/time_series_mapbiomas_patched/"

# Define treecrops
treecrops <- c("abaca", "agave", "almond", "apple", "apricot", "areca", "avocado", 
               "banana", "carob", "cashew", "cashewapple", "cherry", "chestnut", 
               "citrusnes", "coconut", "cocoa", "coffee", "date", "fig", "fruitnes", 
               "grape", "grapefruitetc", "hazelnut", "karite", "kiwi", "kolanut", 
               "lemonlime", "mango", "oilpalm","olive", "orange", "papaya", "peachetc", "pear", 
               "persimmon", "pimento", "pineapple", "pistachio", "plantain", "plum", 
               "quince", "rasberry", "rubber", "sourcherry", "stonefruitnes", 
               "tea", "tung", "vanilla", "walnut")


## LUH2 dataset

path_LUH2 <- "../04_Intensification_TS_expansion/data/LUH_update_states4.nc"
LUH2_rast <- rast(path_LUH2)
LUH2_start_year <- 850

###### 
# calculate treecrops


for (year in c(2020:2024)){
  message("Now processing year ", year)
  
  crop_files <- list.files(croptype_path, pattern = paste0(year,".tif$"), full.names = T)
  crop_files<- crop_files[crop_files!="data/01_raw/CROPGRID/CROPGRIDSv1.08_NC_maps/Countries_2018.nc"]
  
  # prefer the MapBiomas-patched file where a crop has one for this year;
  # fall back to the original back-cast (pred_<crop>_<year>.tif) otherwise
  #crop_files_patched <- list.files(mapbiomas_path, pattern = paste0(year,"_mapbiomas.tif$"), full.names = T)
  #patched_as_orig_basename <- sub("_mapbiomas\\.tif$", ".tif", basename(crop_files_patched))
  #crop_files <- crop_files[!(basename(crop_files) %in% patched_as_orig_basename)]
  #crop_files <- c(crop_files, crop_files_patched)
  
  treecrops_selected <- crop_files[grepl(paste0(treecrops, collapse =
                                                  "|"), crop_files)]
  
  # rast 
  treecrops_rast <- rast(treecrops_selected)
  
  # set ocean / water (-1) to na 
  treecrops_rast[treecrops_rast<0] <- NA
  
  # Extract treecrops in one single layer (to discriminate between plantation and crops for impact assessment)
  tot_treecrops_area <- sum(treecrops_rast)
  
  # Resampling --------------------------------------------------------------
  # resample Cropgrid raster to LUH2 resolution
  
  
  ### 
  # resample and store plantation layer directly? 
  
  LUH2_layer_index <- year - LUH2_start_year + 1
  
  # select crop layers from LUH2 (perannual and annual C3 and C4 crops)
  crop_layers_LUH2 <- c(paste0("c3ann_",LUH2_layer_index),paste0("c4ann_",LUH2_layer_index),
                        paste0("c3per_",LUH2_layer_index),paste0("c4per_",LUH2_layer_index), 
                        paste0("c3nfx_",LUH2_layer_index))
  
  # Subset the rasters
  crops_LUH2 <- LUH2_rast[[crop_layers_LUH2]]
  crops_LUH2_sum <- sum(crops_LUH2, na.rm = T)
  
  # unit of croparea is in percentage per gridcell --> convert this to ha
  LUH2_crop_ha <- crops_LUH2_sum*cellSize(crops_LUH2_sum, unit = "ha")
  
  # resample cropgrid to match LUH2 resolution 
  # unit of cells is area of treecrop per cell 
  CG_treecrops_resampled <- project(tot_treecrops_area, LUH2_crop_ha, method = "sum",
                                    threads=TRUE)
  
  # adapt total sum of treecrops and crops (should not exceed the LUH2 dataset)
  # thus read crops 
  crop_files_crops <- crop_files[!(crop_files%in%treecrops_selected)]
  
  crop_types_stack <- rast(crop_files_crops)
  
  # set ocean / water (-1) to na 
  crop_types_stack[crop_types_stack<0] <- NA
  
  # Extract crops in one single layer (to discriminate between plantation and crops for impact assessment)
  crop_area<- sum(crop_types_stack, na.rm=T)
  
  CG_crop_area_resampled <- project(crop_area, LUH2_crop_ha, method = "sum",
                                    threads=TRUE)
  
  # make sure that the overall sum of treecrops and crops does not exceed LUH2 layer
  scale_to_LUH2 <- function(crops, treecrops, LUH2) {
    crop_sum <- sum(c(crops, treecrops), na.rm = TRUE)
    
    # scale both crops and treecrops so that total matches LUH2
    scaling_factor <- LUH2 / crop_sum
    crops_scaled   <- crops * scaling_factor
    tc_scaled      <- treecrops * scaling_factor 
    
    return(list(crops_scaled = crops_scaled, 
                treecrops_scaled = tc_scaled))
  }
  
  # Apply scaling 
  crops_r_cor <- scale_to_LUH2(CG_crop_area_resampled,CG_treecrops_resampled, LUH2_crop_ha)
  CG_crop_area_final <- crops_r_cor[[1]]
  CG_treecrops_final<- crops_r_cor[[2]]
  
  
  # write treecrops as a raster to use this later as plantation file 
  writeRaster(CG_treecrops_final, paste0(out_path,"treecrops_CG_",year,".tif")
              , overwrite =T)
  
  #save remaining area  
  writeRaster(CG_crop_area_final, paste0(out_path,"LUH2_crops_after_CG_",year,".tif")
              , overwrite =T)
  gc()
}

