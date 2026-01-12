# First half: inspect SPAM Data 
# Second half: 
## 1.) Resample SPAM to match LUH2 
## 2.) Substract treecrops of SPAM from LUH2 to have crop layer
## 3.) Write also resampled SPAM treecrops layer 

library(terra)

### Cropgrid
# Ocean/Water: flagged as “-1
out_path <- "./data/02_resampled/LUH2/Cropgrid/"

cropgrid_path <- "./data/01_raw/CROPGRID/CROPGRIDSv1.08_NC_maps/"
crop_files <- list.files(cropgrid_path, pattern = ".nc$", full.names = T)

# first only choose treecrops
treecrops <- c("abaca", "agave", "almond", "apple", "apricot", "areca", "avocado", 
               "banana", "carob", "cashew", "cashewapple", "cherry", "chestnut", 
               "citrusnes", "coconut", "cocoa", "coffee", "date", "fig", "fruitnes", 
               "grape", "grapefruitetc", "hazelnut", "karite", "kiwi", "kolanut", 
               "lemonlime", "mango", "oilpalm","olive", "orange", "papaya", "peachetc", "pear", 
               "persimmon", "pimento", "pineapple", "pistachio", "plantain", "plum", 
               "quince", "rasberry", "rubber", "sourcherry", "stonefruitnes", 
               "tea", "tung", "vanilla", "walnut")

years = 2020 # consider that for 2000 the naming notations are different 

treecrops_selected <- crop_files[grepl(paste0(treecrops, collapse =
  "|"), crop_files)]

# rast 

tree_croparea_list <- lapply(treecrops_selected, function(f) {
  r <- rast(f)
  r[["croparea"]]
})

treecrops_rast <- rast(tree_croparea_list)

# set ocean / water (-1) to na 
treecrops_rast[treecrops_rast<0] <- NA

# Extract treecrops in one single layer (to discriminate between plantation and crops for impact assessment)
tot_treecrops_area <- sum(treecrops_rast, na.rm = T)

tot_treecrops_area_val <- values(tot_treecrops_area, na.rm =T)
hist(tot_treecrops_area_val)


# Resampling --------------------------------------------------------------
# resample Cropgrid raster to LUH2 resolution

# read LUH2 to have a reference 

## LUH2 dataset

path_LUH2 <- "./data/01_raw/LUH2_data/states.nc"
LUH2_rast <- rast(path_LUH2)
LUH2_start_year <- 850

year = 2020
### 
# resample and store plantation layer directly? 

LUH2_layer_index <- year - LUH2_start_year + 1
  
# select crop layers from LUH2 (perannual and annual C3 and C4 crops)
crop_layers_LUH2 <- c(paste0("c3ann_",LUH2_layer_index),paste0("c4ann_",LUH2_layer_index),
                      paste0("c3per_",LUH2_layer_index),paste0("c4per_",LUH2_layer_index))

# Subset the rasters
crops_rast <- LUH2_rast[[crop_layers_LUH2]]
crops_rast_sum <- sum(crops_rast, na.rm = T)
# unit is in percentage --> convert this to ha
LUH2_crop_ha <- crops_rast_sum*cellSize(crops_rast_sum, unit = "ha")
  
# resample cropgrid to match LUH2 resolution 
# unit of cells is area of treecrop per cell 

CG_treecrops_resampled <- project(tot_treecrops_area, LUH2_crop_ha, method = "sum")
  
plot(CG_treecrops_resampled)
sum(values(CG_treecrops_resampled, na.rm =T ))
sum(values(tot_treecrops_area, na.rm =T ))


if(!dir.exists(out_path)){dir.create(out_path)} # check if out_dir exists, if not create it 

# write treecrops as a raster to use this later as plantation file 
writeRaster(CG_treecrops_resampled, paste0(out_path,"treecrops_CG_",year,".tif")
              , overwrite =T)
  
# assess remaining cropland area from LUH2 
remaining_cropland_ha = LUH2_crop_ha - sum(CG_treecrops_resampled, na.rm = T)
  
# clip negative values 
remaining_cropland_ha <- ifel(remaining_cropland_ha < 0, 0, remaining_cropland_ha)


#save remaining area  
writeRaster(remaining_cropland_ha, paste0(out_path,"LUH2_crops_after_CG_",year,".tif")
            , overwrite =T)


