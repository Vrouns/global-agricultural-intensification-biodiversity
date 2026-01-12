### CROPTYPE IMPACTS WITH CROPGRIDS DATA
# Execute this script after: 
# L01
# Script is divided into several parts 
# 1. Crops and Plantation crop-type assignment 
#   - Load intensity raster 
#   - Resample crop_type rasters to intensity resolution 
#   - Store area of crop type per pixel in one raster per year (layers are the crop types)
#   - Store the share of the crop type by intensity in one raster (like 3% of all medium crops are bananas)
#   - Combine this with biodiversity impact assessment 
# 
# 2. Combine both land use datasets into one table 

# Load packages 
library(dplyr)
library(terra)
library(tidyr)

# define paths 
CG_org_path <- "../data/01_raw/CROPGRID/CROPGRIDSv1.08_NC_maps/"
CG_pred_path <- "../data/05_crop_types/CG/time_series/"
out_path <- "../data/05_crop_types/CG/"

c_intensity_path <- "../data/03_intensity/LUH2/crops/"
tc_intensity_path = "../data/03_intensity/LUH2/plantations/"
bia_path <- "../output/biodiversity_impact_assessment/LUH2/"

lu_types <- c("crops", "plantations")
int_levels <- c("light", "med", "high")

country_rast <- rast("../data/04_bia_inputs/LUH2/country_raster.tif")
shp_country <- vect("H:/02_Projekte/02_LUC biodiversity paper/02_data/country_shp/ne_110m_admin_0_countries.shp")


# Extract plantations
treecrops <- c("abaca", "agave", "almond", "apple", "apricot", "areca", "avocado", 
               "banana", "carob", "cashew", "cashewapple", "cherry", "chestnut", 
               "citrusnes", "coconut", "cocoa", "coffee", "date", "fig", "fruitnes", 
               "grape", "grapefruitetc", "hazelnut", "karite", "kiwi", "kolanut", 
               "lemonlime", "mango", "oilpalm","olive", "orange", "papaya", "peachetc", "pear", 
               "persimmon", "pimento", "pineapple", "pistachio", "plantain", "plum", 
               "quince", "rasberry", "rubber", "sourcherry", "stonefruitnes", 
               "tea", "tung", "vanilla", "walnut")
year = 2000

for (year in c(2000:2019)){
  # Get cropgrid files for this year
  if (year == 2020){# first: Resample CROPGRIDS to LUH2 resolution 
    
    croptype_files <- list.files(CG_org_path, full.names = T,pattern = 
                                   paste0(year,"_.nc$"))
    
    croptype_files<- croptype_files[croptype_files!="../data/01_raw/CROPGRID/CROPGRIDSv1.08_NC_maps/Countries_2018.nc"]  
    treecrop_files <- croptype_files[grepl(paste0(treecrops, collapse = "|"), croptype_files)]
    
    # Read a raster as blank file to resample the map to this resolution 
    crops_int_rast <- rast(paste0(intensity_path,"crops_intensity_",year,".tif"))
    
    # read croparea layer and reproject building pixel wise sum 
    process_crop_group <- function(files, ref_rast) {
      if (length(files) == 0) return(NULL)
      stack <- lapply(files, function(f) {
        r <- rast(f)
        r <- r[["croparea"]]
        rproj <- project(r, ref_rast, method = "sum")
        return(rproj)
      })
      rast(stack) |>
        flip(direction = "vertical") |>
        mask(., . > 0, maskvalues = 0)
    }
    
  } else {
    croptype_files <- list.files(CG_pred_path, full.names = T,pattern = 
                                   paste0(year,".tif$"))
    # Split into treecrops and other crops
    treecrop_files <- croptype_files[grepl(paste0(treecrops, collapse = "|"), croptype_files)]
    crop_files <- setdiff(croptype_files, treecrop_files)
    
    # Function to process both groups parallely 
    process_crop_group <- function(files, ref_rast) {
      if (length(files) == 0) return(NULL)
      
      stack <- lapply(files, function(f) {
        r <- rast(f)
        r_m <- mask(r, r > 0, maskvalues = 0) # remove negative values (= ocean /water)
        rproj <- project(r_m, ref_rast, method = "sum")
        return(rproj)
      })
      stack_r <- rast(stack)
      stack_r <- flip(stack_r, direction = "vertical")
    }
  }
  
  # Read a raster as blank file to resample the map to this resolution 
  crops_int_rast <- rast(paste0(c_intensity_path,"crops_intensity_",year,".tif"))
  tc_int_rast <- rast(paste0(tc_intensity_path,"plantations_intensity_",year,".tif"))
  
  # Process each group
  crops_r <- process_crop_group(crop_files, crops_int_rast)
  treecrops_r <- process_crop_group(treecrop_files, tc_int_rast)
  
  # name the layers 
  names(crops_r) <- gsub(paste0("^pred_(.*)_", year, "\\.tif$"), "\\1", basename(crop_files))
  names(treecrops_r) <- gsub(paste0("^pred_(.*)_", year, "\\.tif$"), "\\1", basename(treecrop_files))
  
  # Scaling to match LUH2 totals
  # make sure that LUH2 cropland extent is maintained: 
  
  scale_to_intensity <- function(r_stack, int_rast) {
    crop_sum <- sum(r_stack, na.rm = TRUE)
    int_sum  <- sum(int_rast, na.rm = TRUE)
    
    # distribute the difference proportionally to each crop
    r_scaled <- r_stack * (int_sum / crop_sum)
    
    return(r_scaled)
  }
  
  crops_r_cor <- scale_to_intensity(crops_r, crops_int_rast)
  treecrops_r_cor <- scale_to_intensity(treecrops_r, crops_int_rast)

  
  # Save
  writeRaster(crops_r_cor, paste0(out_path, "crop_types_", year, ".tif"), overwrite = TRUE)
  writeRaster(treecrops_r_cor, paste0(out_path, "treecrop_types_", year, ".tif"), overwrite = TRUE)
  
}



# crop types per Intensity 
for (year in c(2000:2019)){
  for (lu_type in lu_types) {
    if (lu_type == "crops"){lu_p <- "crop"}
    if (lu_type == "plantations"){lu_p <- "treecrop"}
    
    
    intensity_path <- paste0("../data/03_intensity/LUH2/", lu_type, "/")
    types_path <- paste0(out_path,lu_p, "_types_", year, ".tif")
    impact_path <- paste0(bia_path, "/", lu_type,"/", lu_type,"_impact_", year, ".tif")
    
    # Load intensity raster
    crops_int_rast <- rast(paste0(intensity_path, lu_type, "_intensity_", year, ".tif"))
    crops_int_rast[is.na(crops_int_rast)] <- 0
    
    # Load crop type raster
    crop_types_rast <- rast(types_path)
    
    # Output dir for intensity split
    int_out_dir <- paste0(out_path, "crop_types_int/")

    # Split by intensity & save shares
    for (i in seq_along(int_levels)) {
      bin_mask <- crops_int_rast[[i]] > 0
      ct <- crop_types_rast * bin_mask
      writeRaster(ct, paste0(int_out_dir, lu_type, "_", int_levels[i], "_", year, ".tif"), overwrite = TRUE)
      
      share <- ct / sum(ct, na.rm = TRUE)
      writeRaster(share, paste0(int_out_dir, "crop_types_share/", lu_type, "_", int_levels[i], "_share_", year, ".tif"), overwrite = TRUE)
    }
    
    
    ###########crops_int_rast#####
    # 2. Assign crop types to biodiversity impact
    ########
    
    impact_year <- rast(impact_path)
    impact_year[is.na(impact_year)] <- 0
    df <- data.frame()
    
    for (i in seq_along(int_levels)) {
      int_level <- int_levels[i]
      crop_share <- rast(paste0(int_out_dir, "crop_types_share/", lu_type, "_", int_level, "_share_", year, ".tif"))
      crop_share[is.na(crop_share)] <- 0
      
      area <- rast(paste0(int_out_dir, lu_type, "_", int_level, "_", year, ".tif"))
      crop_type_impact <- crop_share * impact_year[[i]]
      
      impact_country <- zonal(crop_type_impact, shp_country, fun = "sum", na.rm = TRUE)
      area_country <- zonal(area, shp_country, fun = "sum", na.rm = TRUE)
      
      impact_country$country <- shp_country$GEOUNIT
      impact_country$year <- year
      impact_country$intensity <- int_level
      
      cols_to_pivot <- 1:(ncol(impact_country) - 3)
      impact_long <- impact_country %>%
        pivot_longer(cols = cols_to_pivot, names_to = "crop_type", values_to = "impact")
      
      area_country$country <- shp_country$GEOUNIT
      area_long <- area_country %>%
        pivot_longer(cols = cols_to_pivot, names_to = "crop_type", values_to = "area_ha")
      
      df <- bind_rows(df, left_join(impact_long, area_long, by = c("country", "crop_type")))
    }
    
    # Save results
    write.csv(unique(df), paste0(bia_path, "/impact_", lu_type, "_type_", year, ".csv"), row.names = FALSE)
  }
    
} 


######################################################################
# Combine both datasets to one table --------------------------------------
library(data.table)
library(writexl)

bia_path <- "./output/biodiversity_impact_assessment/LUH2/"


# function to read csv and add land use type as column 
prep_table <- function(filepath, lu_type){
  df <- read.csv(filepath)
  df$lu_type <- lu_type
  return(df)
}

# start with crops 
crop_files <- list.files(paste0(bia_path,"crops/"), pattern = "type", 
                         full.names = T)
crop_list <- lapply(crop_files, prep_table, "crops")

#same for plantations 
plant_files <- list.files(paste0(bia_path,"plantations/"), pattern = "type", 
                          full.names = T)
plant_list <- lapply(plant_files, prep_table, "plantations")

# combine both lists to one single df 
lu_df <- rbindlist(crop_list)
lu_df <- rbind(rbindlist(plant_list), lu_df)

# add information from shapefile (continent etc)
shp_df <- as.data.frame(shp_country)[, c("SOV_A3","CONTINENT", "SUBREGION","REGION_UN",
                                         "REGION_WB","GEOUNIT")]
lu_df <- shp_df |>
  full_join(lu_df, by = join_by( GEOUNIT == country))

# modify: unknown got the parameter "sum" --> needs to be adjusted 
lu_df <- lu_df |>
  mutate(crop_type = if_else(crop_type == "sum", "unknown", crop_type))
  


# save df 
# write_xlsx(lu_df, paste0(bia_path,"crop_types_impact_all_years.xlsx"))
write.csv(lu_df, paste0(bia_path,"crop_types_impact_all_years.csv"))



# ################################
# # double check no types 
# ################################
# lu_df <- read.csv(paste0(bia_path,"crop_types_impact_all_years.csv"))
# lu_df_crops_2000 <- lu_df |>
#   filter(year == 2000, lu_type == "crops")
# sum(lu_df_crops_2000$impact, na.rm=T)*10000
# 
# test_uk <- lu_df |>
#   filter(crop_type == "unknown")
# unique(lu_df_crops_2000$crop_type)
# 
# crop_no_type <- read.csv("../output/biodiversity_impact_assessment/LUH2/crops/crops_impact_2000.csv")
# sum(crop_no_type$impact_sum)*10000
# 
# past_no_type <- read.csv("../output/biodiversity_impact_assessment/LUH2/pasture/pasture_impact_2000.csv")
# sum(past_no_type$impact_sum)*10000
