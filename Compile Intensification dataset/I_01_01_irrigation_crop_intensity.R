# Irrigation data 
# Mialyk 2024 data 
# water footprint 
# blue: from irrigation or capillary rise 
library(terra)


path_WF <- "D:/03_Intensification-fragmentation-CFs/data/01_raw/Mialyk_water_footprint/"
path_ext <- "data/02_b_extention_datasets/"
out_path <- "data/03_intensity/LUH2_GCB2025/crops/variables_class/irr_class/"
rep_path <- "data/02_resampled/LUH2/"

start_year = 2000
end_year = 2019
idx_start <- start_year - 1989
idx_end <- end_year - 1989
wf_blue <- rast(paste0(path_WF, "wf_prod_irrigated_blue_1990_2019.nc"))[[c(idx_start:idx_end)]] # only selected years
#wf_blue_allyears <- rast(paste0(path_WF, "wf_prod_irrigated_blue_1990_2019.nc"))
wf_extention <- rast(paste0(path_ext, "wf_blue_irrigated_2020_2024.tif"))

# LUH2
path_LUH2 <- "../04_Intensification_TS_expansion/data/LUH_update_states4.nc"
LUH2_rast <- rast(path_LUH2)

year <- 2015
LUH2_start_year <- 850
LUH2_layer_index <- year - LUH2_start_year + 1

# C3 crop layer
c3_crops <- grep("^c3ann_", names(LUH2_rast), value = TRUE)[LUH2_layer_index]

# Subset the raster
c3_2015 <- LUH2_rast[[c3_crops]]


# LUH2 procedure ----------------------------------------------------------
# first reproject, then classify
out_path <- "data/03_intensity/LUH2_GCB2025/crops/variables_class/fertilizer_class/"

##################################################
# try the other way round: classify first and then resample 
# classify 
water_classified_fine <- rast()

for (i in 1:length(years)){
  year <- years[i]
  idx <- year - 1989
  wf_blue_year <- wf_blue[[i]]
  irr_values <- values(wf_blue_year, na.rm = T) 
  water_quant <- quantile(irr_values, na.rm = T)
  # get quantiles for classification
  # Include min and max ranges explicitly
  class_matrix <- matrix(c(-Inf, water_quant[2], 1,  # Low
                           water_quant[2], water_quant[3], 2, # Medium
                           water_quant[3], Inf, 3),   # High
                         ncol=3, byrow=TRUE)
  water_classified_fine_year<- classify(wf_blue_year, class_matrix, include.lowest=T)
  water_classified_fine <- c(water_classified_fine, water_classified_fine_year)
}

# append also the extention 
for (i in 1:length(2020:2024)){
  wf_blue_year <- wf_extention[[i]]
  irr_values <- values(wf_blue_year, na.rm = T) 
  water_quant <- quantile(irr_values, na.rm = T)
  # get quantiles for classification
  # Include min and max ranges explicitly
  class_matrix <- matrix(c(-Inf, water_quant[2], 1,  # Low
                           water_quant[2], water_quant[3], 2, # Medium
                           water_quant[3], Inf, 3),   # High
                         ncol=3, byrow=TRUE)
  water_classified_fine_year<- classify(wf_blue_year, class_matrix, include.lowest=T)
  water_classified_fine <- c(water_classified_fine, water_classified_fine_year)
}
names_raster <- c(paste0("int_irrigated_blue_", years), paste0("int_irrigated_blue_", 2020:2024))
names(water_classified_fine) <- names_raster

# resample to LUH2
water_class_fine_proj <- project(water_classified_fine, LUH2_rast, method = "average")
water_class_fine_proj_round <- round(water_class_fine_proj)

writeRaster(water_class_fine_proj_round, paste0(out_path,"/wf_blue_classified_2000-2024.tif"), overwrite=T)

hist(water_class_fine_proj_round$int_irrigated_blue_2020)
hist(water_class_fine_proj_round$int_irrigated_blue_2024)

plot(water_class_fine_proj$int_irrigated_blue_2020)
plot(water_class_fine_proj$int_irrigated_blue_2024)




#### 
# now the extention









# Sensitivity analysis ----------------------------------------------------
# how would intensification definition change, if not water amount but irrigated area used? 
# (refer to Scherer et al. 2023 definition)
# test only for 2015
# Load original 
org_class <- rast(paste0(out_path,"/wf_blue_classified_2000-2019_LUH2.tif"))

# were extracted in jupyter notebook (used for projection project)

csv_dir <- "../04_Intensification_TS_expansion/data/trainings_data/02_tables/cropland/irrigated_area_ha_2015.csv"
csv_2015_irr <- read.csv(csv_dir)
csv_2015_irr <- csv_2015_irr[,-3] # remove year column
irr_area_2015 <- rast(csv_2015_irr[, c("lon", "lat", "value")], type = "xyz")
crs(irr_area_2015) <- crs(wf_blue_proj)
plot(irr_area_2015)

# apply Scherer et al. methodology to define area based quantile irrigation thresholds
irr_area_values <- csv_2015_irr$value[csv_2015_irr$value>0]
irr_area_quantiles <- quantile(irr_area_values, na.rm=T)
boxplot(irr_area_values)
# Include min and max ranges explicitly
class_matrix <- matrix(c(-Inf, irr_area_quantiles[2], 1,  # Low
                         irr_area_quantiles[2], irr_area_quantiles[3], 2, # Medium
                         irr_area_quantiles[3], Inf, 3),   # High
                       ncol=3, byrow=TRUE)

water_classified_area <- classify(irr_area_2015, class_matrix, include.lowest=T,
                             filename = paste0(out_path,"/wf_blue_classified_area_thresholds_2015.tif"),
                             overwrite=T)
hist(water_classified_area)


