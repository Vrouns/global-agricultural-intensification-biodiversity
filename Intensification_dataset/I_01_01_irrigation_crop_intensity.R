# Irrigation data 
# Mialyk 2024 data 
# water footprint 
# blue: from irrigation or capillary rise 
library(terra)


path_WF <- "./data/01_raw/Mialyk_water_footprint/"
out_path <- "./output/crops_timeseries/"

start_year = 2000
end_year = 2019
idx_start <- start_year - 1989
idx_end <- end_year - 1989
wf_blue <- rast(paste0(path_WF, "wf_prod_irrigated_blue_1990_2019.nc"))[[c(idx_start:idx_end)]] # only selected years
wf_blue_allyears <- rast(paste0(path_WF, "wf_prod_irrigated_blue_1990_2019.nc"))

# hilda_path
path_hilda <- "./data/raw_data_timeseries/hilda_crops_2/hildap_vGLOB-2.1-crop_geotiff_wgs84/states/"
hilda_15 <- rast(paste0(path_hilda, "hilda_plus_states_",2015,"_GLOB-v2_forest-crop_near_wgs84.tif"))

# LUH2
path_LUH2 <- "./data/01_raw/LUH2_data/states.nc"
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
out_path <- "./data/03_intensity/LUH2/0_intermediate/crops//"
wf_blue_proj <- project(wf_blue, y = c3_2015, method = "sum", filename = paste0(out_path, "/wf_blue_rep_", start_year, "-", end_year,"_LUH2.tif")) 
wf_blue_proj_all_years <- project(wf_blue_allyears, y = c3_2015, method = "sum")

# classify 
irr_values <- values(wf_blue_proj_all_years, na.rm = T) 
water_quant <- quantile(irr_values, na.rm = T)

# Include min and max ranges explicitly
class_matrix <- matrix(c(-Inf, water_quant[2], 1,  # Low
                         water_quant[2], water_quant[3], 2, # Medium
                         water_quant[3], Inf, 3),   # High
                       ncol=3, byrow=TRUE)


water_classified <- classify(wf_blue_proj, class_matrix, include.lowest=T,
                             filename = paste0(out_path,"/wf_blue_classified", start_year, "-", end_year,"_LUH2.tif"))