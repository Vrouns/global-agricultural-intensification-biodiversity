# plantations 

library(terra)
library(tidyr)
library(stringr)

out_dir <- "./data/03_intensity/LUH2_GCB2025/plantations/"
#dir.create(out_dir)

# Define years
start_year <- 2000
end_year <- 2024
years <- c(start_year:end_year)

CG_path <- "./data/02_resampled/LUH2_GCB2025/Cropgrid_mapbiomas_patched/"

# List all Treecrop files (created in script 01_02_03)

CG_files <- list.files(CG_path, full.names = TRUE, pattern = "treecrops_")

for (i in 1:length(CG_files)){
  if (i == 1){
    CG_rast <- sum(rast(CG_files[1]), na.rm = T)
  } else {
    S1 <- rast(CG_files[i])
    S1 <- sum(S1, na.rm = T)
    CG_rast <- c(CG_rast, S1)
    }
    
}
names(CG_rast) <- varnames(CG_rast)

# check treecrop proportion per gridcell to assess intensification 

Cover_part <- CG_rast/(cellSize(CG_rast, unit = "ha"))
summary(values(Cover_part, na.rm = T))

# plot(Cover_part)

# now assign intensity levels 
low <- (Cover_part <= 0.05) * CG_rast
med <- (Cover_part > 0.05 & Cover_part <= 0.25 ) * CG_rast
high <- (Cover_part > 0.25)*CG_rast 
time(high, tstep="years") <- 2000:2024


##################################
# store result rasters (each layer = 1 intensity level)

intensity_stacks <- list()

for (i in 1:length(years)) {
  year = years[i]
  lyr_low <- low[[i]]
  lyr_med <- med[[i]]
  lyr_high <- high[[i]]
  
  s <- c(lyr_low, lyr_med, lyr_high)
  names(s) <- c("low", "medium", "high")
  writeRaster(s, paste0(out_dir, "plantations_intensity_",year,".tif"), 
                        overwrite = T)
}



# Sensitivity analysis using different thresholds -------------------------
# Quantile based thresholds: 
cover_2015 <- Cover_part$treecrops_CG_2015
cover_2015_values <- values(cover_2015, na.rm=T)
cover_2015_values <- cover_2015_values[cover_2015_values > 0]
hist(cover_2015_values)
min(cover_2015_values)
max(cover_2015_values)
quantiles_cover <- quantile(cover_2015_values)
quantiles_cover <- round(quantiles_cover, digits = 4)
quantiles_cover

# data is heavy rightskewed 
# log transform values 
log_cover_2015 <- log(cover_2015_values)
quantiles_log <- quantile(log_cover_2015)

# filter values of cover to at least 2% min value 
cover_2015_values_over_2p <- cover_2015_values[cover_2015_values > .02]
hist(cover_2015_values_over_2p)
quantiles_cover_over_2p <- quantile(cover_2015_values_over_2p)
quantiles_cover_over_2p <- round(quantiles_cover_over_2p, digits = 4)
quantiles_cover_over_2p
# more conservative taking quantile based thresholds (3%, 5%)
# now assign intensity levels 
low <- (cover_2015 <= quantiles_cover_over_2p[2]) * cover_2015
med <- (cover_2015 > quantiles_cover_over_2p[2]& cover_2015 <= quantiles_cover_over_2p[3] ) * cover_2015
high <- (cover_2015 > quantiles_cover_over_2p[3])*cover_2015

plot(high)
time(high, tstep="years") <- years


s <- c(low, med, high)
names(s) <- c("low", "medium", "high")
writeRaster(s, paste0(out_dir, "sensitivity_analysis/plantations_intensity_",year,"_quantiles_over_2pct.tif"), 
              overwrite = T)

hist(s)
