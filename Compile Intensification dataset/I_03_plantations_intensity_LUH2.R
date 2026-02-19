# plantations 

library(terra)
library(tidyr)
library(stringr)

out_dir <- "./data/03_intensity/LUH2_GCB2025/plantations/"
dir.create(out_dir)

# Define years
start_year <- 2000
end_year <- 2019
years <- c(start_year:end_year)

CG_path <- "./data/02_resampled/LUH2_GCB2025/Cropgrid/"

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
time(high, tstep="years") <- years


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

