#################################
## pasture intensity 
# Tian dataset from 2000 to 2019
####################################
library(terra)
library(dplyr)

### read files 
N_past_files <- list.files("./data/01_raw/Tian_pasture_fertilizer/",
                           pattern = ".nc$", full.names = T)

out_path <- "./data/03_intensity/LUH2_GCB2025/pasture/"
if (!dir.exists(out_path)){dir.create(out_path)}

path_LUH2 <- "../04_Intensification_TS_expansion/data/LUH_update_states4.nc"
LUH2_rast <- rast(path_LUH2)

### Assign intensity levels based on Tian pasture dataset  

years = c(2000:2019)

for (year in years){
  message("Now processing ", year)
  # Read pasture N- Files from Tian et al. 
  idx_year <- year-1960
  N_past_Nh4 <- rast(N_past_files[1])[[idx_year]]
  N_past_No3 <- rast(N_past_files[2])[[idx_year]]
  N_past_Nmana <- rast(N_past_files[3])[[idx_year+101]]
  N_past_Nmand <- rast(N_past_files[4])[[idx_year+101]]
  #N_past_rang <- rast(N_past_files[5])[[idx_year+101]]
  
  N_tot_stack <- c(N_past_Nh4,N_past_No3,N_past_Nmana,N_past_Nmand)#,N_past_rang)
  # build sum as total N-input is relevant 
  N_tot <- sum(N_tot_stack, na.rm = T)
  
  # LUH2 files
  LUH2_idx <- year - 850
  past <- LUH2_rast[[paste0("pastr_",LUH2_idx)]]
  
  # resample N-data to LUH2 resolution 
  N_tot_p <- project(N_tot, past, method = "sum")


  # multiply pasture area by cellsize 
  past_ha <- past*cellSize(past, unit = "ha")
  
  N_tot_p_kg <- N_tot_p/1000
  
  N_tot_past <- ifel(past>0,N_tot_p_kg/past_ha,0)
  N_tot_past_val <- values(N_tot_past, na.rm=T)
  # boxplot(N_tot_past_val[N_tot_past_val<max(N_tot_past_val)])
  # max(N_tot_past_val)
  # hist(N_tot_past_val)
  
  # now classify 
  class_matrix <- matrix(c(0, 50, 1,  # Low
                           50, 150, 2, # Medium
                           150, Inf, 3),   # High
                         ncol=3, byrow=TRUE)
  
  N_classified <- classify(N_tot_past, class_matrix,include.lowest=TRUE)
  low <- (N_classified == 1) * past_ha
  med <- (N_classified == 2) * past_ha
  high <- (N_classified == 3) * past_ha

  N_class_stack <- c(low,med,high)
  longnames(N_class_stack) <- "classified by N-input (kg/ha)"
  names(N_class_stack) <- c("past_low","past_med","past_high")
  
  writeRaster(N_class_stack, paste0(out_path,"pasture_intensity_",year,".tif"),
              overwrite = T)
}




