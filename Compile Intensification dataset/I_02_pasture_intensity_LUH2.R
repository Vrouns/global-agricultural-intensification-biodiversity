#################################
## pasture intensity 
# Tian dataset from 2000 to 2019
####################################
library(terra)
library(dplyr)

N_path <- "D:/03_Intensification-fragmentation-CFs/data/01_raw/Tian_pasture_fertilizer/"
### read files 
N_past_files <- list.files(N_path,
                           pattern = ".nc$", full.names = T)

N_past_files_ext <-  list.files(paste0(N_path,"extended_2020-2024"),
                                pattern = "pas.*\\.tif$", full.names = T)

N_range_files_ext <- list.files(paste0(N_path,"extended_2020-2024"),
                                pattern = "range.*\\.tif$", full.names = T)



out_path <- "data/03_intensity/LUH2_GCB2025/pasture/"
if (!dir.exists(out_path)){dir.create(out_path)}

path_LUH2 <- "../04_Intensification_TS_expansion/data/LUH_update_states4.nc"
LUH2_rast <- rast(path_LUH2)

### Assign intensity levels based on Tian pasture dataset  

#Threshold definition 
# Baseline used: low 0-50, medium 50-150, high >150 kg / ha
# Conservative scenzario: low 0-25, med 25-100, high >100
low_threshold <- 50 # upper boundary of low-definition
med_threshold <- 150 # upper boundary of med-definition
class_matrix <- matrix(c(0, low_threshold, 1,  # Low
                         low_threshold, med_threshold, 2, # Medium
                         med_threshold, Inf, 3),   # High
                       ncol=3, byrow=TRUE)


# Base intensity definition -----------------------------------------------

years = c(2000:2019)
year = 2015
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
  LUH2_idx <- year - 849
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

  N_classified <- classify(N_tot_past, class_matrix,include.lowest=TRUE)
  #hist(values(N_classified))
  low <- (N_classified == 1) * past_ha
  med <- (N_classified == 2) * past_ha
  high <- (N_classified == 3) * past_ha

  N_class_stack <- c(low,med,high)
  longnames(N_class_stack) <- "classified by N-input (kg/ha)"
  names(N_class_stack) <- c("past_low","past_med","past_high")
  
  writeRaster(N_class_stack, paste0(out_path,"pasture_intensity_threshold",
                                    low_threshold,"_",med_threshold,"_",year,".tif"),
              overwrite = T)
}


# Extention ---------------------------------------------------------------
years <- 2020:2024
# Create a lookup table from the file names
N_ext_df <- data.frame(
  file = N_past_files_ext,
  filename = basename(N_past_files_ext),
  stringsAsFactors = FALSE
)

# Extract variable and year
N_ext_df$variable <- sub(
  "_(2020|2021|2022|2023|2024)\\.tif$",
  "",
  N_ext_df$filename
)

N_ext_df$year <- as.integer(
  sub(".*_(2020|2021|2022|2023|2024)\\.tif$", "\\1", N_ext_df$filename)
)

# Check the resulting lookup table
N_ext_df

for (year in years) {
  message("Now processing ", year)
  
  # Get files based on variable + year
  N_past_Nh4 <- rast(
    N_ext_df$file[
      N_ext_df$variable == "nfer_pas_nh4" &
        N_ext_df$year == year
    ]
  )
  
  N_past_No3 <- rast(
    N_ext_df$file[
      N_ext_df$variable == "nfer_pas_no3" &
        N_ext_df$year == year
    ]
  )
  
  N_past_Nmana <- rast(
    N_ext_df$file[
      N_ext_df$variable == "nmanure_app_pas" &
        N_ext_df$year == year
    ]
  )
  
  N_past_Nmand <- rast(
    N_ext_df$file[
      N_ext_df$variable == "nmanure_dep_pas" &
        N_ext_df$year == year
    ]
  )
  
  # Combine N inputs
  N_tot <- sum(
    c(N_past_Nh4,N_past_No3, N_past_Nmana,N_past_Nmand),
    na.rm = TRUE
  )
  
  # LUH2
  LUH2_idx <- year - 849
  past <- LUH2_rast[[paste0("pastr_", LUH2_idx)]]
  
  # Resample N data
  N_tot_p <- project(N_tot, past, method = "sum")
  
  # Pasture area
  past_ha <- past * cellSize(past, unit = "ha")
  
  # N input per hectare
  N_tot_p_kg <- N_tot_p / 1000
  
  N_tot_past <- ifel(past > 0,N_tot_p_kg / past_ha,0)
  
  # Classification
  N_classified <- classify(
    N_tot_past,
    class_matrix,
    include.lowest = TRUE
  )
  
  low  <- (N_classified == 1) * past_ha
  med  <- (N_classified == 2) * past_ha
  high <- (N_classified == 3) * past_ha
  
  N_class_stack <- c(low, med, high)
  
  longnames(N_class_stack) <- "classified by N-input (kg/ha)"
  names(N_class_stack) <- c(
    "past_low",
    "past_med",
    "past_high"
  )
  
  writeRaster(
    N_class_stack,
    paste0(out_path,"pasture_intensity_",year,".tif"
    ),
    overwrite = TRUE
  )
}


# Rangeland ---------------------------------------------------------------

### read files 
N_ran_files <- list.files(paste0(N_path,"/nmanure_dep_range/"),
                           pattern = ".nc$", full.names = T)

out_path <- "./data/03_intensity/LUH2_GCB2025/rangeland_class/"
if (!dir.exists(out_path)){dir.create(out_path)}

path_LUH2 <- "../04_Intensification_TS_expansion/data/LUH_update_states4.nc"
LUH2_rast <- rast(path_LUH2)

### Assign intensity levels based on Tian pasture dataset  

years = c(2000:2019)
for (year in years){
  message("Now processing ", year)
  # Read pasture N- Files from Tian et al. 
  idx_year <- year-1960
  manure_ran <- rast(N_ran_files)[[idx_year+101]]

  # LUH2 files
  LUH2_idx <- year - 849
  rang <- LUH2_rast[[paste0("range_",LUH2_idx)]]
  
  # resample N-data to LUH2 resolution 
  N_tot_p <- project(manure_ran, rang, method = "sum")
  
  
  # multiply pasture area by cellsize 
  past_ha <- rang*cellSize(rang, unit = "ha")
  
  N_tot_p_kg <- N_tot_p/1000
  
  N_tot_past <- ifel(rang>0,N_tot_p_kg/past_ha,0)
  N_tot_past_val <- values(N_tot_past, na.rm=T)
  # boxplot(N_tot_past_val[N_tot_past_val<max(N_tot_past_val)])
  # max(N_tot_past)
  # hist(N_tot_past_val)
  # classify using class matrix defined above
  N_classified_rang <- classify(N_tot_past, class_matrix,include.lowest=TRUE)
  low <- (N_classified_rang == 1) * past_ha
  med <- (N_classified_rang == 2) * past_ha
  high <- (N_classified_rang == 3) * past_ha
  
  N_class_stack <- c(low,med,high)
  longnames(N_class_stack) <- "classified by N-input (kg/ha)"
  names(N_class_stack) <- c("rang_low","rang_med","rang_high")
  
  writeRaster(N_class_stack, paste0(out_path,"range_intensity_threshold",
                                    low_threshold,"_",med_threshold,"_",year,".tif"),
              overwrite = T)
}

# Rangeland extention
for (year in 2020:2024){
  message("Now processing ", year)
  # Read pasture N- Files from Tian et al. 
  manure_ran <- rast(N_range_files_ext[grepl(paste0(year, "\\.tif$"), N_range_files_ext)])
  
  # LUH2 files
  LUH2_idx <- year - 849
  rang <- LUH2_rast[[paste0("range_",LUH2_idx)]]
  
  # resample N-data to LUH2 resolution 
  N_tot_p <- project(manure_ran, rang, method = "sum")
  
  
  # multiply pasture area by cellsize 
  past_ha <- rang*cellSize(rang, unit = "ha")
  
  N_tot_p_kg <- N_tot_p/1000
  
  N_tot_past <- ifel(rang>0,N_tot_p_kg/past_ha,0)
  N_tot_past_val <- values(N_tot_past, na.rm=T)
  
  N_classified_rang <- classify(N_tot_past, class_matrix,include.lowest=TRUE)
  low <- (N_classified_rang == 1) * past_ha
  med <- (N_classified_rang == 2) * past_ha
  high <- (N_classified_rang == 3) * past_ha
  
  N_class_stack <- c(low,med,high)
  longnames(N_class_stack) <- "classified by N-input (kg/ha)"
  names(N_class_stack) <- c("rang_low","rang_med","rang_high")
  
  writeRaster(N_class_stack, paste0(out_path,"range_intensity_threshold",
                                    low_threshold,"_",med_threshold,"_",year,".tif"),
              overwrite = T)
}
