#' This script combines all information from intensity dataset and biodiversity impact assessment 

#' Therefore the following scripts have to be runned first 
#' intensity raster dataset has to be copiled for the land use types (scripts I_x)
#' Then, this rasterdata will be converted to country csv dataset (Python script I_04)
#' Calculate biodiversity impact (also on country level, script B_x... in Python)
#' Now: use this script to combine the single csv files into one entire, final csv table that can be used to do graphs in tableau and R 
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)
library(data.table)
library(terra)
library(writexl) # for tableau in xls 

intensity_folder <- ("./output/area_intensity/LUH2/CG/") # where intensity data is stored 
bia_path <- "./output/biodiversity_impact_assessment/LUH2"

lu_types = c("pasture","crops","plantations","abandoned")

# chose which version of pastures you want 
past_vs <- "v2"

#############

# croptype data -----------------------------------------------------------

# First: compile a table with croptype data. --> available for crops and plantations
bia_list <- list()
for (lu in c("crops","plantations")){

  file_pattern = "type.*\\.csv$"
  
  bia_files <- list.files(
    path = file.path(bia_path, lu),
    pattern = file_pattern,
    full.names = TRUE
  )
  
  for (file_path in bia_files) {
    # Extract the land use type and the year from the filename
    land_use <- lu
    
    # Read the CSV file
    data <- read.csv(file_path)
    year <- str_extract(file_path, "\\d{4}")
    data$year <- as.numeric(year)
    # change the year column
    
    # problem: we used different ways to calculate impact assessment. Therefore adjust naming of colums
    
    if("land_use" %in% colnames(data)) {
      colnames(data)[4] <- c("lu_type")} else if (!("lu_type" %in% colnames(data))) {
      data$lu_type = land_use
    }
    if("impact_sum" %in% colnames(data)){
      colnames(data)[2] <- c("impact")} 
    
    # Append the data frame to the list
    if(is.null(bia_list[[paste(land_use)]])){
      bia_list[[paste(land_use)]] <- data
    } else {
      bia_list[[paste(land_use)]] <- rbind(bia_list[[paste(land_use)]],data)
    }
  }
}
croptype_df <- rbindlist(bia_list)

# remove all datapoints that are 0 (better overview, less memory)
croptype_df <- croptype_df %>% 
  filter(area_ha != 0)%>% 
  mutate(intensity = as.factor(intensity))|>
  mutate(intensity = recode(intensity, "light" = "low", "med" = "medium", "high" = "high"),
         intensity = factor(intensity, levels = c("low", "medium", "high")))


# pasture data ------------------------------------------------------------

###### area csv files 
if(past_vs == "v1"){
  # for v1 of pastures (version where intensity defined by cellsize)
  lu_area_files_path <- list.files(intensity_folder, pattern = "^pasture.|^abandoned.|^rangeland.*.csv$",full.names = T)
} else if(past_vs == "v2"){
  # for v2 of pastures (version where intensity defined by pasture area)
  lu_area_files_path <- list.files(intensity_folder, pattern = "^abandoned.|^rangeland.*.csv$",full.names = T)
  # add pastures of v2 (stored in subfolder)
  lu_area_files_path <- c(lu_area_files_path,
                          list.files(paste0(intensity_folder,"v2"), 
                                     pattern = "^pasture.*.csv$",full.names = T))
} else {stop("Please define version of pastures. \t
              v1: Version where intensity was defined by cell area. \t
              v2: Version where intensity was defined by pasture area") }


lu_area_list <- list()

# each file has following columns: country_id, intensity, area_m2, SOV_A3, SOVEREIGNT
# add: land use and year from the file names 

for (file_path in lu_area_files_path) {
  # Extract the land use type and the year from the filename
  land_use <- str_extract(file_path, "pasture|crops|plantations|builtup|forest|abandoned|rangeland")
  year <- str_extract(file_path, "\\d{4}")
  
  # Read the CSV file
  data <- read.csv(file_path)
  
  data$lu_type <- as.factor(land_use) 
  data$year <- as.numeric(year)
  if (land_use == "rangeland"){
    data$intensity <- as.integer(1)}
  # Append the data frame to the list
  lu_area_list[[paste(land_use, year)]] <- data
}

# change abandoned land names 
# for (i in c(1:20)) {
#   names(lu_area_list[[i]])[c(1,2)] <- c("GEOUNIT","area_m2")
#   lu_area_list[[i]][,"lu_type"] = "abandoned"
#   lu_area_list[[i]][,"intensity"] = 0
#   }


area_data <- rbindlist(lu_area_list,use.names = T, fill = T)
area_data$intensity = as.factor(area_data$intensity)


###### biodiversity files 

# read in biodiversity impact assessment data 
lu_types = c("pasture","crops","plantations","abandoned")
bia_path <- "./output/biodiversity_impact_assessment/LUH2"

bia_list2 <- list()# separate list for pasture and abandoned land (other dataformat)

for (lu in c("pasture","abandoned","rangeland")){
  file_pattern = ".impact.*\\.csv$"
  if (lu == "pasture"){
    if(past_vs == "v1"){
      bia_files <- list.files(
        path = file.path(bia_path, lu),
        pattern = file_pattern,
        full.names = TRUE
      )
    } else if(past_vs == "v2"){
      bia_files <- list.files(
        path = file.path(bia_path, lu,"v2"),
        pattern = file_pattern,
        full.names = TRUE
      ) } 
  } else {
    bia_files <- list.files(
      path = file.path(bia_path, lu),
      pattern = file_pattern,
      full.names = TRUE
    )
  }
  
  

  for (file_path in bia_files) {
    print(file_path)
    # Extract the land use type and the year from the filename
    land_use <- lu
  
    # Read the CSV file
    data <- read.csv(file_path)
    year <- str_extract(file_path, "\\d{4}")
    data$year <- as.numeric(year)
    # change the year column
  
    # problem: we used different ways to calculate impact assessment. Therefore adjust naming of colums
  
    if("land_use" %in% colnames(data))
    {colnames(data)[4] <- c("lu_type")} else if (!("lu_type" %in% colnames(data))) {
      data$lu_type = land_use
    }
    if("impact_sum" %in% colnames(data))
    {colnames(data)[colnames(data)== "impact_sum"] <- "impact"} 
  
    # Append the data frame to the list
    if(is.null(bia_list2[[lu]])){
      bia_list2[[lu]] <- data
    } else {
      bia_list2[[lu]] <- rbind(bia_list2[[lu]],data)
    }
  }
  
  if(lu == "abandoned"){
    bia_list2[[lu]]$intensity <- NULL
  } else if (lu == "rangeland"){
    bia_list2[[lu]]$intensity <- as.integer(1)
  }
}

# combine pasture area and pasture biodiversity impact 
bia_df2 <- list_rbind(bia_list2)
bia_df2$intensity <- as.factor(bia_df2$intensity)

pasture_tot <- bia_df2 |>
  select(!SOV)|>
  left_join(area_data, by = join_by(year==year,
                                    lu_type == lu_type, 
                                    country == GEOUNIT,
                                    intensity == intensity)) |>
  mutate(intensity = recode(intensity, `1` = "low", `2` = "medium", `3` = "high"),
         intensity = factor(intensity, levels = c("low", "medium", "high")))



# combine both cropgrids and pasture --------------------------------------------
# add continent etc 
shpcountries <- vect('H:/02_Projekte/02_LUC biodiversity paper/02_data/country_shp/ne_110m_admin_0_countries.shp')
shp_df <- as.data.frame(shpcountries)[, c("SOV_A3","CONTINENT", "SUBREGION","REGION_UN",
                                          "REGION_WB","GEOUNIT")]

# combine data to one df
total_data <- croptype_df |> 
  bind_rows(pasture_tot) |>
  full_join(shp_df, by = join_by(country == GEOUNIT)) |>
    filter(area_ha != 0)

total_data <- unique(total_data)
total_data$SOV <- NULL

total_data$impact <- total_data$impact*10000
sum(total_data$impact)

total_change <- total_data
sum(total_change$area_ha[total_change$crop_type == "rice"], na.rm=T)

# remove 2020 data (old!)
total_change <- total_change|>
  filter(year !=2020)

# add changes
total_change_2000 <- total_change|> 
  filter(year == 2000)|>
  rename(impact_2000 = "impact", 
         area_2000 = "area_ha")

total_change <- total_change |>
  left_join(
    total_change_2000, 
    by = c("country", "intensity", "crop_type", "lu_type", "SOV_A3", "CONTINENT",
           "SUBREGION","REGION_UN","REGION_WB")
  ) |>
  mutate(
    
    # compute absolute changes 
    impact_change = impact - impact_2000, 
    area_change = area_ha - area_2000
  ) |>
  rename(year = "year.x")|>
  select(-c(year.y, impact_2000, area_2000))

# write csv and excel

write.csv(total_change, paste0("./output/lu_int_bia_area_LUH2_CROPGRID_timeseries_2000-2019.csv"),
          row.names = F)

write_xlsx(total_change, 
           paste0("./output/lu_int_bia_area_LUH2_CROPGRID_timeseries_2000-2019.xlsx"))

###############


####
# Cleaned dataset to be used for plotting
###
total_change <- read.csv("output/lu_int_bia_area_LUH2_CROPGRID_timeseries_2000-2019.csv")
croptypes_group <- read.csv("./data/05_crop_types/CG/FAOSTAT/FAOSTAT_codes/crop_groups.csv",
                                                        header = T)

total_change <- total_change |>
  # LU_type in Titel-Case
  mutate(LU_type = case_when(
    lu_type == "crops" ~ "Cropland",
    lu_type == "pasture" ~ "Pasture",
    lu_type == "plantations" ~ "Plantation",
    lu_type == "abandoned" ~ "Abandoned",
    lu_type == "rangeland" ~ "Pasture"
  )) |>
  mutate(intensity = str_to_title(intensity))|>
  mutate(intensity = case_when(
    intensity == "Light" ~"Low",
    intensity == "Med" ~"Medium",
    intensity == "High" ~ "High"
  ))|>
  mutate(country_group = case_when(
    country == "Brazil" ~ "Brazil",
    country == "Mexico" ~ "Mexico",
    country == "Indonesia" ~ "Indonesia",
    country == "China" ~ "China",
    country == "Democratic Republic of the Congo" ~ "DR Congo",
    country == "Tanzania" ~ "Tanzania",
    CONTINENT == "South America" ~ "Other South America",
    CONTINENT %in% c("Asia", "Oceania") ~ "Other Asia and Pacific",
    CONTINENT == "Africa" ~ "Other Africa",
    CONTINENT == "Europe" ~ "Europe",
    CONTINENT == "North America" ~ "Other North America"
  ))|>
  left_join(croptypes_group, join_by(crop_type == crop_type))

total_change_plot_read<- total_change|>
  select("country","year","LU_type","intensity","crop_type","crop_group","country_group","SOV_A3",
         "CONTINENT","SUBREGION","REGION_UN","impact","impact_change",
         "area_ha","area_change")
write.csv(total_change_plot_read, "output/biodiversity_impact_assessment_2000-2019.csv", row.names=F)
total_change_plot_read <- read.csv("output/biodiversity_impact_assessment_2000-2019.csv")
write_xlsx(total_change_plot_read, "output/biodiversity_impact_assessment_2000-2019.xlsx")
