# Data file for shiny 
library(dplyr)
library(stringr)

# load data 
data_type <- read.csv("data_plotting_with_region_croptype.csv") 

data_type <- data_type|>
  as_tibble() |>
  select(!c("X", "SOV_A3","CONTINENT","REGION_UN","SUBREGION","REGION_WB"))

data_type$crop_group <- str_trim(data_type$crop_group)
data_type <- data_type |>
  mutate(
    LU_type = case_when(
      lu_type == "crops" ~ "Cropland",
      lu_type == "pasture" ~ "Pasture",
      lu_type == "plantations" ~ "Plantations",
      lu_type == "abandoned" ~ "Abandoned",
      lu_type == "rangeland" ~ "Pasture"
    ),
    intensity = str_to_title(intensity),
    intensity = ifelse(is.na(intensity),"Abandoned",intensity),
    intensity_f = factor(intensity,levels = c("Abandoned","High","Medium","Low")), 
    lu_type_f = factor(LU_type, levels = c("Abandoned","Cropland","Plantations","Pasture")),
    crop_group = ifelse(is.na(crop_group), LU_type, crop_group),
    region_f = factor(country_group, levels = c("Brazil", "Peru", "Other South America", 
                                                "North America",
                                                "Indonesia", "Other Asia and Pacific", 
                                                "DR Congo", "Tanzania", "Other Africa", 
                                                "Europe")),
    crop_group_f = factor(crop_group, levels = c("rice",                   
                                               "maize",                       
                                               "other cereals", 
                                               "soybeans" ,                   
                                               "oilpalm",                    
                                               "other oilseed crops", 
                                               "bananas",                   
                                               "other fruits and nuts",       
                                               "vegetables, melons and root/tuber crops",
                                               "leguminous crops",          
                                               "sugar beverage and spice crops",  
                                               "other crops",
                                               "Pasture",
                                               "Abandoned")), 
    impact_change = impact_change*100) 
cols <- c(
  # Abandoned land
  "Secondary land" = "#DCD4D0",
  "High" = "#01665e",
  "Medium" = "#35978f",
  "Low"  = "#80cdc1",
  "Cropland" = "#8c510a",
  "Pasture" = "#dfc27d",
  "Plantations" = "#bf812d",
  "Abandoned" = "#DCD4D0",
  "Brazil" =  "#900000", 
  "Peru" = "#ee2400",
  "Other South America" = "#FFB2B2",
  "Mexico" = "#74add1",
  "North America" =  "#4575b4",
  "Indonesia"="#762a83", 
  "China" = "#9970ab", 
  "Other Asia and Pacific" = "#c2a5cf", 
  "DR Congo" = "#6B990F",
  "Tanzania" = "#A3CC51", 
  "Other Africa" = "#E5FFB2", 
  "Europe" = "#E69F60",
    "bananas" = "#e3b505",                     # golden yellow
    "leguminous crops" = "#A7BBEC",            # olive  
    "maize" = "#8CD790",                       # medium purple 
    "oilpalm" = "#D81E5B",                     # deep red  
    "other cereals" = "#9BDEAC",               # cyan-blue  
    "other crops" = "#7f7f7f",                  # dark grey
    "other fruits and nuts" = "#FFA420",       # light yellow-beige  
    "other oilseed crops" = "#F0544F",         # light coral  
    "rice" = "#588157",                        # viol et  
    "soybeans" = "#BE6C77",                    # brown  
    "sugar beverage and spice crops" = "#B3EBF2", # pink-magenta  
    "vegetables, melons and root/tuber crops" = "#AD8A64"
)

# Load once in data.R
raster_lookup <- readRDS("raster_lookup.rds")




coolwarm_hcl <-   c("#002F70","#265BAB","#6889D0","#A3B4E5","#A3B4E5","#D3DBF4","#D3DBF4","#CCDBEE","lightgrey","lightgrey",
                    "#FCFDBFFF", "#FDE992" ,"#FEC98DFF","#FD9567FF","#FA815FFF","#F4685CFF" ,"#E85362FF", "#D6456CFF" ,"#C03A76FF" ,"#AB337CFF")
