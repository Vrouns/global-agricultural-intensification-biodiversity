# Sankey intensity land use type region 
# data preparation 

# Create a table with the following columns:
# Spalte A: country aggregation (integer)
# Spalte B: Intensity (integer)
# Spalte C: land use type (integer)
# Spalte D: crop type (integer)
# Spalte E: Impact 
# Spalte F: year (integer)

library(dplyr)
library(openxlsx)

df <- read.csv("output/biodiversity_impact_assessment_2000-2019_LUH2_GCB2025.csv")

# add country group 
df <- df |>
  filter(!is.na(country_group))

df$intensity[is.na(df$intensity)] <- "Abandoned"
# change rangeland to pasture light 
#unique(df$LU_type)
#df$lu_typ#df$lu_type[df$lu_type == "Rangeland"] <- "Pasture"
df_code <- df

# order of country group 
country_order <- c("Brazil","Peru","Other South America",
                   "North America",
                   "Indonesia","Other Asia and Pacific", 
                   "DR Congo", "Tanzania", "Other Africa", 
                   "Europe")


lu_order <- c("Cropland", "Plantation", "Pasture", "Abandoned")
# 1. Create coded table ---------------------------------------------------

data_sankey <- df %>%
  mutate(
    country = as.numeric(factor(country)),          # Spalte A
      intensity = ifelse(is.na(intensity), "Abandoned", intensity),
      intensity = recode(intensity,
                         "Abandoned" = 3,
                         "abandoned" = 3,
                         "Low" = 2,
                         "Medium" = 1,
                         "High" = 0),   # Spalte B
    lu_type = as.numeric(factor(LU_type, levels = lu_order)),   # Spalte C
    country_group = as.numeric(factor(country_group, levels = country_order))
  ) %>%
  select(
    country, intensity,lu_type,LU_type,crop_type,country_group,      # reordered columns
    impact = impact,                             # Spalte E
    year= year, 
    impact_change = impact_change# Spalte F
  )

# safe table
# Create workbook
wb <- createWorkbook()

addWorksheet(wb, "data_sankey_all_years")
writeData(wb, "data_sankey_all_years", data_sankey)

# safe only change 
data_sankey_change <- data_sankey |>
  filter(year == 2019)|>
  group_by(country_group, intensity, lu_type)|>
  summarize(impact_change = sum(impact_change, na.rm=T))|>
  select(country_group, intensity, lu_type, impact_change)

addWorksheet(wb, "data_sankey_change")
writeData(wb, "data_sankey_change", data_sankey_change)

# 2. Create Lookup table --------------------------------------------------

country_key <- df %>%
  distinct(country_group) %>%
  mutate(
         country_group_key = as.numeric(factor(country_group, levels = country_order)))

intensity_key <- tibble(
  intensity = c("abandoned", "low", "medium", "high"),
  Intensity = c(3,2,1,0)
)

lu_type_key <- df %>%
  distinct(LU_type) %>%
  mutate(lu_key = as.numeric(factor(LU_type, levels = lu_order)))

crop_key <- df %>%
  distinct(crop_type) %>%
  mutate(crop_key = as.numeric(factor(crop_type)))

# write as excel 

# Add each lookup table as a new sheet
addWorksheet(wb, "country_key")
writeData(wb, "country_key", country_key)

addWorksheet(wb, "intensity_key")
writeData(wb, "intensity_key", intensity_key)

addWorksheet(wb, "lu_type_key")
writeData(wb, "lu_type_key", lu_type_key)

addWorksheet(wb, "crop_key")
writeData(wb, "crop_key", crop_key)


# 3. Save everything in one excel -----------------------------------------

# Save Excel file
out_dir <- "output/figures/LUH2_GCB2025/sankeys/sankey_data/"
if(!dir.exists(out_dir))dir.create(out_dir, recursive = T)

saveWorkbook(wb, paste0(out_dir,"sankey_data_lu.xlsx"), overwrite = TRUE)


#####################################################################################################
#######################
# croptypes 
########################

unique(df$crop_type)
# top croptypes (most changes in pdf)

# assign crop order 
crop_order <- c("rice ",                   
                "maize",                       
                "other cereals", 
                "soybeans" ,                   
                "oilpalm ",                    
                "other oilseed crops", 
                "bananas",                   
                "other fruits and nuts",       
                "vegetables, melons and root/tuber crops",
                "leguminous crops",          
                "sugar beverage and spice crops",  
                "other crops")


# 1. Create coded table ---------------------------------------------------

# Coded table for intensity change 
data_sankey_change <- df %>%
  filter(!is.na(crop_type)) %>%
  mutate(
    country = as.numeric(factor(country)),          # Spalte A
    intensity = ifelse(is.na(intensity), "Abandoned", intensity),
    intensity = recode(intensity,
                       "Abandoned" = 3,
                       "Low" = 2,
                       "Medium" = 1,
                       "High" = 0),   # Spalte B
    lu_type = as.numeric(factor(LU_type)),   # Spalte C
    crop_num = as.numeric(factor(crop_group, levels = crop_order)),     # Spalte D
    country_group = as.numeric(factor(country_group, levels = country_order))
  ) %>%
  filter(year == 2019) %>%
  group_by(intensity, lu_type, crop_num, country_group)|>
  summarize(impact_change = sum(impact_change, na.rm=T) )

unique(data_sankey_change$crop_num)
unique(data_sankey_change$country_group)

# safe table
# Create workbook
wb <- createWorkbook()

addWorksheet(wb, "data_sankey_change_2")
writeData(wb, "data_sankey_change_2", data_sankey_change)

# 2. Create Lookup table --------------------------------------------------

crop_key <- df %>%
  distinct(crop_type, crop_group) %>%
  mutate(crop_type_key = as.numeric(factor(crop_type)), 
         crop_category_key = as.numeric(factor(crop_group,levels = crop_order)))

intensity_key <- tibble(
  intensity = c("abandoned", "low", "medium", "high"),
  Intensity = c(3,2,1,0)
)

ct_type_key <- df %>%
  distinct(crop_group) %>%
  mutate(lu_key = as.numeric(factor(crop_group, levels = crop_order)))

country_key <- df %>%
  distinct(country_group) %>%
  mutate(lu_key = as.numeric(factor(country_group, levels = country_order)))

# write as excel 

# Add each lookup table as a new sheet

addWorksheet(wb, "intensity_key")
writeData(wb, "intensity_key", intensity_key)

addWorksheet(wb, "ct_type_key")
writeData(wb, "ct_type_key", ct_type_key)

addWorksheet(wb, "crop_key")
writeData(wb, "crop_key", crop_key)

addWorksheet(wb, "country_key")
writeData(wb, "country_key", country_key)

# 3. Save everything in one excel -----------------------------------------

# Save Excel file
saveWorkbook(wb, "./output/figures/LUH2_GCB2025/sankeys/sankey_data/sankey_data_crops.xlsx", overwrite = TRUE)
