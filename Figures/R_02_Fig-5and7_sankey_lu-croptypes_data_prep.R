# Sankey intensity land use type region 
# data preparation 

# Convert total csv table to 
#   könntest du mir das lu_int_bia.... file in folgendem Format erstellen & teilen:
#   Spalte A: country aggregation (int)
#   Spalte B: Intensity (int)
# Spalte C: land use type (int)
# Spalte D: crop type (int)
# Spalte E: Impact 
# Spalte F: year

library(dplyr)
library(openxlsx)

df <- read.csv("./output/lu_int_bia_area_LUH2_CROPGRID_timeseries_2000-2019v2.csv")

# add country group 
df <- df |>
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
    CONTINENT == "North America" ~ "North America"
  ))

df$intensity[is.na(df$intensity)] <- "abandoned"
# change rangeland to pasture light 

df$lu_type[df$lu_type == "rangeland"] <- "pasture"
df_code <- df

# order of country group 
country_order <- c("Brazil","Mexico","Other South America", 
                   "Indonesia","China","Other Asia and Pacific", 
                   "DR Congo", "Tanzania", "Other Africa", 
                   "Europe", "North America")


lu_order <- c("crops", "plantations", "pasture", "abandoned")
# 1. Create coded table ---------------------------------------------------

data_sankey <- df %>%
  mutate(
    country = as.numeric(factor(country)),          # Spalte A
      intensity = ifelse(is.na(intensity), "abandoned", intensity),
      intensity = recode(intensity,
                         "abandoned" = 3,
                         "low" = 2,
                         "medium" = 1,
                         "high" = 0),   # Spalte B
    lu_type = as.numeric(factor(lu_type, levels = lu_order)),   # Spalte C
    country_group = as.numeric(factor(country_group, levels = country_order))
  ) %>%
  select(
    country, intensity,lu_type,crop_type,country_group,      # reordered columns
    impact = impact,                             # Spalte E
    year= year, 
    impact_change = impact_change# Spalte F
  )

# safe table
# Create workbook
wb <- createWorkbook()

#addWorksheet(wb, "data_sankey_all_years")
# writeData(wb, "data_sankey_all_years", data_sankey)

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
  distinct(lu_type) %>%
  mutate(lu_key = as.numeric(factor(lu_type, levels = lu_order)))

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
saveWorkbook(wb, "./output/figures/sankey_data/sankey_data_lu.xlsx", overwrite = TRUE)


#####################################################################################################
#######################
# croptypes 
########################

unique(df$crop_type)
# top croptypes (most changes in pdf)

df <- read.csv("./output/data_croptypes.csv")

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
  mutate(
    country = as.numeric(factor(country)),          # Spalte A
    intensity = ifelse(is.na(intensity), "abandoned", intensity),
    intensity = recode(intensity,
                       "abandoned" = 3,
                       "low" = 2,
                       "medium" = 1,
                       "high" = 0),   # Spalte B
    lu_type = as.numeric(factor(lu_type)),   # Spalte C
    crop_num = as.numeric(factor(crop_group, levels = crop_order)),     # Spalte D
    country_group = as.numeric(factor(country_group, levels = country_order))
  ) %>%
  filter(year == 2019) %>%
  group_by(intensity, lu_type, crop_num, country_group)|>
  summarize(impact_change = sum(impact_change, na.rm=T) )

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
saveWorkbook(wb, "./output/figures/sankey_data/sankey_data_crops.xlsx", overwrite = TRUE)
