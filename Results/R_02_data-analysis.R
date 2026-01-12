# Results data analysis 
library(dplyr)

full_data <- read.csv("./output/lu_int_bia_area_LUH2_CROPGRID_timeseries_2000-2019v2.csv")

# Global analysis 

# 1. Global analysis ---------------------------------------------------------


# percentage of which intensity increased / decreased since 2000 ? 
data_2000 <- full_data|>
  filter(year == 2000)|>
  group_by(lu_type, intensity, year)|>
  summarize(
            impact = sum(impact, na.rm=T))

data_2019<- full_data|>
  filter(year== 2019)|>
  group_by(lu_type, intensity, year)|>
  summarize(impact_change = sum(impact_change, na.rm=T),
            impact = sum(impact, na.rm=T),
            area_change = sum(area_change, na.rm=T))

data_2019$impact_change_percent <- (data_2019$impact / data_2000$impact)*100
View(data_2019)

# total biodiversity impact per year 

full_data|> 
  group_by(year)|>
  summarize(impact=sum(impact, na.rm=T))

# without secondary land 
full_data|> 
  filter(lu_type != "abandoned")|>
  group_by(year)|>
  summarize(impact=sum(impact, na.rm=T))



# 2. Focus on change 2019-2000 --> Hotspots section ---------------------------


data_2000 <- full_data|>
  filter(year == 2000)|>
  group_by(country)|>
  summarize(
    impact = sum(impact, na.rm=T))

country_impact_change <- full_data|> 
  filter(year == 2019)|>
  group_by(country)|>
  summarize(impact_change=sum(impact_change, na.rm=T), 
            impact = sum(impact, na.rm=T))|>
  mutate(change_anteil = round(impact_change / sum(impact_change, na.rm=T)*100,3))

# top 3 quantity of total change 
# top 3 hotspots
top6 <- country_impact_change %>%
  filter(country %in% c("Brazil", "Indonesia", "Democratic Republic of the Congo", 
                        "Mexico","India","Tanzania", "Kenya"))

# share of global increase
top6_share <- sum(top6$impact_change) / sum(country_impact_change$impact_change) * 100


# 3. Intensification  --------------------------------------------------------


total_intensification_change <- read.csv("output/land-intensification-change-2019-2000.csv")
country_int_change<- total_intensification_change|>
  group_by(GEOUNIT,ChangeClass)|>
  summarize(area_ha = sum(Area_ha, na.rm=T))

# 4. crop types  -------------------------------------------------------------

croptypes_ov <- full_data|>
  group_by(crop_type,year, lu_type)|>
  filter(lu_type %in% c("crops","plantations"))|>
  summarize(impact=sum(impact, na.rm=T), 
            impact_change = sum(impact_change, na.rm=T))


# Rank croptypes by total impact (summing across land-use types)
top_croptypes_impact <- croptypes_ov %>%
  filter(year == 2019, 
         !is.na(crop_type)) %>%
  group_by(crop_type)|>
  summarise(total_change = sum(impact_change, na.rm = TRUE), 
            total_impact = sum(impact, na.rm=T)) %>%
  arrange(desc(total_impact)) %>%
  slice_head(n = 8)

# get percentage of contribution of top 8 crop types 
total_impact_2019 <- croptypes_ov |>filter(year == 2019)
sum(top_croptypes_impact$total_impact) / sum(total_impact_2019$impact, na.rm=T)*100

top_croptypes_change <- croptypes_ov %>%
  filter(year == 2019) %>%
  group_by(crop_type)|>
  summarise(total_change = sum(impact_change, na.rm = TRUE), 
            total_impact = sum(impact, na.rm=T)) %>%
  arrange(desc(total_change)) %>%
  slice_head(n = 3)# same for changes 

# get percentage of contribution of top 8 crop types 
total_impact_2019 <- croptypes_ov |>filter(year == 2019)
sum(top_croptypes_change$total_change) / sum(total_impact_2019$impact_change, na.rm=T)*100
