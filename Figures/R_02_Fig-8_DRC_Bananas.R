######################### 
# Plot a map of banana impacts in Congo 
# accounting for 4 % of global total of biodiversity impacts 
#######################

library(terra)
library(tidyterra)
library(ggplot2)
library(scales)
library(dplyr)
library(ggspatial)    


# load data ---------------------------------------------------------------
# total impacts of plantations in 2019 split by intensity 
plantation_impacts_2019 <- rast("output/biodiversity_impact_assessment/LUH2/plantations/plantations_impact_2019.tif")
plantation_impacts_2000 <- rast("output/biodiversity_impact_assessment/LUH2/plantations/plantations_impact_2000.tif")

# select only medium plantations since here impacts occured 
plantation_impacts_2019_sum <- plantation_impacts_2019$plantations_impact_2019_2
plantation_impacts_2019_sum[is.na(plantation_impacts_2019_sum)] <- 0

plantation_impacts_2000_sum <- plantation_impacts_2000$plantations_impact_2000_2
plantation_impacts_2000_sum[is.na(plantation_impacts_2000_sum)] <- 0

# load CFs of medium intense plantation 
CFs_plant_med <- rast("data/04_bia_inputs/LUH2/CF_raster/habitat_12.tif")


# shape of Congo 
shpcountries <- vect('../02_LUC biodiversity paper/02_data/country_shp/ne_110m_admin_0_countries.shp')
shp_Congo <- shpcountries[shpcountries$SOVEREIGNT== "Democratic Republic of the Congo"]
shp_Congo_org <- shp_Congo

# Distribution of medium intensity bananas 
med_plant_2019 <- rast("data/05_crop_types/CG/crop_types_int/plantations_med_2019.tif")
med_plant_2000 <- rast("data/05_crop_types/CG/crop_types_int/plantations_med_2000.tif")

# in share 
med_plant_2019_share <- rast("data/05_crop_types/CG/crop_types_int/crop_types_share/plantations_med_share_2019.tif")
med_plant_2000_share <- rast("data/05_crop_types/CG/crop_types_int/crop_types_share/plantations_med_share_2000.tif")
med_plant_2019_share[is.na(med_plant_2019_share)] <- 0
med_plant_2000_share[is.na(med_plant_2000_share)] <- 0

# Crop data to Congo shape ------------------------------------------------

# add a bit of surrounding to shape 
shp_Congo <- ext(shp_Congo)*1.05
CF_DRC <- crop(CFs_plant_med, shp_Congo, mask =T)

## medium intense banana share 
DRC_share_plant_med_ban_2019 <- crop(med_plant_2019_share$banana, shp_Congo, mask=T)
DRC_share_plant_med_ban_2000 <- crop(med_plant_2000_share$banana, shp_Congo, mask=T)

# Calculate Impact --------------------------------------------------------
# result of bananas should be 55% of total impact change in Congo
## Impact 
# Total impact 
plantation_impact_2019_DRC <- crop(plantation_impacts_2019_sum, shp_Congo, mask =T)
plantation_impact_2000_DRC <- crop(plantation_impacts_2000_sum, shp_Congo, mask =T)
plantation_impact_change <- plantation_impact_2019_DRC-plantation_impact_2000_DRC

global(plantation_impact_change, na.rm=T, fun = "sum")

# assess with share
impact_share_ban_2019 <- plantation_impact_2019_DRC * DRC_share_plant_med_ban_2019
impact_share_ban_2019[is.na(impact_share_ban_2019)] <- 0

impact_share_ban_2000 <- plantation_impact_2000_DRC * DRC_share_plant_med_ban_2000
impact_share_ban_2000[is.na(impact_share_ban_2000)] <- 0

impact_change_ban_2 <- impact_share_ban_2019 - impact_share_ban_2000

global(impact_change_ban_2, fun = "sum")  
global(impact_share_ban_2019, fun = "sum") - global(impact_share_ban_2000, fun = "sum")

# Plot impact share bananas  ----------------------------------------------
plot(impact_change_ban_2)
plot(shp_Congo, add= T)
plot(shp_Congo_org, add =T)

# crop to drc boundaries 
impact_crop <- crop(impact_change_ban_2, shp_Congo_org, mask=T)

# prepare for plotting 
df_impact_crop <- as.data.frame(impact_crop , xy = TRUE, na.rm=T)
# define palette
coolwarm_hcl <- colorspace::diverging_hcl(12, palette = "Blue-Red 3")
drc <- rnaturalearth::ne_countries(scale = "medium", country = "Democratic Republic of the Congo", returnclass = "sf")
lim <- max(abs(df_impact_crop$plantations_impact_2019_2), na.rm = TRUE)

ggplot() +
  #geom_tile(color = NA) +
  geom_raster(data = df_impact_crop, aes(x, y, fill =  plantations_impact_2019_2))+
  geom_sf(data = drc, fill = NA, color = "grey40", linewidth = 0.4) +
  scale_fill_gradientn(
    colors = coolwarm_hcl,
    values = scales::rescale(c(-lim, 0, lim)),
    limits = c(-lim, lim),        # ensures symmetric scaling around 0
    na.value = "white",
    #trans = pseudo_log_trans(sigma = 1e-13),
    name = expression(Delta*PSL["glo"]), 
    #breaks = breaks
  )+guides(
    fill = guide_colorbar(
      title.vjust = 1,
      title.hjust = 0.5# center title horizontally
    )
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.title = element_blank(),
    # axis.text = element_blank(),
    # axis.ticks = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.height = unit(1.5, "cm"),  # increase height
    legend.key.width = unit(0.4, "cm"),
    #legend.box.spacing = unit(-5, "cm")
    #legend.margin = margin(t = -13)   # negative top margin pulls legend up
    panel.background = element_rect(fill = "white")
     )+
  annotation_scale(location = "bl", style = "ticks")

ggsave("output/figures/Fig_6_Banana_DRC_impact_change_map.pdf", width = 18, height = 13, units = "cm")
