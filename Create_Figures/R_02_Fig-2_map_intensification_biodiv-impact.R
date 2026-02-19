#Create Biodiversity impact map split by intensity 

library(terra)
library(ggplot2)
library(tidyterra)
library(data.table)
library(dplyr)
library(tidyverse)
library(viridis)
library(scales)
library(ggspatial)    
library(cowplot)
library(sf)
library(ggnewscale)

# first compile dataset 
dataset_base = "LUH2_GCB2025" # either LUH2_GCB2025 or  LUH2

fig_path <- file.path("./output/figures/", dataset_base)
lu_types <- c("crops","pasture","plantations", "rangeland","abandoned")
path_biodiv <- file.path("./output/biodiversity_impact_assessment", dataset_base)
shpcountries <- vect('H:/02_Projekte/allgemein_biodiversity_impact/02_data/country_shp/ne_110m_admin_0_countries.shp')
outpath = file.path("./output/figures/", dataset_base)

# for eckert projection
eckertcrs <- crs("ESRI:54012")
safe_extent <- ext(-180, 180, -60, 89)
target_ext <- project(safe_extent, from="EPSG:4326", to=eckertcrs)
template_eckert <- rast(extent=target_ext, res=25000, crs=eckertcrs)

hotspots <- vect("./output/figures/Fig2_data/hotspot_shp.shp")


y1 <- 2000 # year one 
y2 <- 2019
org_rast_list <- list()
rast_list <- list()
plot_list <- list()
change_list <- list()
# loop over land use types
for (lu_type in lu_types){
  # read data into list
  for (y in c(y1,y2)){

    filename <- paste0(lu_type,"_impact_",y,".tif")

    org_rast_list[[as.character(y)]][[lu_type]] <- rast(file.path(path_biodiv,lu_type,
                                                                  filename))
    # plot(org_rast_list[[as.character(y)]][[lu_type]])
    # project to eckert resolution
    # resample raster to eckert IV projection
    org_rast_list[[as.character(y)]][[lu_type]] <- crop(org_rast_list[[as.character(y)]][[lu_type]], safe_extent)
    rast_list[[as.character(y)]][[lu_type]]<- org_rast_list[[as.character(y)]][[lu_type]]/ cellSize(org_rast_list[[as.character(y)]][[lu_type]])

    rast_list[[as.character(y)]][[lu_type]] <- project(rast_list[[as.character(y)]][[lu_type]],
                                                       eckertcrs, method = "bilinear")

    rast_list[[as.character(y)]][[lu_type]] <- rast_list[[as.character(y)]][[lu_type]] * cellSize(rast_list[[as.character(y)]][[lu_type]])
    plot(rast_list[[as.character(y)]][[lu_type]])
    

  }
  change_list[[lu_type]] <- rast_list[[as.character(y2)]][[lu_type]] - rast_list[[as.character(y1)]][[lu_type]]
}

# summarize over land use types
# initialize empty rasters
low <- rast(rast_list[[1]][[1]][[1]],vals = NA)
med <- rast(rast_list[[1]][[1]][[1]], vals = NA)
high <- rast(rast_list[[1]][[1]][[1]], vals = NA)
lu_types2 <- c("crops","pasture","plantations", "rangeland")

for (lu_type in lu_types2) {
  low <- c(low,change_list[[lu_type]][[1]])
  med <- c(med,change_list[[lu_type]][[2]])
  high <- c(high,change_list[[lu_type]][[3]])
  print(lu_type) # note throws error for rangeland, since this only has one layer (low-intensity)
}


biodiv_int <- c(sum(low, na.rm=T),
                sum(med, na.rm=T),
                sum(high, na.rm=T),
                change_list$abandoned)

# write 2000 and 2019 seperately
y2000 <- rast(c(rast_list[["2000"]]))
y2000_tot <- sum(global(y2000, fun = "sum", na.rm = T))*1e6

y2019 <- rast(c(rast_list[["2019"]]))
y2019_tot <-sum(global(y2019, fun = "sum", na.rm = T))*1e6

diff_tot <- y2019_tot - y2000_tot

# mask out ocean
shpcountries <- crop(shpcountries, safe_extent)
shpcountries_proj <- project(shpcountries, eckertcrs)
plot(shpcountries_proj)
names(biodiv_int) <- c("low","med","high","abandoned")

biodiv_int_land <- mask(biodiv_int,shpcountries_proj)
names(biodiv_int_land) <- c("low","med","high","abandoned")
plot(biodiv_int_land)


# write out to replot
output_folder <- file.path("output/figures/", dataset_base)
writeRaster(biodiv_int_land, filename = paste0(output_folder,"/data_change_int_land.tif"),
            overwrite =T)

writeRaster(biodiv_int, filename = paste0(output_folder,"/data_change_int.tif"),
            overwrite =T)

writeRaster(y2000, filename = paste0(output_folder,"/data_2000.tif"),
            overwrite =T)
writeRaster(y2019, filename = paste0(output_folder,"/data_2019.tif"),overwrite=T)
############################
# Plotting 
############################

biodiv_int_land <- rast("output/figures/LUH2_GCB2025/data_change_int.tif")
# plot(biodiv_int_land)
df_biodiv_int <- as.data.frame(biodiv_int_land , xy = TRUE)
colnames(df_biodiv_int) <- c("x","y","low","medium","high","abandoned")


# pseudo-log-scale 
# Create a new fill variable
df_biodiv_int <- df_biodiv_int %>%
  mutate(fill_val = case_when(
    low == 0 ~ -1,      # placeholder for zero values
    TRUE ~ low,
    medium == 0 ~ -1,      # placeholder for zero values
    TRUE ~ medium,
    high == 0 ~ -1,      # placeholder for zero values
    TRUE ~ high, 
    abandoned == 0 ~ -1,      # placeholder for zero values
    TRUE ~ abandoned, 
  ))


# to plot facets, make df a long format 
df_long <- df_biodiv_int|>
  pivot_longer(cols = low:abandoned, 
               names_to = "intensity", 
               values_to = "impact_change", 
               values_drop_na=T)

df_pos <- df_long |>
  filter(impact_change > 0)|>
  group_by(intensity)|>
  summarize(values = sum(impact_change, na.rm=T))
df_pos$type <- "increase"

df_neg <- df_long |>
  filter(impact_change<0)|>
  group_by(intensity)|>
  summarize(values = sum(impact_change, na.rm=T))
df_neg$type <- "decrease"
df_pos_neg <- rbind(df_pos, df_neg)

df_pos_neg$values <- df_pos_neg$values*1e6
df_pos$values <- df_pos$values*1e6

# increases
# ratio abandoned, intensive, medium, low
df_pos$values[1] / sum(df_pos$values) # 27 % abandoned
df_pos$values[2] / sum(df_pos$values) # 24 % high intensity agriculture
df_pos$values[4] / sum(df_pos$values) # 29 % medium intensity agriculture
df_pos$values[3] / sum(df_pos$values) # 18 % low intensity agriculture

# decreases
df_neg$values[1] / sum(df_neg$values) # 4 % abandoned
df_neg$values[2] / sum(df_neg$values) # 15 % high intensity agriculture
df_neg$values[4] / sum(df_neg$values) # 32 % medium intensity agriculture
df_neg$values[3] / sum(df_neg$values) # 48 % low intensity agriculture
barplot(df_pos_neg$values)

sum(df_pos$values)
sum(df_neg$values)

# define palette
coolwarm_hcl <-   c("#002F70",
                    "#1A4F97",
                    "#336BB8",
                    "#4F87D0",
                    "#6AA1DE",
                    "#86B9E8",
                    "#A3CDED",
                    "#BDDDF0",
                    "#D4E8F2",
                    "#DCE6ED",
                    "lightgrey", # neutral color for zero
"#FCFDBFFF", "#FDE992" ,"#FEC98DFF","#FD9567FF","#FA815FFF","#F4685CFF" ,"#E85362FF", "#D6456CFF" ,"#C03A76FF" ,"#AB337CFF")

# define same breaks as in Fig. 1 
breaks = c(-1e-09,-1e-10,-1.0e-11,
           0.000e+00,
           1e-11, 1e-10, 1e-09)

# Position titles inside each panel
facet_titles <- df_long %>%
  group_by(intensity) %>%
  summarize(x = min(x)+2000000, y = max(y)+10000, .groups = "drop") %>%
  mutate(intensity_c =str_to_title(intensity))%>%
  mutate(intensity_c = factor(intensity_c, levels = c("High", "Medium", "Low", "Abandoned"))) %>%
  arrange(intensity_c) %>%
  mutate(label = paste0(LETTERS[row_number()], ": ", intensity_c))

df_long <- df_long %>%mutate(intensity_c =str_to_title(intensity))

y1y2_plot <- ggplot(df_long, aes(x, y, fill = impact_change)) +
  geom_tile(color = NA) +
  facet_wrap(factor(intensity_c, levels = c("High", "Medium", "Low", "Abandoned"))~., 
             nrow = 4,
             strip.position = "left")+ # to put the label on the left side
  scale_fill_gradientn(
    colors = coolwarm_hcl,
    na.value = "white",
    trans = pseudo_log_trans(sigma = 1e-13),
    name = expression(Delta*PSL["glo"]), 
    breaks = breaks
   )+  
  # add extra labels (are then aligned on the left)
  geom_text(data = facet_titles, aes(x = x, y = y, label = label), 
             inherit.aes = FALSE, hjust = 0, vjust = 1, fontface = "bold", 
             size = 4)+
  guides(
    fill = guide_colorbar(
      title.vjust = 1,
      title.hjust = 0.5# center title horizontally
    )
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.key.height = unit(0.2, "cm"),  # increase height
    legend.key.width = unit(3, "cm"),
    strip.placement = "outside",
    strip.text.y.left = element_blank(),  # remove facet label text
    strip.background = element_blank(),   # remove facet strip background
    plot.margin = margin(0, 0, 5.5, -30)      # remove extra plot margin
    )
y1y2_plot

#######################
# barplot 


#######################

ggsave("F02_map_biodiversity.pdf",
       plot = y1y2_plot,
       path = outpath,
       width = 175,  
       height = 240,
       units = "mm",
       device = "pdf")

#################################################
# optional: Barplot: sum of positive and negative values per intensity
barplots_list <- list()
int_levels <- c("high","medium","low","abandoned")
for (i in 1:4){
  barplots_list[[i]] <- ggplotGrob(ggplot(df_pos_neg|>filter(intensity==int_levels[i]), 
                      aes(x = type,y = values, fill = type)) +
    geom_bar(stat = "identity",position = "stack") +
    scale_fill_manual(values = c("positive" = "#9D3D3D", "negative" = "#6889D0")) +
    theme_minimal() +
    facet_wrap(~factor(intensity, levels = c("high","medium","low","abandoned"))~., 
               nrow = 4,
               strip.position = "left")+
    labs(x = NULL, y = "Sum of values"))+
    theme(legend.position = "none",      # remove legend
          axis.title = element_blank(),  # remove axis titles
          axis.text.x = element_blank(), # remove x labels
          axis.ticks = element_blank())
}

y1y2_plot + annotation_custom(grob = barplots_list[[1]])