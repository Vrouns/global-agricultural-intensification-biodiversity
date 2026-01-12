####################################
### Global biodiversity impacts map 
####################################

# Compile Figure 3: Map of biodiversity Impacts 
# 1. 2000 
# 2. 2019
# 3. Change map 

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

fig_path <- "./output/figures/maps/"
lu_types <- c("crops","pasture","plantations", "rangeland","abandoned")
path_biodiv <- "./output/biodiversity_impact_assessment/LUH2/"
shpcountries <- vect('H:/02_Projekte/02_LUC biodiversity paper/02_data/country_shp/ne_110m_admin_0_countries.shp')
eckertcrs <- "ESRI:54012"
shpcountries_proj <- project(shpcountries, eckertcrs)
hotspots <- vect("./output/figures/Fig2_data/hotspot_shp.shp")


y1 <- 2000 # year one 
y2 <- 2019
org_rast_list <- list()
rast_list <- list()
plot_list <- list()
lu_type <- lu_types[1]
# loop over land use types 
for (lu_type in lu_types){
  # read data into list 
  for (y in c(y1,y2)){
    org_rast_list[[as.character(y)]][[lu_type]] <- rast(paste0(path_biodiv,lu_type,
                                                            "/",lu_type,"_impact_",y,".tif"))
    
    org_rast_list[[as.character(y)]][[lu_type]] <- sum(org_rast_list[[as.character(y)]][[lu_type]],
                                                    na.rm=T)
    # project to eckert resolution 
    # resample raster to eckert IV projection
    
    rast_list[[as.character(y)]][[lu_type]]<- org_rast_list[[as.character(y)]][[lu_type]]/ cellSize(org_rast_list[[as.character(y)]][[lu_type]])
    
    rast_list[[as.character(y)]][[lu_type]] <- project(rast_list[[as.character(y)]][[lu_type]],
                                                       eckertcrs, method = "bilinear")
    
    rast_list[[as.character(y)]][[lu_type]] <- rast_list[[as.character(y)]][[lu_type]] * cellSize(rast_list[[as.character(y)]][[lu_type]])
    
  }
  
}

# Figure 2019 -------------------------------------------------------------
y2_rast <- rast(rast_list[[as.character(y2)]])
y2_rast_no_ab <- y2_rast[[-5]]

# for plotting only one year: 
y2_sum <- sum(y2_rast_no_ab, na.rm=T)

# remove the non-land-pixels 
y2_sum_land <- mask(y2_sum,shpcountries_proj)

# prepare for plotting 
df_sum_y2 <- as.data.frame(y2_sum_land , xy = TRUE)

# cut out antarctica and north pole (no impacts anyway)
df_sum_y2_no_poles <- df_sum_y2 %>%
  filter(y > -7000000)  # remove all latitudes below -60°
# plot

# pseudo-log-scale 
# Create a new fill variable
df_sum_y2_no_poles <- df_sum_y2_no_poles %>%
  mutate(fill_val = case_when(
    sum == 0 ~ -1,      # placeholder for zero values
    TRUE ~ sum
  ))

# define nice breaks for pseudo-log scale
summary(df_sum_y2$sum)

breaks<- (c(max(df_sum_y2$sum),1e-8,1e-9, 1e-10,1e-11,1e-12,0))

y2_plot <- ggplot(df_sum_y2_no_poles, aes(x, y, fill = sum)) +
  geom_tile(color = NA) +
  scale_fill_gradientn(
    colours = c("lightgrey", viridis(256, option = "magma", direction = -1)),
    trans = pseudo_log_trans(sigma = 1e-12),
    na.value = "white",
    name = "PSLglo",
    breaks=breaks
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.height = unit(1.5, "cm")  # increase height
  )
y2_plot

# ggsave("Biodiversity_impact_2019.pdf",
#        plot = y2_plot,
#        path = fig_path,
#        width = 9.16,
#        height = 4.05,
#        units = "in",
#        device = "pdf")
#ggsave("output/figures/Biodiversity_impact_2019.jpg", width = 19, height = 8, units = "cm")



# Figure change 2000-2019 -------------------------------------------------
# abandoned land included in the changes

y1_rast <- rast(rast_list[[as.character(y1)]])
y1_sum <- sum(y1_rast, na.rm=T)
y2_sum <- sum(y2_rast, na.rm=T)
y1y2 <- y2_sum - y1_sum
# remove the non-land-pixels 
y1y2_sum_land <- mask(y1y2,shpcountries_proj)

# prepare for plotting 
df_y1y2 <- as.data.frame(y1y2_sum_land , xy = TRUE)

# cut out antarctica and north pole (no impacts anyway)
df_y1y2_no_poles <- df_y1y2 %>%
  filter(y > -7000000)  # remove all latitudes below -60°
# plot

# pseudo-log-scale 
# Create a new fill variable
df_y1y2_no_poles <- df_y1y2_no_poles %>%
  mutate(fill_val = case_when(
    sum == 0 ~ -1,      # placeholder for zero values
    TRUE ~ sum
  ))

# define nice breaks for pseudo-log scale
# summary(df_sum_y1$sum)

# define palette
coolwarm_hcl <-   c("#002F70","#265BAB","#6889D0","#A3B4E5","#A3B4E5","#D3DBF4","#D3DBF4","#CCDBEE","lightgrey","lightgrey",
                    "#FCFDBFFF", "#FDE992" ,"#FEC98DFF","#FD9567FF","#FA815FFF","#F4685CFF" ,"#E85362FF", "#D6456CFF" ,"#C03A76FF" ,"#AB337CFF")



# breaks 
summary(df_y1y2_no_poles$sum)
breaks = c(-1e-09,-1e-10,-1.0e-11,
           0.000e+00,
           1e-11, 1e-10, 1e-09)
breaks = c(-1e-9, -1e-8, -1e-7, 0, 1e-7, 1e-8, 1e-9)


# country zoom boxes 
hotspots <- project(hotspots, "ESRI:54012")

plots_hotspot <- lapply(1:nrow(hotspots), function(i) {
  hotspot_cur <- project(hotspots[i], y1y2_sum_land)
  r_crop <- crop(y1y2_sum_land, ext(hotspots[i]))
  as.data.frame(r_crop, xy = TRUE, na.rm = TRUE)
})
names(plots_hotspot) <- hotspots$Country

# combine with hotspot ID
hotspots_sf <- sf::st_read("./output/figures/Fig2_data/hotspot_shp.shp")

# Intersection per hotspot
# get bounding boxes
bboxes <- st_bbox(hotspots_sf)               # gives global bbox
bboxes_list <- lapply(1:nrow(hotspots_sf), function(i) st_bbox(hotspots_sf[i,]))
names(bboxes_list) <- hotspots_sf$Country

# list for each individual country 
bbox_list <- lapply(1:nrow(hotspots_sf), function(i) {
  bb <- st_bbox(st_transform(hotspots_sf[i,]))
  data.frame(
    Country = hotspots_sf$Country[i],
    xmin = bb["xmin"], xmax = bb["xmax"],
    ymin = bb["ymin"], ymax = bb["ymax"]
  )
})
bbox_df <- do.call(rbind, bbox_list)


# plot


y1y2_plot <- ggplot(df_y1y2_no_poles, aes(x, y, fill = sum)) +
  geom_tile(color = NA) +
  scale_fill_gradientn(
    colors = coolwarm_hcl,
    na.value = "white",
    trans = pseudo_log_trans(sigma = 1e-14),
    name = expression(Delta*PSL["glo"]), 
    breaks = breaks
  )+  
  geom_rect(
    data = bbox_df,
    inherit.aes = FALSE,  # important so it doesn’t try to map x/y
    mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA, color = "black", linetype = "dashed"
  ) +
  geom_text(
    data = bbox_df[1,],
    inherit.aes = FALSE,
    mapping = aes(
      x = xmax + 100000,   # offset a bit to the right
      y = ymax+ 10000,           # top of the box
      label = "a"
    ),
    hjust = 0, vjust = 1,   # align text
    size = 4,
    fontface = "bold")+
  geom_text(
    data = bbox_df[2,],
    inherit.aes = FALSE,
    mapping = aes(
      x = xmax + 100000,   # offset a bit to the right
      y = ymax+ 10000,           # top of the box
      label = "b"
    ),
    hjust = 0, vjust = 1,   # align text
    size = 4,
    fontface = "bold")+
  geom_text(
    data = bbox_df[3,],
    inherit.aes = FALSE,
    mapping = aes(
      x = xmax + 100000,   # offset a bit to the right
      y = ymax+ 10000,           # top of the box
      label = "c"
    ),
    hjust = 0, vjust = 1,   # align text
    size = 4,
    fontface = "bold")+
  
  guides(
    fill = guide_colorbar(
      title.vjust = 1,
      title.hjust = 0.5# center title horizontally
    )
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "left",
    legend.direction = "vertical",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.height = unit(1.5, "cm"),  # increase height
    legend.key.width = unit(0.3, "cm"),
    legend.box.spacing = unit(-5, "cm"),
    plot.margin = margin(b = -10, t = -1)
    #legend.margin = margin(t = -13)   # negative top margin pulls legend up
  )

y1y2_plot


###################### 
# Country zooms
# Extract all hotspots and bind into one df
make_hotspot_plot <- function(data) {
  ggplot(data, aes(x = x, y = y, fill = sum)) +
    geom_raster() +                            # or geom_tile(width=1,height=1)
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(
      colors = coolwarm_hcl,
      na.value = "white",
      trans = pseudo_log_trans(sigma = 1e-13),
      name = expression(Delta*PSL["glo"]),
      breaks = breaks
    ) +
    guides(fill = guide_colorbar(title.vjust = 1, title.hjust = 0.5)) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.background = element_rect(fill = NA, colour = NA),
      plot.background  = element_rect(fill = NA, colour = NA),
      panel.border = element_rect(color = "black", fill = NA),
      panel.spacing = unit(0, "pt")
    ) +
    annotation_scale(location = "tr", style = "ticks") +
    coord_equal(expand = FALSE)
}


Brazil <- make_hotspot_plot(plots_hotspot$Brazil)
Congo <- make_hotspot_plot(plots_hotspot$Congo)
Indonesia <- make_hotspot_plot(plots_hotspot$Indonesia)

# Grid plots
# upper panel will be y1y2 global plot 
# mid panel: hotspotplots
hotspot_plots <- plot_grid(
  Brazil,
  Congo,
  Indonesia,
  nrow = 1,
  ncol = 3, 
  labels = c("a","b","c"), 
  label_y = 1.05
  )  # shift right from left edge (0 = left, 1 = right)
hotspot_plots
# low panel: scalebar 


# Final plot
final_plot <-plot_grid(
  y1y2_plot,
  hotspot_plots,
  ## plot settings
  nrow = 2,
  ncol = 1,
  rel_heights = c(2, 1.1))

final_plot
outp_path = "./output/figures"

# size is approximately A5 
ggsave("map_biodiv_change_2000-2019.pdf",
       plot = final_plot,
       path = outp_path,
       width = 200,  
       height = 148,
       units = "mm",
       device = "pdf")

