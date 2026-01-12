library(terra)
library(ggplot2)
library(DescTools)  # for Gini coefficient
library(tidyr)

### Produce figure of cumulated impact per area 
data_raster <- rast("./output/figures/Fig2_data/biodiversity_change_2000-2019.tif")

# Compute pixel areas in km²
pixel_area <- cellSize(data_raster, unit = "km")
# pixel area of land 
pixel_area_land <- mask(pixel_area, data_raster)

# extract data 
data_vals <- values(data_raster, na.rm=FALSE)
area_vals <- values(pixel_area_land, na.rm=FALSE)

# Mask NAs
mask <- !is.na(data_vals) & !is.na(area_vals)
data_vals <- data_vals[mask]
area_vals <- area_vals[mask]

# Keep only positive impacts
data_vals_pos <- data_vals
data_vals_pos[data_vals_pos < 0] <- 0
total_impact_sum_pos <- sum(data_vals_pos)

# and negatives 
data_vals_neg <- data_vals
data_vals_neg[data_vals_neg > 0] <- 0
data_vals_neg <- abs(data_vals_neg)
total_impact_sum_neg <- sum(data_vals_neg)

# Order by impact
ord <- order(data_vals_pos, decreasing = TRUE)
data_sorted <- data_vals_pos[ord]
area_sorted <- area_vals[ord]

cum_impact <- cumsum(data_sorted) / sum(data_sorted)
cum_area <- cumsum(area_sorted) / sum(area_sorted)
# Data frame for plotting
df <- data.frame(cum_area = cum_area*100, cum_impact = cum_impact*100)
df$type <- "increase"

# repeat for decreases 
ord <- order(data_vals_neg, decreasing = TRUE)
data_sorted <- data_vals_neg[ord]
area_sorted <- area_vals[ord]

cum_impact <- cumsum(data_sorted) / sum(data_sorted)
cum_area <- cumsum(area_sorted) / sum(area_sorted)
df <- rbind(df, data.frame(cum_area = cum_area*100, cum_impact = cum_impact*100, type = "decrease"))

# analysis of data: 
top_area_threshold <- 0.05
impact_top10 <- max(cum_impact[cum_area <= top_area_threshold])
impact_top10

impact_threshold <- 0.99
cum_area <- df|>
  filter(type == "decrease")|>select(cum_area)
cum_impact <- df|>
  filter(type == "decrease")|>select(cum_impact)
area_needed <- min(cum_area[cum_impact >= impact_threshold])
area_needed

Gini(data_vals_pos) # Result is 0.90 --> 1 means very concentrated. 
Gini(data_vals_neg) # Result is 0.97 --> 1 means very concentrated. 

### reduce size of dataframe for faster plotting
reduce_tail <- function(df, keep_fraction = 0.10, target_tail = 2000) {
  # split
  split_id <- which(df$cum_area >= keep_fraction * 100)[1]
  
  head_part <- df[1:split_id, ]
  tail_part <- df[(split_id+1):nrow(df), ]
  
  # down-sample tail
  sel <- seq(1, nrow(tail_part), length.out = target_tail)
  tail_small <- tail_part[round(sel), ]
  
  rbind(head_part, tail_small)
}

df_red_i <- reduce_tail(df|>
                          filter(type == "increase"))
df_red_d <- reduce_tail(df|>
                          filter(type == "decrease"))
df_red_tot <-rbind(df_red_i, df_red_d)
# total impact plot (sum is 100%)

tot_plot <- ggplot(df_red_tot, aes(x = cum_area, y = cum_impact)) +
  geom_line(size = 1.2, color = "darkred") +
  facet_wrap(.~type)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  labs(
    x = "Fraction of total area (%)",
    y = "Cumulative fraction of biodiversity impact (%)",
    title = "Cumulative biodiversity impact per area"
  ) +
  theme_minimal(base_size = 12)
tot_plot

# To do: compute lorenz curve of different crops and add this to the graph.
total_impact_sum = global(data_raster, fun = "sum", na.rm=T)

###########
# Per Intensity ---------------------------------------------------------------
###########
data_int <- rast("output/figures/data_change_int.tif")
data_int <- data_int*1e06

#plot(data_int$low)
df_int <- data.frame(cum_area=double(), cum_impact=double(),intensity=factor())
df_red <- data.frame(cum_area=double(), cum_impact=double(),intensity=factor()) # reduced df

# Global raster of total impact
r_tot <- data_raster

df_red <- NULL
df_int <- NULL
sum_neg <- NULL
sum_pos <- NULL
# derive sum of positive and negative values to compute shares
for (i in 1:4){
r_p <- data_int[[i]]
# extract positive impacts
vals <- values(r_p, na.rm=T)
vals_pos <- vals
vals_pos[vals_pos < 0] <- 0
sum_pos[i] <- sum(vals_pos, na.rm=T)
# Extract negative impacts (same logic as for global)
vals_neg <- vals
vals_neg[vals_neg > 0] <- 0
vals_neg <- abs(vals_neg)
sum_neg[i] <- sum(vals_neg, na.rm=T)
}
tot_sum_int_neg <- sum(sum_neg)
tot_sum_int_pos <- sum(sum_pos)

for (i in 1:nlyr(data_int)) {
  
  r_p <- data_int[[i]]
  
  # Apply EXACT SAME global mask
  vals <- values(r_p)
  mask <- !is.na(vals)
  vals <- vals[mask]
  area_vals <- values(cellSize(r_p, unit ="km"))[mask]
  vals[is.na(vals)]<-0
  vals_pos <- vals
  vals_pos[vals_pos < 0] <- 0 # set negative impacts to 0 
  vals_pos <- abs(vals_pos)
  
  # Sort for cumulative curve
  ord <- order(vals_pos, decreasing = TRUE)
  vals_sorted <- vals_pos[ord]
  area_sorted <- area_vals[ord]
  
  # Cumulative impact normalized by GLOBAL total
  cum_impact <- cumsum(vals_sorted) / tot_sum_int_pos * 100  # <-- Option B
  cum_area   <- cumsum(area_sorted) / sum(area_sorted, na.rm=T) * 100
  
  df_new <- data.frame(
    cum_area   = cum_area,
    cum_impact = cum_impact,
    int        = names(data_int)[i],
    type       = "increase"
  )
  df_new_r <- reduce_tail(df_new)
  
  df_int <- rbind(df_int, df_new)
  df_red <- rbind(df_red, df_new_r)
  
  # same for decrease
  vals_neg <- vals
  vals_neg[vals_neg > 0] <- 0 # set positive impacts to 0 
  vals_neg <- abs(vals_neg)
  
  # Sort for cumulative curve
  ord <- order(vals_neg, decreasing = TRUE)
  vals_sorted <- vals_neg[ord]
  area_sorted <- area_vals[ord]
  
  # Cumulative impact normalized by GLOBAL total
  cum_impact <- cumsum(vals_sorted) / tot_sum_int_neg * 100  # <-- Option B
  cum_area   <- cumsum(area_sorted) / sum(area_sorted, na.rm=T) * 100
  
  df_new <- data.frame(
    cum_area   = cum_area,
    cum_impact = cum_impact,
    int        = names(data_int)[i],
    type       = "decrease"
  )
  
  df_new_r <- reduce_tail(df_new)
  
  df_int <- rbind(df_int, df_new)
  df_red <- rbind(df_red, df_new_r)
}


df_red_tot$int <- "Total"
df_plot <- rbind(df_red, df_red_tot)
# double check 
df_plot|>filter(type == "increase")|>group_by(int)|>summarize(max = max(cum_impact))
df_plot|>filter(type == "decrease")|>group_by(int)|>summarize(max = max(cum_impact))

### further analysis 
t <- values(data_int$low, na.rm=T)
t <- t[t>0]
gini_low <- Gini(t)

t <- values(data_int$med, na.rm=T)
t <- t[t>0]
gini_med <- Gini(t)

t <- values(data_int$high, na.rm=T)
t <- t[t>0]
gini_high <- Gini(t)

t <- values(data_int$abandoned, na.rm=T)
t <- t[t>0]
gini_abandoned <- Gini(t)

# change to capitals for legend 
# df_int$int[df_int$int == "med"] <- "Medium"
# df_int$int <- factor(stringr::str_to_title(df_int$int))
# df$cum_area <- as.numeric(df$cum_area)
# df_int$cum_area <- as.numeric(df_int$cum_area)

df_plot$int[df_plot$int == "med"] <- "Medium"
df_plot$int <- factor(stringr::str_to_title(df_plot$int))
df_plot$cum_area <- as.numeric(df_plot$cum_area)

df_plot$type <- factor(df_plot$type, levels = c("increase", "decrease"),
                       labels = c("Increasing impacts", "Decreasing impacts"))


int_plot <- ggplot(df_plot, aes(x = cum_area, y = cum_impact, 
                    col = factor(int, levels = c("Total","High","Medium","Low","Abandoned")))) +
  geom_line(size = 1.1) +
  facet_grid(rows = vars(type))+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  scale_color_manual(values = c("Total" = "black",
                                "High" = "#01665e",
                                "Medium" = "#35978f",
                                "Low" = "#80cdc1",
                                "Abandoned" = "#DCD4D0"),
                     breaks = c("Total", "High", "Medium", "Low", "Abandoned"),
                     guide = guide_legend(nrow = 1))+
  labs(
    x = "Fraction of total area (%)",
    y = "Cumulative fraction of biodiversity impacts (%)"
  ) +
  theme_minimal(base_size = 10)+
  theme(
    legend.margin = margin(0, 0, 0, 0), # turned off for alignment
    legend.position = "bottom",
    legend.title = element_blank(), 
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold")
  )

int_plot


# ggsave(plot = int_plot, filename= "output/figures/lorenz_curves/Total_int_inc-dec.pdf",
#        width = 8.0,  
#        height = 3.5,
#        units = "in",
#        device = "pdf")

sum_all <- sum(values(data_raster), na.rm = TRUE)

for(i in 1:nlyr(data_int)){
  print(sum(values(data_int[[i]]), na.rm = TRUE)*1e06)
}

##
# Hotspot map -------------------------------------------------------------
# Compute threshold for top 5% of impacts
shpcountries <- vect('H:/02_Projekte/02_LUC biodiversity paper/02_data/country_shp/ne_110m_admin_0_countries.shp')
shpcountries_proj <- project(shpcountries, data_raster)
data_raster_land <- mask(data_raster, shpcountries_proj)

impact_vals <- values(data_raster_land, na.rm=TRUE)
impact_vals[impact_vals < 0] <- 0
top10_threshold <- quantile(impact_vals, 0.90)

# Create hotspot raster
hotspots <- classify(data_raster_land, rcl = matrix(c(-Inf, top10_threshold, 0,
                                                 top10_threshold, Inf, 1), ncol=3, byrow=TRUE))

# Convert to data frame for ggplot
hotspot_df <- as.data.frame(hotspots, xy = TRUE, na.rm = TRUE)
colnames(hotspot_df)[3] <- "hotspot"

# Plot
ggplot(hotspot_df, aes(x = x, y = y)) +
  geom_tile(aes(fill = factor(hotspot))) +   # make hotspot discrete
  scale_fill_manual(values = c("0" = "lightgrey", "1" = "darkred"),
                    labels = c("0" = "No hotspot", "1" = "Top 10 % Hotspot"),
                    name = "") +
  labs(#title = "Increasing biodiversity impact hotspots", 
       x= "", y="") +
  theme_minimal()+    
  theme(axis.text = element_blank())

#ggsave("output/figures/hotspotmap.pdf", width = 19, height = 10, units = "cm")
ggsave("output/figures/hotspotmap.jpg", width = 19, height = 8, units = "cm")
