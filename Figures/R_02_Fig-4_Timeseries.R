####################################################
## Compile Fig. 1 - Timeseries biodiversity impacts
####################################################
# A & B: Intensity 
# Fig. 1a: Stacked area plot of absolute changes (2000–2019) by intensity.
# 
# Fig. 1b: Stacked area plot of Δ-PSLglo relative changes to 2000
# C & D: Land use type 
# E & F: Region wise

# load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot) # for Panel A+B
library(plotly) # for interactive plots
library(ggpubr)
library(stringr)

# load data
data <- read.csv("./output/biodiversity_impact_assessment_2000-2019.csv")

# Prepare data for plotting 
# summarize by land use type (croptype info not necessary)
data_type <- data |>
  group_by(year, intensity,LU_type, country, country_group)|>
  summarize(impact = sum(impact, na.rm=T),
            area_ha = sum(area_ha, na.rm=T),
            impact_change = sum(impact_change, na.rm=T),
            area_change = sum(area_change, na.rm=T))

## for supplements table 
data_sup <- data_type |>
  filter(year == 2019)|>
  group_by(country, country_group)|>
  summarize(impact_change = sum(impact_change, na.rm=T))|>
  select(country, country_group)

# # plot 
# # define colors
cols <- c(
  # Abandoned land
  "Secondary land" = "#DCD4D0",
  "High" = "#01665e",
  "Medium" = "#35978f",
  "Low"  = "#80cdc1",
  "Cropland" = "#8c510a",
  "Pasture" = "#dfc27d",
  "Plantations" = "#bf812d",
  "Plantation" = "#bf812d",
  "Abandoned" = "#DCD4D0",
  "Brazil" =  "#CC5151", 
  "Mexico" = "#E57E7E", 
  "Other South America" = "#FFB2B2", 
  "Indonesia"="#762a83", 
  "China" = "#9970ab", 
  "Other Asia and Pacific" = "#c2a5cf", 
  "DR Congo" = "#6B990F",
  "Tanzania" = "#A3CC51", 
  "Other Africa" = "#E5FFB2", 
  "Europe" = "#91bfdb", 
  "Other North America" =  "#4575b4"
)

# define factor levels for legend order
# legend_levels <- c(
#   "Secondary land",
#   "Pasture - High","Pasture - Medium","Pasture - Low","Rangeland - Low",
#   "Cropland - High","Cropland - Medium","Cropland - Low",
#   "Plantations - High","Plantations - Medium","Plantations - Low"
#   )

# data_type <- data_type %>%
#   mutate(
#     LU_Intensity = factor(LU_Intensity, levels = legend_levels)
#   ) %>%
#   filter(!is.na(LU_Intensity))


# General trend  ----------------------------------------------------------

general_inc <- data_type |>
  group_by(year)|>
  summarize(impact_change = sum(impact_change, na.rm=T)*100, 
            impact= sum(impact, na.rm=T))

# Intensity ---------------------------------------------------------------

change_int_data <- data_type |>
  #filter(!is.na(intensity))|>
  group_by(year, intensity)|>
  summarize(impact_change = sum(impact_change, na.rm=T), 
            impact= sum(impact, na.rm=T)) |>
  mutate(intensity = case_when(is.na(intensity) ~ 'Abandoned', 
                               intensity == "High" ~ "High",
                               intensity == "Medium" ~ "Medium",
                               intensity == "Low" ~ "Low"))


# change 2000 to compute relative change 
change_int_2000 <- change_int_data|>
  filter(year == 2000)|>
  rename(impact_2000= "impact")|>
  select(c(intensity, impact_2000))

change_int_data <- change_int_data|>inner_join(change_int_2000,
                                              by = "intensity")|>
  mutate(change_rel = ((impact /impact_2000)*100), year.y = NULL)|>
  rename(year = "year.x")
  
# Panel A: Absolute PSLglo
plot_int_abs <- ggplot(change_int_data, aes(x = year, y = impact, fill = intensity)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = cols,guide = "none") +
  labs(x = NULL, y = expression("PSL"["glo"])) +
  theme_minimal(base_size = 10)

plot_int_abs

# Panel B: absolute change intensity
plot_change_intensity_ab <- ggplot(change_int_data, 
                                   aes(x = year, y = impact_change*100, 
                                        fill = factor(intensity, 
                                                      levels = c("Abandoned","High","Medium","Low")))) +
  geom_bar(stat = "identity", width = 0.7) +   # Outline für Abgrenzung
  scale_fill_manual(values = cols) +
  # geom_line(
  #   data = general_inc,
  #   aes(x = year, y = impact_change),
  #   color = "black",
  #   linetype = "dashed",
  #   inherit.aes = FALSE
  # )+
  labs(x = NULL, 
       y = expression("Global Potential Species Loss ("*Delta*" %PSL"["glo"]*")"),
       fill=NULL)+
  scale_y_continuous(limits = c(-0.4, 1.7),expand = c(0, 0)) +
  scale_x_continuous(
    breaks = c(2001, 2005, 2010, 2015, 2019),   # only show these labels
    expand = expansion(mult = c(0, 0)))+
  theme_minimal(base_size = 10) +  
  theme(
    legend.text = element_text(size = 8),
    legend.position = c(0.0, 1.01),   # near top-left
    legend.justification = c("left", "top"),
    legend.key.size = unit(0.4, "cm")
  )+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Null-Linie klar sichtbar

plot_change_intensity_ab

# Panel C: relative change intensity 
plot_change_intensity_rel <- ggplot(change_int_data, aes(x = year, y = change_rel-100, col = intensity)) +
  geom_line(linewidth=0.8) +   # Outline für Abgrenzung
  #ylim(80,max(change_int_data$change_rel)+5)+
  scale_color_manual(values = cols, guide = "none") +
  labs(x = NULL, y = expression("relative change (%)"), 
       fill = "Land-use Intensity")+
  theme_minimal(base_size = 10) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Null-Linie klar sichtbar
#plot_change_intensity_rel


# Lu Type -----------------------------------------------------------------

# Panel C: Changes vs 2000 - land use type 
change_lu_data <- data_type |>
  group_by(year, LU_type)|>
  summarize(impact_change = sum(impact_change, na.rm=T), 
            impact= sum(impact, na.rm=T))

# change 2000 to compute relative change 
change_lu_2000 <- change_lu_data|>
  filter(year == 2000)|>
  rename(impact_2000= "impact")|>
  select(c(LU_type, impact_2000))

change_lu_data <- change_lu_data|>inner_join(change_lu_2000,
                                               by = "LU_type")|>
  mutate(change_rel = ((impact /impact_2000)*100), year.y = NULL)|>
  rename(year = "year.x")

# Total timeseries
plot_change_lu_ts <- ggplot(change_lu_data, aes(x = year, y = impact, fill = LU_type)) +
  geom_bar(stat = "identity", width = 0.7) +   # Outline für Abgrenzung
  scale_fill_manual(values = cols, guide = "none") +
  labs(x = NULL, y = NULL)+
  theme_minimal(base_size = 10) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Null-Linie klar sichtbar
#plot_change_lu_ts

# absolute change
plot_change_lu_ab <- ggplot(change_lu_data, 
                            aes(x = year, y = impact_change*100, 
                                fill = factor(LU_type, levels = c("Abandoned","Cropland","Plantation","Pasture")))) +
  geom_bar(stat = "identity", width = 0.7) +   # Outline für Abgrenzung
  scale_fill_manual(values = cols) +
  labs(x = NULL, y = NULL,fill=NULL)+
  scale_y_continuous(limits = c(-0.4, 1.7), expand = c(0,0)) +
  scale_x_continuous(
    breaks = c(2001, 2005, 2010, 2015, 2019),   # only show these labels
    expand = expansion(mult = c(0, 0.03)))+
  theme_minimal(base_size = 10) +  
  theme(
    legend.text = element_text(size = 8),
    legend.position = c(0.0, 1.01),   # near top-left
    legend.justification = c("left", "top"),
    legend.key.size = unit(0.4, "cm"),
    axis.text.y = element_blank(),     # removes y-axis numbers
    axis.ticks.y = element_blank(),
    plot.margin = margin(3, -5, 3, -1)     # optional: removes tick marks
  )+
  # geom_line(
  #   data = general_inc,
  #   aes(x = year, y = impact_change),
  #   color = "black",
  #   linetype = "dashed",
  #   inherit.aes = FALSE
  # )+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Null-Linie klar sichtbar
plot_change_lu_ab

# relative change 
plot_change_lu_rel <- ggplot(change_lu_data, aes(x = year, y = change_rel-100, col = LU_type)) +
  geom_line(linewidth=0.8) +   # Outline für Abgrenzung
  scale_color_manual(values = cols, guide = "none") +
  labs(x = NULL, y = NULL,fill=NULL)+
  theme_minimal(base_size = 10) +

  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Null-Linie klar sichtbar
#plot_change_lu_rel

# Regions -----------------------------------------------------------------


# Panel C: Hotspot Countries 2019 (relative to 2000)
data_change_2019 <- data_type |>
  filter(year == 2019) |>
  group_by(country, lu_type, intensity) |>
  summarise(impact_change = sum(impact_change, na.rm = TRUE), .groups = "drop")  |>
  mutate(LU_type = case_when(
    lu_type == "crops" ~ "Cropland",
    lu_type == "pasture" ~ "Pasture",
    lu_type == "plantations" ~ "Plantations",
    lu_type == "abandoned" ~ "Abandoned",
    lu_type == "rangeland" ~ "Rangeland"
  )) |>
  mutate(intensity = str_to_title(intensity))|>
  # add combined for fill plotting 
  mutate(LU_Intensity = ifelse(LU_type == "Abandoned", "Secondary land",
                               paste(LU_type, intensity, sep = " - ")))

# Rank countries by total change (summing across land-use types)
top_countries <- data_change_2019 %>%
  group_by(country) %>%
  summarise(total_change = sum(impact_change, na.rm = TRUE)) %>%
  arrange(desc(total_change)) %>%
  slice_head(n = 7) %>%
  pull(country)

# Following countries / regions are most interesting / will be displayed: 
selected_countries <- c("Brazil", "Mexico", "Other South America", 
                        "Indonesia", "China", "Other Asia and Pacific", 
                        "DR Congo", "Tanzania", "Other Africa", 
                        "Europe", "Other North America")


# Re-summarise
region_data <- data_type %>%
  group_by(year,country_group) %>%
  summarise(impact_change = sum(impact_change, na.rm = TRUE), 
            impact = sum(impact, na.rm = T))

# compute relative changes 
region_data_2000 <- region_data|>
  filter(year == 2000)|>
  rename(impact_2000= "impact")|>
  select(c(country_group, impact_2000))

region_data <- region_data|>inner_join(region_data_2000,
                                             by = "country_group")|>
  mutate(change_rel = ((impact /impact_2000)*100), year.y = NULL)|>
  rename(year = "year.x")

# Order countries by total height
country_order <- region_data %>%
  filter(year == 2019) |>
  group_by(country_group) %>%
  summarise(total = sum(impact_change)) %>%
  arrange(desc(total)) %>%
  pull(country_group)

# order always by same order (region-wise)
country_order <- c("Brazil","Mexico","Other South America", 
                   "Indonesia","China","Other Asia and Pacific", 
                   "DR Congo", "Tanzania", "Other Africa", 
                    "Other North America", "Europe")
# Region colors 
col_reg <- c("Brazil" =  "#CC5151", 
             "Mexico" = "#E57E7E", 
             "Other South America" = "#FFB2B2", 
             "Indonesia"="#762a83", 
             "China" = "#9970ab", 
             "Other Asia and Pacific" = "#c2a5cf", 
             "DR Congo" = "#6B990F",
             "Tanzania" = "#A3CC51", 
             "Other Africa" = "#E5FFB2", 
             "Europe" = "#91bfdb", 
             "Other North America" =  "#4575b4")


plot_reg_ts <- ggplot(region_data, aes(x = year,
                                       y = impact, fill = factor(country_group, levels = country_order))) +
  geom_bar(stat = "identity", width = 0.5) + 
  scale_fill_manual(values = col_reg, guide ="none") +
  labs(x = NULL, y = expression(Delta*"PSL"["glo"])) +
  theme_minimal(base_size = 10) +
  #theme(legend.position = "none")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
plot_reg_ts

# absolute change
plot_change_reg_ab <- ggplot(region_data, aes(x = year,
                    y = impact_change*100, fill = factor(country_group, levels = country_order))) +
  geom_bar(stat = "identity", width = 0.5) + 
  scale_fill_manual(values= col_reg) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_y_continuous(limits = c(-0.4, 1.7), expand = c(0,0)) +
  scale_x_continuous(
    breaks = c(2001, 2005, 2010, 2015, 2019),   # only show these labels
    expand = expansion(mult = c(0, 0.03)))+
  theme_minimal(base_size = 10) +  
  theme(
    legend.text = element_text(size = 8),
    legend.position = c(0.0, 1.01),   # near top-left
    legend.justification = c("left", "top"),
    legend.key.size = unit(0.4, "cm"),
    axis.text.y = element_blank(),     # removes y-axis numbers
    axis.ticks.y = element_blank(),
    plot.margin = margin(3, -2, 3, 2)     # optional: removes tick marks
  )+
  # geom_line(
  #   data = general_inc,
  #   aes(x = year, y = impact_change),
  #   color = "black",
  #   linetype = "dashed",
  #   inherit.aes = FALSE
  # )+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
plot_change_reg_ab

# relative change 
plot_change_reg_rel <- ggplot(region_data, aes(x = year,
                                              y = change_rel-100, 
                                              col = factor(country_group, levels = country_order))) +
  geom_line(linewidth = 0.7) + 
  scale_color_manual(values = col_reg, guide ="none") +
  labs(x = NULL, y = NULL, col = NULL) +
  theme_minimal(base_size = 10) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
#plot_change_reg_rel


# Combine plots -----------------------------------------------------------


# horizontal plot of absolute change 
plots_stacked <- plot_grid(
  # columns: intensity, land use, regions 
  plot_change_intensity_ab, plot_change_lu_ab, plot_change_reg_ab,
  labels = c("A","B","C"),
  label_fontface = "bold",
  rel_widths = c(1, 1, 1), # adjust relative widths
  # Optional: decrease spacing
  hjust = -0.1,            # moves plots closer together
  nrow = 1, 
  align = "hv",        # align both horizontally and vertically
  axis = "tblr",
  label_x = 0.1,    # move label closer to plot
  label_y = 0.98    # vertical position
)
plots_stacked

outp_path = "./output/figures"

# size is approximately A5 
# for 1-panel-figure: 
ggsave("F1_TS_biodiv_3_panel_change.pdf",
       plot = plots_stacked,
       path = outp_path,
       width = 180,  
       height = 115,
       units = "mm",
       device = "pdf")


# Additional analysis (no figures but in text) ----------------------------
#

lu_int_data <- data|>group_by(year, intensity, lu_type)|>
  summarize(impact = sum(impact, na.rm=T), 
            impact_change = sum(impact_change, na.rm=T))

# change 2000 to compute relative change 
lu_int_data_2000 <- lu_int_data|>
  filter(year == 2000)|>
  rename(impact_2000= "impact")|>
  select(c(lu_type, impact_2000))

lu_int_data <- lu_int_data|>inner_join(lu_int_data_2000,
                                             by = c("lu_type", "intensity"))|>
  mutate(change_rel = ((impact /impact_2000)*100), year.y = NULL)|>
  rename(year = "year.x")

# check proportion of top countries to biodiversity change 
top_bio_change <- data |> 
  filter(year == 2019)|>
  group_by(country_group)|>
  summarize(impact_change = sum(impact_change,na.rm=T))

top_bio_change$proportion <- (top_bio_change$impact_change / sum(top_bio_change$impact_change))*100
