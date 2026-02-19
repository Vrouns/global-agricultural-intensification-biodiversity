# Fig 4: Crop type impacts 
library(dplyr)
library(ggplot2)
library(cowplot) # for Panel A+B
library(RColorBrewer)
library(ggpubr)
#library(xlsx)


# Apply to your dataframe
data <- read.csv("output/biodiversity_impact_assessment_2000-2019_LUH2_GCB2025.csv")
outp_path = "./output/figures/LUH2_GCB2025"
data_croptypes <- data|>filter(!is.na(crop_type))

## for supplements table 
data_sup <- data |>
  filter(year == 2019)|>
  group_by(crop_type, crop_group, LU_type)|>
  summarize(impact_change = sum(impact_change, na.rm=T))|>
  select(crop_type, crop_group, LU_type)
# now safe this and plot it 
#write.csv(data_croptypes, "./output/data_croptypes.csv", row.names = F)

# Region colors 
col_reg <- c(  "Brazil" =  "#900000", 
               "Peru" = "#ee2400",
               "Other South America" = "#FFB2B2",
               "North America" =  "#4575b4",
               "Indonesia"="#762a83", 
               "China" = "#9970ab", 
               "Other Asia and Pacific" = "#c2a5cf", 
               "DR Congo" = "#6B990F",
               "Tanzania" = "#A3CC51", 
               "Other Africa" = "#E5FFB2", 
               "Europe" = "#E69F60")

col_intensity = c(
  "High" = "#01665e",
  "Medium" = "#35978f", 
  "Low"  = "#80cdc1")

col_crops <- c(
  "bananas" = "#e3b505",                     # golden yellow
  "leguminous crops" = "#A7BBEC",            # olive  
  "maize" = "#8CD790",                       # medium purple 
  "oilpalm " = "#D81E5B",                     # deep red  
  "other cereals" = "#9BDEAC",               # cyan-blue  
  "other crops" = "#7f7f7f",                  # dark grey
  "other fruits and nuts" = "#FFA420",       # light yellow-beige  
  "other oilseed crops" = "#F0544F",         # light coral  
  "rice " = "#588157",                        # viol et  
  "soybeans" = "#BE6C77",                    # brown  
  "sugar beverage and spice crops" = "#B3EBF2", # pink-magenta  
  "vegetables, melons and root/tuber crops" = "#AD8A64" # neutral grey                 # dark grey
)

all_cols <- c(col_intensity, col_reg, col_crops)

##############################
# Figure croptypes over time 
##############################
# crop order as in sankey (nach crop-groups geordnet)
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

# Panel 1 intensity -------------------------------------------------------
change_int_data <- data_croptypes |>
  group_by(year, intensity)|>
  summarize(impact_change = sum(impact_change, na.rm=T), 
            impact= sum(impact, na.rm=T))

# change 2000 to compute relative change 
change_int_2000 <- change_int_data|>
  filter(year == 2000)|>
  rename(impact_2000= "impact")|>
  select(c(intensity, impact_2000))

change_int_data <- change_int_data|>inner_join(change_int_2000,
                                               by = "intensity")|>
  mutate(change_rel = ((impact /impact_2000)*100), year.y = NULL)|>
  rename(year = "year.x")

plot_change_intensity_ab <- ggplot(change_int_data, 
                                   aes(x = year, y = impact_change*100, 
                                       fill = factor(intensity, 
                                      levels = c("High","Medium","Low")))) +
  geom_bar(stat = "identity", width = 0.7) +   
  scale_fill_manual(values = col_intensity, 
                    labels = function(x) paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))) +
  labs(x = NULL, y = expression("Global Potential Species Loss ("*Delta*" %PSL"["glo"]*") "), 
       fill=NULL)+
  scale_y_continuous(limits = c(-0.23, 0.93)) +

  scale_x_continuous(
    breaks = c(2001, 2005, 2010, 2015, 2019),   # only show these labels
    expand = expansion(mult = c(0, 0))  # remove extra space at ends
  )+
  theme_minimal(base_size = 10) +  
  theme(
    legend.text = element_text(size = 8),
    legend.position = "top",   # near top-left would be c(0.004, 0.98)
    legend.justification = c("center"),
    legend.key.size = unit(0.4, "cm"), 
    labels = toupper
  )+
guides(fill=guide_legend(ncol=1,title.position="top"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Null-Linie klar sichtbar
plot_change_intensity_ab

# Panel 2 crop type  ------------------------------------------------------
# Panel C: Changes vs 2000 - land use type 
change_croptype <- data_croptypes |>
  group_by(year, crop_group)|>
  summarize(impact_change = sum(impact_change, na.rm=T), 
            impact= sum(impact, na.rm=T))
data_croptypes[is.na(data_croptypes$crop_group),]

# change 2000 to compute relative change 
change_ct_2000 <- change_croptype|>
  filter(year == 2000)|>
  rename(impact_2000= "impact")|>
  select(c(crop_group, impact_2000))

change_croptype <- change_croptype|>inner_join(change_ct_2000,
                                             by = "crop_group")|>
  mutate(change_rel = ((impact /impact_2000)*100), year.y = NULL)|>
  rename(year = "year.x")

cropgroups_order <- c("oilpalm ","maize","rice ","bananas","soybeans",
                      "vegetables, melons and root/tuber crops","other oilseed crops",
                      "sugar beverage and spice crops",
                      "other fruits and nuts","other crops","other cereals",
                      "leguminous crops"
                      )

plot_change_ct_ab <- ggplot(change_croptype, aes(x = year, y = impact_change*100, 
                                                 fill = factor(crop_group, levels = crop_order))) +
  geom_bar(stat = "identity", width = 0.7) +   # Outline für Abgrenzung
  #geom_area(stat = "identity")+
  scale_fill_manual(values = col_crops, labels = c("Rice ",                   
                                                   "Maize",                       
                                                   "Other cereals", 
                                                   "Soybeans" ,                   
                                                   "Oilpalm ",                    
                                                   "Other oilseed \ncrops", 
                                                   "Bananas",                   
                                                   "Other fruits & nuts",       
                                                   "Vegetables, melons \n& root/tuber crops",
                                                   "Leguminous crops",          
                                                   "Sugar beverage \n& spice crops",  
                                                   "Other crops") ) +
  labs(x = NULL, y = NULL,fill=NULL)+
  scale_y_continuous(limits = c(-0.23, 0.93)) +
  theme_minimal(base_size = 10) +  
  theme(
    legend.text = element_text(size = 8),
    legend.position = "top",   # near top-left would be c(0.010, 0.98)
    legend.justification = c("center"),
    legend.key.size = unit(0.4, "cm"),
    axis.text.y = element_blank(),     # removes y-axis numbers
  )+
  scale_x_continuous(
    breaks = c(2001, 2005, 2010, 2015, 2019),   # only show these labels
    expand = expansion(mult = c(0, 0))) +
  guides(fill=guide_legend(ncol=2,title.position="top"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Null-Linie klar sichtbar
plot_change_ct_ab

# relative change
plot_change_ct_rel <- ggplot(change_croptype, aes(x = year, y = change_rel, 
                                                 col = factor(crop_group, levels = crop_order))) +
  geom_line() +   # Outline für Abgrenzung
  #geom_area(stat = "identity")+
  scale_color_manual(values = col_crops, labels = c("rice ",                   
                                                   "maize",                       
                                                   "other cereals", 
                                                   "soybeans" ,                   
                                                   "oilpalm ",                    
                                                   "other oilseed crops", 
                                                   "bananas",                   
                                                   "other fruits & nuts",       
                                                   "vegetables, melons & \nroot/tuber crops",
                                                   "leguminous crops",          
                                                   "sugar beverage \n& spice crops",  
                                                   "other crops") ) +
  #labs(x = NULL, y = NULL,fill=NULL)+
  #scale_y_continuous(limits = c(-0.23, 0.87)) +
  theme_minimal(base_size = 10) +  
  theme(
    legend.text = element_text(size = 10),
    legend.position = "bottom",   # near top-left would be c(0.010, 0.98)
    legend.justification = c("left", "top"),
    legend.key.size = unit(0.4, "cm"),
    axis.text.y = element_blank(),     # removes y-axis numbers
    # axis.ticks.y = element_blank(),
    # plot.margin = margin(3, -5, 3, -1)     # optional: removes tick marks
  )+
  guides(fill=guide_legend(ncol=2))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Null-Linie klar sichtbar
plot_change_ct_rel


# Panel 3 region  ---------------------------------------------------------
region_data <- data_croptypes %>%
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


# for uniform plots always plot with same order
country_order <- c("Brazil","Peru", "Other South America",
                   "North America",
                   "Indonesia","Other Asia and Pacific", 
                   "DR Congo", "Tanzania", "Other Africa", 
                   "Europe")



plot_change_reg_ab <- ggplot(region_data, aes(x = year,
                                              y = impact_change*100, fill = factor(country_group, levels = country_order))) +
  geom_bar(stat = "identity", width = 0.5) + 
  scale_fill_manual(values= col_reg, labels = c("Brazil","Peru","Other South \nAmerica",
                                                "North America", 
                                                "Indonesia","Other Asia \nand Pacific", 
                                                "DR Congo", "Tanzania", "Other Africa", 
                                                 "Europe")) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_y_continuous(limits = c(-0.23, 0.93)) +
  theme_minimal(base_size = 10) +  
  theme(
    legend.text = element_text(size = 8),
    legend.position ="top",   # near top-left would be  c(0.017, 0.98)
    legend.justification = c("center"),
    legend.key.size = unit(0.4, "cm"),
    axis.text.y = element_blank(),     # removes y-axis numbers
    # axis.ticks.y = element_blank(),
    # plot.margin = margin(3, -2, 3, 2)     # optional: removes tick marks
  )+
  scale_x_continuous(
    breaks = c(2001, 2005, 2010, 2015, 2019),   # only show these labels
    expand = expansion(mult = c(0, 0.05)))+
  guides(fill=guide_legend(ncol=2,title.position="top"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")

plot_change_reg_ab

# Combine -----------------------------------------------------------------

plots <- ggarrange(
  plot_change_intensity_ab + theme(legend.position="none"),
  plot_change_ct_ab + theme(legend.position="none"),
  plot_change_reg_ab + theme(legend.position="none"),
  labels = c("A","B","C"),
  nrow = 1,
  align = "hv"
)
legendA <- get_legend(plot_change_intensity_ab)
legendB <- get_legend(plot_change_ct_ab)
legendC <- get_legend(plot_change_reg_ab)

legends <- ggarrange(legendA, legendB, legendC, ncol = 3,align = "h")

plots_stacked <- ggarrange(plots, legends, ncol = 1, heights = c(1,0.5))
plots_stacked


# for 1-panel-figure: 
ggsave("F06_TS_Int_croptypes_Reg.pdf",
       plot = plots_stacked,
       path = outp_path,
       width = 180,  
       height = 120,
       units = "mm",
       device = "pdf")






