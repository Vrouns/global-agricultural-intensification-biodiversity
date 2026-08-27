# Create Sankeys
# before: create table in R (keys & intensity table, stored as xlsx)
# then: open in matlab script --> prepare data for plotting here. 

library(networkD3)
library(tidyverse)

############################################################################################################################################################
# Figure 2: change from 2000 to 2019 <-  link intensity vs land use type vs region vs intensity
############################################################################################################################################################

nodes <- read.csv("./output/figures/LUH2_GCB2025/sankeys/sankey_data/BD_int_lu_reg_int_names.csv", sep=",", header=TRUE)
links <- read.csv("./output/figures/LUH2_GCB2025/sankeys/sankey_data/BD_int_lu_reg_int_links.csv", sep=",", header=TRUE)
links$value <- links$value*10000
links <- links[links$value>0,]

#####


# colors
cols <- c("#01665e","#35978f","#80cdc1", "#DCD4D0",
"#8c510a","#bf812d","#dfc27d","#DCD4D0",  
"#CC5151","#FFB2B2",
"#74add1","#4575b4",
"#762a83","#9970ab","#c2a5cf", 
"#6B990F","#A3CC51", "#E5FFB2", 
"#E69F00")

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
  "Europe" = "#E69F60"
)

#v2
my_color2 <- 'd3.scaleOrdinal().domain([
"a","b","c","d",
"e","f","g","h",
"i","j", "k",
"l","m",
"n","o",
"p","q",
"r", 
"aa","bb","cc","dd",
"ee","ff","gg","hh",
"ii","jj",
"kk", "ll","mm","nn",
"oo"])
.range([
"#01665e","#35978f","#80cdc1", "#DCD4D0",
"#8c510a","#bf812d","#dfc27d","#DCD4D0",  
"#900000","#ee2400","#FFB2B2",    
"#5588ff", "#9CC9EE",                  
"#762a83","#c2a5cf",              
"#6B990F", "#E5FFB2",             
"#E69F60"                         
])'


nodes$label <- ""  # neues Feld mit leeren Labels

sankey<-sankeyNetwork(Links = links, Nodes = nodes, 
              Source = "source", Target = "target",
              Value = "value", 
              NodeID = "label", # change to name if you want it with labels
              fontSize= 12, nodeWidth = 30, 
              sinksRight = T, iterations = 0, 
              colourScale=my_color2,
              LinkGroup="group", 
              NodeGroup="group", 
              nodePadding = 0.5, 
              units = "PSLglo")
sankey

saveNetwork(sankey, "./output/figures/LUH2_GCB2025/sankeys/sankey_int-lu-no-labels.html", selfcontained = TRUE)

#######################
#########################
# Croptypes sankey --------------------------------------------------------
nodes2 <- read.csv("./output/figures/LUH2_GCB2025/sankeys/sankey_data/BD_int_croptype_reg_int_names.csv", header=TRUE)
nodes2$label <- ""  # neues Feld mit leeren Labels

links2 <- read.csv("./output/figures/LUH2_GCB2025/sankeys/sankey_data/BD_int_croptype_reg_int_links.csv", header=TRUE)
links2$value <- links2$value*10000
links2$value <- abs(links2$value)
links2 <- links2|>filter(value >0)

# overview 
# intensity_group = {'a','b','c'};
# croptype_group  = {'d','e','f','g','h','i','j','k','l','m','n','o','p'};
# "maize", "other cereals", "soybeans" , "oilpalm " , "other oilseed crops", "bananas", "other fruits and nuts" ,
# "plantain", "vegetables, melons and root/tuber crops", "cocoa", "leguminous crops","sugar beverage and spice crops", "other crops"     
# country_group   = {'q','r','s','t','u','v','w','x','y','z'};

my_color3 <- 'd3.scaleOrdinal().domain([
"a","b","c","d","e","f","g","h","i","j",
"k","l","m","n","o","p","q","r","s","t",
"u","v","w","x","y","aa","bb","cc","dd",
"ee","ff","gg","hh","ii","jj","kk","ll","mm","nn","oo"])
.range([
            "#01665e","#35978f", "#80cdc1",   // intensity
            "#588157", "#8CD790" ,            // "maize", "other cereals", 
            "#BE6C77","#D81E5B", "#F0544F",   //  "soybeans","oilpalm " , "other oilseed crops", 
            "#e3b505", "#FFA420","#AA7711",   // "bananas", "other fruits and nuts","plantain",
            "#7A9E3A",  "#C47F45", "#8E6C88", // "vegetables, melons and root/tuber crops", "cocoa", "sugar beverage and spice crops",
            "#5B8E8A", "#DCD4D0",             // "leguminous crops", "other crops"
            "#900000","#ee2400","#FFB2B2",    // Brazil, Peru, Other South America
             "#5588ff", "#9CC9EE",                  // Guatemala, Other North America
            "#762a83","#c2a5cf",              // Indonesia, Asia Pacific
            "#6B990F", "#E5FFB2",             // DR Congo , Africa 
            "#E69F60"                         // Europe
])'

sankey_ct<-sankeyNetwork(Links = links2, Nodes = nodes2, 
                         Source = "source", Target = "target",
                         Value = "value", 
                         NodeID = "name", # for no labels set to label
                         fontSize= 12, nodeWidth = 30, 
                         sinksRight = T, iterations = 0, 
                         colourScale=my_color3,  
                         LinkGroup="group", 
                         NodeGroup="group", 
                         nodePadding = 0.5, 
                         units = "PSLglo")


sankey_ct
saveNetwork(sankey_ct, "./output/figures/LUH2_GCB2025/sankeys/sankey_int-croptype-reg_with-labels.html", selfcontained = TRUE)


####################
# For text: 
####################
data_change <- read.csv("output/biodiversity_impact_assessment_2000-2024_LUH2_GCB2025_rev1.csv")
library(dplyr)
data_change <- data_change|>
  filter(year == 2024)

data_change$intensity[data_change$LU_type == "Abandoned"] <- "Abandoned"

data_pos <- data_change |>
  filter(impact_change > 0)

sum(data_pos$impact_change)

# check for scale in sankey (Powerpoint)
data_pos |>
  filter(intensity == "Abandoned")|>
  summarize(ab_i = sum(impact_change))

data_pos |>
  filter(intensity == "Low")|>
  summarize(low_i = sum(impact_change, na.rm=T))

data_pos |>
  filter(intensity == "Medium")|>
  summarize(med_i = sum(impact_change))

data_pos |>
  filter(intensity == "High")|>
  summarize(high_i = sum(impact_change))

data_neg <- data_change |>
  filter(impact_change < 0)

sum(data_neg$impact_change)
data_neg |>
  filter(intensity == "High")|>
  summarize(low_i = sum(impact_change))

#######
# for presentation: adopt colors
#######
colors_ct <-c("#80cdc1","#35978f","#01665e","#DCD4D0", # intensity 
              
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
              
              "vegetables, melons and root/tuber crops" = "#AD8A64", # neutral grey  
              "#900000","#ee2400","#FFB2B2", # South america 
              "#4575b4", # North america
              "#762a83","#c2a5cf", # asia
              "#6B990F","#A3CC51", "#E5FFB2", # Africa 
              "#E69F60"# Europe 
)

# only high intensity colored 
my_color3 <- 'd3.scaleOrdinal().domain([
"a","b","c","d","e","f","g","h","i","j",
"k","l","m","n","o","p","q","r","s","t",
"u","v","w","x","y","aa","bb","cc","dd",
"ee","ff","gg","hh","ii","jj","kk","ll","mm","nn","oo"])
.range([
            "#DCD4D0","#35978f", "#DCD4D0", // intensity
            "#DCD4D0", "#8CD790" , "#DCD4D0", // rice maize other cereals 
            "#DCD4D0", "#DCD4D0" , "#DCD4D0",  // soybean, oilpalm, oilseed crops
            "#DCD4D0", "#DCD4D0" , "#DCD4D0", // bananas, other fruits, vegetables
            "#DCD4D0", "#DCD4D0" , "#DCD4D0", // legouminious crops, sugar beverage, other
            "#900000", "#DCD4D0" , "#DCD4D0",  // Brazil, Peru, Other South America
            "#DCD4D0", // North America
            "#DCD4D0", "#DCD4D0", // Indonesia, Asia Pacific
            "#DCD4D0", "#DCD4D0" , "#E5FFB2", // DR Congo, Tanzania, Africa 
            "#DCD4D0" // Europe
])'


sankey_ct<-sankeyNetwork(Links = links2, Nodes = nodes2, 
                         Source = "source", Target = "target",
                         Value = "value", 
                         NodeID = "label", # for no labels set to label
                         fontSize= 12, nodeWidth = 30, 
                         sinksRight = T, iterations = 0, 
                         colourScale=my_color3,  
                         LinkGroup="group", 
                         NodeGroup="group", 
                         nodePadding = 0.5, 
                         units = "PSLglo")


sankey_ct
#dir.create("./output/figures/presentation")
out_dir_sankey<- "./output/figures/presentation/"
saveNetwork(sankey_ct, 
            paste0(out_dir_sankey, "maize_brazil_africa.html"), selfcontained = TRUE)



######## 
# new try
#######

highlight_crops <- c(
  "oilpalm ",   # auf trailing spaces achten!
  "maize",
  "rice ",
  "bananas",
  "soybeans"
)


nodes2 <- nodes2 |>
  dplyr::mutate(
    group = ifelse(name %in% highlight_crops, name, "other")
  )

# change links 
links2 <- links2 |>
  dplyr::left_join(
    nodes2 |> dplyr::select(node_id = no, crop_group = group),
    by = c("source" = "node_id")
  ) |>
  dplyr::mutate(
    group = crop_group
  )

# reduce color scale 
my_color3 <- '
d3.scaleOrdinal()
  .domain([
    "oilpalm ",
    "maize",
    "rice ",
    "bananas",
    "soybeans",
    "other"
  ])
  .range([
    "#D81E5B",  // oil palm – deep red
    "#8CD790",  // maize – green
    "#588157",  // rice – dark green
    "#e3b505",  // bananas – yellow
    "#BE6C77",  // soybeans – muted red
    "#BFBFBF"   // other – neutral grey
  ])
'
sankey_ct <- sankeyNetwork(
  Links = links2,
  Nodes = nodes2,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",
  LinkGroup = "group",
  fontSize = 12,
  nodeWidth = 30,
  nodePadding = 6,
  sinksRight = TRUE,
  iterations = 0,
  colourScale = my_color3,
  units = "PSLglo"
)

sankey_ct
