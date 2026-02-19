# Create Sankeys
# before: create table in R (keys & intensity table, stored as xlsx)
# then: open in matlab script --> prepare data for plotting here. 

library(networkD3)


############################################################################################################################################################
# Figure 2: change from 2000 to 2019 <-  link intensity vs land use type vs region vs intensity
############################################################################################################################################################

nodes <- read.csv("./output/figures/LUH2_GCB2025/sankeys/sankey_data/BD_int_lu_reg_int_names.csv", sep=",", header=TRUE)
links <- read.csv("./output/figures/LUH2_GCB2025/sankeys/sankey_data/BD_int_lu_reg_int_links.csv", sep=",", header=TRUE)
links$value <- links$value*10000
links <- links[links$value>0,]
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
"i","j",
"k","l",
"m","n","o",
"p","q","r", 
"s",
"aa","bb","cc","dd",
"ee","ff","gg","hh",
"ii","jj",
"kk",
"ll","mm","nn","oo"])
.range([
"#01665e","#35978f","#80cdc1", "#DCD4D0",
"#8c510a","#bf812d","#dfc27d","#DCD4D0",  
"#900000","#ee2400","#FFB2B2",
"#4575b4",
"#762a83","#c2a5cf", 
"#6B990F","#A3CC51", "#E5FFB2", 
"#E69F60"
])'


nodes$label <- ""  # neues Feld mit leeren Labels

sankey<-sankeyNetwork(Links = links, Nodes = nodes, 
              Source = "source", Target = "target",
              Value = "value", 
              NodeID = "name", # change to name if you want it with labels
              fontSize= 12, nodeWidth = 30, 
              sinksRight = T, iterations = 0, 
              colourScale=my_color2,
              LinkGroup="group", 
              NodeGroup="group", 
              nodePadding = 0.5, 
              units = "PSLglo")
sankey

saveNetwork(sankey, "./output/figures/LUH2_GCB2025/sankeys/sankey_int-lu-with-labels.html", selfcontained = TRUE)

#######################
#########################
# Croptypes sankey --------------------------------------------------------
nodes2 <- read.csv("./output/figures/LUH2_GCB2025/sankeys/sankey_data/BD_int_croptype_reg_int_names.csv", header=TRUE)
nodes2$label <- ""  # neues Feld mit leeren Labels

links2 <- read.csv("./output/figures/LUH2_GCB2025/sankeys/sankey_data/BD_int_croptype_reg_int_links.csv", header=TRUE)
links2$value <- links2$value*10000
links2$value <- abs(links2$value)
links2 <- links2|>filter(value >0)


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

my_color3 <- 'd3.scaleOrdinal().domain([
"a","b","c","d","e","f","g","h","i","j",
"k","l","m","n","o","p","q","r","s","t",
"u","v","w","x","y","aa","bb","cc","dd",
"ee","ff","gg","hh","ii","jj","kk","ll","mm","nn","oo"])
.range([
            "#01665e","#35978f", "#80cdc1",
            "#588157", "#8CD790" , "#9BDEAC", 
            "#BE6C77","#D81E5B", "#F0544F", 
            "#e3b505", "#FFA420","#AD8A64",
            "#A7BBEC",  "#B3EBF2", "#7f7f7f",  
            "#900000","#ee2400","#FFB2B2",
             "#4575b4", 
            "#762a83","#c2a5cf", 
            "#6B990F","#A3CC51", "#E5FFB2", 
            "#E69F60"
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
