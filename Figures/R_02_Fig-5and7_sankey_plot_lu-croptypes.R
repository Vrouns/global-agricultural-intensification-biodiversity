# Vroni BD paper Figure 2 <-  sankey
library(networkD3)


############################################################################################################################################################
# Figure 2: change from 2000 to 2019 <-  link intensity vs land use type vs region vs intensity
############################################################################################################################################################

nodes <- read.csv("./output/figures/sankeys/sankey_data/BD_int_lu_reg_int_names_2.csv", sep=",", header=TRUE)
#colnames(nodes) <- c("name","group")
links <- read.csv("./output/figures/sankeys/sankey_data/BD_int_lu_reg_int_links_2.csv", sep=",", header=TRUE)
#colnames(links) <- c( "source", "target" ,   "value" ,    "group" )
links$value <- links$value*10000
links <- links[links$value>0,]
# colors

col_reg <- c("Brazil" =  "#60043c", 
             "Mexico" = "#a90769", 
             "Other South America" = "#f20b97", 
             "Indonesia"="#ffd647", 
             "China" = "#e0ae00", 
             "Other Asia and Pacific" = "#ae8700", 
             "DR Congo" = "#4b7248",
             "Tanzania" = "#19a337", 
             "Other Africa" = "#9bf09d", 
             "Europe" = "#99ccff", 
             "North America" =  "#5588ff")

new_col <-c("#80cdc1","#35978f","#01665e","#DCD4D0", # intensity 
            
            "#8c510a","#bf812d","#dfc27d","#DCD4D0", # lu types 
            
            "#60043c","#a90769","#f20b97",# South america 
            "#ffd647","#ffa700","#ff7400", # asia
            "#4b7248","#19a337", "#9bf09d", # Africa 
            "#99ccff","#5588ff"# North america and europe 
)

# dezente Farbpalette countries
my_color2 <- 'd3.scaleOrdinal().domain([
"a","b","c","d","e","f","g","h","i","j",
"k","l","m","n","o","p","q","r","s","t",
"u","v","w","x","y","z","aa","bb","cc","dd",
"ee","ff","gg","hh","ii","jj","kk","ll","mm","nn","oo"])
.range([
"#01665e","#35978f","#80cdc1", "#DCD4D0",
            "#8c510a","#bf812d","#dfc27d","#DCD4D0",  
            "#CC5151","#E57E7E","#FFB2B2",
            "#762a83","#9970ab","#c2a5cf", 
            "#6B990F","#A3CC51", "#E5FFB2", 
            "#91bfdb","#4575b4"
])'

# knalligere Farbpalette Countries 

my_color3 <- 'd3.scaleOrdinal().domain([
"a","b","c","d","e","f","g","h","i","j",
"k","l","m","n","o","p","q","r","s","t",
"u","v","w","x","y","z","aa","bb","cc","dd",
"ee","ff","gg","hh","ii","jj","kk","ll","mm","nn","oo"])
.range([
"#80cdc1","#35978f","#01665e","#DCD4D0", 
            "#8c510a","#bf812d","#dfc27d","#DCD4D0",  
            "#60043c","#a90769","#f20b97", 
            "#ffd647","#ffa700","#ff7400", 
            "#4b7248","#19a337", "#9bf09d", 
            "#99ccff","#5588ff"
])'
nodes$label <- ""  # neues Feld mit leeren Labels

sankey<-sankeyNetwork(Links = links, Nodes = nodes, 
              Source = "source", Target = "target",
              Value = "value", 
              NodeID = "label",
              fontSize= 12, nodeWidth = 30, 
              sinksRight = T, iterations = 0, 
              colourScale=my_color2,
              LinkGroup="group", 
              NodeGroup="group", 
              nodePadding = 0.5, 
              units = "PSLglo")
sankey

saveNetwork(sankey, "./output/figures/sankey_int-lu-reg_no-labels.html", selfcontained = TRUE)






# Croptypes sankey --------------------------------------------------------
nodes2 <- read.csv("./output/figures/sankeys/sankey_data/BD_int_croptype_reg_int_names.csv", header=TRUE)
nodes2$label <- ""  # neues Feld mit leeren Labels

links2 <- read.csv("./output/figures/sankeys/sankey_data/BD_int_croptype_reg_int_links.csv", header=TRUE)
links2$value <- links2$value*10000
links2$value <- abs(links2$value)
links2 <- links2|>filter(value >0)


new_col <-c("#80cdc1","#35978f","#01665e","#DCD4D0", # intensity 
            
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
            "#CC5151","#E57E7E","#FFB2B2",# South america 
            "#762a83","#9970ab","#c2a5cf", # asia
            "#6B990F","#A3CC51", "#E5FFB2", # Africa 
            "#91bfdb","#4575b4"# North america and europe 
)

my_color3 <- 'd3.scaleOrdinal().domain([
"a","b","c","d","e","f","g","h","i","j",
"k","l","m","n","o","p","q","r","s","t",
"u","v","w","x","y","z","aa","bb","cc","dd",
"ee","ff","gg","hh","ii","jj","kk","ll","mm","nn","oo"])
.range([
            "#01665e","#35978f", "#80cdc1",
            "#588157", "#8CD790" , "#9BDEAC", 
            "#BE6C77","#D81E5B", "#F0544F", 
            "#e3b505", "#FFA420","#AD8A64",
            "#A7BBEC",  "#B3EBF2", "#7f7f7f",  
            "#CC5151","#E57E7E","#FFB2B2",
            "#762a83","#9970ab","#c2a5cf", 
            "#6B990F","#A3CC51", "#E5FFB2",  
            "#91bfdb","#4575b4","#7f7f7f"
])'

sankey_ct<-sankeyNetwork(Links = links2, Nodes = nodes2, 
                         Source = "source", Target = "target",
                         Value = "value", 
                         NodeID = "label",
                         fontSize= 12, nodeWidth = 30, 
                         sinksRight = T, iterations = 0, 
                         colourScale=my_color3,  
                         LinkGroup="group", 
                         NodeGroup="group", 
                         nodePadding = 0.5, 
                         units = "PSLglo")
sankey_ct
saveNetwork(sankey_ct, "./output/figures/sankey_int-croptype-reg_no-labels.html", selfcontained = TRUE)
