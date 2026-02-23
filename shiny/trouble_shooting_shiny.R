library(terra)
library(shiny)
library(dplyr)
library(plotly)
library(stringr)
library(shinyWidgets)
library(leaflet)
library(terra)
library(tidyterra)
library(ggplot2)
library(leaflet)
library(htmltools)  

test <- rast("croptype_maps/change_rice_high.tif")
plot(test)
sum(values(test, na.rm=T))
test <- rast("croptype_maps/change_rice_med.tif")
plot(test)
sum(values(test, na.rm=T))
test <- rast("croptype_maps/change_rice_light.tif")
plot(test)
sum(values(test, na.rm=T))

test2 <- rast("croptype_maps/change_group_rice_total.tif")
plot(test2)
sum(values(test2, na.rm=T))

lu_rast_2019 <- rast(paste0("../biodiversity_impact_assessment/LUH2/","crops","/","crops","_impact_2019.tif"))
lu_rast_2000 <- rast(paste0("../biodiversity_impact_assessment/LUH2/","crops","/","crops","_impact_2000.tif"))

## manually for rice 
low_rice <- rast("../../data/05_crop_types/CG/crop_types_int/crop_types_share/crops_light_share_2019.tif")
med_rice <- rast("../../data/05_crop_types/CG/crop_types_int/crop_types_share/crops_med_share_2019.tif")
high_rice <- rast("../../data/05_crop_types/CG/crop_types_int/crop_types_share/crops_high_share_2019.tif")

low_rice_bdi <- low_rice*lu_rast_2019$crops_impact_2019_1
med_rice_bdi <- med_rice*lu_rast_2019$crops_impact_2019_2
high_rice_bdi <- high_rice*lu_rast_2019$crops_impact_2019_3

# same for 2000 
low_rice_2 <- rast("../../data/05_crop_types/CG/crop_types_int/crop_types_share/crops_light_share_2000.tif")
med_rice_2 <- rast("../../data/05_crop_types/CG/crop_types_int/crop_types_share/crops_med_share_2000.tif")
high_rice_2 <- rast("../../data/05_crop_types/CG/crop_types_int/crop_types_share/crops_high_share_2000.tif")

low_rice_bdi_2 <- low_rice_2*lu_rast_2000$crops_impact_2000_1
med_rice_bdi_2 <- med_rice_2*lu_rast_2000$crops_impact_2000_2
high_rice_bdi_2 <- high_rice_2$rice*lu_rast_2000$crops_impact_2000_3


test<- c(low_rice_bdi,med_rice_bdi,high_rice_bdi)
test_sum <- app(test, sum)

# diff rice 
change_low <- low_rice_bdi - low_rice_bdi_2
change_low
sum(values(change_low$rice, na.rm=T))
rep_delta_imp <- project(change_low, "ESRI:54012", method = "sum")
sum(values(rep_delta_imp, na.rm=T))
?project

####
cur_share <- impacts_types_shares[["y2019"]][[lu_types[lu]]][[intensity[int]]] 
cur_imp <-  biodiv_lu_list[["y2019"]][[lu_types[lu]]][[int]]
ct_imp_2019 <- cur_share*cur_imp

cur_share <- impacts_types_shares[["y2000"]][[lu_types[lu]]][[intensity[int]]] 
cur_imp <-  biodiv_lu_list[["y2000"]][[lu_types[lu]]][[int]]
ct_imp_2000 <- cur_share*cur_imp

delta_imp <- ct_imp_2019 - ct_imp_2000

rep_delta_imp <- project(delta_imp, "ESRI:54012", method = "sum")
# mask out ocean 
rep_delta_imp_land <- mask(rep_delta_imp,shpcountries_proj)


###
#
biodiv_int <- rast("../figures/data_change_int.tif")
sum(values(biodiv_int, na.rm=T))

### 
r_o <- rast("output/shiny/croptype_maps/lu_maps/change_cropland_medium.tif")

r2 <- rast("croptype_maps/crop_group/change_group_leguminous_crops_high.tif")

#plot(cropland_low)
#?project
r_p1 <- project(r_o, r2, method= "sum")
plot(r)
plot(r2)
tmpl <- rast(ext(r_o), crs = "+proj=longlat +datum=WGS84", res = 14850)
r_p2 <- project(r_o, tmpl)
res(tmpl)
res(r_o)
plot(r_p2)

#r_p2 <- project(r_o, "+proj=longlat +datum=WGS84", method = "sum")

# to get % PSL glo 
r <- r_o
r <- r_p2
r <- r*1e6
# mit leavelet plotten 
pal <- colorBin(
  palette = c(
    "#002F70",        # bin 1
    "#265BAB",        # bin 2
    "#A3B4E5",      # bin 3  <-- 0 belongs here
    "#A3B4E5",
    "lightgrey",# bin 4  <-- if you want symmetric zero band
    "#FDE992",
    "#FEC98DFF",
    "#FD9567FF",
    "#E85362FF"
  ),right = FALSE,
   domain = values(r, na.rm = TRUE),
   bins = c(-1e-4, -1e-5, -1e-6, 0, 1e-6, 1e-5, 1e-4, 1e-3),
  na.color = "transparent"
)

leaflet() %>% 
  addTiles() %>%
  addRasterImage(r, colors = pal) %>%
  addScaleBar()%>%
setView(lng = 30, lat = 45, zoom = 1.5) %>%
  addLegend(
    pal = pal,
    values = values(r),
    title = HTML(paste0("<br>% &Delta;PSL<sub>glo</sub>")),
    labFormat = function(type, x) {
      formatC(x, format = "e", digits = 0)
    }
  ) 

plot(r)
###
# cells appear larger 
###

res(r)
res(r_o)
mean(values(cellSize(r, unit ="km")))
mean(values(cellSize(r_o, unit ="km")))

?cellSize


# original cellsize
org <- rast("output/biodiversity_impact_assessment/LUH2/crops/crops_impact_2000.tif")
mean(values(cellSize(org, unit ="km")))
org_clip <- terra::mask(org, org>0)
plot(org_clip)
man