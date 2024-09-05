library(leaflet)
library(shiny)
library(shinydashboard)
library(mapview)
library(sf)
library(scatterplot3d)
library(ggplot2)
library(sjPlot)
library(rgl)
library(shinycssloaders)
library(sp)
library(car)
library(gstat)
library(shinyWidgets)
library(raster)
library(gstat)
library(sp)
library(spdep)
library(nabor)
library(shinyjs)

mycolb <- rgb(0,0,255, alpha = 125,max=255)#blu
mycolr <- rgb(255,0,0, alpha = 125,max=255)#rosso
mycolg <- rgb(0,255,0, alpha = 125,max=255)#verde
mycolc <- rgb(255,165,0, alpha = 125,max=255)#magenta


TempModis <- terra::rast("data/VenetoCorrectedMODIS_LST_Avg2017.tif")
TempDEM <- terra::rast("data/VenetoDEM.tif")
veneto <- sf::read_sf("../dati/venetoComuni.gpkg")
if(file.exists("data/data.rda")){
  load(file="data/data.rda")
} else {
  map <-  mapview::mapview(veneto, hide=T) +
    mapview::mapview( raster::raster(TempModis),   layer.name="Temperature", query.digits=0 ) +
    mapview::mapview(raster::raster(TempDEM),   hide=T,  layer.name="Elevation", query.digits=0)
  map.map <- map@map
  save(map.map, file="data/data.rda")

}

logit <- function(text, type="message"){
  colo <- "black"
  if(type=="warning"){
    colo <- "orange"
  } else if(type=="error"){
    colo <- "red"
  }
  command <- sprintf('$("#logdiv").append("<b style=\\"color:%s\\">%s</b>: %s<br>");', colo,
                     format(Sys.time(), "%b %d - %X"),
                     text )

  shinyjs::runjs( command )

  shinyjs::runjs( "$('#logdiv').scrollTop($('#logdiv')[0].scrollHeight);" )
#


}

#l1<-list( sf::as_Spatial(veneto),pch=16,col=2)
# er <- tryCatch({ load(file="data/data.rda") },
#                 error = function(e){
#                   print("dd")
#                 },
#                warning = function(e){
#                 message("loading all")
#                  map <- mapview::mapview(veneto, hide=T) +
#                    mapview::mapview( raster::raster(TempModis),   layer.name="Temperature", query.digits=0 ) +
#                    mapview::mapview(raster::raster(TempDEM),   hide=T,  layer.name="Quota", query.digits=0)
#
#                  save(map, file="data/data.rda")
#
#                })



