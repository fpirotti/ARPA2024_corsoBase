###############  Spatial data formats IO - cheat sheet -----

# sf library for vector data
library(sf)
# terra library for grid data
library(terra)
# mapview to view data interactively
library(mapview)

# read veneto GPKG polygon
veneto <- sf::read_sf("GeospatialApp/data/veneto.gpkg")

# read temperature
modisTemp <- terra::rast("GeospatialApp/data/VenetoCorrectedMODIS_LST_Avg2017.tif")

# setup 2 transects
 # mapview(raster::raster(modisTemp ),
 #         layer.name = "Temperatura") +
 #   mapview(veneto)+
 #   mapview(t1)+
 #   mapview(t2)

# NB click two points to get the transect
plot(modisTemp)
              # ----- old way without tidyverse way
              # click <- locator(n = 2) %>% data.frame %>% as.matrix
              # t1 <- vect(list(click), type="Lines", crs="EPSG:4326")
              # plot(t1, add=T)
              #

t1 <- locator(n = 2) %>%
  data.frame %>%
  as.matrix %>% list %>%
  vect(type="Lines", crs="EPSG:4326")  # %>% plot(add=T)

# AFTER RUNNING CODE ABOVE,  click two points on the plot to get the transect!

t1 %>% plot(add=T)



t2 <- locator(n = 2) %>%
  data.frame %>%
  as.matrix %>% list %>%
  vect(type="Lines", crs="EPSG:4326")  # %>% plot(add=T)

t2 %>% plot(add=T, col="red")


t1.values <- terra::extractAlong(modisTemp, t1)
plot(t1.values$mean, ylim=c(2,15), xlab="km",
     ylab="Temperatura °C",  type="l")
t2.values <- terra::extractAlong(modisTemp, t2, online = T, bilinear = T)
lines(t2.values$mean,  type="l", col="red" )


