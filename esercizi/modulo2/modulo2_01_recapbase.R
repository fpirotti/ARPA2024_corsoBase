###############  Spatial data formats IO - cheat sheet -----

# sf library for vector data
library(sf)
# terra library for grid data
library(terra)
# mapview to view data interactively
library(mapview)

# read veneto GPKG polygon
veneto <- sf::read_sf("esercizi/modulo2/data/veneto.gpkg")

# read temperature
modisTemp <- terra::rast("esercizi/modulo2/data/VenetoCorrectedMODIS_LST_Avg2017.tif")

# setup 2 transects
 # mapview(raster::raster(modisTemp ),
 #         layer.name = "Temperatura") +
 #   mapview(veneto)+
 #   mapview(t1)+
 #   mapview(t2)

# NB click two points to get the transect
plot(modisTemp)
# NB click two points to get the transect
# ----- old way without tidyverse way
# click <- locator(n = 2) %>% data.frame %>% as.matrix
# t1 <- vect(list(click), type="Lines", crs="EPSG:4326")
# plot(t1, add=T)
#

t1 <- locator(n = 2) %>%
  data.frame %>%
  as.matrix %>% list %>%
  vect(type="Lines", crs="EPSG:4326")  # %>% plot(add=T)

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
lines(t2.values$mean,  type="l", col="red", add=T)


# a static map
met_sf %>%
  ggplot()+
  geom_sf(aes(colour=valore))+
  geom_sf_text(aes(label=valore, colour = valore), # map colour and valore
               size=2.5,
               nudge_y = 0.02)+
  theme_void()


