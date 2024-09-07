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





nm <- length(t2)
plot(t2[1:(nm-1)], t2[2:nm], pch="x", cex=0.5, xlab="km", ylab="Temperatura" )

cc=1
CovX<-cov(t2, t2)
covs<-list("0"=CovX, "1"=cov(t2[1:(nm-1)], t2[2:nm] ) )


for(lag in seq(6, 30, 5)){
  message(lag)

  covs[[as.character(lag)]]<-cov(t2[1:(nm-lag)], t2[(lag+1):nm])
  points(t2[1:(nm-lag)], t2[(lag+1):nm], pch="x",cex=0.5, col=cc )
  cc=cc+1

}



dati<- data.frame(Temperatura=t2, Distanza_Km=1:length(t2) )
# modello.lm <- lm(data = dati)
modello.lm <- lm(Temperatura~Distanza_Km, data = dati)
summary(modello.lm)


dati<- data.frame(Temperatura=t1, Distanza_Km=1:length(t1) )
# modello.lm <- lm(data = dati)
#
modello.lm <- lm(Temperatura~ I(Distanza_Km*20), data = dati)

summary(modello.lm)


library(ggplot2)

ggplot(dati, aes(x = Distanza_Km^2, y = Temperatura )) +
  geom_point() +
  stat_smooth(method = "lm") + theme_bw()
