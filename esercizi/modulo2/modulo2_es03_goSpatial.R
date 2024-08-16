############### Check stationarity -----
library(terra)
library(tidyterra)

temperature <- terra::rast("GeospatialApp/data/VenetoCorrectedMODIS_LST_Avg2017.tif")
## aggiungiamo una potenziale covariata - la quota!
DEM <- terra::rast("GeospatialApp/data/VenetoDEM.tif")
veneto <- sf::read_sf("GeospatialApp/data/veneto.gpkg")

## sample 1000 points
smpl <- terra::spatSample(temperature, method="random",
                        size=1000, na.rm=T, as.points=T)
q <- terra::extract(DEM, smpl, ID=F )
smpl$altitude<- q$VenetoDEM

plot(temperature)
plot(smpl, pch="x", add=T)
