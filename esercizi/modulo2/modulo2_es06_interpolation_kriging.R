library(terra)
library(sf)
library(sp)

# load objects from modulo2_es05_interpolation_deterministic.R  ------------
load(file="residuals01interpolation.rda")
#
temperature <- terra::rast("esercizi/modulo2/data/VenetoCorrectedMODIS_LST_Avg2017.tif")

# sample 1000 points
smpl <- terra::spatSample(temperature, method="random",
                          size=1000, na.rm=T,  as.points=T)
names(smpl)<-c("Temp")
# create a grid for predicting value at locations
veneto <- sf::read_sf("esercizi/modulo2/data/veneto.gpkg")
# nb our data are in lat long!
ra = terra::rast(veneto, res=0.01)
rveneto<- terra::rasterize(veneto, ra)
plot(rveneto)
temperature.rveneto = terra::resample(temperature, rveneto)
plot(temperature.rveneto)




##  simple nn -----

boxplot(residuals$diffs~residuals$type, outline=FALSE)
