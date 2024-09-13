library(terra)
library(sf)
library(sp)

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
interp.nn <- terra::interpNear(rveneto, smpl, field="Temp", radius=1)
plot(interp.nn)
temperature.rveneto.interp.nn = terra::mask(interp.nn,veneto)
plot(temperature.rveneto.interp.nn)

#how good did it go?
diffs.nn = temperature.rveneto.interp.nn-rveneto.temp
plot(diffs.nn)
hist(diffs.nn, breaks=100)
summary(diffs.nn)





##  linear nn -------
##  practically it uses TINs (minimum circumcircle of voronoi)
interp.nn.linear <- terra::interpNear(rveneto, smpl, field="Temp",
                                      interpolate=T, radius=0.1)
plot(interp.nn.linear)
temperature.rveneto.interp.nn.linear = terra::mask(interp.nn.linear,veneto)
plot(temperature.rveneto.interp.nn.linear)

#how good did it go?
diffs.nn.linear = temperature.rveneto.interp.nn.linear-rveneto.temp
plot(diffs.nn.linear)
hist(diffs.nn.linear, breaks=100)
summary(diffs.nn.linear)
boxplot(diffs.nn.linear)





##  IDW pow 1 -------
interp.idw1 <- terra::interpIDW(rveneto, smpl, field="Temp",
                                      power=1, radius=0.1)
plot(interp.idw1)
temperature.rveneto.interp.idw1 = terra::mask(interp.idw1,veneto)
plot(temperature.rveneto.interp.idw1)

#how good did it go?
diffs.idw1 = temperature.rveneto.interp.idw1-rveneto.temp
plot(diffs.idw1)
hist(diffs.idw1, breaks=100)
summary(diffs.idw1)
boxplot(diffs.idw1)





##  IDW pow 2 -------
interp.idw2 <- terra::interpIDW(rveneto, smpl, field="Temp",
                                power=2, radius=0.1)
plot(interp.idw2)
temperature.rveneto.interp.idw2 = terra::mask(interp.idw2,veneto)
plot(temperature.rveneto.interp.idw2)

#how good did it go?
diffs.idw2 = temperature.rveneto.interp.idw2-rveneto.temp
plot(diffs.idw2)
hist(diffs.idw2, breaks=100)
summary(diffs.idw2)
boxplot(diffs.idw2)


residuals =
rbind(
  data.frame( diffs= na.omit(diffs.nn[][,1] ), type= "NN"),
  data.frame( diffs= na.omit(diffs.nn.linear[][,1] ), type= "Linear"),
  data.frame( diffs= na.omit(diffs.idw1[][,1] ), type= "idw1"),
  data.frame( diffs= na.omit(diffs.idw2[][,1] ), type= "idw2")
  )

boxplot(residuals$diffs~residuals$type, outline=FALSE)
