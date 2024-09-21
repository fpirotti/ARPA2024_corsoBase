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
#
temperature.rveneto.nn = terra::resample(temperature, rveneto, method="near")
temperature.rveneto.bl = terra::resample(temperature, rveneto)
plot(temperature.rveneto)


##  simple nn -----
interp.nn <- terra::interpNear(rveneto, smpl, field="Temp", radius=1)
plot(interp.nn)
temperature.rveneto.interp.nn = terra::mask(interp.nn,rveneto)
plot(temperature.rveneto.interp.nn)
plot(smpl, add=T, pch="+")

plot(temperature.rveneto.nn)
#how good did it go?
diffs.nn = temperature.rveneto.interp.nn - temperature.rveneto.nn
plot(diffs.nn)
hist(diffs.nn, breaks=100)
summary(diff.nn)
quantile(diffs.nn$layer[], c(0.1,0.25,0.5,0.75, 0.9 ), na.rm=T)





##  linear nn -------
##  practically it uses TINs (minimum circumcircle of voronoi)
interp.nn.linear <- terra::interpNear(rveneto, smpl, field="Temp",
                                      interpolate=T, radius=0.1)
plot(interp.nn.linear)
temperature.rveneto.interp.nn.linear = terra::mask(interp.nn.linear,veneto)
plot(temperature.rveneto.interp.nn.linear)

#how good did it go?
diffs.nn.linear = temperature.rveneto.interp.nn.linear - temperature.rveneto.nn
plot(diffs.nn.linear)
hist(diffs.nn.linear, breaks=100)
summary(diffs.nn.linear)
boxplot(diffs.nn.linear)





##  IDW pow 1 -------
interp.idw1 <- terra::interpIDW(rveneto, smpl, field="Temp",
                                      power=1, radius=0.15)
plot(interp.idw1)
plot(smpl, add=T, pch="+")
temperature.rveneto.interp.idw1 = terra::mask(interp.idw1,veneto)
plot(temperature.rveneto.interp.idw1)

#how good did it go?
diffs.idw1 = temperature.rveneto.interp.idw1-temperature.rveneto.nn
plot(diffs.idw1)
hist(diffs.idw1, breaks=100)
summary(diffs.idw1)
boxplot(diffs.idw1)





##  IDW pow 2 -------
interp.idw2 <- terra::interpIDW(rveneto, smpl, field="Temp",
                                power=2, radius=0.2)
plot(interp.idw2)
temperature.rveneto.interp.idw2 = terra::mask(interp.idw2,veneto)
plot(temperature.rveneto.interp.idw2)
plot(temperature.rveneto.nn)
#how good did it go?
diffs.idw2 = temperature.rveneto.interp.idw2-temperature.rveneto.nn
plot(diffs.idw2)
hist(diffs.idw2, breaks=100)
summary(diffs.idw2)
boxplot(diffs.idw2)



cc = data.frame( diffs = na.omit(diffs.nn[][,1] ), type= "NN")
# create a data frame with residuals ------------
residuals =
rbind(
  data.frame( diffs= na.omit(diffs.nn[][,1] ), type= "NN"),
  data.frame( diffs= na.omit(diffs.nn.linear[][,1] ), type= "Linear"),
  data.frame( diffs= na.omit(diffs.idw1[][,1] ), type= "idw1"),
  data.frame( diffs= na.omit(diffs.idw2[][,1] ), type= "idw2")
  )

boxplot( residuals$diffs~residuals$type, outline=FALSE)

# save objects for next steps ------------
save(residuals, smpl, file="esercizi/modulo2/data/modulo2_es05_interpolation_deterministic.rda")


xy=terra::geom(smpl)
modello.lm <- lm(Temp~xy[,3:4], data = smpl)
summary(modello.lm)

plot(modello.lm)

plot(modello.lm$fitted.values, smpl$Temp)
