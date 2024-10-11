library(terra)
library(sf)
library(sp)

# recupero i dati da es 3
load(file="esercizi/modulo2/smpl.df.rda")

# we use temperature raster to
# have a template for the output raster
temperature <- terra::rast("esercizi/modulo2/data/VenetoCorrectedMODIS_LST_Avg2017.tif")

# create a grid for predicting value at locations
veneto <- sf::read_sf("esercizi/modulo2/data/veneto.gpkg")
# nb our data are in lat long!
ra = terra::rast(veneto, res=0.02)
plot(ra)
rveneto<- terra::rasterize(veneto, ra)
plot(rveneto)
#
temperature.rveneto.nn = terra::resample(temperature, rveneto, method="near")
# temperature.rveneto.bl = terra::resample(temperature, rveneto)
plot(temperature.rveneto.nn)
plot(temperature.rveneto.bl)
plot(temperature.rveneto.bl-temperature.rveneto.nn)
hist(temperature.rveneto.bl-temperature.rveneto.nn)

smpl<- terra::vect(smpl.df,geom=c("x","y"))
##  simple nn -----
interp.nn <- terra::interpNear(rveneto, smpl, field="Temp", radius=1)

# interp.nn <- terra::interpNear(rveneto,
#                                as.matrix(smpl.df[,c("x","y","Temp")]),
#                                radius=1)

plot(interp.nn)
temperature.rveneto.interp.nn = terra::mask(interp.nn,rveneto)
plot(temperature.rveneto.interp.nn)
# plot(smpl, add=T, pch="+")

#how good did it go?
diffs.nn = temperature.rveneto.interp.nn - temperature.rveneto.nn
plot(diffs.nn)
hist(diffs.nn, breaks=100)
summary(diffs.nn)
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



# cc = data.frame( diffs = na.omit(diffs.nn[][,1] ), type= "NN")
# create a data frame with residuals ------------
residuals =
list(
   NN= na.omit(diffs.nn[][,1] ),
   linear= na.omit(diffs.nn.linear[][,1] ),
   idw1= na.omit(diffs.idw1[][,1] ),
   idw2= na.omit(diffs.idw2[][,1] )
  )

res <- reshape2::melt(residuals)
boxplot( res$value~res$L1, outline=FALSE)

# save objects for next steps ------------
save(residuals, smpl.df, file="esercizi/modulo2/data/modulo2_es05_interpolation_deterministic.rda")


xy=terra::geom(smpl)
modello.lm <- lm(Temp~xy[,3:4], data = smpl)
summary(modello.lm)

plot(modello.lm)

plot(modello.lm$fitted.values, smpl$Temp)




