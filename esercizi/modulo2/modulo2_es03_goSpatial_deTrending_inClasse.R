library(terra)
#raster temperatura
temperature <- terra::rast("esercizi/modulo2/data/VenetoCorrectedMODIS_LST_Avg2017.tif")
## aggiungiamo una potenziale covariata - la quota! - vedi slide "External Drift Kriging"
DEM <- terra::rast("esercizi/modulo2/data/VenetoDEM.tif")

smpl <- terra::spatSample(temperature, method="random",
                          size=1000, na.rm=T,  as.points=T)
# estraggo i valori di quota
q <- terra::extract(DEM, smpl, ID=F )
smpl$altitude <- q$VenetoDEM

xy <- terra::geom(smpl)[,3:4]

names(smpl)  <-  c("Temp", "Quota.m" )
# na.omit elimina eventuali righe con NA o NaN
smpl.df <-  na.omit( as_tibble( cbind(smpl, xy) ) )
save(smpl.df, file="esercizi/modulo2/smpl.df.rda")
library(tidyverse)
smpl.df2 <- smpl %>% cbind(xy) %>% as_tibble %>% na.omit

modello.lm <- lm(Temp~`Quota.m`, data = smpl.df)
summary(modello.lm)


modello.lm2 <- lm(Temp~`Quota.m`+x+y, data = smpl.df)
summary(modello.lm2)

library(gstat)
library(sp)


# smpl.sf = na.omit( sf::st_as_sf(smpl) )

smpl.sf = smpl %>%  sf::st_as_sf() %>% na.omit()

x <- gstat::variogram(Temp~1, data= smpl.sf[1:100,], cloud=TRUE)
plot(x)

x <- gstat::variogram(Temp~1, data= smpl.sf, covariogram=T, cloud=FALSE)
plot(x)


x <- gstat::variogram(Temp~Quota.m, data= smpl.sf,  covariogram=F,cloud=FALSE)
plot(x)


x <- gstat::variogram(Temp~Quota.m, data= smpl.sf,
                      # alpha= seq(0,360, 60),  # con o senza direzione
                      covariogram=F,cloud=FALSE)
plot(x)

z <- as(raster::raster(temperature), "SpatialGridDataFrame")

x <- gstat::variogram(Temp~1,smpl.sf,
                      map= T, cutoff=20, width=1 )
plot(x)

# variogramma interattivo - solo 100 punti per memoria
x <- variogram(Temp~1, data= smpl.sf[1:100,], cloud=TRUE)
plot(plot(x, identify = TRUE), smpl.sf[1:100,])



t.fit<- gstat::fit.variogram(t.vgm, vgm("Sph"));
t.fit        # use when don't have estimate of range, sill
plot(t.vgm, t.fit)  #got a nice fit?





# parameters of variogram function
# width=??,
# cutoff = input$cutoff,
# alpha = as.numeric(input$angles),
# covariogram=input$covariogram=="Covariogram",
