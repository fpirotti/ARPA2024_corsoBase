library(terra)
library(sf)
library(sp)
library(gstat)
library(tidyverse)

# load objects from modulo2_es05_interpolation_deterministic.R  ------------
load(file="esercizi/modulo2/data/modulo2_es05_interpolation_deterministic.rda")
# raster temperatura
temperature <- terra::rast("esercizi/modulo2/data/VenetoCorrectedMODIS_LST_Avg2017.tif")

# create a grid for predicting value at locations
veneto <- sf::read_sf("esercizi/modulo2/data/veneto.gpkg")
# nb our data are in lat long so res is in degrees!
ra = terra::rast(veneto, res=0.02)
rveneto<- terra::rasterize(veneto, ra)


# resample temperature to our veneto area
temperature.rveneto = terra::resample(temperature, rveneto ) %>% terra::mask(rveneto)
plot(temperature.rveneto)




# OK ordinary kriging ----
###
v <- variogram(Temp~1,
               locations = ~x+y,
               data=smpl.df,width=0.1,
               cutoff=3)
plot(v)
# esegui funzione vgm senza argomenti per lista di funzioni matematiche
vgm()
# applica un modello matematico "Gau" al nostro variogramma
mv <- fit.variogram(v, vgm(1,model="Sph",1,psill=30 ) )
# vedi ?vgm per opzioni di dati iniziali per la regressione
# mv <- fit.variogram(v, vgm(1, "Sph", 0.4,1 ))
plot(v,mv)
# vediamo le caratteristiche del modello (nugget, sill e range)
mv
# creo oggetto gstat da usare poi nella funzione di interpolazione
gOK <- gstat(NULL, "Temperatura", Temp~1, smpl.df,
             locations=~x+y, model=mv)
# applico l'interpolazione
OK <- terra::interpolate(rveneto, gOK,  debug.level=0)
# plot(temperature.rveneto.interp.OK$Temperatura.var)
# salvo il raster con i valori stimati con kriging semplice  ----
# writeRaster(temperature.rveneto.interp.OK, "temperature.rveneto.interp.OK.tif", overwrite=T)
# provate a cambiare argomenti in vgm e
#  argomento"nmax" di gstat !

### test accuratezza ----
diffs.nn = OK$Temperatura.pred - temperature.rveneto
# plot(diffs.nn)
# hist(diffs.nn, breaks=100)
# summary(diffs.nn)
# quantile(diffs.nn$Temperatura.pred[], c(0.1,0.25,0.5,0.75, 0.9 ), na.rm=T)
residuals[["KrigOrd"]] <- na.omit(diffs.nn$Temperatura.pred[][,1] )


# universal kriging ----
# lm.res <- lm(Temp~x+y, data=smpl.df)
vu <- variogram(Temp~x+y, ~x+y, data=smpl.df)
plot(vu)
mu <- fit.variogram(vu, vgm(0.1, model='Sph', psill=6, range=0.9))
plot(vu,mu)
mu
gUK <- gstat(NULL, "Temp", Temp~x+y, smpl.df,
             locations=~x+y, model=mu)
UK <- interpolate(rveneto, gUK, debug.level=0)

### test accuratezza ----
diffs.nn = UK$Temp.pred - temperature.rveneto
residuals[["KrigUniv"]] <- na.omit(diffs.nn$Temp.pred[][,1] )


# external drift kriging ----
vu <- variogram(Temp~Quota.m+y+x, ~x+y, data=smpl.df)
mu <- fit.variogram(vu, vgm('Sph'))
plot(vu, mu)
edK <- gstat(NULL, "Temp", Temp~Quota.m+y+x, smpl.df,
             locations=~x+y, model=mu)

DEM <- terra::rast("esercizi/modulo2/data/VenetoDEM.tif")
rveneto.quota <- terra::resample(DEM, rveneto) %>% terra::mask(rveneto)
names(rveneto.quota) <- "Quota.m"
EDK <- interpolate(rveneto.quota,
                                              edK,
                                              debug.level=0)

### test accuratezza ----
diffs.nn = EDK$Temp.pred - temperature.rveneto
residuals[["KrigED"]] <- na.omit(diffs.nn$Temp.pred[][,1] )
plot(EDK)

# co-kriging ----
gCoK <- gstat(NULL, 'Quota', Quota.m~1, smpl.df,
              locations=~x+y,nmax = 20)
gCoK <- gstat(gCoK, 'Temp', Temp~1, smpl.df,
              locations=~x+y, nmax = 20)
coV <- variogram(gCoK, cutoff=2)
plot(coV, type='b', main='Co-variogram')
coV.fit <- fit.lmc(coV, gCoK, vgm(model='Sph'))
coV.fit
plot(coV, coV.fit, main='Fitted Co-variogram')
coV.fit$set=list(nocheck=1)
coK <- interpolate(rveneto, coV.fit, debug.level=10)
plot(coK)

### test accuratezza ----
diffs.nn = coK$Temp.pred - temperature.rveneto
residuals[["KrigeCO"]] <- na.omit(diffs.nn$Temp.pred[][,1] )

res <- reshape2::melt(residuals)
boxplot( res$value~res$L1, outline=FALSE)



# block kriging ----
block_size <- c(0.6, 0.6)  # Block size in x and y directions
# Convert terra raster to spatial points (sp)
sp_grid  <- as.data.frame(rveneto, xy = TRUE, na.rm = FALSE)
coordinates(sp_grid) <- ~x + y
# Make the grid gridded
gridded(sp_grid) <- TRUE

BK <- predict(gCoK, newdata = sp_grid, block = block_size)
predicted_values <- BK$Temp.pred

# Assign the predicted values to the raster
blK <- rast(rveneto)
values(blK) <- predicted_values
plot(blK)

### test accuratezza ----
diffs.nn = blK$layer - temperature.rveneto
residuals[["KrigeblK"]] <- na.omit(diffs.nn$layer[][,1] )


# TABELLA FINALE METRICHE ERRORI -----
# deviazione standard dell'errore,
sapply(residuals, function(x){ round(sd(x),4) } )
# RMSE, - sembra simile ma perchè la media è molto vicina a zero
sapply(residuals, function(x){ round((sum(x^2)/length(x))^0.5,4) })
# errore medio (bias)
sapply(residuals, function(x){ round( mean(x),4) })

