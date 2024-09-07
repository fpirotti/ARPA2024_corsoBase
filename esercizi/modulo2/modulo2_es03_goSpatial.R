############### Check stationarity -----
library(terra)
library(tidyterra)

# Inserire il percorso CORRETTO di datindel vostro disco
# NB se usate un progetto in RStudio la cartella di riferimento (radice)
# da cui parte il percorso è quella del progetto!
# E' ..........
temperature <- terra::rast("esercizi/modulo2/data/VenetoCorrectedMODIS_LST_Avg2017.tif")
## aggiungiamo una potenziale covariata - la quota!
DEM <- terra::rast("esercizi/modulo2/data/VenetoDEM.tif")
# comuni del Veneto (non serve per ora)
#veneto <- sf::read_sf("esercizi/modulo2/data/veneto.gpkg")

## sample 1000 points
smpl <- terra::spatSample(temperature, method="random",
                        size=1000, na.rm=T, as.points=T)
## extract temperature
q <- terra::extract(DEM, smpl, ID=F )
smpl$altitude<- q$VenetoDEM

plot(temperature)
plot(smpl, pch="x", add=T)


smpl$altitude <- terra::extract(DEM, smpl[,1:2], ID=F )[[1]]/1000
smpl$altitudeM <- smpl$altitude *1000

names(smpl)  <-  c("x","y","Temp", "Quota.km", "Quota.m")
smpl<-as_tibble(smpl)

plot(Temp~`Quota.km`, data = smpl)
modello.lm <- lm(Temp~`Quota.km`, data = smpl)
summary(modello.lm)


plot(Temp~`Quota.m`, data = smpl)
modello.lm <- lm(Temp~`Quota.m`, data = smpl)
summary(modello.lm)

