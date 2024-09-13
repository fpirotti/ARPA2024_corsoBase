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

# sample 1000 points
smpl <- terra::spatSample(temperature, method="random",
                        size=1000, na.rm=T,  as.points=T)

# extract Height above sea level
q <- terra::extract(DEM, smpl, ID=F )
smpl$altitude<- q$VenetoDEM
# you might get some NAs due to sample points falling outside
# the DEM
# scale height to km
smpl$altitude.km <- smpl$altitude / 1000

# xy columns
xy <- terra::geom(smpl)[,3:4]

plot(temperature)
plot(smpl, pch="x", add=T)

# rename to pretty names
names(smpl)  <-  c("Temp", "Quota.m", "Quota.km")
# na.omit elimina eventuali righe con NA o NaN
smpl.df<- na.omit( as_tibble( cbind(smpl, xy) ) )

# linear regression
modello.lm <- lm(Temp~`Quota.km`, data = smpl.df)
summary(modello.lm)

modello.lm2 <- lm(Temp~`Quota.km`+x+y, data = smpl.df)
summary(modello.lm2)


plot(Temp~`Quota.km`, data = smpl.df)
abline(modello.lm, col="red", lwd=3)
abline(modello.lm2, col="green", lwd=3)

# come facciamo a visualizzare un plot con modello multi-variabile?
plot(x=smpl.df$Temp,
     y=modello.lm$fitted.values,
     pch="+",
     xlab="Temperature Measured",
     ylab="Temperature Modelled")
points(x=smpl.df$Temp,
       y=modello.lm2$fitted.values,
       pch="°", col="blue")
abline(a=0, b=1, col="red", lwd=3)


# oppure librerie più sofisticate come
sjPlot::plot_model(modello.lm2,
                   show.values = TRUE )
