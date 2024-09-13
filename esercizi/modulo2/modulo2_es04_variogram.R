############### Check stationarity 2 -----
library(terra)
library(tidyterra)
library(gstat)
library(sp)


# please run the previous R script
# to load necessary data

# plot clearly shows that detrending is needed regarding Y and Quota
sjPlot::plot_model(modello.lm2,
                   show.values = TRUE )


coordinates(smpl.df) <- ~x+y
crs(smpl.df)<-CRS("epsg:4326")

x <- variogram(Temp~1, data= smpl.df[1:100,], cloud=TRUE)

plot(plot(x, identify = TRUE), smpl.df[1:100,])
plot(plot(x, digitize = TRUE), smpl.df[1:100,])


cov(smpl.df[1:500,]$Temp, smpl.df[1:500,]$Temp)

x <- variogram(Temp~1, data= smpl.df[1:100,])
plot(x)
