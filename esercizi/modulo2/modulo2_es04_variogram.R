############### Check stationarity 2 -----
library(terra)
library(tidyterra)
library(gstat)
library(sp)
library(sf)


# please run the previous R script
# to load necessary data

# plot clearly shows that detrending is needed regarding Y and Quota
# sjPlot::plot_model(modello.lm2,
#                    show.values = TRUE )

smpl.sf = smpl %>%  sf::st_as_sf() %>% na.omit()
# coordinates(smpl.df) <- ~x+y
# crs(smpl.df)<-CRS("epsg:4326")

x <- variogram(Temp~1, data= smpl.sf[1:100,], cloud=TRUE)
plot(x)
plot(plot(x, identify = TRUE), smpl.sf[1:100,])


library(scatterplot3d)
scatplot<-scatterplot3d( sf::st_coordinates(smpl.sf)[,1], sf::st_coordinates(smpl.sf)[,2],
                         smpl.sf$Quota.m, main="3D Scatterplot")
fit <- lm(smpl.sf$Quota.m ~ sf::st_coordinates(smpl.sf)[,1] + sf::st_coordinates(smpl.sf)[,2]) # FIT A PLANE THROUGH THE DATA
summary(fit)
scatplot$plane3d(fit)



x <- variogram(Temp~Quota.m, data= smpl.sf[1:100,], cloud=TRUE)
plot(plot(x, identify = TRUE), smpl.sf[1:100,])


#plot empirical variogram
t.vgm<-variogram(Temp~1,data=smpl.sf)
plot(t.vgm)

t.fit<-fit.variogram(t.vgm, vgm("Sph"));
t.fit        # use when don't have estimate of range, sill

t.vgm<-variogram(Temp~Quota.m,data=smpl.sf)
t.fit<-fit.variogram(t.vgm, vgm("Sph"));
plot(t.vgm)

plot(t.vgm, t.fit)  #got a nice fit?


