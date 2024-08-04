## ----setup, include=FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----wd, echo=FALSE-----------------------------------------------------------------------------
#working directory
wd <- getwd()
setwd("esercizi")


## ----librerie,results='hide',message=F,warning=F------------------------------------------------
library(sp)
#carica anche la libreria sp
library(gstat)

library(sf)
library(terra)

library(RColorBrewer)#utile per la gestione dei colori?
library(classInt)


## ----colori,results='hide',message=F,warning=F--------------------------------------------------
mycolb <- rgb(0,0,255, alpha = 125,max=255)#blu
mycolr <- rgb(255,0,0, alpha = 125,max=255)#rosso
mycolg <- rgb(0,255,0, alpha = 125,max=255)#verde
mycolc <- rgb(255,165,0, alpha = 125,max=255)#magenta


## ----dati---------------------------------------------------------------------------------------
data<-read.table("esercizi/centraline.txt",header=T)
data[1:10,] #prime dieci righe del dataset


## ----SpatialPointsDataFrame creation------------------------------------------------------------
coordinates(data)<- ~ x+y

plot(data)
## ----logpm10------------------------------------------------------------------------------------
data$logpm10<- log(data$pm10)


## ----shapefile lombardia------------------------------------------------------------------------
lombardia <- sf::read_sf("lombardia.shp")
capoluoghi <- sf::read_sf("capoluoghi_lombardia.shp")

#definizione dei layer da aggiungere alla mappa della regione
l1<-list( sf::as_Spatial(capoluoghi),pch=16,col=2)
l2<-list("sp.text", sf::st_coordinates(capoluoghi), capoluoghi$com)

#mappa della regione
spplot( sf::as_Spatial(lombardia) ,1,sp.layout=list(l1,l2),
        colorkey=F,col.regions="white")


## ----summary------------------------------------------------------------------------------------
summary(data)


## ----plot coordinate----------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(data$x,data$logpm10)
lines(lowess(data$x,data$logpm10),col=2)
plot(data$y,data$logpm10)
lines(lowess(data$y,data$logpm10),col=2)


## ----bubble dati--------------------------------------------------------------------------------
l3<-list( sf::as_Spatial(lombardia))
bubble( data, "logpm10", col = c(mycolr, mycolb),sp.layout=list(l3,l1,l2),do.sqrt=F)


## ----superficie lineare-------------------------------------------------------------------------
x<-data$x
y<-data$y
z<-data$logpm10
linear.fit<-lm(z~x+y)
summary(linear.fit)


## ----superficie quadratica----------------------------------------------------------------------
quadr.fit<-lm(z~x+y+I(x^2)+I(y^2)+x*y)
summary(quadr.fit)


## ----bubble residui-----------------------------------------------------------------------------
data$residui<-residuals(quadr.fit)
bubble(data, "residui", col = c(mycolr, mycolb),sp.layout=list(l3,l1,l2),do.sqrt=F)


## ----vgm grezzi---------------------------------------------------------------------------------
vgm.cloud <- variogram(logpm10~1, data,cloud=T)
plot(vgm.cloud)
vgm.emp <- variogram(logpm10~1, data,cutoff=1.5e5,width=15e3)
plot(vgm.emp,col=1,pch=16)


## ----vgm emp residui----------------------------------------------------------------------------
vgm.emp.quad <- variogram(logpm10~x+y+I(x^2)+I(y^2)+x*y, data,cutoff=1.5e5,width=15e3)
plot(vgm.emp.quad,col=1,pch=16)


## ----vgm emp dir--------------------------------------------------------------------------------
vgm.dir<-variogram(logpm10~x+y+I(x^2)+I(y^2)+x*y, data, alpha=c(0,45,90,135),cutoff=1.5e5)
plot(vgm.dir,multipanel=F)


## ----vgm param err------------------------------------------------------------------------------
assex<-seq(1,to=1.5e5,by=10)
vario.expo.wls<-fit.variogram(vgm.emp.quad,model=vgm(0.1,"Exp",100))


## ----vgm param----------------------------------------------------------------------------------
assex<-seq(1,to=1.5e5,by=10)
vario.expo.wls<-fit.variogram(vgm.emp.quad,model=vgm(0.1,"Exp",1000))
y.expo<-variogramLine(vario.expo.wls,dist_vector=assex)
vario.sphe.wls<-fit.variogram(vgm.emp.quad,model=vgm(0.1,"Sph",100000))
y.sphe<-variogramLine(vario.sphe.wls,dist_vector=assex)
vario.gaus.wls<-fit.variogram(vgm.emp.quad,model=vgm(0.1,"Gau",10000))
y.gaus<-variogramLine(vario.gaus.wls,dist_vector=assex)
vario.mate.wls<-fit.variogram(vgm.emp.quad,model=vgm(0.1,"Mat",1000),fit.kappa=T)
y.mate<-variogramLine(vario.mate.wls,dist_vector=assex)
plot(vgm.emp.quad$dist,vgm.emp.quad$gamma,xlab="distance",ylab="semivariance",pch=16,ylim=c(0,0.022))
lines(y.expo,col=2,lwd=2)
lines(y.sphe,col=3,lwd=2)
lines(y.gaus,col=4,lwd=2)
lines(y.mate,col=6,lwd=2)
legend("bottomright",legend=c("Exponential","Spherical","Gaussian","Matérn"),col=c(2:4,6),lwd=2)


## ----griglia1-----------------------------------------------------------------------------------
passo<-2000
punti.x<-seq(from=bbox(lombardia)[1,1]-4000,length=120,by=passo)
plot(lombardia)
abline(v=punti.x)


## ----griglia2-----------------------------------------------------------------------------------
punti.y<-seq(from=bbox(lombardia)[2,1]-4000,by=passo,length=120)
plot(lombardia)
abline(h=punti.y)


## ----griglia3-----------------------------------------------------------------------------------
cp<-expand.grid(x=punti.x,y=punti.y)
coordinates(cp)<- ~ x+y
gridded(cp)<-T
proj4string(cp)<-proj4string(lombardia)
summary(cp)
spplot(cp,sp.layout=list(l3),pch=16,col.regions=1,cex=0.1,colorkey=list())


## ----griglia4-----------------------------------------------------------------------------------
a<-over(cp,lombardia)
cp$indi<-!is.na(a$COD_REG)
spplot(cp,col.regions=2:1,pch=16,cex=0.1,sp.layout=list(l3),colorkey=F)

cp<-cp[cp$indi,]
spplot(cp,pch=16,cex=0.1,sp.layout=list(l3),col.regions=1,colorkey=F)


## ----idw1---------------------------------------------------------------------------------------
proj4string(data)<-proj4string(lombardia)
idw.est.1 <- idw(formula=logpm10~1,data, newdata=cp,idp=1)
l4<-list(data,pch=17,col=1)
spplot(idw.est.1,zcol="var1.pred",sp.layout=list(l3,l4),
       col.regions=topo.colors(100))


## ----idw2---------------------------------------------------------------------------------------
idw.est.2 <- idw(formula=logpm10~1,data, newdata=cp,idp=2)
spplot(idw.est.2,zcol="var1.pred",sp.layout=list(l3,l4),
       col.regions=topo.colors(100))


## ----idw3---------------------------------------------------------------------------------------
idw.est.3 <- idw(formula=logpm10~1,data, newdata=cp,idp=3)
spplot(idw.est.3,zcol="var1.pred",sp.layout=list(l3,l4),
       col.regions=topo.colors(100))


## ----krige cv1----------------------------------------------------------------------------------
krig.cv.exp <- krige.cv(logpm10~x+y+I(x^2)+I(y^2)+x*y, data,
               nfold=dim(data)[1], model = vario.expo.wls)
CV.exp<-mean(krig.cv.exp$residual^2)
CV.exp


## ----krige cv2----------------------------------------------------------------------------------
krig.cv.sph <- krige.cv(logpm10~x+y+I(x^2)+I(y^2)+x*y, data,
                        nfold=dim(data)[1], model = vario.sphe.wls)
CV.sph<-mean(krig.cv.sph$residual^2)
CV.sph


## ----krige cv3----------------------------------------------------------------------------------
krig.cv.gau <- krige.cv(logpm10~x+y+I(x^2)+I(y^2)+x*y, data,
                        nfold=dim(data)[1], model = vario.gaus.wls)
CV.gau<-mean(krig.cv.gau$residual^2)
CV.gau


## ----krige cv4----------------------------------------------------------------------------------
krig.cv.mat <- krige.cv(logpm10~x+y+I(x^2)+I(y^2)+x*y, data,
                        nfold=dim(data)[1], model = vario.mate.wls)
CV.mat<-mean(krig.cv.mat$residual^2)
CV.mat


## ----krige--------------------------------------------------------------------------------------
krig.mat <- krige(logpm10~x+y+I(x^2)+I(y^2)+x*y, data,
            newdata=cp, model = vario.mate.wls)

spplot(krig.mat,zcol="var1.pred",sp.layout=list(l3,l4),
       col.regions=topo.colors(100))



## ----grandescala--------------------------------------------------------------------------------
cp$grandescala<-predict(quadr.fit,newdata=cp)
spplot(cp,zcol="grandescala",sp.layout=list(l1,l2,l3,l4),
       col.regions=topo.colors(100))


## ----krige var----------------------------------------------------------------------------------
krig.mat$var1.sterr<-sqrt(krig.mat$var1.var)
spplot(krig.mat,zcol="var1.sterr",sp.layout=list(l1,l2,l3,l4),
       col.regions=topo.colors(100))


## ----cv plot1-----------------------------------------------------------------------------------
par(pty="s")
plot(krig.cv.mat$observed,krig.cv.mat$observed-krig.cv.mat$residual,
     xlab="observed",ylab="expected")
abline(a=0,b=1)


## ----cv plot2-----------------------------------------------------------------------------------
bubble(krig.cv.mat, "residual", col = c(mycolr, mycolb),sp.layout=list(l3))

