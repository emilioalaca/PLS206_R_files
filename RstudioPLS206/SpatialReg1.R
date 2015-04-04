library(spdep)
data(meuse.grid)
coordinates(meuse.grid)<-c("x", "y")
gridded(meuse.grid)<-TRUE #make into SpatialPixelsDataFrame
plot(meuse.grid)
meuse.knn<-knearneigh(coordinates(meuse.grid), 8) #define k nearest neighbors
meuse.nb<-knn2nb(meuse.knn) #make neighbors from knn
meuse.lw<-nb2listw(meuse.nb, style="S")
?spplot #learn about plotting data
spplot(meuse.grid, zcol=c("dist"))
