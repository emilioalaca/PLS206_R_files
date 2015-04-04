library(spdep)
data(columbus)
polynb<-poly2nb(polys) #create a neighbors object
col.gal.nb #there is already another neighbors object created by a diferent method
polynb #they have some similarity
colqueen<-nb2listw(col.gal.nb) #create the weights matrix based on neighbors
polylw<-nb2listw(polynb)
colqueen$weights[1:5] #see the weights
polylw$weights[1:5]
col1<-lm(CRIME~INC+HOVAL,data=columbus)
summary(col1)
lm.morantest(col1, polylw) #errors are correlated.
col2<-lagsarlm(CRIME~INC+HOVAL,data=columbus, polylw) #make a model using an effect of spatially lagged Y with spatial autocorrelation rho
summary(col2)
moran.test(resid(col2),polylw) #correlation of errors has been corrected by using a spatially lagged Y effect.
plot(polys)
plot.listw(polylw,coords)