## ========================= Bootstrapping cases =============================================

library(boot) #load necessary package

datab<-read.csv("../Examples/colinDemoData.csv", header=T) # read collin data for the example; may need to specify full path
str(datab)
# The boot function needs the user to specify a function that returns the statistic
# of interest, or a vector with various statistics of interest.
# We create the function "betas" to get all the estimated regression coefficients.

betas<-function(data,i)
{ #starts body of function
bootsample<-data[i,] # creates a bootstrapped sample with a set of rows "i"
model<-lm(Y~X1+X2, data=bootsample) # uses the bootstrap sample to estimate betas; no colin variables
coef(model) # extracts and returns betas
} # end body of function

noCol.boot<-boot(datab,betas,R=2000) # performs boostrapping and saves boot object into noCol.boot

noCol.boot # see part of object; t#*'s refer to the different quantities calculated
str(noCol.boot)
original.model<-lm(data=datab,Y~X1+X2) # calculate non-bootstrapped model
tstar <- apply(noCol.boot$t,2,mean)
summary(original.model)
tstar-coef(original.model)
hist(noCol.boot$t[,1], freq=FALSE, breaks=40)
lines(density(noCol.boot$t[,1]))
hist(noCol.boot$t[,2], freq=FALSE, breaks=40)
lines(density(noCol.boot$t[,2]))
hist(noCol.boot$t[,3], freq=FALSE, breaks=40)
boot.ci(noCol.boot, index=1)
boot.ci(noCol.boot, index=2, type="bca")
boot.ci(noCol.boot, index=3)
mean(noCol.boot$t[,1])
confint(original.model)
# Collinear data

betasr<-function(data,i)
{
bootsample<-data[i,]
model<-lm(Yr~X1r+X2r, data=bootsample)
coef(model)
}

Col.boot<-boot(datab,betasr,R=1000)

originalr.model<-lm(data=datab,Yr~X1r+X2r)

summary(originalr.model)
boot.ci(Col.boot, index=2)
boot.ci(Col.boot, index=3)
confint(originalr.model)

# As a short exercise, modify the first function betas to do both correlated and uncorrelated X's within the same bootstrapping.
# ================================ Resampling residuals =====================================================
# we are pretending that X's are fixed etc.
resids<-residuals(original.model)
X1<-datab$X1
X2<-datab$X2
res.data<-data.frame(resids, X1, X2) # make data frame with residuals and predictors

yhat<-fitted(original.model) # save the predicted values for original model

# create function with subset i of rows for each bootstrap sample
beta.res<-function(whatever.data, i) {
  y<-yhat+whatever.data[i,1]
  bdata<-data.frame(y,X1,X2)
  model<-lm(y~X1+X2, data=bdata)
  coef(model)
}

noCol.boot.res<-boot(res.data, beta.res, R=2000) # perform bootstrapping
boot.ci(noCol.boot.res, index=2, type="bca")
par(mfrow=c(3,2))
plot(noCol.boot,index=2)
plot(Col.boot,index=2)
plot(noCol.boot.res,index=2)