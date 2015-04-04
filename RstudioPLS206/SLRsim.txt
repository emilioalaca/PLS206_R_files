beta0<-200
beta1<-50
beta2<--2
sigma<-100
x<-c(1,1,1,3,3,3,4,4,7,7,7,7,9,9,12,12,13,13,14,14,14,18,18,18)
length(x)
y<-beta0+beta1*x+beta2*x^2+rnorm(24,0, sigma)
slr<-lm(y~x)
yhat9<-slr$coef%*%c(1,9)
yhat9
plot(slr)
plot(y~x)
length(slr$coef)
slr
slr$residuals%*%slr$residuals
slr$residuals%*%slr$residuals/(length(y)-length(slr$coef))
summary(slr)
mse<-slr$residuals%*%slr$residuals/(length(y)-length(slr$coef))
sqrt(mse)
bcoef<-matrix(0,1000,2)
for(i in 1:1000){
newy<-beta0+beta1*x+beta2*x^2+rnorm(24,0, sigma)
new.slr<-lm(newy~x)
bcoef[i,]<-new.slr$coef
}
plot(density(bcoef[,2]),xlab="estimated slope")
abline(v=mean(bcoef[,2]))
abline(v=quantile(bcoef[,2],c(0.025,0.975)))
rexp(1)
bcoefX<-matrix(0,1000,2)
for(i in 1:1000){
rexp(2)
for(i in 1:1000){
newy<-beta0+beta1*x+beta2*x^2+rexp(1)*10)
for(i in 1:1000){
newy<-beta0+beta1*x+beta2*x^2+rexp(1)*10
new.slr<-lm(newy~x)
bcoefX[i,]<-new.slr$coef
}
plot(density(bcoefX[,2]),xlab="estimated slope")
beta1<-50#
beta2<--2#
sigma<-100#
x<-c(1,1,1,3,3,3,4,4,7,7,7,7,9,9,12,12,13,13,14,14,14,18,18,18)#
length(x)#
y<-beta0+beta1*x+rnorm(24,0, sigma)#
slr<-lm(y~x)#
yhat9<-slr$coef%*%c(1,9)#
yhat9#
plot(slr)#
plot(y~x)#
length(slr$coef)#
slr#
#extract residuals and then calculate mse using matrix operations#
slr$residuals%*%slr$residuals#
mse<-slr$residuals%*%slr$residuals/(length(y)-length(slr$coef))#
sqrt(mse)#
summary(slr) #compare to the results in the lm object#
#create a matrix to contain results of simulation#
bcoef<-matrix(0,1000,2)#
#use for () to run simulations#
for(i in 1:1000){#
newy<-beta0+beta1*x+beta2*x^2+rnorm(24,0, sigma)#
new.slr<-lm(newy~x)#
bcoef[i,]<-new.slr$coef#
}
beta0<-200#
beta1<-50#
beta0<-200#
sigma<-100#
x<-c(1,1,1,3,3,3,4,4,7,7,7,7,9,9,12,12,13,13,14,14,14,18,18,18)#
length(x)#
y<-beta0+beta1*x+rnorm(24,0, sigma)#
slr<-lm(y~x)#
yhat9<-slr$coef%*%c(1,9)#
yhat9#
plot(slr)#
plot(y~x)#
length(slr$coef)#
slr#
#extract residuals and then calculate mse using matrix operations#
slr$residuals%*%slr$residuals#
mse<-slr$residuals%*%slr$residuals/(length(y)-length(slr$coef))#
sqrt(mse)#
summary(slr) #compare to the results in the lm object#
#create a matrix to contain results of simulation#
bcoef<-matrix(0,1000,2)#
#use for () to run simulations#
for(i in 1:1000){#
newy<-beta0+beta1*x+rnorm(24,0, sigma)#
new.slr<-lm(newy~x)#
bcoef[i,]<-new.slr$coef#
}#
#plot results and confidence interval#
plot(density(bcoef[,2]),xlab="estimated slope")#
abline(v=mean(bcoef[,2]))#
abline(v=quantile(bcoef[,2],c(0.025,0.975)))
library(ellipse)
confint(slr)
quantile(bcoef[,2],c(0.025,0.975))
quantile(bcoef[,1],c(0.025,0.975))
predict(slr,c(1,0))
predict(slr,c(1)
)
plot(ellipse(slr))
points(bcoef[,2],bcoef[,1])
for(i in 1:1000){
points(bcoef[i,1],bcoef[i,2])
}
plot(ellipse(slr), type="l")
