beta0<-10
beta1<-2
sigma<-10
y<-beta0+beta1*x+rnorm(length(x),0, sigma)
x<-c(1,1,1,3,3,3,4,4,7,7,7,7,9,9,12,12,13,13,14,14,14,18,18,18)
plot(y~x)
for(i in 1:100){
	y<-beta0+beta1*x+rnorm(length(x),0, sigma)
	slr<-lm(y~x)
	lines(coef(slr)[1]+coef(slr)[2]*c(0:40/2)~c(0:40/2), lty=2, lwd=1)
}

confint(lm(y~x))
