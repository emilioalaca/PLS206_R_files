# Bootstrap demo: what good is bootstrapping? What is the impact of bias correction? Chi-square distribution.
# Median

# Sample a population of residuals that is distributed with a Chi square distribution.

e<-rchisq(20,2)
hist(e)
plot(density(rchisq(500,2)))

# Simulate a dataset with chi-square errors with 2 df.
# Note that the errors can only be positive!
# Each time we call the function we simulate a sample.
ychi<-function(n) {rchisq(n,2)}

# Now take one sample and apply "traditional" methods and bootstrapping with and w/out bias correction.
mysmpl<-ychi(25)
hist(mysmpl)
abline(v=median(mysmpl))
mydata<-as.data.frame(mysmpl) # Traditional method
names(mydata)<-c("y")
(mymed<-median(mydata$y))

library(boot)


myboot<-function(data, i) {
	data<-data[i,]
	median(data)
	}
	
boot1<-boot(mydata,myboot,1999)
plot(boot1, index=1)
boot.ci(boot1,index=1)
confint(lm1)

# Streamlined, compact program
# 200 simulations:
bciSlope<-list("slopes")
bciIntercept<-list("intercept")
for (i in 1:200) {
	mydata<-as.data.frame(cbind(ychi(5,0.5,15),seq(1:15)))
	names(mydata)<-c("y","x")
	lm1<-lm(y~x,mydata)
	boot1<-boot(mydata,myboot,999)
	bciS<-boot.ci(boot1,index=2)
	bciSlope[[i]]<-rbind(confint(lm1)[2,],bciS$normal[2:3],bciS$basic[4:5],bciS$percent[4:5],bciS$bca[4:5])
	bciI<-boot.ci(boot1,index=1)
	bciIntercept[[i]]<-rbind(confint(lm1)[1,],bciI$normal[2:3],bciI$basic[4:5],bciI$percent[4:5],bciI$bca[4:5])
}

# Plot the slopes
plot(2, xlim=c(0,1),ylim=c(0,201))

for (i in 1:200) lines(x=c(bciSlope[[i]][1,]), y=c(i,i), col="blue",lwd=2)
abline(v=0.5)

for (i in 1:200) lines(x=c(bciSlope[[i]][5,]), y=c(i+0.3,i+0.3), lty=1, col="red",lwd=2)

for (i in 1:200) lines(x=c(bciSlope[[i]][4,]), y=c(i+0.6,i+0.6), lty=1, col="green",lwd=2)


# Plot the intercept
plot(2, xlim=c(0,15),ylim=c(0,201))

for (i in 1:200) lines(x=c(bciIntercept[[i]][1,]), y=c(i,i), col="blue",lwd=2)
abline(v=5)

for (i in 1:200) lines(x=c(bciIntercept[[i]][5,]), y=c(i+0.3,i+0.3), lty=1, col="red",lwd=2)

for (i in 1:200) lines(x=c(bciIntercept[[i]][4,]), y=c(i+0.6,i+0.6), lty=1, col="green",lwd=2)

