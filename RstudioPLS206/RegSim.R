## Simulation of simple regression
## when assumptions hold.

# Enter parameter values
beta0<-200
beta1<-50
sigma<-300

# Create a vector of values for x, the predictor.
x<-c(1,1,1,3,3,3,4,4,7,7,7,7,9,9,12,12,13,13,14,14,14,18,18,18)
length(x)

# Simulate one realization of the observations.
y<-beta0+beta1*x+rnorm(length(x),0, sigma)
y
# Note how the recycling function results in the correct
# vector of observations.

beta0
beta1
beta1*x
rnorm(length(x),0, sigma)

# Perform the regression using the lm function
slr<-lm(y~x) 

slr

summary(slr) # a summary of the regression results
names(slr)   # names of all components of object slr
str(slr)  #structure of the object slr
slr$coefficients # looking at some of the components of object slr.
slr$residuals
slr$fitted.values
slr$model
coef(slr)    # estimated regression coefficients.
confint(slr) # confidence interval for regression coefficients.
vcov(slr) # Matrix of covariance of estimated regression coefficients.
anova(slr) # Analysis of variance of regression.
(yhat9<-slr$coef%*%c(1,9)) #Prediction for x=9. Note the use of parenthesis to create and object and display it.
plot(slr)  # Default plots show diagnostics for assumptions.
par(mfrow = c(2,2)) #  Asking to see the diagnostics in one view, it is: see 4 plots in one page
plot(slr) # Deafult plots show in one page
dev.off() # Restarting the plot viewer 
plot(y~x)  # Plot data.
plot(y~x, xlab="Predictor", ylab="Response variable", ylim=c(0,1400)) # Better with labels for axes.
lines(coef(slr)[1]+coef(slr)[2]*(c(0:40/2))~c(0:40/2), lty=2, lwd=3) #Add a line for the fitted values.
lines(beta0+beta1*c(0:40/2)~c(0:40/2), lty=1) # Add line for true model
## The MSE can be calculated "by hand" using matrix algebra
slr$residuals%*%slr$residuals
mse<-slr$residuals%*%slr$residuals/(length(y)-length(slr$coef))
sqrt(mse)

## Creating a function can streamline the simulation process.
## We pass the true model parameters as arguments of the function.

my.slr.sim<-function(b0, b1,s) {
	beta0<-b0
	beta1<-b1
	sigma<-s
	x<-c(1,1,1,3,3,3,4,4,7,7,7,7,9,9,12,12,13,13,14,14,14,18,18,18)
	y<-beta0+beta1*x+rnorm(length(x),0, sigma)
	slr<-lm(y~x)
	plot(y~x, xlab="Predictor", ylab="Response variable")
	lines((coef(slr)[1]+coef(slr)[2]*(c(0:40/2)))~c(0:40/2), lty=2, lwd=3)
	lines(beta0+beta1*c(0:40/2)~c(0:40/2), lty=1)
	}
my.slr.sim(10,20,100) #run repeatedly to get random simulations
## Load package car for additional functions.

## Simulations to look at the distributions of the estimated coefficients.
bcoef<-matrix(NA,1000,2)  #make a matrix of zeroes to receive the estimated coefficients.

## Perform simulation loop, then we use for().
for(i in 1:1000){
newy<-beta0+beta1*x+rnorm(24,0, sigma)
new.slr<-lm(newy~x)
bcoef[i,]<-coef(new.slr)
}

## Manual calculation of prediction for x=3 from simulated betas - results in a vector
yhat9<-bcoef[,1]+bcoef[,2]*9
yhat18<-bcoef[,1]+bcoef[,2]*18
my.ys <-cbind(yhat3,yhat5)
#plot a histogram of predicted yhats
hist(yhat3)
sqrt(var(yhat9))
sqrt(var(yhat18))
mean.y3<-mean(yhat3)
res.y3 <- yhat3-mean.y3
sum(res.y3^2)/999
# estimated variance of individual prediction=MSE(1+1/n+(x-xbar)^2/SSx) variance predict individual observed Y
# variance of individual prediction=sigma^2*(1+1/n+(x-xbar)^2/SSx) variance predict individual observed Y

xbar <- mean(x)
SSx <- sum((x-xbar)^2)
(x-xbar)%*%(x-xbar)

sqrt(v9 <- sigma^2*(1/24 + (9-xbar)^2/SSx))
sqrt(v18<- sigma^2*(1/24 + (18-xbar)^2/SSx))
## Plot smooth histograms of results for the calculated slope.
plot(density(bcoef[,2]),main="Frequency of estimated beta1",xlab="Estimated slope")
abline(v=mean(bcoef[,2]))
abline(v=quantile(bcoef[,2],c(0.025,0.975)), lty=3)
abline(v=beta1, lwd=3)

## Confidence intervals for estimated parameters
confint(slr)

## Confidence intervals for predictions
# Create a data.frame with the values for x and use "predict"
newdata = data.frame(x=c(3,10))
predict(slr, newdata,interval="confidence") # for expected value of y given x=3 and x=10
predict(slr, newdata,interval="predict")  # for individual value of y given x=3 and x=10

# For a difference between two predictions it is necessary to do some calculations.
# For example you want to compare yhat(9) vs. yhat(18) and create a CI for the difference.
# yhat(9)  = bo + b1*9 = 1*bo + 9*b1 = (1,9) %*% (bo,b1)
# yhat(18) = bo + b1*18 = 1*bo + 18*b1 = (1,18) %*% (bo,b1)
# yhat(18) - yhat(9) = ( (1,18) - (1,9) ) %*% (bo,b1) = (0,9) %*% (bo,b1)
# c(1,18)-c(1,9)
# more on this later
