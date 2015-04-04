# Collinearity demo
library(MASS)
library(car)
my.sigma <- matrix(c(1,0.99,0.99,1), nrow=2, byrow=TRUE)
codata <- as.data.frame(mvrnorm(n=100, mu=c(0,0), Sigma=my.sigma))
names(codata) <- c("X1r", "X2r")
codata$Yr <- 10 + codata$X1r + 5*codata$X2r + (my.epsilon <- rnorm(length(codata$X2r), mean=0, sd=3))
codata$X1 <- sample(codata$X1r)
codata$X2 <- sample(codata$X2r)
codata$Y <- 10 + codata$X1 + 5*codata$X2 + my.epsilon

m1r <- lm(Yr~X1r+X2r, codata)
summary(m1r)
vif(m1r)
m1 <- lm(Y~X1+X2, codata)
summary(m1)
vif(m1)

scatter3d(codata$Yr ~ codata$X1r + codata$X2r)
scatter3d(codata$Y ~ codata$X1 + codata$X2)

# Variance of estimated parameters IS NOT THE SAME as variance of predictions or estimated Y.

newdata <- data.frame("Y"=c(NA,NA), "X1"=c(0.2, 0.1), "X2"=c(0.2, 0.5), "Yr"=c(NA,NA), "X1r"=c(0.2, 0.1), "X2r"=c(0.2, 0.5))
predict(m1, newdata=newdata, se.fit=TRUE)
predict(m1r,newdata=newdata, se.fit=TRUE)