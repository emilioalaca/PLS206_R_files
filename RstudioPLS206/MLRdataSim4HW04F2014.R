# bird abundance in forest patches simulation
# create predictors
library(MASS)
library(car)
library(Matrix)
library(leaps)
my.mat01 <- matrix(c(1.00,0.05,0.03,0.02,0.00,0.02,-0.02,
                     0.05,1.00,0.60,0.50,0.00,0.00,-0.10,
                     0.03,0.60,1.00,0.40,0.10,0.03,0.58,
                     0.02,0.50,0.40,1.00,0.55,0.05,0.57,
                     0.00,0.00,0.10,0.55,1.00,0.07,0.45,
                     0.02,0.00,0.03,0.05,0.07,1.00,0.05,
                    -0.02,-0.10,0.58,0.57,0.45,0.05,1.00), nrow=7, byrow=TRUE)

my.sigma <- nearPD(my.mat01,keepDiag=TRUE)

Xmat <-mvrnorm(n=100, mu=c(0,0,0,0,0,0,0), Sigma=my.sigma$mat)

Y <- 25 + Xmat %*% c(0.7,1.4,0.1,-0.2,0,0.9,0) + rnorm(dim(Xmat)[1], mean=0, sd=1.2)

birdata <- as.data.frame(cbind(Y,Xmat))
class(birdata )
str(birdata)

names(birdata) <- c("bd", "grzinv", "dist", "height", "peri", "mammal", "area", "leg")

m1 <- lm(bd ~ grzinv + dist + height + peri + mammal + area + leg, birdata)
summary(m1)
vif(m1)
aic01 <- stepAIC(m1, trace=0)
summary(aic01)
vif(aic01)

birds.best3<-summary(best3<-regsubsets(bd ~ grzinv + dist + height + peri + mammal + area + leg, birdata, nbest=3, nvmax=7, method=c("exhaustive")))
birds.best3
birds.best3$which[which.min(birds.best3$bic),]

write.csv(birdata,"birds.csv")
