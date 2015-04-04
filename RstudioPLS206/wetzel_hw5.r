###### Wetzel - PLS206 HW05 ######

options(show.signif.stars=F)

bulls <- read.csv("Bulls.csv", header=T)
bulls$Breed <- as.factor(bulls$Breed)

model <- lm(SalePr ~ YrHgt + FtFrBody + PrctFFB + Frame + BkFat + SaleHt + SaleWt, data=bulls)

summary(model)
anova(model)
bulls$fitted <- fitted(model)
bulls$residuals <- residuals(model)
#bulls$rstudent <- rstudent(model)
bulls$hatvalues <- hatvalues(model)
#bulls$cooks.distance <- cooks.distance(model)


## 1. checking normality ##
shapiro.test(bulls$residuals)
hist(bulls$residuals, main="Histogram of residuals", xlab="residuals",las=1)
shapiro.test(bulls$rstudent)

## 2. checking heterogeneity of errors ##
par(mfrow=c(3,3))
for(i in 3:9){
	plot(bulls$residuals ~ bulls[,i], xlab=names(bulls)[i])
	}

plot(residuals ~ fitted, data=bulls, xlab="predicted SalePr", las=1)

m <- tapply(X=bulls$SalePr, INDEX=bulls$Breed, FUN=mean) # mean of SalePr by Breed
s <- tapply(X=bulls$SalePr, INDEX=bulls$Breed, FUN=sd) # sd of SalePr by Breed
plot(s ~ m, las=1, xlab="mean", ylab="std deviation", pch=20)
abline(lm(s ~ m))

plot(bulls$residuals ~ bulls$Breed, xlab='Breed', ylab='SalePr residuals')

library(car) # load the car package to use levene.test

with(bulls, levene.test(residuals, Breed)) # test of homogeneity of variance by breed

## car's levene.test uses the absolute deviations to the group median, whereas JMP uses deviations to the group mean. R is more robust to deviations from distributional assumptions.

## 3. testing independence of errors ##
# (serial correlation)
library(lmtest) # load required package for dwtest
dwtest(model, alternative="two.sided")

## 4. inspection of linearity ##
av.plots(model, ask=FALSE, identify.points=FALSE) # partial regression plots

## 5. outliers in X-dim (hats and JD) ##
with(bulls, plot(hatvalues, las=1, pch=20))
crit.hat <- 2 * 8 / 76
abline(h=crit.hat, lty=2, col="red")
which(bulls$hatvalues > crit.hat)

# jackknifed distance?
X <- with(bulls, data.frame(YrHgt , FtFrBody , PrctFFB , Frame , BkFat , SaleHt , SaleWt))
Sx <- cov(X)
D2 <- mahalanobis(X, colMeans(X), Sx) # this is a linear function of hat values! YES! Look at equations for both and see.

library(bootstrap) # load package that has jackknife()

## Write a function that returns the distance for each observation when it is not included in the dataset. Note the power and compactness of R's subsetting
theta<-function(x,xdata) 
{mahalanobis(xdata[-x,],colMeans(xdata[x,]), cov(xdata[x,]))}

jackD2<-jackknife(1:dim(X)[1], theta, X) #jackknife indicating to remove eac of the whole set of rows in X

sqrt(jackD2$jack.values)  #compare with the values saved in JMP

## 6. checking collinearity
as.data.frame(vif(model))

## 7. MSPR

bulls.odd <- bulls[1:76%%2!=0,-10:-12]
bulls.even <- bulls[1:76%%2==0,-10:-12]

model.odd <- lm(SalePr ~ YrHgt + FtFrBody + PrctFFB + Frame + BkFat + SaleHt + SaleWt, data=bulls.odd)
model.even <- lm(SalePr ~ YrHgt + FtFrBody + PrctFFB + Frame + BkFat + SaleHt + SaleWt, data=bulls.even)

xvals.odd <- as.data.frame(bulls.odd[,3:9])
xvals.even <- as.data.frame(bulls.even[,3:9])

odd.model.predict.for.even.data <- predict(model.odd, xvals.even, interval="prediction", level=0.95)
even.model.predict.for.odd.data <- predict(model.even, xvals.odd, interval="prediction", level=0.95)

# the next line had the even and odd mismatched and I fixed it.

MSPR <- sum((c(bulls.odd$SalePr, bulls.even$SalePr) - c(even.model.predict.for.odd.data[,1], odd.model.predict.for.even.data[,1]))^2) / nrow(bulls)
anova(model)