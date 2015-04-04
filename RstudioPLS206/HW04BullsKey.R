# HW04 key
bulls <- read.csv("..//Examples//Bulls.txt", header=TRUE)
names(bulls)

bull1 <- lm(SalePr ~ YrHgt + FtFrBody + PrctFFB + Frame + BkFat + SaleHt + SaleWt, bulls)
library(car)
library(HH)
hov(residuals(bull1)~as.factor(Breed), bulls)
shapiro.test(residuals(bull1))
qqPlot(bull1)
residualPlots(bull1)
avPlots(bull1)
influenceIndexPlot(bull1)
outlierTest(bull1)
max(hatvalues(bull1))
2*length(coef(bull1))/length(residuals(bull1))
X <- bulls[,3:9] # give mv data short name for simplicity

library(bootstrap) # load package that has jackknife()

## Write a function that returns the distance for each observation when it is not included in the dataset. Note the power and compactness of R's subsetting
theta<-function(x,xdata) 
{mahalanobis(xdata[-x,],colMeans(xdata[x,]), cov(xdata[x,]))}

jackD2<-jackknife(1:dim(X)[1], theta, X) #jackknife indicating to remove eac of the whole set of rows in X

jackD2$jack.values  #compare with the Chi-square distribution
qqPlot(jackD2$jack.values,distribution="chisq", df=ncol(X))

vif(bull1)
library(leaps)

bulls.best3<-summary(best3<-regsubsets(SalePr ~ YrHgt + FtFrBody + PrctFFB + Frame + BkFat + SaleHt + SaleWt, bulls, nbest=3, nvmax=7, method=c("exhaustive")))
bulls.best3

bulls.best3$which[which.min(bulls.best3$bic),] #retrieves the list of variables in the best model according to the BIC criterion

bull2 <- update(bull1, ~ .- YrHgt -PrctFFB - SaleWt)
summary(bull2)
library(DAAG)
cv.lm(df=bulls, bull1, m=3, seed=floor(1000*runif(1))) # 3 fold cross-validation
cv.lm(df=bulls, bull2, m=3, seed=floor(1000*runif(1))) # 3 fold cross-validation