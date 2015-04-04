# read the data into R using the .csv file with headers
sp<-read.csv("../Examples/spartina.txt", header=T)
sm1 <- lm(bmss ~ sal + pH + K + Na + Zn, sp)
summary(sm1)

library(car)

sm1.sres <- rstandard(sm1) # these are residuals divided by their estimated individual sd: res <- e / sqrt(MSE*(1 - hat))
sm1.stures <- rstudent(sm1) # deleted studentized residuals
sm1.hats <- hatvalues(sm1)

residualPlots(sm1)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(sm1) # shows residual and influence plots
plot(sm1, which=5, cook.levels=qf(0.5, length(coef(sm1)),sm1$df.residual))
dwt(sm1) # test of autocorrelation of residuals. Only valid if observations are in temporal or spatial order and approximately evenly spaced.
vif(sm1) # values over 5 are undesirable and over 10 indicate excessive collinearity.
sm2 <- update(sm1, ~ (sal + pH + K + Na + Zn)^2)
anova(sm2,sm1)
library(HH)
hov(residuals(sm1) ~ loc, sp)
hov(residuals(sm1) ~ typ, sp)
hov(residuals(sm1) ~ factor(paste(loc,typ)), sp)
hovPlot(residuals(sm1) ~ factor(paste(loc,typ)), sp)
hovPlot(residuals(sm1) ~ loc, sp)
hovPlot(residuals(sm1) ~ typ, sp)
library(MASS)
boxcox(sm1) # transform Y?
# new_y = (y^lambda-1)/lambda if lambda!=0 else new_y = log(y)
sm3 <- update(sm1, ((bmss^0.4-1)/0.4) ~.)
plot(sm3, which=1)
hovPlot(residuals(sm3) ~ factor(paste(loc,typ)), sp)
avPlots(sm1) # added-variable or leverage plots

# Other remedial measure for HOV: GENERALIZED LEAST SQUARES

library(nlme)
sm4 <- gls(bmss ~ sal + pH + K + Na + Zn, data=sp, 
           weights = varIdent(form = ~1|loc*typ))

summary(sm4)
plot(sm4)
plot(residuals(sm4, type="normalized")~factor(paste(loc,typ)), data=sp)
plot(fitted(sm3), rstandard(sm3))
1-pt(max(abs(residuals(sm4, type="n"))),39)
hist(residuals(sm4, type="n"))
coplot(residuals(sm4, type="n") ~ Na|factor(paste(loc,typ)), data=sp)
plot(sm4, resid(., type="n") ~ fitted(.)|loc) # plot method for nlme




outlierTest(sm1, cutoff=10)

sp[which(sm1.hats>0.27),]

X <- sp[,c("sal","pH","K","Na","Zn")] # give mv data short name for simplicity

library(bootstrap) # load package that has jackknife()

## Write a function that returns the distance for each observation when it is not included in the dataset. Note the power and compactness of R's subsetting
theta<-function(x,xdata) 
{mahalanobis(xdata[-x,],colMeans(xdata[x,]), cov(xdata[x,]))}

jackD2<-jackknife(1:dim(X)[1], theta, X) #jackknife indicating to remove each of the whole set of rows in X

jackD2$jack.values  #compare with the Chi-square distribution
qqPlot(jackD2$jack.values,distribution="chisq", df=ncol(X))
X[which(jackD2$jack.values==max(jackD2$jack.values)),]
plot(Na~K,X) # The odd ball is an observation with high Na and low K

vif(update(sm1,data=sp[-18,]))

qqPlot(lm(sqrt(bmss) ~ sal + pH + K + Na + Zn, sp))
shapro.test(residuals(lm(log(bmss) ~ sal + pH + K + Na + Zn, sp)))

# K-fold cross validation
library(DAAG)
cv.lm(df=sp, sm1, m=3, seed=floor(1000*runif(1))) # 3 fold cross-validation