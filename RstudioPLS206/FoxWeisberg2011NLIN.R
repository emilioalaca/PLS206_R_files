#Code Based on
#Nonlinear Regression Fox and Weisberg (2010)
#with modifications by Emilio A. Laca (2013)

##-------------------------------------------------------##
## An R Companion to Applied Regression, Second Edition  ##
##    Script for Appendix on Nonlinear  Regression       ##
##                                                       ##
##    John Fox and Sanford Weisberg                      ##
##    Sage Publications, 2011                            ##
##-------------------------------------------------------##

if (exists(".options")) options(.options)
options(width=80,show.signif.stars=FALSE,digits=4,scipen=1)

library(car)
plot(population ~ year, data=USPop, main="(a)")
abline(lm(population ~ year, data=USPop))

#
curve(1/(1+exp(-(-1 + 1*x))), from=-5, to=5, main="Logistic growth curve")
abline(h=1/2, lty=2)
abline(v=1, lty=2)
curve(1/(1+exp(-( - 1*x))), from=-5, to=5, main="Logistic decline")

# The nls() function is used for nonlinear regression without random effects or problematic residuals.
args(nls)
args(nls.control)
lm(logit(population/400) ~ year, USPop)
pop.mod <- nls(population ~ theta1/(1 + exp(-(theta2 + theta3*year))),
               start=list(theta1 = 400, theta2 = -49, theta3 = 0.025),
               data=USPop, trace=FALSE)
summary(pop.mod)
deltaMethod(pop.mod, "-theta2/theta3")
pop.mod <- update(pop.mod, trace=FALSE) # suppress trace

confint(pop.mod) #runs additional calculations to get CI's for parameters
##=========== application of bootstrapping ====================
#compare to bootstrapping results (ignoring autocorrelation of residuals)
library(boot)
boot.nls <- function(data, my.start, i) {
  data$pop.i <- data$pred + data$res[i]
  nls.i <- nls(pop.i ~ theta1/(1 + exp(-(theta2 + theta3*year))), start=my.start, data=data)
  -coef(nls.i)[2]/coef(nls.i)[3]
}
bdata <- cbind(USPop, "res"=residuals(pop.mod), "pred"=fitted(pop.mod))
theta2over3 <- boot(bdata,boot.nls,R=3000, my.start=list(theta1 = 400, theta2 = -49, theta3 = 0.025))
boot.ci(theta2over3, type="bca") # asymmetric
mean(theta2over3$t[,1])
hist(theta2over3$t[,1], breaks=80)
##===========END application of bootstrapping ====================


plot(population ~ year, USPop, xlim=c(1790, 2100), ylim=c(0,450))
with(USPop, lines(seq(1790, 2100, by=10),
                  predict(pop.mod, data.frame(year=seq(1790, 2100, by=10))), lwd=2))
points(2010, 307, pch="x", cex=1.3)
abline(h=0, lty=2)
abline(h=coef(pop.mod)[1], lty=2)
abline(h=.5*coef(pop.mod)[1], lty=2)
abline(v= -coef(pop.mod)[2]/coef(pop.mod)[3], lty=2)
with(USPop, plot(year, residuals(pop.mod), type='b'))
abline(h=0, lty=2)
pop.ss <- nls(population ~ SSlogis(year, phi1, phi2, phi3), data=USPop)
summary(pop.ss)
deltaMethod(pop.mod, "1/theta3")
USPop$decade <- (USPop$year - 1790)/10
(pop.ss.rescaled <- nls(population ~ SSlogis(decade, nu1, nu2, nu3), data=USPop))

set.seed(12345) # for repeatability
out4 <- bootCase(pop.ss, B=999)
data.frame(summary(pop.ss)$coef[,1:2],
           bootMean=apply(out4,2,mean), bootSD=apply(out4,2,sd))

Data <- data.frame(rbind(data.frame(country="US", USPop[,1:2]),
                         data.frame(country="Canada", CanPop)))
some(Data)
scatterplot(population ~ year|country, data=Data, box=FALSE,
            reg=FALSE)
library(nlme)
m.list <- nlsList(population ~ SSlogis(year, phi1, phi2, phi3)|country,
                  pool=FALSE, data=Data)
summary(m.list)
(sds <- sapply(m.list, sigmaHat))
(betas <- lapply(m.list, coef))
(vars  <- lapply(m.list, vcov))
(betas <- unlist(betas))
zero <- matrix(0, nrow=3, ncol=3)
(var <- rbind( cbind(vars[[1]], zero), cbind(zero, vars[[2]])))
deltaMethod(betas, "US.phi3 - Canada.phi3", vcov=var)
deltaMethod(betas, "US.phi2 - Canada.phi2", vcov=var)
w <- ifelse(Data$country=="Canada", (sds[1]/sds[2])^2, 1)

#============ nlme() and ngls() =================
# Better method to do nonlinear models with nonlinear parameters that depend on categorical variables

# gnls() fits a nonlinear model using generalized least squares. The errors are allowed to be correlated and/or have unequal variances.
help(gnls)

gnls1 <- gnls(population ~ SSlogis(year, Asym, xmid, scal), data = Data,
              Asym + xmid + scal ~ country,
              start = c(400,-200,1975,30,40,5))
summary(gnls1)
plot(gnls1)
plot(fitted(gnls1)[Data$country=="US"],residuals(gnls1)[Data$country=="US"], col="blue")
points(fitted(gnls1)[Data$country=="Canada"],residuals(gnls1)[Data$country=="Canada"], col="red")
# autocorrelation and heterogeneity of variance suspected
plot(ACF(gnls1), alpha=0.01)

gnls2 <- update(gnls1, weights=varIdent(form=~country), corr=corAR1())
plot(ACF(gnls2, type="n"), alpha=0.01)

gnls3 <- update(gnls1, weights=varIdent(form=~country), corr=corARMA(p=0,q=5))
plot(ACF(gnls3, type="n"), alpha=0.01)

plot(Variogram(gnls1,form=~year,maxDist=100))

gnls4 <- update(gnls1, weights=varIdent(form=~country), corr=corSpher(form=~year))
plot(ACF(gnls4, type="n"), alpha=0.01)


plot(fitted(gnls2)[Data$country=="US"],residuals(gnls2, type="n")[Data$country=="US"], col="blue")
points(fitted(gnls2)[Data$country=="Canada"],residuals(gnls2, type="n")[Data$country=="Canada"], col="red")

#============ END nlme() and ngls() =================