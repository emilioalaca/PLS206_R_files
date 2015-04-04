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
curve(1/(1+exp(-(-1 + 1*x))), from=-5, to=5, main="(b)")
abline(h=1/2, lty=2)
abline(v=1, lty=2)
args(nls)

args(nls.control)
lm(logit(population/400) ~ year, USPop)
pop.mod <- nls(population ~ theta1/(1 + exp(-(theta2 + theta3*year))),
     start=list(theta1 = 400, theta2 = -49, theta3 = 0.025),
     data=USPop, trace=TRUE)
summary(pop.mod)
deltaMethod(pop.mod, "-theta2/theta3")

pop.mod <- update(pop.mod, trace=FALSE) # suppress trace

confint(pop.mod)
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

# ============== END OF MODULE 1 (E. A. Laca) ==================

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
Data$can <- ifelse(Data$country=="Canada", 1, 0)
form1 <- population ~ (1 - can)*(phi11/(1 + exp(-(year - phi21)/phi31))) +
                      can*(phi12/(1 + exp(-(year - phi22)/phi32)))
b <- coef(m.list)
m1 <- nls(form1, data=Data, weights=w, start=list(phi11=b[1, 1], phi12=b[1, 2],
       phi21=b[1, 2], phi22=b[2, 2], phi31=b[1, 3], phi32=b[2, 3]))
form2 <- population ~ (1 - can)*(phi11/(1 + exp(-(year - phi21)/phi3))) +
                      can*(phi12/(1 + exp(-(year - phi22)/phi3)))
m2 <- nls(form2, data=Data, weights=w, start=list(phi11=b[1, 1], phi12=b[1, 2],
       phi21=b[1, 2], phi22=b[2, 2], phi3=b[1, 3]))

anova(m2, m1)

confint(m2)
model <- function(theta1, theta2, theta3, year){
     yhat <- theta1/(1 + exp(-(theta2 + theta3*year)))
     term <- exp(-(theta2 + theta3*year))
     gradient <- cbind((1 + term)^-1, # in proper order
         theta1*(1 + term)^-2 * term,
         theta1*(1 + term)^-2 * term * year)
     attr(yhat, "gradient") <- gradient
     yhat
     }
(nls(population ~ model(theta1, theta2, theta3, year),
     data=USPop, start=list(theta1=400, theta2=-49, theta3=0.025)))
(model2 <- deriv(~ theta1/(1 + exp(-(theta2 + theta3*year))), # rhs of model
     c("theta1", "theta2", "theta3"), # parameter names
     function(theta1, theta2, theta3, year){} # arguments for result
     ))
(nls(population ~ model2(theta1, theta2, theta3, year),
     data=USPop, start=list(theta1=400, theta2=-49, theta3=0.025)))
