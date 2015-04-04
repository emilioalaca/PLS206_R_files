filaree<-read.csv("../Examples/xmpl_filaree.csv", header=TRUE)
options(contrasts =c("contr.sum", "contr.poly"))
names(filaree) <- c("group", "days", "lnwt")
filaree$group<-factor(filaree$group)
# Create a continuous variable for temperature
filaree$t <- NA
filaree$t[filaree$group==1] <- 10
filaree$t[filaree$group==2] <- 15
filaree$t[filaree$group==3] <- 20
FullModel<-lm(lnwt ~ days + t + days:t + days:I(t^2), filaree)
anova(FullModel)
ReducedModel <- lm(lnwt ~ days + t + days:t, filaree)
anova(FullModel, ReducedModel) # same as the test above
SSE.ReducedModel <- deviance(ReducedModel)
SSE.FullModel <- deviance(FullModel)
dfe.ReducedModel <- df.residual(ReducedModel)
dfe.FullModel <- df.residual(FullModel)
MSEfull <- SSE.FullModel/dfe.FullModel
(fstat <- ((SSE.ReducedModel-SSE.FullModel)/(dfe.ReducedModel-dfe.FullModel))/MSEfull)
print("The p value is")
1-pf(fstat,dfe.ReducedModel-dfe.FullModel,dfe.FullModel)
summary(ReducedModel)
(PRESS.statistic <- sum( (resid(ReducedModel)/(1-hatvalues(ReducedModel)))^2 ))
library(car) # awesome diagnostic plots
Anova(FullModel, type=3)
outlierTest(ReducedModel) # Bonferonni p-value for most extreme obs
qqPlot(ReducedModel, main="QQ Plot") #qq plot for studentized resid
shapiro.test(residuals(ReducedModel))
residualPlots(ReducedModel)
influenceIndexPlot(ReducedModel)
filaree$dayg <- NA
filaree$dayg[filaree$days<10] <- "A"
filaree$dayg[filaree$days>10&filaree$days<20] <- "B"
filaree$dayg[filaree$days>20&filaree$days<25] <- "C"
filaree$dayg[filaree$days>25&filaree$days<35] <- "D"
filaree$dayg[filaree$days>35] <- "E"
FullestModel <- lm(lnwt ~ group*dayg, filaree)
anova(ReducedModel, FullestModel)
betterModel <- lm(lnwt~days + t+  I(days^2) + t:days, filaree)
residualPlots(betterModel)
anova(FullestModel,betterModel)
plot(betterModel)
evenBetterModel<-lm(lnwt~days + t + t:days+  I(days^2) + I(t^2), filaree)
residualPlots(evenBetterModel)


## A function to use identify to select points, and overplot the
## points with another symbol as they are selected
identifyPch <- function(x, y = NULL, n = length(x), pch = 19, ...)
  {
  xy <- xy.coords(x, y); x <- xy$x; y <- xy$y
  sel <- rep(FALSE, length(x)); res <- integer(0)
  while(sum(sel) < n) {
    ans <- identify(x[!sel], y[!sel], n = 1, plot = FALSE, ...)
    if(!length(ans)) break
    ans <- which(!sel)[ans]
    points(x[ans], y[ans], pch = pch)
    sel[ans] <- TRUE
    res <- c(res, ans)
  }
  res
}
