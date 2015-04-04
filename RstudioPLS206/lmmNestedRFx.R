# Example with real data for mixed models.
# A complete block design was set up in five sites selected to be representative of annual rangelands. Each site had an identical layout of five plots in each of three blocks. Each plot randomly received on of the five treatments and was split into a grazed and an excluded (fenced) area. forage mass was measured in a series of 4-6 quadrats inside and outside the exclosure at the end of the grazing season.
# Data and code Copyright 2013 Emilio A. Laca, Theresa Becchetti, Josh Davy

dwt <- read.csv("../Examples/nestedRFx.csv", header=TRUE)
dwt$block <- as.factor(dwt$block)
dwt$rep <- as.factor(dwt$rep)
str(dwt)

options(contrasts=c("contr.sum", "contr.poly"))

library(nlme)
library(multcomp)
library(car)
library(lattice)
m1 <- lme(fixed = weight.lb.ac~trtNS*in.out, random=~1|site/blk/plot, data=dwt)
intervals(m1) # check the precision of the estimated variances. Look OK.
plot(m1) #heterogeneity of variance
m2 <- update(m1, weights = varPower(form=~fitted(.)))
intervals(m2)
plot(m2) # heteroscedasticity fixed but problems estimating vcov's
m6 <- update(m2, fixed=.~.+site, random=~1|blk/plot)
intervals(m6)

m7 <- update(m6, random=list(blk=pdCompSymm(~plot-1))) # see p.161 MEMS
intervals(m7)
anova(m7)
qqnorm(m7, ~ resid(.)|in.out)
qqPlot(resid(m7)) # m7 has vcov estimates but poor distribution of residuals
qqPlot(ranef(m7))
plot(m7)
plot(residuals(m7)~plot, data=dwt)
anova(lm(residuals(m7)~plot, data=dwt))

m8 <- update(m7, random=list(blk=pdDiag(~1), plot=pdSymm(~1)))
intervals(m8)
m9 <- update(m6, fixed=lnwt~., weights=NULL)
plot(m9) # log transformation was too strong
m10 <- update(m9, fixed=log(weight.lb.ac+400)~.,)
plot(m10)
intervals(m10)
m11 <- lme(fixed =log(weight.lb.ac+400) ~ trtNS*in.out + site, random=~1|blk/plot, data=dwt)
plot(m11)
intervals(m11)
anova(m11)
qqnorm(m11, ~ resid(.)|in.out)
qqPlot(resid(m11))
qqnorm(m11, ~ranef(., level=1))
qqPlot(ranef(m11,level=1))
qqPlot(ranef(m11,level=2))
plot(residuals(m11)~plot, data=dwt)

library(lme4) #fit with lmer to be able to use ezPlot
lmer1 <- lmer(lnwt400 ~ trtNS * in.out + site + (1|blk/plot), data=dwt)
anova(lmer1)
Anova(lmer1)
library(ez)
ezpred1 <- ezPredict(fit = lmer1)
(ezplot1 <- ezPlot2(ezpred1, x=trtNS, split=in.out))
(ezplot2 <- ezPlot2(ezpred1, x=trtNS, diff=in.out))
# summary(glht(lmer1, linfct=mcp(in.out*trtNS="Tukey")))
N00in<- c(1,0,0,0,0,1,0,0,0,0,0,0,0,0)