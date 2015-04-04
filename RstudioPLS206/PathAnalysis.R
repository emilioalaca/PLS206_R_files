# PATH ANALYSIS

wheat <- read.csv("../Examples/xmpl_PATH.dat", header=TRUE)
str(wheat)
wheat$variety <- factor(wheat$variety)
wheat$rep <- factor(wheat$rep)
# path coefficients are the standardized partial regression coefficients
swheat <- cbind(wheat[,1:3], scale(wheat[,4:11]))
lm1 <- lm(yield ~ SPM + KSP + KWT, wheat)
lm2 <- lm(yield ~ SPM + KSP + KWT - 1, swheat)
summary(lm1)
summary(lm2)
str(summary(lm2))
arrow.e <- sqrt(1-summary(lm2)$r.squared)
cormat1 <- cor(swheat[c(6:8,11)])
cor.test(swheat$SPM,swheat$KSP)
cor.test(swheat$SPM,swheat$KWT)
cor.test(swheat$KSP,swheat$KWT)