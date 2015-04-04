d1 <- read.csv("BothExptByPlot.dat", header=TRUE)

d1.nna <- d1[!is.na(d1$IRmg.min.kgLW)&!is.na(d1$BiteMass.mg.kgLW),]
library(nlme)
m1 <- gnls(model = IRmg.min.kgLW ~ SSmicmen(BiteMass.mg.kgLW, IRmax, kmp), data = d1.nna, params = IRmax + kmp ~ spp*expt,  start = c(IRmax = c(300,300,300,300), kmp = c(0.1,0.1,0.1,0.1)))

m2 <- update(m1, params = IRmax + kmp ~ spp + expt, start = c(IRmax = c(300,300,300), kmp = c(4,4,4)))

anova(m1,m2)

m3 <- update(m2, params = list(IRmax ~ spp + expt, kmp ~ spp), start = c(IRmax = c(300,300,300), kmp = c(4,4)))

anova(m2,m3)

m4 <- update(m3, params = list(IRmax ~ spp, kmp ~ spp), start = c(IRmax = c(300,300), kmp = c(4,4)))

anova(m3,m4)

m5 <- update(m3, params = list(IRmax ~ expt, kmp ~ spp), start = c(IRmax = c(300,300), kmp = c(4,4)))

anova(m3,m5)

m6 <- update(m3, params = list(IRmax ~ spp + expt, kmp ~ 1), start = c(IRmax = c(300,300,300), kmp = c(4)))

anova(m3,m6) # m6 is the best model

summary(m6)

m7 <- update(m3, params = list(IRmax ~ spp, kmp ~ 1), start = c(IRmax = c(300,300), kmp = c(4)))

# bootstrapped CI for curves

library(boot)

# Test for differences among species or experiments in nlin coeffs using bootstrapping.

nlin.difs <- function(data,i) tryCatch(coef(update(m6, data=data[i,]))[2:3], error=function(e) rep(NA,2))
boot.m6.coefs <- boot(d1.nna, nlin.difs, 1999, strata=d1.nna$expt.spp)
boot.ci(boot.m6.coefs, type="bca", index=1)
boot.ci(boot.m6.coefs, type="bca", index=2)

nlin.difs7 <- function(data,i) {tryCatch(coef(update(m7, data=data[i,])), error=function(e) rep(NA,3))}
boot.m7.coefs <- boot(d1.nna, nlin.difs7, 1999, strata=d1.nna$spp)
boot.ci(boot.m7.coefs, type="bca", index=1)
boot.ci(boot.m7.coefs, type="bca", index=2)
(km.ci <- boot.ci(boot.m7.coefs, type="bca", index=3))

Vmax.avena <- na.omit(boot.m7.coefs$t[,1])
Vmax.cynodon <- na.omit((boot.m7.coefs$t[,1]+boot.m7.coefs$t[,2]))
km <- na.omit(boot.m7.coefs$t[,3])

(biting.time.ci.avena <- quantile(km/Vmax.avena, probs=c(0.025, 0.975)))
(biting.time.ci.cynodon <- quantile(km/Vmax.cynodon, probs=c(0.025, 0.975)))
(bite.time.diff <- quantile(km/Vmax.avena-km/Vmax.cynodon , probs=c(0.025, 0.975)))
pred.m7 <- cbind(expand.grid(BiteMass.mg.kgLW=seq(0.2,5.8,0.2),spp=c("avena","cynodon")),IRmg.min.kgLW=as.numeric(predict(m7,expand.grid(BiteMass.mg.kgLW=seq(0.2,5.8,0.2),spp=c("avena","cynodon")))))

plot(pred.m7$BiteMass.mg.kgLW[pred.m7$spp=="avena"], pred.m7$IRmg.min.kgLW[pred.m7$spp=="avena"], type="l", lty=2, lwd=3, ylim=c(0,200), xlab="Bite mass (mg per kg LW)", ylab="Intake rate (mg per kg LW per minute", cex.lab=1.7) 
lines(pred.m7$BiteMass.mg.kgLW[pred.m7$spp=="cynodon"], pred.m7$IRmg.min.kgLW[pred.m7$spp=="cynodon"], lwd=3)
points(d1.nna$BiteMass.mg.kgLW[d1.nna$spp=="cynodon"], d1.nna$IRmg.min.kgLW[d1.nna$spp=="cynodon"], pch=1, cex=1.5)
points(d1.nna$BiteMass.mg.kgLW[d1.nna$spp=="avena"], d1.nna$IRmg.min.kgLW[d1.nna$spp=="avena"], pch=19, cex=1.5)

(rsq.m7 <- ((var(d1.nna$IRmg.min.kgLW) * (length(d1.nna$IRmg.min.kgLW)-1)) - residuals(m7) %*% residuals(m7))/(var(d1.nna$IRmg.min.kgLW) * (length(d1.nna$IRmg.min.kgLW)-1)) )

predict.df <- as.data.frame(expand.grid(BiteMass.mg.kgLW = seq(0.5,5.5,1), spp = c("avena", "cynodon"), expt = c("grazed-down", "growth")))

pred.ir <- function(data,i) { tryCatch(predict(update(m6, data=data[i,]),predict.df), error=function(e) rep(NA,24)) }


boot.m6 <- boot(d1.nna, pred.ir, 999, strata=d1.nna$expt.spp )
boot.ci(boot.m6, type="bca", index=2)
avg.yhat <- apply(na.omit(boot.m6$t), 2, mean)
boot.m6$t0 - avg.yhat
plot(boot.m6$t0~avg.yhat)
abline(0,1)
y.ci <- matrix(nrow=2, ncol=24)
for (i in 1:24) { y.ci[,i] <- boot.ci(boot.m6, index=i, type="bca")$bca[4:5]}
y.ci <- t(y.ci)
predict.df <- cbind(predict.df,y.ci)
names(predict.df) <- c(names(predict.df)[1:3], "loCI", "upCI")

ir.lines <- cbind(xs <- expand.grid(BiteMass.mg.kgLW = seq(0.2,5.5,0.1), spp = c("avena", "cynodon"), expt = c("grazed-down", "growth")),predict(m6,xs))

write.csv(predict.df, file="IRcis.txt")
write.csv(ir.lines, file="IRpred.txt")

library(ggplot2)

qplot(BiteMass.mg.kgLW, IRmg.min.kgLW, data = d1.nna, colour = spp, size=2) + geom_smooth(method = "lm", se = FALSE, size=1)


# Check if the phase of the height ~ bite mass relationship reveals differences.
d1.nna$set <- "none"
d1.nna$set[d1.nna$spp=="avena" & d1.nna$height>40] <- "inverse"
d1.nna$set[d1.nna$spp=="cynodon" & d1.nna$height>=25] <- "inverse"
d1.nna$set[d1.nna$set=="none"] <- "direct"
plot(d1.nna$BiteMass.mg.kgLW[d1.nna$set=="direct"], d1.nna$IRmg.min.kgLW[d1.nna$set=="direct"], pch=19, cex=1.5)
points(d1.nna$BiteMass.mg.kgLW[d1.nna$set=="inverse"], d1.nna$IRmg.min.kgLW[d1.nna$set=="inverse"], cex=1.5)

growth <- d1.nna[d1.nna$expt=="growth",]
plot(growth$BiteMass.mg.kgLW[growth$set=="direct"], growth$IRmg.min.kgLW[growth$set=="direct"], pch=19, cex=1.5)
points(growth$BiteMass.mg.kgLW[growth$set=="inverse"], growth$IRmg.min.kgLW[growth$set=="inverse"], cex=1.5)
plot(growth$BiteMass.mg.kgLW[growth$set=="direct"], growth$IRmg.min.kgLW[growth$set=="direct"], pch=19, cex=1.5)



## USING A MODELS WITH A DIFFERENT PARAMETERIZATION: IIRmax and Tj ==========================

ir.m0 <- gnls(IRmg.min.kgLW ~ BiteMass.mg.kgLW /(Tj + BiteMass.mg.kgLW/IRmax), data = d1.nna, params = IRmax + Tj ~ spp*expt,  start = c(IRmax = as.numeric(coef(m1)[1:4]), Tj = c(0.013, 0.001,0.001,0.001)), weights = varExp(form= ~BiteMass.mg.kgLW))

ir.m1 <- gnls(IRmg.min.kgLW ~ BiteMass.mg.kgLW /(Tj + BiteMass.mg.kgLW/IRmax), data = d1.nna, params = IRmax + Tj ~ spp*expt,  start = c(IRmax = as.numeric(coef(m1)[1:4]), Tj = c(0.013, 0.001,0.001,0.001)), weights = varPower(form= ~BiteMass.mg.kgLW))

anova(ir.m0,ir.m1)

ir.m2 <- update(ir.m1, params = IRmax + Tj ~ spp + expt, start = c(IRmax = as.numeric(coef(ir.m1)[1:3]), Tj = as.numeric(coef(ir.m1)[5:7])))

anova(ir.m1,ir.m2)

ir.m3 <- update(ir.m2, params = list(IRmax ~ spp + expt, Tj ~ spp), start = c(IRmax = as.numeric(coef(ir.m2)[1:3]), Tj = as.numeric(coef(ir.m1)[5:6])))

anova(ir.m1,ir.m3)

ir.m4 <- update(ir.m3, params = list(IRmax ~ spp, Tj ~ spp), start = c(IRmax = as.numeric(coef(ir.m2)[1:2]), Tj = as.numeric(coef(ir.m1)[5:6])))

anova(ir.m3,ir.m4) # ir.m3 is better
summary(ir.m3)

ir.m5 <- update(ir.m3, params = list(IRmax ~ spp + expt, Tj ~ 1), start = c(IRmax = as.numeric(coef(ir.m2)[1:3]), Tj = as.numeric(coef(ir.m3)[4])))

anova(ir.m3,ir.m5)
anova(ir.m4,ir.m5) # ir.m5 is best
summary(ir.m5)
plot(residuals(ir.m5, type= "normalized")~fitted(ir.m5))

library(multcomp)

H0s <- rbind(IRmax.avena =        c(1,0,0.5,0),
             IRmax.cynodon =      c(1,1,0.5,0),
             IRmax.growth =      c(1,0.5,1,0),
             IRmax.grazed =      c(1,0.5,0,0)             
           )
## Create object with tests and ask for summary
tH0s <- glht(ir.m5, linfct = H0s)
summary(tH0s)

pred.ir.m5 <- cbind(my.grid<-expand.grid(BiteMass.mg.kgLW=seq(0.2,5.8,0.2),spp=c("avena","cynodon"),expt=c("grazing","growth")),IRmg.min.kgLW=as.numeric(predict(ir.m5,my.grid)))

plot(pred.ir.m5$BiteMass.mg.kgLW[pred.ir.m5$spp=="avena"&pred.ir.m5$expt=="grazing"], pred.ir.m5$IRmg.min.kgLW[pred.ir.m5$spp=="avena"&pred.ir.m5$expt=="grazing"], type="l", lty=2, lwd=4, ylim=c(0,200), xlab="Bite mass (mg per kg LW)", ylab="Intake rate (mg per kg LW per minute", cex.lab=1.7, mgp=c(2.5,1,0))
lines(pred.ir.m5$BiteMass.mg.kgLW[pred.ir.m5$spp=="avena"&pred.ir.m5$expt=="growth"], pred.ir.m5$IRmg.min.kgLW[pred.ir.m5$spp=="avena"&pred.ir.m5$expt=="growth"], lty=2, lwd=1)
lines(pred.ir.m5$BiteMass.mg.kgLW[pred.ir.m5$spp=="cynodon"&pred.ir.m5$expt=="grazing"], pred.ir.m5$IRmg.min.kgLW[pred.ir.m5$spp=="cynodon"&pred.ir.m5$expt=="grazing"], lwd=4)
lines(pred.ir.m5$BiteMass.mg.kgLW[pred.ir.m5$spp=="cynodon"&pred.ir.m5$expt=="growth"], pred.ir.m5$IRmg.min.kgLW[pred.ir.m5$spp=="cynodon"&pred.ir.m5$expt=="growth"], lwd=1)

points(d1.nna$BiteMass.mg.kgLW[d1.nna$spp=="avena"&pred.ir.m5$expt=="grazing"], d1.nna$IRmg.min.kgLW[d1.nna$spp=="avena"&pred.ir.m5$expt=="grazing"], pch=19, cex=1.5)
points(d1.nna$BiteMass.mg.kgLW[d1.nna$spp=="cynodon"&d1.nna$expt=="grazing"], d1.nna$IRmg.min.kgLW[d1.nna$spp=="cynodon"&d1.nna$expt=="grazing"], pch=1, cex=1.5)
points(d1.nna$BiteMass.mg.kgLW[d1.nna$spp=="avena"&d1.nna$expt=="growth"], d1.nna$IRmg.min.kgLW[d1.nna$spp=="avena"&d1.nna$expt=="growth"], pch=17, cex=1.5)
points(d1.nna$BiteMass.mg.kgLW[d1.nna$spp=="cynodon"&d1.nna$expt=="growth"], d1.nna$IRmg.min.kgLW[d1.nna$spp=="cynodon"&d1.nna$expt=="growth"], pch=2, cex=1.5)
