# Discriminant analysis using MASS
#  Loosely based on http://www.statmethods.net/advstats/discriminant.html by  Robert I. Kabacoff
library(MASS)
my.seeds <- read.csv("../Examples/seedDA.txt", header=TRUE)
# Check assumptions and determine if variance matrices are different.
library(mvoutlier)
library(car)

tseeds <- my.seeds[my.seeds$hold.out==0,]# first, get residuals
mvres <- as.data.frame(residuals(manova(cbind(area,perimeter,length,width,area.per,len.wid) ~ spp, tseeds)))
names(mvres) <- c("area","perimeter","length","width","area.per","len.wid")
# outl <- aq.plot(mvres, delta=qchisq(1-0.05/150, df=4)) # only one using Bonferroni correction. LIB.

# Visualize multivariate distribution
tseeds <- cbind(tseeds, mvres)
tseeds$color <- "orange"
tseeds$color[tseeds$spp=="brho"] <- "blue"
tseeds$color[tseeds$spp=="brma"] <- "green"
library(rgl)
open3d()
# The following chunk can be repeated for all combinations of the 4 characteristics of the flowers.

names(tseeds)[9:14] <- paste(names(tseeds)[9:14],".res",sep="")
plot3d(tseeds$area.per.res,tseeds$length.res,tseeds$width.res, type="s", col=tseeds$color, size=1) # plot residuals

cov901brho <- cov(tseeds[tseeds$spp=="brho",9:11]) # calculate covariances for ellipses
cov678virginica <- cov(tseeds[tseeds$spp=="virginica",6:8])
cov678versicolor <- cov(tseeds[tseeds$spp=="versicolor",6:8])

mean901brho <- apply(tseeds[tseeds$spp=="brho",2:4],2,mean) # calculate centroids for ellipses
mean678virginica <- apply(tseeds[tseeds$spp=="virginica",1:3],2,mean)
mean678versicolor <- apply(tseeds[tseeds$spp=="versicolor",1:3],2,mean)

plot3d(ellipse3d(cov901brho), col="blue",alpha=0.4, add=TRUE) # add ellipses to residuals
plot3d(ellipse3d(cov678virginica), col="green",alpha=0.4, add=TRUE)
plot3d(ellipse3d(cov678versicolor), col="orange",alpha=0.4, add=TRUE)

plot3d(tseeds$sl,tseeds$sw,tseeds$pl, type="s", col=tseeds$color, size=1) # plot original data
plot3d(ellipse3d(cov678setosa, centre=mean678setosa), col="blue",alpha=0.4, add=TRUE) # add ellipses to plot
plot3d(ellipse3d(cov678virginica, centre=mean678virginica), col="green",alpha=0.4, add=TRUE)
plot3d(ellipse3d(cov678versicolor, centre=mean678versicolor), col="orange",alpha=0.4, add=TRUE)


# (repeat for other combinations of 3 out of 4 residuals)
# Even without repeating, there is enough evidence that the variance covariance matrices are not homogeneous. Use quadratic discriminant functions.

qdseeds <- qda(x=tseeds[,2:7], grouping=tseeds$spp, prior=c(1,1,1)/3)
str(qdseeds)
summary(qdseeds)
library(klaR)
partimat(spp ~ sl + sw + pl + pw, data=my.seeds, method="qda")
qd.pred <- predict(qdseeds, newdata=my.seeds[,1:4], method="debiased") # method corrects for unknown parameters of the posterior distribution.
xtabs(~my.seeds$spp+qd.pred$class) # shows results of classification of the training data.
qd.pred2 <- predict(qdseeds, newdata=my.seeds[,1:4], method="debiased", prior=c(0.1,0.00,0.9))
xtabs(~my.seeds$spp+qd.pred2$class)

# Discriminant analysis with candisc package
library(candisc)
mvmod <- lm(cbind(sl,sw,pl,pw) ~ spp, my.seeds)
summary(mvmod)
mvmod
str(mvmod)
cd.mvmod <- candisc(mvmod)
plot(cd.mvmod) # biplot ob observations and canonical variates

can1 <- cd.mvmod$scores$Can1 # extract canonical variables from candisc object
can2 <- cd.mvmod$scores$Can2

my.seeds <- cbind(my.seeds, can1, can2)

summary(lm(can1 ~ spp, my.seeds))
summary(lm(can2 ~ spp, my.seeds))

library(heplots)