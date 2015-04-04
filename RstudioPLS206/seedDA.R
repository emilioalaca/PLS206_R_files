library(MASS)
seeds <- read.csv("../Examples/seedDA.txt", header=TRUE)
# Determine if variance matrices are different.
# first, get residuals
mvres <- as.data.frame(residuals(manova(cbind(spp,area,perimeter,length,width,area.per,len.wid) ~ spp, seeds)))
names(mvres) <- paste(names(mvres),".r", sep="")
seeds <- cbind(seeds, mvres)

# Visualize multivariate distribution

seeds$color <- "orange"
seeds$color[seeds$spp=="setosa"] <- "blue"
seeds$color[seeds$spp=="virginica"] <- "green"
library(rgl)
open3d()
# The following chiunk can be repeated for all combinations of the 4 characteristics of the flowers.
plot3d(seeds$esl,seeds$esw,seeds$epl, type="s", col=seeds$color, size=1) # plot residuals

cov678setosa <- cov(seeds[seeds$spp=="setosa",6:8]) # calculate covariances for ellipses
cov678virginica <- cov(seeds[seeds$spp=="virginica",6:8])
cov678versicolor <- cov(seeds[seeds$spp=="versicolor",6:8])

mean678setosa <- apply(seeds[seeds$spp=="setosa",1:3],2,mean) # calculate centroids for ellipses
mean678virginica <- apply(seeds[seeds$spp=="virginica",1:3],2,mean)
mean678versicolor <- apply(seeds[seeds$spp=="versicolor",1:3],2,mean)

plot3d(ellipse3d(cov678setosa), col="blue",alpha=0.4, add=TRUE) # add ellipses to residuals
plot3d(ellipse3d(cov678virginica), col="green",alpha=0.4, add=TRUE)
plot3d(ellipse3d(cov678versicolor), col="orange",alpha=0.4, add=TRUE)

plot3d(seeds$sl,seeds$sw,seeds$pl, type="s", col=seeds$color, size=1) # plot original data
plot3d(ellipse3d(cov678setosa, centre=mean678setosa), col="blue",alpha=0.4, add=TRUE) # add ellipses to plot
plot3d(ellipse3d(cov678virginica, centre=mean678virginica), col="green",alpha=0.4, add=TRUE)
plot3d(ellipse3d(cov678versicolor, centre=mean678versicolor), col="orange",alpha=0.4, add=TRUE)


# (repeat for other combinations of 3 out of 4 residuals)
# Even without repeating, there is enough evidence that the variance covariance matrices are not homogeneous. Use quadratic discriminant functions.

qdseeds <- qda(x=seeds[,1:4], grouping=seeds$spp, prior=c(1,1,1)/3)
str(qdseeds)

summary(qdseeds)
library(klaR)
partimat(spp ~ sl + sw + pl + pw, data=seeds, method="qda")
qd.pred <- predict(qdseeds, newdata=seeds[,1:4], method="debiased") # method corrects for unknown parameters of the posterior distribution.
xtabs(~seeds$spp+qd.pred$class) # shows results of classification of the training data.
qd.pred2 <- predict(qdseeds, newdata=seeds[,1:4], method="debiased", prior=c(0.1,0.00,0.9))
xtabs(~seeds$spp+qd.pred2$class)

# Canonical variates for quadratic DA are not uniquely defined. There are various approaches, including SAVE sliced average variance estimation. For this exercise we simply use the lda canonical variates.


# Discriminant analysis with candisc package
library(candisc)
mvmod <- lm(cbind(spp,area,perimeter,length,width,area.per,len.wid) ~ spp, seeds)
summary(mvmod)
mvmod
str(mvmod)
cd.mvmod <- candisc(mvmod)
plot(cd.mvmod) # biplot ob observations and canonical variates

can1 <- cd.mvmod$scores$Can1 # extract canonical variables from candisc object
can2 <- cd.mvmod$scores$Can2

seeds <- cbind(seeds, can1, can2)

summary(lm(can1 ~ spp, seeds))
summary(lm(can2 ~ spp, seeds))

library(heplots)