# Discriminant analysis using MASS
#  Loosely based on http://www.statmethods.net/advstats/discriminant.html by  Robert I. Kabacoff
library(MASS)
my.iris <- read.csv("../Examples/iris.txt", header=TRUE)
# Check assumptions and determine if variance matrices are different.
library(mvoutlier)
library(car)
# first, get residuals
mvres <- as.data.frame(residuals(manova(cbind(sl,sw,pl,pw) ~ spp, my.iris)))
names(mvres) <- c("esl","esw","epl","epw")
outl <- aq.plot(mvres, delta=qchisq(1-0.05/150, df=4)) # only one using Bonferroni correction. LIB.

# Visualize multivariate distribution
my.iris <- cbind(my.iris, mvres)
my.iris$color <- "orange"
my.iris$color[my.iris$spp=="setosa"] <- "blue"
my.iris$color[my.iris$spp=="virginica"] <- "green"
library(rgl)
open3d()
# The following chiunk can be repeated for all combinations of the 4 characteristics of the flowers.
plot3d(my.iris$esl,my.iris$esw,my.iris$epl, type="s", col=my.iris$color, size=1) # plot residuals

cov678setosa <- cov(my.iris[my.iris$spp=="setosa",6:8]) # calculate covariances for ellipses
cov678virginica <- cov(my.iris[my.iris$spp=="virginica",6:8])
cov678versicolor <- cov(my.iris[my.iris$spp=="versicolor",6:8])

mean678setosa <- apply(my.iris[my.iris$spp=="setosa",1:3],2,mean) # calculate centroids for ellipses
mean678virginica <- apply(my.iris[my.iris$spp=="virginica",1:3],2,mean)
mean678versicolor <- apply(my.iris[my.iris$spp=="versicolor",1:3],2,mean)

plot3d(ellipse3d(cov678setosa), col="blue",alpha=0.4, add=TRUE) # add ellipses to residuals
plot3d(ellipse3d(cov678virginica), col="green",alpha=0.4, add=TRUE)
plot3d(ellipse3d(cov678versicolor), col="orange",alpha=0.4, add=TRUE)

plot3d(my.iris$sl,my.iris$sw,my.iris$pl, type="s", col=my.iris$color, size=1) # plot original data
plot3d(ellipse3d(cov678setosa, centre=mean678setosa), col="blue",alpha=0.4, add=TRUE) # add ellipses to plot
plot3d(ellipse3d(cov678virginica, centre=mean678virginica), col="green",alpha=0.4, add=TRUE)
plot3d(ellipse3d(cov678versicolor, centre=mean678versicolor), col="orange",alpha=0.4, add=TRUE)


# (repeat for other combinations of 3 out of 4 residuals)
# Even without repeating, there is enough evidence that the variance covariance matrices are not homogeneous. Use quadratic discriminant functions.

qdiris <- qda(x=my.iris[,1:4], grouping=my.iris$spp, prior=c(1,1,1)/3)
str(qdiris)

summary(qdiris)
library(klaR)
partimat(spp ~ sl + sw + pl + pw, data=my.iris, method="qda")
qd.pred <- predict(qdiris, newdata=my.iris[,1:4], method="debiased") # method corrects for unknown parameters of the posterior distribution.
xtabs(~my.iris$spp+qd.pred$class) # shows results of classification of the training data.
qd.pred2 <- predict(qdiris, newdata=my.iris[,1:4], method="debiased", prior=c(0.1,0.00,0.9))
xtabs(~my.iris$spp+qd.pred2$class)

# Canonical variates for quadratic DA are not uniquely defined. There are various approaches, including SAVE sliced average variance estimation. For this exercise we simply use the lda canonical variates.


# Discriminant analysis with candisc package
library(candisc)
mvmod <- lm(cbind(sl,sw,pl,pw) ~ spp, my.iris)
summary(mvmod)
mvmod
str(mvmod)
cd.mvmod <- candisc(mvmod)
plot(cd.mvmod) # biplot ob observations and canonical variates

can1 <- cd.mvmod$scores$Can1 # extract canonical variables from candisc object
can2 <- cd.mvmod$scores$Can2

my.iris <- cbind(my.iris, can1, can2)

summary(lm(can1 ~ spp, my.iris))
summary(lm(can2 ~ spp, my.iris))

library(heplots)