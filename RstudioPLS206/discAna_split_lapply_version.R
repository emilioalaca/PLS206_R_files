# Discriminant analysis using MASS
# Loosely based on http://www.statmethods.net/
# advstats/discriminant.html by  Robert I. Kabacoff

# Load required packages
library(mvoutlier)
library(car)
library(MASS)
library(rgl)
library(klaR)
library(rpart)

# Load data
data(iris)
my.iris <- iris
names(my.iris) <- c("sl", "sw", "pl", "pw", "spp")

# Check assumptions and determine if variance matrices are different.
# first, get residuals
iris.manova <- manova(cbind(sl, sw, pl, pw) ~ spp, my.iris)
mvres <- as.data.frame(residuals(iris.manova))

names(mvres) <- c("esl", "esw", "epl", "epw")

# only one using Bonferroni correction. LIB.
outl <- aq.plot(mvres, delta = qchisq(1 - 0.05 / 150, df = 4)) 

# Visualize multivariate distribution
my.iris <- cbind(my.iris, mvres)
colors <- c("blue", "green", "orange")
my.iris$color <- factor(my.iris$spp, labels = colors)

# Plot residuals
# The following chiunk can be repeated for all combinations of 
# the 4 characteristics of the flowers.
open3d()
with(my.iris, plot3d(esl, esw, epl, type = "s", col = color, size = 1))

# Different way to do computations - split and lapply
iris.by.spp <- split(my.iris, my.iris$spp)

# calculate covariances for ellipses
iris.cov <- lapply(iris.by.spp, function(x) cov(x[,6:8]))
 
# calculate centroids for ellipses
iris.means <- lapply(iris.by.spp, function(x) colMeans(x[,1:3]))

# add ellipses to residuals
sapply(seq_along(iris.cov), function(i) plot3d(ellipse3d(iris.cov[[i]]), 
       col = colors[i], alpha = 0.4, add = TRUE))

# plot original data
with(my.iris, plot3d(sl, sw, pl, type = "s", col = color, size = 1))

# add ellipses to plot
sapply(seq_along(iris.cov), function(i) 
  plot3d(ellipse3d(iris.cov[[i]], centre = iris.means[[i]]),
         col = colors[i], alpha = 0.4, add = TRUE))

# (repeat for other combinations of 3 out of 4 residuals)
# Even without repeating, there is enough evidence that the 
# variance covariance matrices are not homogeneous. 
# Use quadratic discriminant functions.
qdiris <- qda(x = my.iris[,1:4], 
              grouping = my.iris$spp,
              prior = c(1, 1, 1) / 3)

str(qdiris)
qdiris

# Partition plots
partimat(spp ~ sl + sw + pl + pw, data = my.iris, method = "qda")

# method corrects for unknown parameters of the posterior distribution.
qd.pred <- predict(qdiris, newdata = my.iris[,1:4], method = "debiased") 

# shows results of classification of the training data.
xtabs(~ my.iris$spp + qd.pred$class) 

# Random Forest Classifiers
iris.part <- rpart(spp ~ ., my.iris)
iris_part_pred <- predict(iris.part, my.iris, type = "class")
table(iris_part_pred, my.iris$spp)

# Using Prior Info
qd.pred2 <- predict(qdiris, newdata = my.iris[,1:4],
                    method = "debiased", prior = c(0.1,0.00,0.9))
xtabs(~ my.iris$spp + qd.pred2$class)

# Discriminant analysis with candisc package
library(candisc)
mvmod <- lm(cbind(sl, sw, pl, pw) ~ spp, my.iris)
summary(mvmod)
mvmod
str(mvmod)
cd.mvmod <- candisc(mvmod)

# biplot ob observations and canonical variates
plot(cd.mvmod) 

# extract canonical variables from candisc object
can1 <- cd.mvmod$scores$Can1 
can2 <- cd.mvmod$scores$Can2

my.iris <- cbind(my.iris, can1, can2)

summary(lm(can1 ~ spp, my.iris))
summary(lm(can2 ~ spp, my.iris))

library(heplots)

