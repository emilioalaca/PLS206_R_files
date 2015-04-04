
library(MASS)
library(car)
library(Matrix)
library(rgl)

my.mat01 <- matrix(c(1.00,0.85,0.09,
                     0.85,1.00,0.60,
                     0.09,0.60,1.00), nrow=3, byrow=TRUE)

my.sigma <- as.matrix(nearPD(my.mat01,keepDiag=TRUE)$mat)


xyz <- as.data.frame(mvrnorm(n = 30, mu = c(0,0,0), Sigma = my.sigma))
str(xyz)
names(xyz) <- c("x","y","z")
open3d()
plot3d(xyz$x,xyz$y,xyz$z, type="s", col="green", size=1)
plot3d(ellipse3d(my.sigma), col="blue", alpha=0.4)
