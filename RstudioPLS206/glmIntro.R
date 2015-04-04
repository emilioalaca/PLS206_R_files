# Reverse engineering: from models to data
# Fictitious example: imagine that mammal species richness in a ha of land has a Poisson distribution whose mean increases as a function of distance to populated centers.

x <- runif(40,1,100) # generate distances randomly or haphazardly
x
B0 <- 1.9
B1 <- 0.013

mu <- exp(B0 + B1*x)
round(mu,1)
rpois(length(mu),mu)
(y <- apply(as.matrix(mu),1, function(x) rpois(1,x)))

# y is the observed number of species in each plot.
 # Now we fit the model

glm1 <- glm(y ~ x, family = poisson(link = "log"))
summary(glm1)

plot(glm1) # plots have different interpretation than for linear models

# Predictions

xnew <- data.frame(x=1:100)
my.preds <- predict(glm1, newdata=xnew, type="link", se=TRUE)
str(my.preds)
# predictions are in log scale. This is the scale where errors are asymptotically normal

upper <- exp(my.preds$fit + 2 * my.preds$se.fit)
lower <- exp(my.preds$fit - 2 * my.preds$se.fit)
plot(y~x)
lines(xnew$x,upper, lty=2)
lines(xnew$x,lower, lty=2)
lines(xnew$x,exp(my.preds$fit), lty=1)


# A real example
library(faraway)
data(gala)
