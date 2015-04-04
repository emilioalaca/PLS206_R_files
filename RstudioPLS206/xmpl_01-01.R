# Example 01-01

# Simulate data
n <- 50
x <- 1:n # values for time
y.s <- 150 + 80 * sin(x/3.5) + x # structural part of relationship, in this case it is known.
r.error <- rnorm(length(x), mean = 0, sd = sqrt(2500)) # random component, truly iid normal.
y <- y.s + r.error # simulated observed values.

# Plot the observations and then the true relationship.
plot(y ~ x, pch=19, cex=1.5)
lines(y.s ~ x)

# Use a linear regression as a first guess

my.lr <- lm(y ~ x)
summary(my.lr)
coef(my.lr)
str(coef(my.lr))
abline(coef(my.lr)["(Intercept)"],coef(my.lr)["x"]) # Add the prediction line to the plot

# How do we know if the model is correct? We don't. But we can find out if it is incorrect.

my.residuals <- residuals(my.lr)

plot(my.residuals[2:n],my.residuals[1:(n-1)])
cor(my.residuals[2:n],my.residuals[1:(n-1)])
