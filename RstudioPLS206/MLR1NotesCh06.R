# MULTIPLE LINEAR REGRESSION ====================================

# Read data file

bf <- read.csv("..//Examples//bodyfatb.dat")

# Calculate MLR

m1 <- lm(y ~ x1 + x2 + x3, data = bf)

summary(m1)

anova(m1)

library(car)

Anova(m1, type = 2)
Anova(m1, type = 3)

# Test each predictor or term one at a time.

drop1(m1, scope = ~ x1 + x2 + x3)

# Change order of predictors

m2 <- lm(y ~ x3 + x2 + x1, data = bf)

anova(m2)
Anova(m2, type = 3) # order does not affect type 3 SS
Anova(m1, type = 3)

# Confidence intervals for parameters

confint(m1)

# Standardized estimated partial regression coefficients

m3 <- lm(scale(y) ~ scale(x1) + scale(x2) + scale(x3), data=bf)

summary(m3)

# Relative importance of predictors

library(relaimpo)

# Variance inflation factor

vif(m1)

# Predicted Residual Sum of Squares

library(MPV)

PRESS(m1)

deviance(m1)

ls()

m4 <- lm(y ~ x1 + x2, data=bf)

deviance(m4)
PRESS(m4)
Anova(m4, type=3)

m5 <- lm(y ~ x2, data=bf)

deviance(m5)
PRESS(m5)

# Durbin-Watson test for autocorrelation of residuals.
# This only makes sense if the observations are in order of time
# or space, and more or less with constant intervals or distances
# between adjacent observations.
durbinWatsonTest(m5)
dwt(m5)

# Partial regression and Added-variable plots
avPlots(m1)
y.others <- residuals(lm(y ~ x2 + x3, data = bf))
x1.others <- residuals(lm(x1 ~ x2 + x3, data = bf))
par(mfrow=c(2,1))
plot(y.others ~ x1.others)
avPlots(m1, terms = ~.-x2 - x3)