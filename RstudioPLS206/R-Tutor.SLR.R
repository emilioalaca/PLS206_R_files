data(faithful)

eruption.lm = lm(eruptions ~ waiting, data=faithful)

# Then we extract the parameters of the estimated regression equation with the coefficients function.

coeffs = coefficients(eruption.lm); coeffs 

# We now fit the eruption duration using the estimated regression equation.

waiting = 80           # the waiting time 
duration = coeffs[1] + coeffs[2]*waiting 
duration 

# Answer
#Based on the simple linear regression model, if the waiting time since the last eruption has been 80 minutes, we expect the next one to last 4.1762 minutes.

# Alternative Solution
# We wrap the waiting parameter value inside a new data frame named newdata.

newdata = data.frame(waiting=80) # wrap the parameter

# Then we apply the predict function to eruption.lm along with newdata.

predict(eruption.lm, newdata)    # apply predict 

# Then we extract the coefficient of determination from the r.squared attribute of its summary.

summary(eruption.lm)$r.squared 

str(summary(eruption.lm))

# Then we print out the F-statistics of the significance test with the summary function.

summary(eruption.lm) 

anova(eruption.lm)

# Then we create a new data frame that set the waiting time value.

newdata = data.frame(waiting=80)

# CI for EXPECTED duration. Apply the predict function and set the predictor variable in the newdata argument. We also set the interval type as "confidence", and use the default 0.95 confidence level.

predict(eruption.lm, newdata, interval="confidence") 

# CI for an individual duration. Apply the predict function and set the predictor variable in the newdata argument. We also set the interval type as "confidence", and use the default 0.95 confidence level.

predict(eruption.lm, newdata, interval="predict")

# Residual plots

eruption.res = resid(eruption.lm)

plot(faithful$waiting, eruption.res, 
            ylab="Residuals", xlab="Waiting Time", 
            main="Old Faithful Eruptions")

abline(0, 0)                  # reference line

# Standardized residuals

eruption.stdres = rstandard(eruption.lm)

my.std.res <- eruption.res / (sqrt(deviance(eruption.lm) / df.residual(eruption.lm)))

# Normal quantile plot

qqnorm(eruption.stdres, 
            ylab="Standardized Residuals", 
            xlab="Normal Scores", 
            main="Old Faithful Eruptions") 

qqline(eruption.stdres)
