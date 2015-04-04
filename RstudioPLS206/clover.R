clover<-read.csv("..//Examples//clover.csv", header=TRUE)

clover

# Change options for contrasts so treatment effects
# are calculated as differences from the overall mean
# instead of the default for R, by which treatment
# effects are calculated as differences from the
# first treatment.

options(contrasts =c("contr.sum", "contr.poly"))

# Make sure you have the correct names for the columns
# of the data

names(clover)
names(clover) <- c("group", "days", "lnwt")


# Transform the variable group into a factor

class(clover$group)

clover$group<-factor(clover$group)

# Specify linear model with all effects and interactions

mymodel<-lm(lnwt~group*days, clover)

# Display the summary of the results

summary(mymodel)

# Display the analysis of variance for sequential
# and partial sum of squares

anova(mymodel)

# Load nlme and car packages to be able to see the different types of SS

library(nlme); library(car)

help(Anova)
help(aov)

Anova(mymodel, type=3)

anova(mymodel, type="marginal")



# Create L vector for B11, the slope for group 1****************************

(L4B11<-c(0,0,0,1,1,0))

# Calculate B11
# using the matrix product of L4B11 and the
# vector of estimated coefficients and effects

(B11<-L4B11%*%mymodel$coef)

L4B11%*%coef(mymodel) # same thing

# so both mymodel$coef and coef(mymodel) retrieve the
# vector of estimated coefficients


# Obtain the MSE

(mymodel.MSE<-deviance(mymodel)/df.residual(mymodel))

# The estimated variance of a linear combination of estimated parameters can
# be calculated in different ways: let B be the vector of estimated parameters,
# a. Vhat{L'B}= MSE*L'*inverse(X'X)*L
# b. Vhat{L'B}= L'*Vhat{B}*L
# AND
# Vhat{B} can be obtained in a couple of different ways:
# 1. Vhat{B} is vcov(mymodel)
# 2. Vhat{B} is MSE*summary(mymodel, cov=TRUE)$cov
#
# summary(mymodel, cov=TRUE)$cov obtains the inverse(X'X), also known
# as the unscaled variance covariance matrix because it only needs
# to be scaled by the variance to become the var-covariance matrix.
# 
# The combination of methods yields different ways to obtain Vhat{L'B}.
# Any of them is correct for the homework.

(vcvB<-vcov(mymodel))  # vcvB is the variance-covariance matrix of B

mymodel.MSE*summary(mymodel)$cov # is the same except for rounding errors.

# X'X-1 or inverse(X'X) matrix for mymodel
# note that you can get X with model.matrix(mymodel)

x.prime.x.inv<-solve(t(model.matrix(mymodel))%*%model.matrix(mymodel))

# Variance of estimated B11 calculated in two different ways

(varB11<-L4B11%*%vcvB%*%L4B11)

mymodel.MSE*L4B11%*%x.prime.x.inv%*%L4B11


# standard error of B11

(stdrB11<-sqrt(varB11))

# t Ratio for B11

(tB11<-B11/stdrB11)

# pvalue for test that B11=0

(ttailB11<-1-pt(tB11,df.residual(mymodel)))

# Create L vector for B12

(L4B12<-c(0,0,0,1,0,1))

# partial regression coefficient B12

(B12<-L4B12%*%mymodel$coef)

# The L for the difference B11-B12 is L4B11-L4B12:

(L4B11.B12<-L4B11-L4B12)

# Difference between B11 and B12 is calculated as:

(B11.B12<-L4B11.B12%*%mymodel$coef)

# Calculate the variance of the difference B11-B12.

(varB11.B12<-L4B11.B12%*%vcvB%*%L4B11.B12)
mymodel.MSE*L4B11.B12%*%x.prime.x.inv%*%L4B11.B12 #same thing

# t Ratio for B11.B12
tB11.B12<-B11.B12/stdrB11.B12

(ttailB11.B12<-1-pt(abs(B11.B12/sqrt(varB11.B12)),df.residual(mymodel)))


# The following section calculates predicted yields.
# In addition to the slopes we need the intercepts.
# Create vector for B01
L4B01<-c(1,1,0,0,0,0)

# Create vector for yhat1.30, the estimates log of plant weight in
# group 1 and at day 30.

(L4yhat1.30<-L4B01+30*L4B11)

# Predicted yield for group 1 at day 30

(yhat1.30<-L4yhat1.30%*%mymodel$coef)

# Create L vector for B13
(L4B13<-c(0,0,0,1,-1,-1))

# Create L vector for B03
(L4B03<-c(1,-1,-1,0,0,0))

#  L vector and then predicted yield for group 3 at day 25

(L4yhat3.25<-L4B03+25*L4B13)

(yhat3.25<-L4yhat3.25%*%mymodel$coef)

# L vector for the difference in log plant weight.

(L4yhat1.30.yhat3.25<-L4yhat1.30-L4yhat3.25)

# Difference between yhat1.30 and yhat3.25

(yhat1.30.yhat3.25<-L4yhat1.30.yhat3.25%*%mymodel$coef)

# variance of yhat1.30.yhat3.25
var.y130y325<-mymodel.MSE*L4yhat1.30.yhat3.25%*%x.prime.x.inv%*%L4yhat1.30.yhat3.25

# p-value for test that yhats are not different:
(ttaily130.y325<-1-pt(yhat1.30.yhat3.25/sqrt(var.y130y325),df.residual(mymodel)))

# Diagnostic plots
# Prepare the output window to contain all graphs

par(mfrow=c(2,2))

plot(mymodel)

qt(0.025,df.residual(mymodel))*stdrB11

# Get the variance covariance of the estimated parameters

vcov(mymodel)

# Calculate the correlation based on the covariance matrix.

cov2cor(vcov(mymodel))


