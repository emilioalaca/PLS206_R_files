# Firts, it is necessary to load the boot package.
library(boot)
# Second, we read the data from a comma-separated-value file
# that has the variable names in the first row.
notodat<-read.csv("/Users/ealaca/Documents/AGR206/PLS206F10/Examples/Notonecta.csv", header=TRUE)
# Next, we create the function that returns the estimated
# coefficients for the nonlinear model for each bootstrap sample.
gammas<-function(data,i)
{
bootsample<-data[i,]
noto.i<-nls(Ne ~ a*No/(1 + a*No*Th), data=bootsample, start= list(a=0.1, Th=0.01))
coef(noto.i)
}
# Notice that we do not use a Self Starting nonlinear function, so we need to
# pass initial values for each parameter. Self-starting functions are available
# for some models, including the Michaelis-Menten equation. These functions have
# internal programming to figure out good starting values from the data.
# At the next step we use the boot function to create 2000 bootstrap samples
# and the corresponding estimated parameters.
noto.boot<-boot(notodat, gammas,R=2000)
# Use the boot.ci function to obtain the confidence intervals
# first for a
boot.ci(noto.boot, index=1)
# and second for Th
boot.ci(noto.boot, index=2)
# Now we make the estimates based on the "large-sample"
# asymptotic properties.
confint(noto.nls<-nls(Ne ~ a*No/(1 + a*No*Th),
 data=notodat, start= list(a=0.1, Th=0.01)))
# Notice that the "original" CI based on the assumptions of "large-sample"
# for a is (0.34, 0.75) whereas the bootstrap CI is narrower (0.37, 0.69).
# Both methods yield similar CI's for Th.
plot(density(noto.boot$t[,1]),xlab="estimated rate of successful search")
abline(v=mean(noto.boot$t[,1]))
abline(v=coef(noto.nls)[1], lty=2)
abline(v=c(0.33642092, 0.75444568), lty=2)
abline(v=c(0.3666,  0.6907), lty=1)
plot(density(noto.boot$t[,2]),xlab="handling time")
abline(v=mean(noto.boot$t[,2]))
abline(v=coef(noto.nls)[2], lty=2)
abline(v=c(0.02529264, 0.05437306), lty=2)
abline(v=c(0.0250,  0.0582), lty=1)
#
# The BCa CI for Th based on the bootstrap is wider than the one based on the
# asymptotic properties. This is relaed to the fact that the data show 
# heterogeneity of variance, with more variance in the range that contains
# information about Th and less variance in the range of low prey density,
# where the data has information about a.
 