# MODEL DEVELOPMENT Spartina dataset
# read the data into R using the .csv file with headers
sp<-read.csv("../Examples/spartina.csv", header=T)
#
# load the leaps package to do regression subsets
library(leaps)
#
# In one line, run all possible subsets with up to 14 predictors
# recording the best 3 models for each number of predictors, and
# create the summary object "apm" that contains the information
# necessary to compare the models and pick one.
apm<-summary(best3<-regsubsets(bmss~sal+pH+K+Na+Zn+H2S+Eh7+acid+P+Ca+Mg+Mn+Cu+NH4, data=sp, nbest=3, nvmax=14, method=c("exhaustive")))
#
plot(apm$cp~1+as.numeric(rownames(apm$which)), xlab="p+1", ylab="Mallow's Cp") # plot the Cp agains the row index -note groups of 3
plot(apm$cp~1+as.numeric(rownames(apm$which)), ylim=c(1.5,15)) # expand to see individual points
cbind(1+as.numeric(rownames(apm$which)),apm$cp) # compare Cp and p+1 by looking at numbers
plot(apm$adjr2~1+as.numeric(rownames(apm$which)), xlab="p+1", ylab="Adjusted R-sq") # plot the Cp agains the row index -note groups of 3

apm$which # print the list of variables in each model
#
coef(best3, 1:40) # extract the model coefficients for all 40
#
coef(best3,c(7,10)) # or for just models 7 and 10
#
# look for additional methods for resubsets and associated objects

help(regsubsets)


apm$which[which.min(apm$bic),] #retrieves the list of variables in the best model according to the BIC criterion

# get the summary for only the best model for each p
apm1<-summary(best1<-regsubsets(bmss~sal+pH+K+Na+Zn+H2S+Eh7+acid+P+Ca+Mg+Mn+Cu+NH4, data=sp, nbest=1, nvmax=14, method=c("exhaustive")))

p<-c(2:15) # create a vector for p = number of parameters

AICc<-apm1$bic - p*log(45) + 2*p + 2*p*(p+1)/(45-p-1) # calculate AICc

plot(p,AICc) # plot the AIC vs. p

coef(best1,which.min(AICc)) # get the coefficients for best model

thebest<-lm(bmss~sal + K + Zn + Eh7 + Cu + NH4, data=sp) # make best model

summary(thebest) # inspect results

xvals<-as.data.frame(t(colMeans(sp[,c(4:17)]))) # calculate average predictor values, transpose and put into data frame for prediction

predict(thebest, xvals, interval="prediction", level=0.90, se=T) # make prediction for individual observation with alpha=0.90

predict(thebest, xvals, interval="confidence", level=0.90, se=T) # make prediction for mean bmss with alpha=0.90

full<-lm(bmss~sal+pH+K+Na+Zn+H2S+Eh7+acid+P+Ca+Mg+Mn+Cu+NH4, data=sp) # make full model

predict(full, xvals, interval="prediction", level=0.90, se=T) # make prediction for individual observation with alpha=0.90

predict(full, xvals, interval="confidence", level=0.90, se=T) # make prediction for mean bmss with alpha=0.90

plot(thebest) # plot residuals for best model

########### Diagnostics #################### (from http://www.statmethods.net/stats/rdiagnostics.html)

library(car)
library(MASS)

# Normality of Residuals
# qq plot for studentized resid
qqPlot(thebest, main="QQ Plot")
# distribution of studentized residuals
sresid <- studres(thebest)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit) 

# Assessing Outliers
outlierTest(thebest) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid
leveragePlots(thebest) # leverage plots 

# Influential Observations
# added variable plots
avPlots(thebest)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(mtcars)-length(thebest$coefficients)-2))
plot(thebest, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(thebest, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )


outlierTest(thebest) # Bonferonni p-value for most extreme obs
qqPlot(thebest, main="QQ Plot") #qq plot for studentized resid
leveragePlots(thebest) # leverage plots 
# Influential Observations
# added variable plots
av.Plots(thebest)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(spartina)-length(thebest$coefficients)-2))
plot(thebest, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(thebest, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# Evaluate Collinearity
vif(thebest) # variance inflation factors
vif(thebest) > 10 # problem?


