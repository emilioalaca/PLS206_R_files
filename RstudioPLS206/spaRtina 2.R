# read the data into R using the .csv file with headers
sp<-read.csv("spartina.csv", header=T)
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
plot(apm$cp) # plot the Cp agains the row index -note groups of 3
#
apm$which # print the list of variables in each model
#
coef(best3, 1:40) # extract the model coefficients for all 40
#
coef(apm,c(7,10)) # or for just models 7 and 10
#
# look for additional methods for resubsets and associated objects