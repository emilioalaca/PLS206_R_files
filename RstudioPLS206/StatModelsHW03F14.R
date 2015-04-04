#<<Data and full model>>=
clover<-read.csv("~/Documents/clover.csv", header=TRUE)
options(contrasts =c("contr.sum", "contr.poly"))
names(clover) <- c("group", "days", "lnwt")
clover$group<-factor(clover$group)
# Create a continuous variable for temperature
clover$t <- NA
clover$t[clover$group==1] <- 10
clover$t[clover$group==2] <- 15
clover$t[clover$group==3] <- 20

FullModel<-lm(lnwt ~ days + t + days:t + days:I(t^2), clover)

  
#  Now, calculate the components of the test and then obtain the F-value.

#<<F-test by type III SS>>=
  anova(FullModel)
#@
  
#  The type III sum of squares tests each term as if it were entered last, which is equivalent to compare a full and reduced model that is nested in the full model. The result indicates that it is not necessary to include a quadratic term for temperature affecting the slope with respect to days (i.e., the RGR).

#<<F-test by hand>>=
  ReducedModel <- lm(lnwt ~ days + t + days:t, clover)
anova(FullModel, ReducedModel) # same as the test above
SSE.ReducedModel <- deviance(ReducedModel)
SSE.FullModel <- deviance(FullModel)
dfe.ReducedModel <- df.residual(ReducedModel)
dfe.FullModel <- df.residual(FullModel)
MSEfull <- SSE.FullModel/dfe.FullModel
(fstat <- ((SSE.ReducedModel-SSE.FullModel)/(dfe.ReducedModel-dfe.FullModel))/MSEfull)
print("The p value is")
1-pf(fstat,dfe.ReducedModel-dfe.FullModel,dfe.FullModel)
#@
  
#Create a model with only those terms that you think are necessary and obtain estimated parameters. Report the formula, summary, $R^2$, PRESS and diagnostic plots. [20]
#The results above indicate that although the conceptual model required a quadratic effect for temperature, the plants were grown in temperatures that did not result in a reduction of the rate at which RGR increased with increasing temperature. We use the ReducedModel.

summary(ReducedModel)
(PRESS.statistic <- sum( (resid(ReducedModel)/(1-hatvalues(ReducedModel)))^2 ))
library(car) # awesome diagnostic plots
outlierTest(ReducedModel) # Bonferonni p-value for most extreme obs

  

# Diagnostic plots:Report any problems or potential problems. [20]

qqPlot(ReducedModel, main="QQ Plot") #qq plot for studentized resid 


#The quantile plot shows an s-shaped pattern of points, although few points are aoutside the confidence band. This pattern indicates that there are problems with the normality of the data, if the model is correct. It possible the the pattern results from a misspeified model. For that we look at the residual plots. 


residualPlots(ReducedModel)
  
#Residual plots obtained with the \verb!residualPlots()! method of the car package indicate that there is a quadratic response to $days$ that has to be added to the model, as it is now in the residuals. The quadratic response is significant as indicated by the tests in the output table. The t-tests are obtained by adding the squared predictors to the model last. A significant test indicates that there is a significant quadratic or curvilinear tendency.

  influenceIndexPlot(ReducedModel)


#{Perform a test of Lack of Fit} 

#Perform the test and interpret the results. [20]

#For a test of lack-of-fit it is necessary to have "true" or "near replicates." True (near) replicates are observations that have equal (similar) values for all predictors. In this data set we have true replicates in temperature and near replicates in days. The test consists in comparing a fullmodel where each combination of day and temperature gets a parameter, as if both predictors were continuous, vs. the model with continuous predictors. A "cell" is the set of observations that the same or similar values of all predictors.

#First, create the grouping variable for days.


  clover$dayg <- NA
clover$dayg[clover$days<10] <- "A"
clover$dayg[clover$days>10&clover$days<20] <- "B"
clover$dayg[clover$days>20&clover$days<25] <- "C"
clover$dayg[clover$days>25&clover$days<35] <- "D"
clover$dayg[clover$days>35] <- "E"

  
#  Next, make the "fullest" model and compare to the reduced model.

FullestModel <- lm(lnwt ~ group*dayg, clover)
anova(ReducedModel, FullestModel)

  
#There is significant lack of fit, meaning that the reduced model is rejected because the deviations from the mean of each "cell" to the prediction by the reduced model are significantly larger than expected on the basis of the MSE from the fullest model. This is just another result showing that in reality the RGR is not constant as plants age. A quadratic term should be added for days or a better mechanistic model should be constructed.
