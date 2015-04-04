`library(faraway)
data(meatspec) # load meat spectrum data
model1 <- lm(fat ~ ., meatspec[1:172,]) # Model with all 100 V's as predictors.
summary(model1)$r.squared
rmse <- function(x,y) sqrt(mean((x-y)^2)) #make a function to calculate the square root of the mean squared error
rmse(model1$fit,meatspec$fat[1:172]) # Check the square root of the MSE for the training data. Differs from the regular MSE because the df for the rmse is n instead of df.residual
rmse(predict(model1,meatspec[173:215,]),meatspec$fat[173:215]) # Calculate the sqrt of the MSPR using the data held out and the prediction based on the model derived with the training data. Note the large difference between training and validation sets.

# Essentially the same, but instead of reserving the same data for validation, using k-fold cross-validation to find a good model for predicting the fat content.

library(pls)
# principal component regression
pcrmod <- pcr(fat ~ ., data=meatspec[1:172,], validation="CV",ncomp=40)
validationplot(pcrmod) # notice how the validation error RMSEP goes up when the third component is added. This method allows to select the best number of components by RMSEP, which is best when the model is mainly for prediction.
validationplot(plsg, ylim=c(0,4))
coefplot(pcrmod,ncomp=1:5)
plot(coef(lm(fat~.,meatspec)))

#additional examples, not necessarily the answer to any specific homework question

plsg <- plsr(fat ~ ., data=meatspec[1:172,], ncomp=100, validation="CV")
coefplot(plsg,ncomp=1,xlab="Frequency")
validationplot(plsg, ylim=c(0,4))
plot(RMSEP(plsg)$val[1,1,], type="l",ylim=c(0,4))


ypred <- predict(plsg,ncomp=14)
rmse(ypred,meatspec$fat[1:172])
ytpred <- predict(plsg,meatspec[173:215,],ncomp=14)
rmse(ytpred,meatspec$fat[173:215])
plot(pcrmod$validation$PRESS)