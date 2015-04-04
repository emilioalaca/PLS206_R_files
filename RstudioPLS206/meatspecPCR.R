library(faraway)
data(meatspec) # load meat spectrum data
model1 <- lm(fat ~ ., meatspec[1:172,]) # Model with all 100 V's as predictors.
summary(model1)$r.squared
rmse <- function(x,y) sqrt(mean((x-y)^2)) #make a function to calculate the square root of the mean squared error
rmse(model1$fit,meatspec$fat[1:172]) # Check the square root of the MSE for the training data. Differs from the regular MSE because the df for the rmse is n instead of df.residual
rmse(predict(model1,meatspec[173:215,]),meatspec$fat[173:215]) # Calculate the sqrt of the MSPR using the data held out and the prediction based on the model derived with the training data. Note the large difference between training and validation sets.
model2 <- step(model1, trace=0) # Perform automated stepwise regression using the AIC. trace=0 prevents verbose output. We would not want to do all subsets because there would be a total of 2^100 models. Just to list them you would need about 2^70 modern computers with a terabyte disk each.
rmse(model2$fit,meatspec$fat[1:172]) # Check rmse for selected model.
rmse(predict(model2,meatspec[173:215,]),meatspec$fat[173:215]) # Calculate rmse for selected model with validation data held out.
library(mva) # No longer needed. prcomp is part of stats.
meatpca <- prcomp(meatspec[1:172,-101]) # Do PCA on spectra of meat samples. Note that fat is left out. Based on covariance matrix.
round(meatpca$sdev,3) # sqrt of eigenvalues. Note how quickly they decline.
meatpca2<- prcomp(meatspec[1:172,-101], scale.=T) # PCA based on correlation matrix.
sum(meatpca$sdev^2) # Since they were not based on correlation they the sum is not = nvars.
sum(meatpca2$sdev^2)
matplot(1:100,meatpca2$rot[,1:3],type="l",xlab="Light frequency",ylab="") # Plot the first 3 eigenvectors from the matrix of eigenvectors meatpca$rotation
model3 <- lm(fat ~ meatpca$x[,1:4]  , meatspec[1:172,]) # PCR on the first 4 PC's unscaled
model4 <- lm(fat ~ meatpca2$x[,1:4]  , meatspec[1:172,]) # PCR on the first 4 PC's scaled
rmse(model3$fit,meatspec$fat[1:172]) # Check sqrt MSE for model with 4 PC's unscaled
rmse(model4$fit,meatspec$fat[1:172]) # Check sqrt MSE for model with 4 PC's scaled
plot(model1$coef[-1],ylab="Coefficient") # Coefficients of the 100 V's in model 1
svb <- meatpca$rot[,1:4] %*% model3$coef[-1] # Calculate coefficients of V's based on PCR
plot(svb,ylab="Coefficient") #Coefficients of the 100 V's in model 3 based on 4 PC's
plot(meatpca$sdev[1:10],type="l",ylab="SD of PC",xlab="PC number") # Scree plot

# Selecting the best set of PC to obtain a model with the lowest MSPR.

mm <- apply(meatspec[1:172,-101],2,mean) # Obtain a vector mm with the means for the 100 V's in the first 172 observations
tx <- as.matrix(sweep(meatspec[173:215,-101],2,mm)) # Center the observations held out with the means for the training data set (for consistency with the estimated parameters)
nx <- tx %*%  meatpca$rot[,1:4] # Obtain the PC scores for the observations held out
pv <- cbind(1,nx) %*% model3$coef # Make predictions for held-oud data based on model 3
rmse(pv,meatspec$fat[173:215]) # Calculate the sqrt MSPR for model 3
rmsmeat <- numeric(50) # Make a container vector for all the MSPR's for the 50 PC's

# Calculate and save the sqrt MSPR for models using from 1 to 50 PC's (unscaled)
for(i in 1:50){
nx <- tx %*%  meatpca$rot[,1:i]
m <- lm(fat ~ meatpca$x[,1:i]  , meatspec[1:172,])
pv <- cbind(1,nx) %*% m$coef
rmsmeat[i] <- rmse(pv,meatspec$fat[173:215])
}
plot(rmsmeat,ylab="sqrt(MSPR)",xlab="No. of Components")
which.min(rmsmeat)
min(rmsmeat)


# Calculate and save the sqrt MSPR for models using from 1 to 50 PC's (scaled)
sdVs<-apply(meatspec[1:172,-101],2,sd) # Get standard deviations of columns
tx2<-as.matrix(sweep(tx,2,sdVs,FUN="/"))
rmsmeat2 <- numeric(50) # Make a container vector for all the MSPR's for the 50 PC's based on the correlation (scaled)
for(i in 1:50){
nx <- tx2 %*%  meatpca2$rot[,1:i]
m <- lm(fat ~ meatpca2$x[,1:i]  , meatspec[1:172,])
pv <- cbind(1,nx) %*% m$coef
rmsmeat2[i] <- rmse(pv,meatspec$fat[173:215])
}
plot(rmsmeat2,ylab="sqrt(MSPR)",xlab="No. of Components", ylim=c(1.7,3))
which.min(rmsmeat2)
min(rmsmeat2)

m21<-lm(fat~meatpca2$x[,1:21], meatspec[1:172,])
nx <- tx2 %*%  meatpca2$rot[,1:21]
pv21<-cbind(1,nx) %*% m21$coef

# Essentially the same, but instead of reserving the same data for validation, using k-fold cross-validation to find a good model for predicting the fat content.
library(pls)
pcrmod <- pcr(fat ~ ., data=meatspec[1:172,], validation="CV",ncomp=100)
validationplot(pcrmod)
plsg <- plsr(fat ~ ., data=meatspec[1:172,], ncomp=50, validation="CV")
coefplot(plsg,ncomp=4,xlab="Frequency")
validationplot(plsg)
ypred <- predict(plsg,ncomp=14)
rmse(ypred,meatspec$fat[1:172])
ytpred <- predict(plsg,meatspec[173:215,],ncomp=14)
rmse(ytpred,meatspec$fat[173:215])
