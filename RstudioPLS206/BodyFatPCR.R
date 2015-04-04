# PCR Body fat example
# Keep in mind that the data used in this example does not represent a typical case where one would apply PCR. I am using a simple and small data set for the purpose of explanation of the steps.

bfat<-read.csv("~/Google Drive/PLS206F13/Examples/bodyfatb.dat", header=T)
bfat <- cbind(scale(bfat[,1:3]),bfat) # join standardized variables with the original data
names(bfat)[1:3] <- c("z1", "z2", "z3")
str(bfat)
cov(bfat[, 1:3]) # the covariance of standardized variables is the correlation matrix
Z <- as.matrix(bfat[, 1:3])
t(Z) %*% Z/(nrow(Z)-1) #  covariance is Z'Z/(n-1)
cov(Z)
pcaX <- prcomp(bfat[,4:6], scale=T) # perform PCA using the correlation matrix
pcaZ <- prcomp(Z)
str(pcaZ) # the "prcomp object has 5 slots

# pcaX$x has the PC scores
pcaX$x
identical(all.equal(pcaX$x, pcaZ$x), TRUE) # the PC scores are the same; doing PCA using the correlation matrix is teh same as doing PCA using the covariance of the standardized variables.
round(pcaX$rotation - pcaZ$rotation, 4) # the eigenvectors are the same

pcaX$sdev # sqrt(eigenvalues) are the same
pcaZ$sdev # the sdev are the sqrt(eigenvalues) and represent the sqrt(variance) concentrated into each PC. The total is equal to the no. of columns in X and Z.

pcaZ$sdev^2
sum(pcaZ$sdev^2)
apply(pcaZ$x, 2, var) # the eigenvalues are the variances of the PC's

sum(pcaX$sdev^2) # each column of X results in a column in Z and each column in Z has a variance of 1, so the sum of the variances is 3

library(pls)
library(car)

m1<-lm(y  ~z1 + z2 + z3, bfat)
vif(m1) # there is strong collinearity
summary(m1) #Note that R-sq=0.8014 etc.

bfat <- cbind(bfat, pcaZ$x) #join the data and PC scores
str(bfat)
m2<-lm(y ~ PC1 + PC2 + PC3, bfat) 
vif(m2) # PC's are uncorrelated, so there is zero collinearity!
summary(m2) # note that R-sq etc are the same, but gammas for PC's are significant.


mean(bfat$y) # note that when using Z, the intercept is Ybar and it is the same, whether using Z or PC scores as predictors.

bfat$cy <- scale(bfat$y, center=TRUE, scale=FALSE)
# If we center the response variable, too, then the intercept is not necessary in the model, because it becomes zero.
m3<-lm(cy  ~z1 + z2 + z3 - 1, bfat)
summary(m3)

m4<-lm(cy ~ PC1 + PC2 + PC3 - 1, bfat)
summary(m4)

(betas <- pcaZ$rotation %*% coef(m4)) # the coefficients of regressing on Z (betas) can be calculated using the matrix of eigenvectors of Z'Z and the coefficients of regressing on the PC's (gammas).

sqrt(diag(var.betas <- pcaZ$rotation %*% vcov(m4) %*% t(pcaZ$rotation))) # we realize that each beta is a linear combination of betasg, and apply the second equation of statistics to recreate the standard errors of the estimated betas.

# By using PCR we will obtain biased betas (betasg) with much smaller variance.

# Drop principal components that explain a very little part of Z and that are not significant in the models with PC's. In this case we drop only PC3.

m5 <- update(m4, . ~ . - PC3)
summary(m5) # note how the gammas remaining do not change. The wonders of orthogonality!
Vg <-pcaZ$rotation[,-3]

(betasg <- Vg %*% coef(m5))
sqrt(diag(var.betasg <- Vg %*% vcov(m5) %*% t(Vg)))
round(var.betas,5)
round(var.betasg,5)

# USING THE PLS PACKAGE

bf.pcr <- pcr(cy ~ z1 + z2 + z3, data=bfat, validation="LOO",ncomp=3)
validationplot(bf.pcr) # notice how the validation error RMSEP goes up when the third component is added. This method allows to select the best number of components by RMSEP, which is best when the model is mainly for prediction.

coefplot(bf.pcr,ncomp=1)
coefplot(bf.pcr,ncomp=2)
coefplot(bf.pcr,ncomp=3)

# Cool example with image http://www.r-bloggers.com/reconstructing-principal-component-analysis-matrix/