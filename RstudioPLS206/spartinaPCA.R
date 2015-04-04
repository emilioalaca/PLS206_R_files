# PCA for PLS 206

# Loading the data 
# read the data into R using the csv file with headers
spartina<-read.csv("../Examples/spartina.txt", header=T)

# For PCA keep only numerical variables
spa<-spartina[,4:17]

# ======== Testing assumptions etc. ============= ==================
# Loading libraries
library(mvnormtest)

# Testing the multivariate normality
# Null hypothesis: "the samples come from a MV Normal distribution" against the Alternative hypothesis: "the samples do not come from a MV Normal distribution
mat <- t(spa)
mshapiro.test(mat)

# Correlation among variables (Testing linearity)
install.packages(c("corrplot","psych", "caret"))
library(psych)
library(corrplot)
library(caret)
library(bootstrat)
round(cor(spa),2) # correlation matrix
pairs.panels(spa, lm=TRUE, rug=FALSE, method="pearson")
correlations<-cor(spa)
corr.plot(correlations, order="hclust")
findCorrelation(correlations, cutof=.75) #which variables are correlated with values above 0.75

# Testing for Outliers 
# Mahalanobis distances for outliers identification, NO JACKKNIFED DISTANCES YET
(mean<-colMeans(spa)) #means of each variable
(Sx<-cov(spa)) #covariance matrix
D2<-mahalanobis(spa,mean,Sx)
D2
spa2<-as.data.frame(cbind(spa, D2))
sort(spa$D2, decreasing=TRUE)
chiq<-qchisq((spa$D2-0.5)/(ncol(spa)-1),(ncol(spa)-1)) #chi-square quantile, -1 because spa has already D2 as additonal column
qqplot(chiq,spa$D2, main = expression("Q-Q plot of Mahalanobis" * ~D^2 * " vs. quantiles of" * ~ chi[3]^2))
abline(0, 1, col = 'red')
#identify(qqplot(chiq,spa$D2, main = expression("Q-Q plot of Mahalanobis" * ~D^2 * " vs. quantiles of" * ~ chi[3]^2)))
qqplot(qchisq(ppoints(45), df = 14), D2)
qqline(D2, distribution = function(p) qchisq(p, df = 14), prob = c(0.1, 0.6), col = 2)

library(bootstrap) # load package that has jackknife()
library(car)
## Write a function that returns the distance for each observation when it is not included in the dataset. Note the power and compactness of R's subsetting
theta<-function(x,xdata) 
{mahalanobis(xdata[-x,],colMeans(xdata[x,]), cov(xdata[x,]))}

jackDsq<-jackknife(1:dim(spa)[1], theta, spa) #jackknife indicating to remove each of the whole set of rows in spa

jackDsq$jack.values  #compare with the Chi-square distribution
qqPlot(jackDsq$jack.values,distribution="chisq", df=ncol(spa))
spa[which(jackDsq$jack.values>quantile(jackDsq$jack.values,probs=c(0.9))),]
# ========== END Testing assumptions etc. =============
# ======== Other methods for MV outliers ============
# Mahalanobis distance using chemometrics
install.packages("chemometrics")
library(chemometrics) # I am having problems loading this library using Rstudio on Mac, it works well using Rgui
mvomah<-Moutlier(spa,quantile =0.975, plot=TRUE)
mvomah$cutoff
summary(mvomah$md)
sort(mvomah$md, decreasing=TRUE)
which(mvomah$md > mvomah$cutoff)

# A nice way for identifying outliers (Robust Mahalanobis)
install.packages("mvoutlier") # X11 required on Mac
library(mvoutlier)
aq.plot(spa, delta=qchisq(0.975, df=ncol(spa)), quan=1/2, alpha=0.05)
chisq.plot(spa, quan=1/2, ask=TRUE)
dd.plot(spa, quan=1/2, alpha=0.025)
pcout(spa, makeplot = FALSE, explvar = 0.99, crit.M1 = 1/3, crit.c1 = 2.5, crit.M2 = 1/4, crit.c2 = 0.99, cs = 0.25, outbound = 0.25)
#=========== END other methods ==========================
#=================

# Principal Component Analysis
sp.pca<-prcomp(spa[,1:14], scale=TRUE) # create PCA model using the saled and centered data
help(prcomp) #read about the difference between prcomp and princomp
help(princomp) # mainly princomp uses formula and eigen instead of svd
sp.pca2<-princomp(~sal+pH+K+Na+Zn+H2S+Eh7+acid+P+Ca+Mg+Mn+Cu+NH4, data=spa, cor=TRUE, scores=TRUE)

#### Looking at what prcomp did #####

summary(sp.pca) # obtain sqrt(eigenvalues), marginal and cumulative proportion of variance 

par(mfrow=c(1,2)) #set parameters to insert graphs (in this case 2 columns and 1 rows)

biplot(sp.pca) # Display biplot

plot(sp.pca,main="") #Display scree plot

percentVar<- sp.pca$sd^2/sum(sp.pca$sd^2)*100 #Cumulative percentage of variance which each component accounts for
percentVar

predict(sp.pca)[,1] # see scores for all observations on PC1

predict(sp.pca)[,2] # and on PC2

var(predict(sp.pca)[,1]) # variance of the first principal component

var(predict(sp.pca)[,2]) # variance of the second principal component

sp.pca$rotation #see eigenvectors

sp.pca$sdev # extract standard deviations of each principal component

sp.pca$sdev^2 # extract variances of each principal component

std.sp <- scale(spartina[,4:17]) # get the centered and scaled data matrix

as.numeric(std.sp[1,]) %*% sp.pca$rotation[,1] # see where PC scores are calculated

#### Looking at what princomp did #####

sp.pca2$scores # view all PC scores

cormat<-cor(cbind(sp.pca2$scores,spa)) #view the loadings used to make the biplot
# matrix is too large to see, so we can do specific correlations:
cor(spa[,11],sp.pca$x[,1:2])


# be careful to note that in R "loadings" is used for the elements of the eigenvectors
# whereas in PLS206 we use the term for the correlations between original variables and PC's

# Permutation method to check for significance of eigenvalues.
eigenval.mat <- matrix(nrow=1000,ncol=14)
for (i in 1:1000) eigenval.mat[i,]<-prcomp(apply(spa,2,sample), scale=TRUE)$sdev^2
hist(eigenval.mat[,1])
sp.pca$sdev^2
hist(eigenval.mat[,2])
hist(eigenval.mat[,3]) # only 2 e-values appear to be significantly greater than expected by chance.
