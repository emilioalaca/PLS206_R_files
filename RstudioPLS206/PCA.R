spa <- read.csv("../Examples/spartina.txt")
#=================
str(spa)
# Principal Component Analysis
sp.pca<-prcomp(spa[,4:17], scale=TRUE) # create PCA model using the saled and centered data
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

std.sp <- scale(spa[,4:17]) # get the centered and scaled data matrix

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
# =======================================
# 3D representation
library(rgl)
plot3d(spa$Mg, spa$Ca,spa$H2S)







