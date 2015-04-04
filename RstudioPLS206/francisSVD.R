env <-read.csv("..//Examples//francis.txt", header=FALSE)
X <- env[,2:5]
names(X) <- c("X1", "X2", "X3", "X4")
X <- scale(as.matrix(X))
svdX <- svd(X)
svdX$d
XprimeX <- t(X)%*%X
eigen(XprimeX)$values
eigen(XprimeX/(dim(X)[1]-1))$values
sum(eigen(XprimeX/(dim(X)[1]-1))$values)