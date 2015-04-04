# Partition of X's into component scores and back

# Use Body fat x matrix.

bfat<-read.csv("..//Examples/bodyfatb.dat", header=T)
bfat
Xmat <- as.matrix(bfat[,1:3])
Xmat

# calculate scores
pcaXmat <- prcomp(Xmat)
centeredXmat <- scale(Xmat, center=TRUE, scale=FALSE)
Wmat <- pcaXmat$x
Vmat <- pcaXmat$rotation
centeredXmat%*%Vmat-Wmat #centeredXmat %*% Vmat gets the PC scores W
apply(Wmat,2,mean)
apply(centeredXmat,2,mean)
Wmat%*%Vmat
Xmat.svd <- svd(Xmat)