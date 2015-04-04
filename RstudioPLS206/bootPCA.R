# =============== Bootstrapping PCA =============================
# =============== Read and prepare data, load packages ==========

spart <- read.csv("../Examples/spartina.txt", header=TRUE)
str(spart)
library(boot)


# ============ create function to return statistics of interest ========
pca4boot <- function(pca.0, data, i) { #corrected for PC sign reversals
  data.i <- data[i,]
  pca.i <- prcomp(data.i[,4:17], scale=TRUE)
  eval1 <- pca.i$sdev[1]^2
  chng.sign1 <- sign(cor(pca.0$rotation[,1],pca.i$rotation[,1]))
  chng.sign2 <- sign(cor(pca.0$rotation[,2],pca.i$rotation[,2]))
  load.Zn.pc1 <- chng.sign1*cor(data.i[,8],pca.i$x[,1])
  load.Zn.pc2 <- chng.sign2*cor(data.i[,8],pca.i$x[,2])
  result <- c(eval1,load.Zn.pc1,load.Zn.pc2)
  return(result)
}

# =========== perform bootstrapping and request CI's ==================

boot.results <- boot(spart,pca4boot,1999, pca.0=spart.pca)
boot.ci(boot.results, index=1, type="bca")
boot.ci(boot.results, index=2, type="bca")
plot(density(boot.results$t[,1]))
plot(density(boot.results$t[,2]))
plot(density(boot.results$t[,3]))
hist(boot.results$t[,2])
hist(boot.results$t[,3])

spart.pca <- prcomp(spart[,4:17], scale=TRUE)
cor(spart[,8],spart.pca$x[,1])
biplot(spart.pca)
cor(spart[,4:17])

reversal <- function(pca.0,data,i){
  data.i <- data[i,4:17]
  pca.i <- prcomp(data.i, scale.=TRUE)
  cor(pca.0$rotation[,1],pca.i$rotation[,1])
}

reversal.boot <- boot(spart,reversal,1999,pca.0=spart.pca)
plot(density(reversal.boot$t[,1]))

# ============== what is going on with the loadings!?? ============
fun1 <- function(data) {
  my.data <- spart[sample(1:dim(spart)[1], replace=TRUE),]
  pca.i <- prcomp(my.data[,4:17], scale.=T)
  eval1 <- pca.i$sdev[1]^2
  load.Zn.pc1 <- abs(cor(data[,8],pca.i$x[,1]))
  load.Zn.pc2 <- abs(cor(data[,8],pca.i$x[,2]))
  result <- c(eval1,load.Zn.pc1,load.Zn.pc2)
  return(result)
}

fun1(spart)
biplot(pca.i)