# Bootstrapping for correlation
library(boot) #load necessary package

datab<-read.csv("../Examples/colinDemoData.csv", header=T) # read collin data for the example; may need to specify full path
str(datab)
my.cor <- function(data, index) {
  bootsample <- data[index,]
  cbind(cor(bootsample[,2],bootsample[,3]),cor(bootsample[,5],bootsample[,6]))
}

boot.cor <- boot(datab,my.cor,1000)
plot(boot.cor)
hist(boot.cor$t[,1], breaks=30)
hist(boot.cor$t[,2], breaks=30)
boot.ci(boot.cor, index=1, type="bca")