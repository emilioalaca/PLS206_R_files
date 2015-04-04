spart<-read.csv("spartina.csv", header=TRUE)
pc14<-prcomp(spart[,4:17], scale.=TRUE)
pc14
pc14$sdev
pc14$sdev^2
var(predict(pc14)[,1])
par(mfrow=c(2,2))
biplot(pc14)
