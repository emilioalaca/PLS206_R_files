d2 <- read.csv("../Examples/HW07hrdata.txt",header=TRUE)[,-1]
d2
m1 <- glm( cbind(nH, nspurs-nH) ~ factor(block) + factor(nitro), family = binomial, data=d2)
m1
par(mfrow=c(2,2))
plot(m1)
summary(m1)
1-pchisq(1.46,3)

predict(m1, newdata=data.frame(block=rep(3,4),nitro=rep(c(125,350),)))

