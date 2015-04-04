#Silence of the digestion lambs
library(candisc)
dig <- read.csv("../Examples//xmpl_digMANOVA.txt", header=TRUE)
str(dig)
mv1 <- manova(cbind(DM.d,NDF.d,ADF.d,Cellu.d) ~ diet, data=dig)
mv1
summary(mv1, test="W")
summary(mv1, test="H")
summary(mv1, test="P")
summary.aov(mv1)
n <- dim(dig)[1]
H <- (n-1)*cov(mv1$fitted.values)
E <- (n-1)*cov(mv1$residuals)
T <- (n-1)*cov(dig[,3:6])

E
H
E + H
T

anova(lm(DM.d~diet, dig))

mlm1 <- lm(cbind(DM.d,NDF.d,ADF.d,Cellu.d) ~ diet, data=dig)
can1 <- candisc(mlm1)
plot(can1)
can1.scores <- can1$scores
class(can1.scores)
can1.scores
anova(lm(Can1~diet, data=can1.scores))

heplot(mlm1)
heplot3d(mlm1)

anova()


dig$color <- "orange"
dig$color[dig$diet=="AA"] <- "blue"
dig$color[dig$diet=="AO"] <- "green"
dig$color[dig$diet=="MM"] <- "red"
dig$color[dig$diet=="OO"] <- "yellow"
plot3d(dig$ADF.d,dig$DM.d,dig$NDF.d, type="s", col=dig$color, size=1)

