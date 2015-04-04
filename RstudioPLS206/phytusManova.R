#phytusMANOVA
phytus <- read.csv("../Examples/phytus.txt", header=TRUE)
library(car)
library(candisc)
mlm1 <- lm(cbind(seeds,peaksc) ~ ecotype*location, data=phytus)
MVmlm1 <- Manova(mlm1)
summary(MVmlm1)
candi.eco <- candisc(mlm1, term="ecotype")
plot(candi.eco)
candi.loc <- candisc(mlm1, term="location")
plot(candi.loc)
candi.ecoloc <- candisc(mlm1, term="ecotype:location")
plot(candi.ecoloc)
heplot(mlm1)
candi.ecoloc.scores <- candi.ecoloc$scores
str(candi.ecoloc.scores)
points(candi.ecoloc.scores$Can2~candi.ecoloc.scores$Can1)
lm1 <- lm(Can1 ~ ecotype*location, candi.ecoloc.scores)
summary(lm1)
anova(lm1)
library(ggplot2)
cor(phytus[,3:4],candi.ecoloc.scores[,3:4])
newdata <- expand.grid(ecotype=c("basin","sierra"), location =c("1desert","2mountain","3greenhouse"))
dplot <-cbind(newdata, Can1=predict(lm1,newdata))

ggplot(dplot, aes(x = location, y = Can1, colour = ecotype)) +
  geom_point() +
  geom_line() +
  labs(x = "location", y = "Predicted Can1")
# lines need to be added
