#http://www.ats.ucla.edu/stat/r/dae/zipoisson.htm. Accessed 2 Dec 2014.

require(ggplot2)
require(pscl)
require(boot)
zinb <- read.csv("http://www.ats.ucla.edu/stat/data/fish.csv")
zinb <- within(zinb, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
})

summary(zinb)
ggplot(zinb, aes(count)) + geom_histogram() + scale_x_log10()
hist(zinb$count, breaks=40)
plot(table(zinb$count))

summary(m1 <- zeroinfl(count ~ child + camper | persons, data = zinb))
m2 <- zeroinfl(count ~ child + camper | 1, data = zinb)
m3 <- zeroinfl(count ~ child + camper, data = zinb)
# the formula specifies response ~ model for count | model for zero inflation

mnull <- update(m1, . ~ 1)
summary(mnull)
pchisq(2 * (logLik(m1) - logLik(mnull)), df = 3, lower.tail = FALSE)
summary(p1 <- glm(count ~ child + camper, family = poisson, data = zinb))
vuong(p1, m1)
plot(residuals(m1, type="pearson")~fitted(m1)) # Inspect Residuals
plot(zinb$count~fitted(m1))

#(bootstrapping gives better estimates of errors)

newdata1 <- expand.grid(0:3, factor(0:1), 1:4)
colnames(newdata1) <- c("child", "camper", "persons")
newdata1 <- subset(newdata1, subset=(child<=persons))
newdata1$phat <- predict(m1, newdata1)

ggplot(newdata1, aes(x = child, y = phat, colour = factor(persons))) +
  geom_point() +
  geom_line() +
  facet_wrap(~camper) +
  labs(x = "Number of Children", y = "Predicted Fish Caught")

#Take a look at the effect of persons on the probability of fishing
pi.parms <- coef(m1, model= "zero") # parameters of the linear predictor eta for pi
zmat <- model.matrix(m1, model="zero") # values of the predictors for data
g <- zmat %*% pi.parms # predicted or fitted values of linear predictor for data
p.fishing <- exp(g)/(1 + exp(g)) # predicted probability that each group was fishing, based on the number of persons in the group.
# more directly but less didactic (also into a different class):
p.fishing2 <- predict(m1, type="zero")
head(p.fishing)
head(p.fishing2)
class(p.fishing)
class(p.fishing2)

# Exercise: get the predictions for the count part of the model

mu.parms <- coef(m1, model= "count") # parameters of the linear predictor eta for count
zmat2 <- model.matrix(m1, model="count") # values of the predictors for data
g2 <- zmat2 %*% mu.parms # predicted or fitted values of linear predictor for data
mu <- exp(g2) # predicted mean of Poisson for each group that was fishing 
mu2 <- predict(m1, typ="count")
head(mu)
head(mu2)
class(mu)
class(mu2)

# Predictions combining both zeros and counts

expected.count <- mu * (1-p.fishing)
head(expected.count)
head(predict(m1, type="response"))
