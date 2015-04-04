growth <- read.csv("../Examples/growthHW06.txt", header=TRUE)
str(growth)
library(nlme)
data(Orthodont)
m1.lis <- lmList(Orthodont)
m1.lme <- update(m1.lis)
plot(comparePred(m1.lis,m1.lme))
m13 <- growth[which(growth$subject=="M13"),]
lmeControl(maxIter=100)

plot(m13$age,m13$distance)
lml1 <- lmList(distance~I(age-11)|subject, data=growth)
plot(intervals(lml1))
lme1 <- lme(lml1)
lme1 <- lme(distance ~ gender*I(age-11), random=~1|subject, data=growth)

lme2 <- update(lme1,  random= ~ I(age-11)|subject)
lme3 <- update(lme2, fixed= . ~ I(age-11)*gender)

q5 <- lme(lml1)

lml2 <- lmList(distance ~ I(age-11)|Subject, Orthodont)
lme

ranef(lme3)
newdata <- data.frame(subject=rep(c("M11","M13","F03","F04"),c(2,2,2,2)), gender=rep(c("Male", "Female"),c(4,4)), age=rep(14:15,4))
newdata
predict(lme3,newdata=newdata, level=0:1)
plot(augPred(lme3, primary=~age))
plot(lme3,resid(., type="p")~fitted(.)|gender)
lme4 <- update(lme3, weights=varIdent(form=~1|gender))
anova(lme3,lme4)
plot(lme4,resid(., type="p")~fitted(.)|gender)
var(resid(lme3)[growth$gender=="Male"])
var(resid(lme3)[growth$gender=="Female"])
var(resid(lme4)[growth$gender=="Male"])
var(resid(lme4)[growth$gender=="Female"])
var(resid(lme4, type="p")[growth$gender=="Male"])
var(resid(lme4, type="p")[growth$gender=="Female"])

lme5 <- update(lme4,random=~1|subject)
lme5
anova(lme4,lme5)
