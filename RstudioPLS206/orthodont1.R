library(nlme)
data(Orthodont)
ortho <- as.data.frame(Orthodont)
str(ortho)
names( ortho )
levels( ortho$Sex)
m1.lis <- lmList( distance ~ age | Subject, data = ortho )
coef( m1.lis )
intervals( m1.lis )
plot( intervals ( m1.lis ) )
m2.lis <- lmList(distance ~ I( age - 11 ) | Subject, ortho ) 
plot( intervals( m2.lis ) )
lme1 <- lme( distance ~ age, data = ortho, random = ~ 1 | Subject )
summary( lme1 )
lme1.ML <- update( lme1, method = "ML" )
summary( lme1.ML )
lme2 <- update( lme1, random = ~ age | Subject )
anova( lme1, lme2 ) # not significantly different
random.effects( lme1 ) # same as ranef()
ranef( lme1.ML )
coef( lme1 )
coef(lme1.ML)
plot(compareFits(coef(lme1), coef(lme1.ML)))   # Figure 1.15
#plot( augPred(lme1), aspect = "xy", grid = T )   # Figure 1.16
lme3 <- update(lme2, fixed= . ~ Sex*I(age-11), random= ~ I(age-11)|Subject)
intervals(lme3)
lme3
# each observation has a fixed component in the intercept, a fixed effect of age, a random Subject effect on the intercept and a random Subject effect on the slope. Y = B0 + dB0i + B1*age + dB1i*age + epsilon


# Checking assumptions
plot(lme3)
plot(lme3, resid(., type="p")~fitted(.) | Sex, id = 0.05, adj = -0.3)
lme4 <- update( lme3, weights = varIdent(form= ~1|Sex))
anova(lme3,lme4)
summary(lme4)

qqnorm(lme4, ~resid(.) | Sex)
plot(augPred(lme4))
