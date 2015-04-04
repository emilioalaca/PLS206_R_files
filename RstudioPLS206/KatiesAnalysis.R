datak <- read.csv("..//Examples//KatiesData.txt", header=TRUE)

library(lsmeans) # this can show you how the linear functions are constructed
mk1 <- lm(calls ~ block + treatment, datak) # no continuous variables, all "intercepts"
summary(mk1)
mk1$contrasts # both factors in mk1 are using contr.treatment
coef(mk1) # is a vector that contains the following elements"
# Name by R   estimate   Meaning in terms deviations from overall value
# (Intercept)   9.825    = overall mean (aka intercept) + dBlockA + dTreatCAT
# blockB       -3.500    = dBlockB - dBlockA
# blockC       -7.200    = dBlockC - dBlockA
# blockD       11.400    = dBlockD - dBlockA
# treatmentCON -7.625    = dTreatCON - dTreatCAT
# treatmentGRA 16.250    = dTreatGRA - dTreatCAT
# treatmentSPI -6.750    = dTreatSPI - dTreatCAT
# treatmentTUL -3.000    = dTreatTUL - dTreatCAT (see handwritten notes)
##
# How do we get the coefficents in contr.sum mode? You can change the options
# or simply solve teh equations above to get the dBlock... and dTreat... values.
# The set of simultaneous linear equations above can be expressed in matrix form
# to simplify the solution.
# I put the matrix together by making pieces to avoid having to type it.
# I included rows to represent the fact that the sum of dBloc = 0 and sum dTreat=0

lmat <- matrix(data=0, nrow=10, ncol=10)
lmat[1,c(1,2,6)]<-1
lmat[2:4,2]<--1
lmat[5,2:5]<-1
lmat[6:9,6]<--1
lmat[10,6:10]<-1
lmat[1,6]<-1
for (i in 1:9) lmat[i,i+1]<-1
lmat[5,6]<-0

b <- c(coef(mk1)[1:4],0,coef(mk1)[5:8],0) # adding the zero results for rows 5 and 10
my.coefs <- solve(a=lmat,b=b) # coefficients in deviation format.

# Corroborate by refitting model using contr.sum
options(contrasts=c("contr.sum", "contr.poly"))
mk2 <- lm(calls ~ block + treatment, datak)
summary(mk2)
coef(mk2)

# NOW THE PRACTICAL PART: COMPARE YOUR MEANS
TukeyHSD(aov(mk1), "treatment", conf.level=0.95)
TukeyHSD(aov(mk2), "treatment", conf.level=0.95) # Note that the calculations adjust
# for the type of contr. used in the model. Results is the same as before.
TukeyHSD(aov(mk1), "block", conf.level=0.95) # I put this in just to point out
# that block D is very different from the rest. Just a heads up.

# Using lsmeans()
lsmeans(mk1, specs = pairwise ~ treatment, adjust = "bonferroni")
lsmeans(mk1, specs = pairwise ~ treatment, adjust = "tukey")
# see that bonferroni is more conservative than tukey.




# What follows is an additional way to operate to go from contr.treatment to sum
# Not so well documented and not necessary unless you want to really get good at it.

mk1.lf <- lsmeans(mk1,specs =list(~1, ~block, ~ treatment), lf=TRUE)
# lsmeans gives you the matrix of coefficients to go from betas given by lm to 
# treatment means or any other combination of parameters. It uses glht.
# I used lsmeans to make a matrix to go from betas to means and then modified the
# matrix to make it into a "beta2fx" matrix.

beta2means <- rbind(as.matrix(mk1.lf$" lsmeans"),as.matrix(mk1.lf$"block lsmeans"),as.matrix(mk1.lf$"treatment lsmeans"))
dimnames(beta2means)[[1]][1] <- "mu.."
dimnames(beta2means)
(beta2fx <- beta2means - rbind(rep(0,8),rep(1,9)%o%beta2means[1,]))
(fx.coefs <- beta2fx %*% coef(mk1))
