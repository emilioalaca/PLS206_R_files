xmpl_Filaree<-read.csv("xmpl_filaree.csv", header=TRUE)
xmpl_Filaree
# Change options for contrasts so treatment effects
# are calculated as differences from the overall mean
# instead of the default for R, by which treatment
# effects are calculated as differences from the
# first treatment.
options(contrasts =c("contr.sum", "contr.poly"))
# Transform the variable Tgroup into a factor
xmpl_Filaree$Tgroup<-as.factor(xmpl_Filaree$Tgroup)
# Specify linear model with all effects and interactions
mymodel<-lm(lnwt~Tgroup*day, xmpl_Filaree)
# Display the summary of the results
summary(mymodel)
# Ask for the ANOVA table
anova(mymodel)
library(car)
Anova(mymodel)
Anova(mymodel, type=2)
Anova(mymodel, type=3)
# Create L vector for b.hat.11
L4b.hat.11<-c(0,0,0,1,1,0)
L4b.hat.11
# Get estimated partial regression coefficient b.hat.11
# using the meatrix product of L4b.hat.11 and the
# vector of estimated coefficients and effects
b.hat.11<-L4b.hat.11%*%mymodel$coef
b.hat.11
L4b.hat.11%*%coef(mymodel)
# so both mymodel$coef and coef(mymodel) retrieve the
# vector of estimated coefficients

# Obtain the MSE
mymodel.MSE<-deviance(mymodel)/df.residual(mymodel)
mymodel.MSE
anova(mymodel)
# The X'X-1 covariance of estimate parameters can be extracted from the model summary
summary(mymodel,cov=TRUE)$cov
solve(t(model.matrix(mymodel))%*%model.matrix(mymodel))
# X'X-1 matrix for mymodel
x.prime.x.inv<-summary(mymodel,cov=TRUE)$cov
# variance of estimated b.hat.11
varb.hat.11<-mymodel.MSE*L4b.hat.11%*%x.prime.x.inv%*%L4b.hat.11
varb.hat.11
# standard error of b.hat.11
stdrb11<-sqrt(varb.hat.11)
stdrb11
# t Ratio for b.hat.11
tb.hat.11<-b.hat.11/stdrb11
tb.hat.11
# pvalue for test that b.hat.11=0
ttailb11<-1-pt(tb.hat.11,df.residual(mymodel))
ttailb11
# Create L vector for b.hat.12
L4b.hat.12<-c(0,0,0,1,0,1)
L4b.hat.12
# partial regression coefficient b.hat.12
b.hat.12<-L4b.hat.12%*%mymodel$coef
L4b11.b12<-L4b.hat.11-L4b.hat.12
# difference between b.hat.11 and b.hat.12
b11.b12<-L4b11.b12%*%mymodel$coef
# variance of b11.b12
varb11.b12<-mymodel.MSE*L4b11.b12%*%x.prime.x.inv%*%L4b11.b12
varb11.b12
# standard error of b11.b12
stdrb11.b12<-sqrt(varb11.b12)
stdrb11.b12
# t Ratio for b11.b12
tb11.b12<-b11.b12/stdrb11.b12
tb11.b12
ttailb11.b12<-1-pt(tb11.b12,df.residual(mymodel))
ttailb11.b12
# The following section calculates predicted yields
# In addition to the slopes we need the intercepts
# Create vector for b.hat.01
L4b.hat.01<-c(1,1,0,0,0,0)
L4b.hat.01
# Create vector for yhat1.30
L4yhat1.30<-L4b.hat.01+30*L4b.hat.11
L4yhat1.30
# Predicted yield for group 1 at day 30
yhat1.30<-L4yhat1.30%*%mymodel$coef
yhat1.30
# Create L vector for beta13
L4beta13<-c(0,0,0,1,-1,-1)
L4beta13
# Create L vector for beta03
L4beta03<-c(1,-1,-1,0,0,0)
L4beta03
# Predicted yield for group 3 at day 25
L4yhat3.25<-L4beta03+25*L4beta13
L4yhat3.25
yhat3.25<-L4yhat3.25%*%mymodel$coef
yhat3.25
# comparison of yhats, difference between yhat1.30 and yhat3.25
L4yhat1.30.yhat3.25<-L4yhat1.30-L4yhat3.25
L4yhat1.30.yhat3.25
# difference between yhat1.30 and yhat3.25
yhat1.30.yhat3.25<-L4yhat1.30.L4yhat3.25%*%mymodel$coef
yhat1.30.yhat3.25<-L4yhat1.30.yhat3.25%*%mymodel$coef
yhat1.30.yhat3.25
variance of yhat1.30.yhat3.25
# variance of yhat1.30.yhat3.25
var.y130y325<-mymodel.MSE*L4yhat1.30.yhat3.25%*%x.prime.x.inv%*%L4yhat1.30.yhat3.25
var.y130y325
stdr.var.y1301325<-sqrt(var.y130y325)
# p-value for test that yhats are not different:
ttaily130.y325<-1-pt(yhat1.30.yhat3.25/stdr.var.y1301325,df.residual(mymodel))
ttaily130.y325
# Now, use linear.hypothesis from car or multcomp