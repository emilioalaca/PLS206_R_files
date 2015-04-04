# we load the data into R using the .csv file with headers
Nmin<-read.csv("stepwise regression data.csv", header=T)
names(Nmin)
str(Nmin)
# we generate our model
sm1 <- lm( NetNmin28~  WEON+WEOC +WECN + SOM +TSN +TSC  +TSCN+SolvitaCO2+SHI , Nmin)
summary(sm1)
anova(sm1)
library(car)
Anova(sm1,type="II")
Anova(sm1,type="III")

shapiro.test(residuals(sm1)) # Null hypothesis: "the samples come from a MV Normal distribution" against the Alternative hypothesis: "the samples do not come from a MV Normal distribution"
qqPlot(sm1, main="QQ Plot") #qq plot for studentized resid
avPlots(sm1,terms=~.)
#Q2:Testing homogeneity of variance:
##By location
Nmin$location<-factor(Nmin$location)
class(Nmin$location)
library(HH)
hov(residuals(sm1)~ location, Nmin)
summary(sm1)
residualPlots(sm1)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
## by Type
Nmin$location<-factor(Nmin$Type)
class(Nmin$Type)
library(HH)
hov(residuals(sm1)~ Type, Nmin)
summary(sm1)
residualPlots(sm1)
layout(matrix(c(1,2,3,4),2,2)) 


#Q3: Testing independence of errors
dwt(sm1)

#Q4: Testing for liniarity 
leveragePlots(sm1) #leverage plots

#Q5 finding Outliers 

#Using the hats values
### citical value =2 *(p/n)  p=number of parameter (number of independent variables +1) n=number of observation
dim(Nmin)
2*(10/35)
sm1.hats <- hatvalues(sm1)
Nmin[which(sm1.hats>0.5714286),]

# Using the jackknife distance  

X <- Nmin[,c("WEON","WEOC" ,"WECN",  "SOM", "TSN","TSC","TSCN","SolvitaCO2","SHI")] 
library(bootstrap) # load package that has jackknife()
## Write a function that returns the distance for each observation when it is not included in the dataset. Note the power and compactness of R's subsetting
theta<-function(x,xdata) 
{mahalanobis(xdata[-x,],colMeans(xdata[x,]), cov(xdata[x,]))}
jackD2<-jackknife(1:dim(X)[1], theta, X) #jackknife indicating to remove each of the whole set of rows in X
jackD2$jack.values  #compare with the Chi-square distribution
qqPlot(jackD2$jack.values,distribution="chisq", df=ncol(X))
X[which(jackD2$jack.values==max(jackD2$jack.values)),] #maximum value
qchisq(1-0.05/35,9) #gives me the critical chisquare value and I can check this effectively is an outlier
X[which(jackD2$jack.values>qchisq(1-0.05/35,9)),] #outlier for real
max(jackD2$jack.values) #chi square value for the furthest observation (51)

#Q6: Testing for collinearity

vif(sm1) # values over 5 are undesirable and over 10 indicate excessive collinearity.

#Q7 and Q8: Chosing a good model

install.packages("leaps")

library(leaps)


install.packages("MASS")
library(MASS) 


apm1<-summary(best1<-regsubsets(NetNmin28~  WEON+WEOC +WECN + SOM +TSN +TSC  +TSCN+SolvitaCO2+SHI , Nmin, nbest=1, nvmax=9, method=c("exhaustive")))

p<-c(1:9) # create a vector for p = number of parameters (columns 1 till 9 cause that's all the data)

AICc<-apm1$bic - p*log(35) + 2*p + 2*p*(p+1)/(35-p-1) # calculate AICc # 35 is number of observations and p number of parameters

plot(p,AICc) # plot the AIC vs. p and you look at the minimm one and for example if its 7 then you choose the one for 7 variables

coef(best1,which.min(AICc)) # get the coefficients for best model (negative signs affect negatively)

#Q9:K-fold Crossvalidation

sm2 <- lm(NetNmin28~  WEON+WEOC+TSC, Nmin)
summary(sm2)
library(DAAG)

cv.lm(df=Nmin, sm2, m=3) # 4 fold cross-validation

anova (sm1,sm2) # done to compare full model vs best model and expect to get ns values and thus I can use either of them and generally use the simplest one.


