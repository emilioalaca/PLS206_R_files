options(show.signif.stars=F)
data <- read.csv("phytus.dat")
options(contrasts =c("contr.sum", "contr.poly")) #make order or parameters same as in JMP just to keep it simple

Y <- with(data, cbind(seeds, peaksc))
model <- manova(Y ~ ecotype * location, data=data)

df = df.residual(model)
summary.aov(model)

manova_sum <- summary.manova(model, test='Wilks')
manova_sum
SSTR1 = manova_sum$SS$ecotype
SSTR2 = manova_sum$SS$location
SSTR3 = manova_sum$SS$'ecotype:location'
SSE = manova_sum$SS$Residuals
SSTR <- SSTR1 + SSTR2 + SSTR3
SSTR1
SSTR2
SSTR3
SSTR
SSE

#data1 <- data[which(data$eco.loc=='basin1desert'),]
#data2 <- data[which(data$eco.loc=='basin2mountain'),]
#data3 <- data[which(data$eco.loc=='basin3greenhouse'),]
#data4 <- data[which(data$eco.loc=='sierra1desert'),]
#data5 <- data[which(data$eco.loc=='sierra2mountain'),]
#data6 <- data[which(data$eco.loc=='sierra3greenhouse'),]

#yb1 = matrix(apply(data1[,3:4], 2, mean))
#yb2 = matrix(apply(data2[,3:4], 2, mean))
#yb3 = matrix(apply(data3[,3:4], 2, mean))
#yb4 = matrix(apply(data4[,3:4], 2, mean))
#yb5 = matrix(apply(data5[,3:4], 2, mean))
#yb6 = matrix(apply(data6[,3:4], 2, mean))
#yb = cbind(yb1, yb2, yb3, yb4) # A matrix with trt means as columns

# try using the "by" method to see means
ymeans<-with(data,by(data[,3:4],eco.loc,mean)) #produces a by object that needs work to become a matrix

#however, we are really interested in the fitted values or LS means, which can differ from the cell means
#thus, use linear combinations of parameters or fitted(model)
#get the treatment means -if you want to see them- as LC of estimated parameters
rownames(trt.mat)<-c("basin1desert","basin2mountain","basin3greenhouse","sierra1desert","sierra2mountain","sierra3greenhouse")#name the linear combinations in trt.mat
trt.means<-trt.mat%*%model$coef
#########################
# F tests for contrasts
(B<-model$coef)#see estimated parameters
L.basin1.basin2<-c(1,1,1,0,1,0)-c(1,1,0,1,0,1) # L for difference between basin1desert-basin2sierra
L.basin1.basin2 # see it; these L's coefs sum to 0 so they are contrasts
t(model$coef)%*%L.basin1.basin2 #estimate the difference between centroids just to get a feeling for what's going on
# the vector of differences has a bivariate normalm distribution with expectation (0,0) under assumptions and Ho of not difference between treatments. So now we have to factor in the variance and get a probability. This is accomplished with Hotelling's T-sq

z0<-L.basin1.basin2 #make name shorter for readable equation
Z<-model.matrix(model) #get the design matrix as used to get the estimated parameters
m<-dim(Y)[2] #get number of response variables or columns of Y
(n<-dim(Y)[1]) #get and show number of observations; make sure there are no missing values in Y so effective n is what we get
(r<-dim(Z)[2]-1) #get and show number of predictors or columns of Z minus 1 for the intercept
(S<-manova_sum$SS$Residuals/(n)) #estimate sigma
(den<-as.numeric(t(z0)%*%solve(t(Z)%*%Z)%*%z0)) #calculate part of the T-sq formula that corresponds to L'(X'X)-1L in MLR (to tie concepts)
(testF<-(t(t(B)%*%z0/sqrt(den))%*%solve(S*n/(n-r-1))%*%(t(B)%*%z0/sqrt(den)))*(n-r-m)/(m*(n-r-1))) #messy!!! I'll have to show you the formula with nicer symbols
df(testF,m,n-r-m) # p-level for difference.


# now repeat for all the z0's fro the rest of the comparisons
# for Bonferroni, simply compare the p for testF to 0.05/15



#the canonical variables are obtained as can=YMV where Y is Y, M is the response design matrix, in our case it is an identity matrix with as many columns and rows as response variables, and V is the matrix of eigenvectors of E-1H where H is the SS for the interaction (for the hw question)

(E<-manova_sum$SS$Residuals)
(H<-manova_sum$SS$'ecotype:location')
(V<-eigen(solve(E)%*%H)[2]) #not working yet but I need to go eat!


