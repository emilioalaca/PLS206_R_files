library(nlme)
data(BodyWeight) #each package comes with complete examples to learn their use.
?BodyWeight # Read about example
options(contrasts=c("contr.treatment", "contr.poly"))

class(BodyWeight) # this is a groupedData object
# therefore, rat is already a grouping factor included in the randm effects by default. See the estimated random effects in the model. There are random effects in the intercept and time slope for each rat.
bw1.lme<-lme(weight~Time*Diet, BodyWeight, random=~Time)
bw2.lme<-lme(weight~Time*Diet, BodyWeight, random=~Time|Rat)
bw1.lme;bw2.lme
plot(bw1.lme, resid(., type="p")~fitted(.)) # typical heterogeneity of variance
bw2.lme<-update(bw1.lme,weights=varPower(form=~fitted(.)))

plot(bw2.lme, resid(., type="p")~fitted(.)) #check residuals again
#Suspect temporal autocorrelation of residuals, but sampling times are irregular so we use a spatial approach
vg<-Variogram(bw2.lme, form=~Time, maxDist=42)
vg
plot(vg) #plot the semivariogram
bw3.lme<-update(bw2.lme, corr=corExp(form=~Time))
bw3.lme
intervals(bw3.lme)# note that this example has heterogeneity of variance and lack of independence.
anova(bw2.lme, bw3.lme) #check significance of  adding a model for lack of independence.
plot(Variogram(bw3.lme, form=~Time, maxDist=42))
# what variogram model was used? find the default

plot( Variogram(bw3.lme, form = ~ Time, maxDist = 42, resType = "n", robust = T) ) # check variogram of data corrected for temporal correlation.
