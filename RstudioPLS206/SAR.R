# we create 30 plots. We will assume for simplicity that we sample five individuals within each plot (equal sampling effort across plots). So set up an empty 5 x 30 array:
  
plot.spp <- array(dim=c(5,30)) # I changed the number of plots to be different from the number of species so there is less confusion.

# Now lets assume that we have an overall species richness of 20 (we will just call them sp.a, sp.b, sp.c, etc…)

(species <- paste("sp.",c(letters[1:20]), sep="")) #

#================ procedure A to create vector of proportions ===========
#We need to assign a probability of occurrence to each species, making sure the probabilities sum to one. So we simulate 20 probabilities:
  
probs <- numeric(20) 
probs[1:8] <- runif(8, 0, 0.1)
for(i in 9:19){
  probs[i] <- runif(1, 0, 1-sum(probs[1:i-1]))
}
probs[20] <- 1-sum(probs[1:19]) # Modified procedure to make sure it sums to 1.

#The first line sets up an empty container vector. The second line randomly assigns the first 8 species a probability between 0 and 0.1 (this is to prevent any species from having a probability of 0.7 or higher and dominating all the plots because that would be boring). The for( ) loop makes sure each remaining species has a probability between 0 and one minus the sum of all other probabilities (constraining them to equal one). You can check the constraint:
  
sum(probs)

# which should equal 1.

# ==================== Procedure B to "create" 30 plots, each with five individuals randomly selected from the species list ====

# Note that in reality the number of species and the probabilities will change from plot to plot, because we would expect some combinations of species to be more frequent than expected on the basis of the marginal probabilites. 

# Now for each plot (column) in our plot array, we want to sample five individuals randomly using the probabilities we just created (because the probability of picking any one species is equal to its overall probability).

for(i in 1:30){
  plot.spp[,i] <- sample(species, size=5, replace=T, prob=probs)
}

# We sample with replacement to account for the possibility that species can occur multiple times within a plot. Essentially, this says the probability of an individual in a plot belonging to a given species is equal to the relative abundance or each species.

# =================

# Now we work on generating our random SAR curve. Let’s assume that we will randomly sample plots 20 times, and that we will increase the number of plots sampled (i.e. we will sample 1 plot 20 times, then 2 plots 20 times, then 3 plots 20 times, etc..). For each sampling event, we will calculate species richness.

# We set up an empty 20 x 30 container (20 sampling events for 30 different plot numbers):
  
SAR.mat <- array(dim=c(20,30))

# We use a nested for( ) loop to simulate the sampling:
  
for(j in 1:30){
  for(i in 1:20){
    plot.index <- sample(1:30, j, replace=F)
    SAR.plot <- c(plot.spp[,plot.index])
    SAR.mat[i,j] <- length(unique(SAR.plot))
  }
}

# The first loop tells the program to sample j plots going from 1 to 30. This is our ‘total plot area sampled’ going from 1 sq m to 30 sq m. The second loop is the sampling event, going from 1-20. plot.index is the index of sampled plots (i.e. randomly pull j plots from the 20 that we have). The length(unique( )) command just calculates the number of unique species i.e. species richness.

# Now we have a 20 x 30 array containing 20 sampling events (rows) for each possible area (columns). Set up a vector relating the columns to areas, calculate the mean species richness of each column (area), calculate the 95% confidence interval, and then plot:
  
areas <- 1:20
means <- apply(SAR.mat, MARGIN=2, mean)
lower.95 <- apply(SAR.mat, MARGIN=2, function(x) quantile(x, 0.025))
upper.95 <- apply(SAR.mat, MARGIN=2, function(x) quantile(x, 0.975))
par(mar=c(4,4,1,1)+0.2)
plot(areas, means, type='n', xlab=expression('Area '*(m^-2)),
     ylab='Species Richness', cex.lab=1.2,
     ylim=c(0,12))
polygon(x=c(areas, rev(areas)),
        y=c(lower.95, rev(upper.95)),
        col='grey90')
lines(areas, means, lwd=2)

# Should look nice. If it doesn’t, rerun the above code in its entirety and you’ll get a different plot.

# Now we want to fit a model to this. The common SAR model is



# We can log transform each side to get the following model to fit with a linear regression:
  
# We can fit the model and then plot the curve

SAR.mod <- lm(log(means) ~ log(areas))
summary(SAR.mod)
curve(exp(coef(SAR.mod)[1])*x^coef(SAR.mod)[2], add=T, from=0, to=20, col='red', lwd=2)

# Unfortunately, the log-transformed model actually fits a model assuming multiplicative errors:
# S=(cA^z)e
  
# The model we really want to fit is: S=(cA^z)+e

# See Xiao et al (2011) in Ecology for a good description of the differences between the two models above and how they have been misused in allometric studies. We can fit the second model using a non-linear regression, using the log-linear model parameters as reasonable starting estimates. We then plot the nls( ) curve and tack on a legend.

SAR.nls <- nls(means ~ a*areas^b,
               start=list('a'=exp(coef(SAR.mod)[1]),
                          'b'=coef(SAR.mod)[2]))
curve(coef(SAR.nls)[1]*x^coef(SAR.nls)[2], add=T, from=0, to=20, col='blue', lwd=2)
legend('topleft', lty=1, col=c('black', 'red', 'blue'),
       legend=c('Median Species Richness', 'Linear Model Fit', 'Nonlinear Model Fit'),
       cex=0.8,
       bty='n')
