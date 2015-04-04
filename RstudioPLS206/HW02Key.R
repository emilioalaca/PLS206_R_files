# We hypothesize that relative growth rate (RGR) is the same for all groups, independent of temperature.
#=====================================================================================
# 1. Write out the full model (equation). State the null and alternative hypotheses in terms of values of parameters. [20]

# lnwt = B0 + dB01*G1 + dB02*G2 + dB03*G3 + days*(B1 + dB11*G1 + dB12*G2 + dB13*G3) + epsilon
# Ho: dB11 = dB12 = dB13 = 0 or B11 = B12 = B13
# Ha: at least one of the following not true B11 = B12, B12 = B13, B11 = B13                      
                      
#=====================================================================================
# 2. Perform a test of this hypothesis by analyzing the data with R, and interpret the corresponding F test. [20]

clover<-read.csv("..//Examples//clover.csv", header=TRUE)

# Change options for contrasts so treatment effects
# are calculated as differences from the overall mean
# instead of the default for R, by which treatment
# effects are calculated as differences from the
# first treatment.

options(contrasts =c("contr.sum", "contr.poly"))

# Make sure you have the correct names for the columns
# of the data

names(clover)
names(clover) <- c("group", "days", "lnwt")


# Transform the variable group into a factor

clover$group<-factor(clover$group)

# Specify linear model with all effects and interactions

mymodel<-lm(lnwt~group*days, clover)

# Display the analysis of variance for sequential
# and partial sum of squares

anova(mymodel) # the interaction tests the null hypothesis. Low p-value => reject Ho.


                    
#=====================================================================================
# 3. Determine which group, if any, differs in RGR. Use the Bonferroni correction. [20]

# load the multcomp package to access glht()
library(multcomp)

# Create matrix whose rows are L vectors for the tests Bi=Bj
H0q3 <- rbind(B11_B13 =   c(0,0,0,0,2,1),
              B11_B12 =   c(0,0,0,0,1,-1),
              B12_B13 =   c(0,0,0,0,1,2))

# Use glht with bBonferroni correction for tests

tH0q3 <- glht(mymodel, linfct = H0q3)
summary(tH0q3, test = adjusted("bonferroni")) # All RGR's are different from each other.


                    
#=====================================================================================
# 4. Determine if the weight of plants grown in the lowest temperature for 37 days is significantly different from the weight of plants grown at the highest temperature for 22 days: First, (a) state Ho and Ha in words and with equations, or using statements about parameter values; then, (b) to test this hypothesis, build an L vector and use it in a linear hypothesis in R, and state your conclusion. [20]

# a) Ho: the weight of plants grown in the lowest temperature for 37 days is equal to the weight of plants grown at the highest temperature for 22 days.
# Yhat1.37 = Yhat3.22 or Yhat1.37 - Yhat3.22 = 0 or
# B01 + B11*37 = B03 + B13*22

# b) Yhat1.37 = B01 + B11*37 = (1, 1, 0, 37, 37,  0)*BetaVector
#    Yhat3.22 = B03 + B13*22 = (1,-1,-1, 22,-22,-22)*BetaVector
#  difference =                (0, 2, 1, 15, 59, 22)*BetaVector

H0q4 <-  as.matrix(rbind(Y1.37_Y3.22=c(0,2,1,15,59,22)))
tH0q4 <- glht(mymodel, linfct = H0q4)
summary(tH0q4) # wwights do not differ significantly.
                    
#=====================================================================================
# 5. Why would the above analysis be incorrect if the same 3 plants per temperature treatment had been used throughout the experiment as depicted below? What typical assumption of regression would probably be violated? Why? Assume that the plants are not destroyed when measured (if you have trouble assuming this, imagine that you are measuring height instead of mass). Assume that the plants are not in the same greenhouses. [20]

# Whenever the same experimental material is measured more than once over space or time, there is a repeated measurement. Repeated measurements may lead to violation of the assumption of uncorrelated errors, because it is likely that any effect of unmeasured variables that is present in one location or at one time in the experimental unit will also be present in other places or times. The measurements on the plants may show temporal autocorrelation. For example, if a plant is partially eaten by a caterpillar it will have a negative residual that persists over time. The plant will be smaller than expected based on the whole set that include mostly ungrazed plants.


  