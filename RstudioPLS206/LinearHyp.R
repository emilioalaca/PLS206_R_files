## Using package multcomp for multiple comparisons and linear hypotheses

library(multcomp)
library(car)
library(ggplot2)

## Example of ANCOVA with emphasis on interactions: filaree =====

filaree <- read.csv("xmpl_filaree.csv") # Make sure you have the correct path
filaree$Tgroup<-as.factor(filaree$Tgroup) # Tgroup should be a nominal variable
filaree$day <- as.numeric(filaree$day)
options(contrasts=c("contr.sum", "contr.poly"))

# Explore data
scatterplot(filaree$lnwt ~ filaree$day | filaree$Tgroup, boxplot="xy")

m1 <- lm(lnwt~Tgroup*day, data=filaree)
anova(m1)
summary(m1)
coef(m1)
Anova(m1, type=3)
plot(m1) # graphical check of assumptions

# Plot the data and regression lines.

qplot(day, lnwt, data = filaree, colour = Tgroup, size=2) + geom_smooth(method = "lm", se = TRUE, size=1)

## Create a matrix where each row is a set of coefficients to calculate
## a linear combination of estimated parameters.
## Check order of estimated coefficients first.
coef(m1)
H0s <- rbind(B0 =        c(1,0,0,0,0,0),
             dB01 =      c(0,1,0,0,0,0),
             dB02 =      c(0,0,1,0,0,0),
             dB03 =      c(0,-1,-1,0,0,0),
             B11 =       c(0,0,0,1,1,0),
             B13 =       c(0,0,0,1,-1,-1),
             B11_B13 =   c(0,0,0,0,2,1),
             Y1.37 =     c(1,1,0,37,37,0),
             Y3.22 =     c(1,-1,-1,22,-22,-22),
             Y1.37_Y3.22=c(0,2,1,15,59,22))
## Create object with tests and ask for summary
tH0s <- glht(m1, linfct = H0s)
summary(tH0s, test = univariate())
summary(tH0s, test = adjusted("bonferroni"))

## Simultaneous test of differences between relative growth rates (slopes)
H01s <- rbind(B12_B11 =   c(0,0,0,0,-1,1),
              B13_B11 =   c(0,0,0,0,-2,-1),
              B13_B12 =   c(0,0,0,0,-1,-2))
tH01s <- glht(m1, linfct = H01s)
summary(tH01s, test = adjusted("bonferroni"))

## USING lht from car (linearHypothesis)

lht(m1, "day=0") # Tests that the effect of "day" is zero, etc
lht(m1, "day=0.1")
lht(m1, "day=0.095")
lht(m1, "(Intercept) + 20 * day = 4.5")



## A SECOND and OPTIONAL EXAMPLE =========================================
## Example of ANCOVA with mice litter weight data ==
## The main reason for the covariates is correction before comparisons
## No interactions are included as they are assumed to be nil.
data("litter")
head(litter)
help(litter) # read description of data set
str(litter)

## There are two covariates that can be used to correct litter weight:
## gestation time (gesstime) and number of pups (number)
## Example from package help

### fit ANCOVA model to data
amod <- aov(weight ~ dose + gesttime + number, data = litter)

### define matrix of linear hypotheses for `dose'
(doselev <- as.integer(levels(litter$dose)))

### create a named vector with number of observations in each group
n <- summary(litter$dose) #creates a vector with 4 numbers labeled with dose levels

### create K matrix with coeficients for linear hypotheses
### otrend is ordinal trend, atrend is linear and ltrend is logarithmic
K <- rbind(contrMat(n, "Tukey"),
           otrend = c(-1.5, -0.5, 0.5, 1.5),
           atrend = doselev - mean(doselev),
           ltrend = log(1:4) - mean(log(1:4)))

### set up multiple comparison or test object
Kht <- glht(amod, linfct = mcp(dose = K), alternative = "less")

### cf. Westfall (1997, Table 2)
summary(Kht, test = univariate())
summary(Kht, test = adjusted("bonferroni"))
summary(Kht, test = adjusted("Shaffer"))
summary(Kht, test = adjusted("Westfall"))
summary(Kht, test = adjusted("single-step"))
## END OF SECOND EXAMPLE ===================
