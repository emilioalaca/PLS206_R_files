# LINEAR MIXED MODELS AND BEYOND
# Mixed-Effects Models in S and S-PLUS. Pinheiro and Bates 2000. Springer.

library(nlme)
library(lattice)
system.file("scripts", package = "nlme")

data(Oats)
# 1.6  A Split-Plot Experiment
# A.15 Oats—Split-plot Experiment on Varieties of Oats
# These data have been introduced by Yates (1935) as an example of a split- plot design. The treatment structure used in the experiment was a 3×4 full factorial, with three varieties of oats and four concentrations of nitro- gen. The experimental units were arranged into six blocks, each with three whole-plots subdivided into four subplots. The varieties of oats were as- signed randomly to the whole-plots and the concentrations of nitrogen to the subplots. All four concentrations of nitrogen were used on each whole- plot.
# The data, presented in Figure 1.20 (p. 47), are analyzed in Venables and Ripley (1999, §6.11).
# The display formula for these data is
# yield ~ nitro | Block
# based on the columns named:
#   yield: the subplot yield (bushels/acre).
# nitro: nitrogen concentration (cwt/acre)—0.0, 0.2, 0.4, or 0.6. Block: a factor identifying the block—I through VI.
# Variety: oats variety—Golden Rain, Marvellous, or Victory.


class(Oats)
str(Oats)

# Physically, there are three levels of grouping of the experimental units: block, plot, and subplot. Because the treatments are randomly assigned at each level of grouping, we may be tempted to associate random effects with each level. However, because there is only one yield recorded for each subplot we cannot do this as we would saturate the model with random effects. We use a random intercept at each of the block and the whole plot levels.

fm1Oats.lis <- lmList( yield ~ nitro| Block/Variety, data=Oats)
fm1Oats <- lme( fixed=yield ~ ordered(nitro) * Variety, data = Oats,
                random = ~ 1 | Block/Variety )
anova( fm1Oats )
fm2Oats <- update( fm1Oats, yield ~ ordered(nitro) + Variety )
anova( fm2Oats)
summary( fm2Oats )
fm3Oats <- update( fm1Oats, yield ~ ordered( nitro ) )
summary( fm3Oats )
fm4Oats <-
  lme( yield ~ nitro, data = Oats, random = ~ 1 | Block/Variety )
summary( fm4Oats )
VarCorr( fm4Oats )
intervals( fm4Oats )
plot(augPred(fm4Oats), aspect = 2.5, layout = c(6, 3),
     between = list(x = c(0, 0, 0.5, 0, 0))) # produces Figure 1.21
plot(comparePred(fm4Oats,fm1Oats.lis)) # adds the within-group fits
plot(compareFits(coef(fm1Oats.lis),coef(fm4Oats)))
# Checking assumptions: only normality and homogeneity of variance

plot(fm4Oats)
bwplot(Block~residuals(fm4Oats), data=Oats)
bwplot(Variety~residuals(fm4Oats), data=Oats)
bwplot(ordered(nitro)~residuals(fm4Oats), data=Oats)
plot(intervals(fm1Oats.lis))

fm5Oats <- update(fm4Oats, random=~1|Block)
anova(fm4Oats,fm5Oats)