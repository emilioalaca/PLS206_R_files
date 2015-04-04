# http://www.math.wustl.edu/~victor/classes/ma322/r-eg-12.txt


Example R programs and commands
12. Multivariate scatterplots and MANOVAs

# All lines preceded by the "#" character are my comments.
# All other left-justified lines are my input.
# All other indented lines are the R program output.

# Generate some normally distributed random points:
x <- rnorm(50,0,1)     # 50 copies of N(0,1)
y <- rnorm(50,0,10)    # 50 copies of N(0,10)
xy <- cbind(x,y)       # 50 coordinate pairs (x,y)
plot(xy)

# Rotation of coordinates:
rot <- matrix(c(1,-1,1,1),nrow=2,ncol=2)
xy0 <- xy %*% rot  # rotate by 45 degrees
plot(xy0)

# Repeat to get 50 more rotated and shifted bivariate normals:
x <- rnorm(50,0,1)     # 50 copies of N(0,1)
y <- rnorm(50,0,10)    # 50 copies of N(0,10)
xy <- cbind(x,y)
xy1 <- xy %*% rot + 1  # rotate by 45 degrees and shift by (1,1)
plot(xy1)

# Combine the two 50-point data sets and label them:
xys<-rbind(xy0,xy1)
hilo <- factor(gl(2,50,len=2*50)) # label "1" for xy0, "2" for xy1 
blackred<-c(gl(2,50))
plot(xys, col=blackred)        # Black for xy0, red for xy1

# Plot the means and ranges of the individual coordinates:
plot(xys[,1], col=blackred)          # first coordinate values 
plot(xys[,1]~hilo)            # first coordinate box plot 
plot(xys[,2], col=blackred)          # second coordinate values
plot(xys[,2]~hilo)            # second coordinate box plot

# Accumulate the sums of squares:
fit <- manova(xys~hilo)

# ANOVA on the individual coordinates:
summary.aov(fit)

# MANOVA on the pairs of coordinates:
summary.manova(fit)


###################################################
# Single-factor MANOVA on tabulated data:
#
# Suppose we are given the following centipede
# haemolymph amino acid data:
#
#  Male centipedes:
#
#  Alanine  Aspartic Acid  Tyrosine
#  -------  -------------  --------
#    7.0         17.0        19.7
#    7.3         17.2        20.3
#    8.0         19.3        22.6
#    8.1         19.8        23.7
#    7.9         18.4        22.0
#
# Female centipedes:
#
#  Alanine  Aspartic Acid  Tyrosine
#  -------  -------------  --------
#    7.3         17.4        22.5
#    7.7         19.8        24.9
#    8.2         20.2        26.1
#    8.3         22.6        27.5
#    6.4         23.4        28.1
#
# Read the data from the table using "scan()":
data=scan()
7.0         17.0        19.7
7.3         17.2        20.3
8.0         19.3        22.6
8.1         19.8        23.7
7.9         18.4        22.0
7.3         17.4        22.5
7.7         19.8        24.9
8.2         20.2        26.1
8.3         22.6        27.5
6.4         23.4        28.1
#
# Each measurement is a row of 3 values, so make a matrix:
#   <> "ncol=3" since there are three coordinates per measurement
#   <> "byrow=TRUE" to fill the matrix by rows
#   <> "dimnames" is optional but it puts nice labels atop the columns;
#		  the first NULL means just default labels on the rows.
aacon<-matrix(data,ncol=3,byrow=TRUE,dimnames=list(NULL,c("Ala","Asp","Tyr")))
aacon
#
# Make factor labels for the single factor:
#  <> the first 5 triples are for males, the next 5 for females
#  <> encapsulating "gl()" within "factor()" is optional
gender<-factor(gl(2,5,10,labels=c("Male","Female")))
gender
#
# Function "manova()" computes the sums of squares and crossed
# products and labels them for use in ANOVAs and MANOVAs:
manova(aacon~gender)

# Get a p-value for the effect of gender on the combination of 3
#  measurements with the "summary()" function.  If applied to the
#  output of "manova()" it uses Pillai's statistic by default.
#   
summary(manova(aacon~gender))
# Use Wilks', Hotelling-Lawley's, or Roy's statistics instead:
summary(manova(aacon~gender),test="W")
summary(manova(aacon~gender),test="H")
summary(manova(aacon~gender),test="R")

# Perform 3 ANOVAs on the individual responses:
summary(aov(aacon~gender))
# Note: alternative calls are possible:
summary.aov(manova(aacon~gender))        
summary.manova(aov(aacon~gender))
summary.aov(aov(aacon~gender))
summary.manova(manova(aacon~gender))



###################################################
# Multifactorial MANOVA
# 
# Suppose we are given the following bird plasma calcium and water
# loss data:
#
# No Hormone Treatment:
# --------------------------------------------
#        Female                   Male
# --------------------    --------------------
# Plasma Ca   H20 loss    Plasma Ca   H20 loss
# ---------   --------    ---------   --------
#    16.5         76          14.5        80
#    18.4         71          11.0        72
#    12.7         64          10.8        77
#
# Hormone Treatment:
# --------------------------------------------
#        Female                   Male
# --------------------    --------------------
# Plasma Ca   H20 loss    Plasma Ca   H20 loss
# ---------   --------    ---------   --------
#      39.1       71          32.0       65
#      26.2       70          23.8       69
#      21.3       63          28.8       67
#
# Read the data:
data<-scan()
16.5       76          14.5       80
18.4       71          11.0       72
12.7       64          10.8       77
39.1       71          32.0       65
26.2       70          23.8       69
21.3       63          28.8       67
#
# Put the data into a matrix:
#   <> "ncol=2" since there are two coordinates per measurement
#   <> "byrow=TRUE" to fill the matrix by rows
#   <> "dimnames" is optional but it puts nice labels atop the columns;
#		  the first NULL means just default labels on the rows.
#
bird<-matrix(data,ncol=2,byrow=TRUE,dimnames=list(NULL,c("PLCA","H2OL")))
bird
#
# Make factor labels for the two factors:
#  <> the first 6 rows are for "no hormone treatment", the next 6 for
#     "hormone treatment".
#  <> rows are alternating "male" and "female".
#  <> there are 12 measurements in all.  This is the default for
#     "hormone" but must be specified for "gender".
#  <> encapsulating "gl()" within "factor()" is optional.
#
hormone<-factor(gl(2,6,labels=c("No hormone treatment","Hormone treatment")))
gender<-factor(gl(2,1,12,labels=c("Female","Male")))
#
# 
# Function "manova()" computes the sums of squares and crossed
# products and labels them for use in ANOVAs and MANOVAs:
#
#  All factors and interactions:
#
manova(bird~hormone*gender)

# Get a p-value for the effect of gender on the combination of 3
#  measurements with the "summary()" function.  If applied to the
#  output of "manova()" it uses Pillai's statistic by default.
#   
summary(manova(bird~hormone*gender))
# Use Wilks', Hotelling-Lawley's, or Roy's statistics instead:
summary(manova(bird~hormone*gender),test="W")
summary(manova(bird~hormone*gender),test="H")
summary(manova(bird~hormone*gender),test="R")

# Perform ANOVAs on the individual responses:
summary(aov(bird~hormone*gender))

# Note: alternative calls are possible:
summary.aov(manova(bird~hormone*gender))
summary.manova(aov(bird~hormone*gender))
summary.aov(aov(bird~hormone*gender))
summary.manova(manova(bird~hormone*gender))