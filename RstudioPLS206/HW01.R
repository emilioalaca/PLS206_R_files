# Simulation for HW01

# Read the data file with the "original" or actual field observations.
d1 <- read.csv(file="../Examples/PfertParSim.txt", header=TRUE)
head(d1)

# Create the model to represent the data generating process (DGP).
# Note that we will use a simple linear regression and normality assumptions,
# although the actual DGP is a quadratic model.

G0 <- 200
G1 <- 50
G2 <- -2 # note the "-" sign
S<-100

V <- d1$Pfert

W.str <- G0 + G1 * V + G2 * V^2 # "true" structural part of the model

(Y.9 <- G0 + G1 * 9 + G2 * 9^2)
(Y.18 <- G0 + G1 * 18 + G2 * 18^2)
  
W <- W.str + rnorm(length(V), 0, S)

results <- as.data.frame(matrix(NA, nrow = 5000, ncol = 4))
names(results) <- c("individualW.V9", "individualW.V18", "W.hat.V9", "W.hat.V18" )

for (i in 1:5000) {
  W <- W.str + rnorm(length(V), 0, S)
  m1 <- lm(W ~ V)
  results[i,] <-c( W[min(which(V==9))], W[min(which(V==18))], predict(m1,data.frame(V=9)), predict(m1,data.frame(V=18)))
}

head(results)

sqrt(diag(var(na.omit(results))))

100^2*(1/length(V)+(9-mean(V))^2/sum((V-mean(V))^2))
100^2*(1/length(V)+(18-mean(V))^2/sum((V-mean(V))^2))

apply(results, 2, mean)

m2 <- lm(yield ~ Pfert, data=d1)
summary(m2)

predict(m2, data.frame(Pfert=c(9,18)), se.fit=TRUE)

apply