# Matrix approach to regression
# Why: without understanding (X'X)^-1 you can't go much further.

(A <- matrix(data = c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE))

class(A)

matrix(data = c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3) # not the same!

(B <- matrix(data = c(-1, 10, -1, 5, 5, 8), nrow = 2, ncol = 3, byrow = TRUE))
 
A+B

A-B


(y <- matrix(data = c(1,2,3), nrow = 3, ncol = 1, byrow = TRUE))

is.vector(y)

class(y)

x <- c(1,2,3)

class(x)

is.vector(x)

t(A)
dim(A)
dim(t(A))

A * B # element to element multiplication

A %*% B # not comformable, either these were never meant to be
#         multiplied or one or both are entered or calculated
#         incorrectly

A %*% t(B) # this works and differs from

t(B) %*% A # matrix multiplication is not commutative

# Inner vs. outer product.
y %*% y # y is a 3 x 1 matrix so it is not conformable with itself
y%*%t(y) # results in 3 x 3 natrix
t(y) %*% y

x %*% x # for vectors %*% is the inner product and R makes them conform

# look at the visual for multiplying matrices.

#   SIMPLE LINEAR REGRESSION

# use the P fertilization data

d1 <- read.csv(file="../Examples/PfertParSim.txt", header=TRUE)

slr <- lm(yield ~ Pfert, data=d1)
X <- model.matrix(slr) # get and see the model matrix
Y <- d1$yield # make response vector

# Equations derived by OLS indicate

beta_hat <- solve(t(X)%*%X) %*% t(X)%*%Y #solve() inverts the matrix
coef(slr)
beta_hat # right!
# beta_hat is a vector with two random variables,
# also known as a random vector. Theory says that under
# the assumptions Y = beta0 + beta1 * X + epsilon_i and
# epsilon_i ~ iid N(0,S), beta_hat is bivariate normal
# with covariance matrix equal to solve(t(X) %*% X) * S^2.
# Because we usually do not know S^2 we estimate it with the MSE.
# The MSE is the best estimate you can get of the variance of epsilon. Pretty much always for any model.

error <- Y - X %*% beta_hat
MSE <- as.numeric((t(error) %*% error) /(length(Y)-length(beta_hat)))
summary(slr)$sigma^2 # check against lm() internal calculation
(V_hat.beta_hat <- solve(t(X) %*% X) * MSE)
vcov(slr) # check against lm() internal calculation
