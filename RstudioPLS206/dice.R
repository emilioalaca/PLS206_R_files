# Dice and slice

my.score <- function() sum(sort(sample (1:6, 7, replace=TRUE), )[5:7])

hist(apply(as.matrix(1:100000), 1, function(x) sum(sort(sample (1:6, 7, replace=TRUE))[5:7])))

# Probability that score is less than x using n simulations

p.score.lt <- function (x, n=1000) sum(apply(as.matrix(1:n), 1, function(x) sum(sort(sample (1:6, 7, replace=TRUE))[5:7])) < x)/n

p.score.lt2 <- function (x, n=1000) {
  less.than <- 0
  for (i in 1:n) {
    less.than <-less.than + as.integer(sum(sort(sample (1:6, 7, replace=TRUE))[5:7]) < x)
  }
  less.than/n
  
}
