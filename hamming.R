y <- rep(1:2, each=50)                          # True class memberships
x <- y %*% t(rep(1, 20)) + rnorm(100*20) < 1.5  # Dataset with 20 variables
design.set <- sample(length(y), 50)
test.set <- setdiff(1:100, design.set)

# Calculate distance and nearest neighbors
library(e1071)
d <- hamming.distance(x)
NN <- apply(d[test.set, design.set], 1, order)

# Predict class membership of the test set
k <- 5
pred <- apply(NN[, 1:k, drop=FALSE], 1, function(nn){
  tab <- table(y[design.set][nn])
  as.integer(names(tab)[which.max(tab)])      # This is a pretty dirty line
}

# Inspect the results
table(pred, y[test.set])

library(e1071)
H <- hamming.distance(x) 
x1 = c(1,0,0)
x2 = c(1,1,0)
sum(x1 == x2)/length(x1)
x = data.frame(x1,x2)
x
