y <- rep(1:2, each=50)                          # True class memberships
x <- y %*% t(rep(1, 20)) + rnorm(100*20) < 1.5  # Dataset with 20 variables
design.set <- sample(length(y), 80)
test.set <- setdiff(1:100, design.set)

x =data.frame(a = c(1,1,3),b = c(2,3,2),c = c(1,2,4))
similarity = function(x){
      y = data.frame()
      
      for(i in 1:nrow(x)){
        for(j in 1:nrow(x)){
          y[i,j] = sum(x[i,] == x[j,])/ncol(x)
          if (i==j){
            y[i,j] = 0
          }
        }
      }
      return(as.matrix(y))
    }

d= similarity(x)
# Calculate distance and nearest neighbors
library(e1071)
d <- hamming.distance(x)


# Predict class membership of the test set

myKnn = function(test.set,train.set,NN,k){
  d = similarity(x)
  NN <- apply(d[train.set, test.set], 1, order)
  k <- 5
  pred <- apply(NN[, 1:k, drop=FALSE], 1, function(nn){
    tab <- table(y[test.set][nn])
    as.integer(names(tab)[which.max(tab)])      
  })
  return(pred)
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
