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

d= similarity(data)
# Calculate distance and nearest neighbors
library(e1071)
d <- hamming.distance(x)

train.set = row.names(data_train)
test.set = row.names(data_test)
# Predict class membership of the test set

data_temp = data[c(1:100),]
trainIndex <- createDataPartition(data_temp$class, p=0.60, list=FALSE)
data_train <- data_temp[trainIndex,]
data_test <- data_temp[-trainIndex,]





myKnn = function(train,test,class,k){
  if(ncol(train)!=ncol(test)){
    print("Number of features in train and test datasets should be same.")
  }
  else if(nrow(train)!= length(class)){
    print("Number of classse and number of rows in train datasets should be same.")
  } else {
    y=data.frame()
    for(i in 1:nrow(test)){
      for(j in 1:nrow(train)){
        y[i,j] = sum(test[i,] == train[j,])/ncol(train)
        
      }
    }
    
    NN <- t(apply(y, 1, function(x){order(x,decreasing=T)}))
    pred <- apply(NN[, 1:k, drop=FALSE], 1, function(nn){
      tab <- table(class[nn])
      names(tab)[which.max(tab)]
    })
    return(pred)
  }
}


pred = myKnn(data_train[,c(2:23)],data_test[,c(2:23)],data_train$class,k=1)
accuracy_1 = confusionMatrix(pred, data_test$class)$overall["Accuracy"]


myKnn = function(test.set,train.set,NN,k){
  d = similarity(data_temp)
  NN <- apply(d[train.set, test.set], 1, order)
  k <- 5
  pred <- apply(NN[, 1:k, drop=FALSE], 1, function(nn){
    tab <- table(y[design.set][nn])
    as.integer(names(tab)[which.max(tab)])      
  })
  return(pred)
}




similarity = function(x,y){
  for 
  
  
}


for(i in 1:nrow(data_test)){
  for( j in 1:nrow(data_train)){
    
    
    
  }
  
  
}

row.names(data_test)
