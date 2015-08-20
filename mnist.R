install.packages("R.matlab")
library(R.matlab)
setwd("C:\\Users\\Vaibhav\\Desktop\\ISB\\DMG2")
data_MNIST <- readMat('mnist.mat')
head(data_MNIST)
str(data_MNIST)
head(data_MNIST$train0)

data_final =data.frame()
for(i in seq(0,9)){
  train = data.frame(data_MNIST[paste('train',i,sep='')])
  names(train) = c(1:784)
  train$class = i
  test = as.data.frame(data_MNIST[paste('test',i,sep='')])
  names(test) = c(1:784)
  test$class = i
  data_final = rbind(data_final,train,test)
}



require(caret)
library(MASS)


#Remove zero variance columns
x = nearZeroVar(data_final, saveMetrics = TRUE)
str(x, vec.len=2)
zero_Var_Cols = row.names(x[x[,"zeroVar"] > 0,])

data_filtered = data_final[,-c(as.numeric(zero_Var_Cols))]


MNIST.lda <- lda(class ~ ., data=data_filtered)

MNIST.lda.values <- predict(MNIST.lda)
ldahist(data = MNIST.lda.values$x[,1],g=data_filtered$class )
