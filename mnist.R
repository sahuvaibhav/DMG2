install.packages("R.matlab")
library(R.matlab)
setwd("F:\\Analytics\\ISB Study\\DM2\\DMG2")
data_MNIST <- readMat('mnist.mat')
head(data_MNIST)
str(data_MNIST)
head(data_MNIST$train9)

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

#================ LDA ===================================

library(caret)
library(MASS)
library(dplyr)

#Remove zero variance columns
x = nearZeroVar(data_final, saveMetrics = TRUE)
str(x, vec.len=2)
zero_Var_Cols = row.names(x[x[,"zeroVar"] > 0,])

data_filtered = data_final[,-c(as.numeric(zero_Var_Cols))]



MNIST.lda <- lda(class ~ ., data=data_filtered)

data_3 = data_filtered[sample(as.numeric(row.names(data_filtered[data_filtered$class == 3,])),50),]
data_5 = data_filtered[sample(as.numeric(row.names(data_filtered[data_filtered$class == 5,])),50),]
data_8 = data_filtered[sample(as.numeric(row.names(data_filtered[data_filtered$class == 8,])),50),]

data_358 = rbind(data_3,data_5,data_8)

MNIST.lda.values <- predict(MNIST.lda,data_358)
ldahist(data = MNIST.lda.values$x[,1],g=data_358$class )

plot(MNIST.lda.values$x[,1],MNIST.lda.values$x[,2]) 
text(MNIST.lda.values$x[,1],MNIST.lda.values$x[,2],data_358$class,cex=0.7,pos=4,col=data_358$class)

mnist_pred = as.data.frame(MNIST.lda.values$x)
ggplot(mnist_pred,aes(x=mnist_pred$LD1, y=mnist_pred$LD2)) + 
  geom_point(shape=21,aes(fill=factor(data_358$class),color = factor(data_358$class)),size=4) +
             geom_text(label=data_358$class,vjust=1.5)



data_1 = data_filtered[sample(as.numeric(row.names(data_filtered[data_filtered$class == 1,])),50),]
data_7 = data_filtered[sample(as.numeric(row.names(data_filtered[data_filtered$class == 7,])),50),]
data_9 = data_filtered[sample(as.numeric(row.names(data_filtered[data_filtered$class == 9,])),50),]

data_179= rbind(data_1,data_7,data_9)

MNIST.lda.values <- predict(MNIST.lda,data_179)
ldahist(data = MNIST.lda.values$x[,1],g=data_179$class )

plot(MNIST.lda.values$x[,1],MNIST.lda.values$x[,2]) 
text(MNIST.lda.values$x[,1],MNIST.lda.values$x[,2],data_179$class,cex=0.7,pos=4,col=data_179$class)

mnist_pred = as.data.frame(MNIST.lda.values$x)
ggplot(mnist_pred,aes(x=mnist_pred$LD1, y=mnist_pred$LD2)) + 
  geom_point(shape=21,aes(fill=factor(data_179$class),color = factor(data_179$class)),size=4) +
  geom_text(label=data_179$class,vjust=1.5) 


#===================== PCA ===================================

mnist.pca <- prcomp(data_filtered[,-785],center = TRUE,scale. = TRUE) 
print(mnist.pca)
plot(mnist.pca, type = "l")
summary(mnist.pca)

MNIST.pca.values = predict(mnist.pca, newdata=data_358)
mnist_pred = as.data.frame(MNIST.pca.values)
ggplot(mnist_pred,aes(x=mnist_pred$PC1, y=mnist_pred$PC2)) + 
  geom_point(shape=21,aes(fill=factor(data_358$class),color = factor(data_358$class)),size=4) +
  geom_text(label=data_358$class,vjust=1.5) 

MNIST.pca.values = predict(mnist.pca, newdata=data_179)
mnist_pred = as.data.frame(MNIST.pca.values)
ggplot(mnist_pred,aes(x=mnist_pred$PC1, y=mnist_pred$PC2)) + 
  geom_point(shape=21,aes(fill=factor(data_179$class),color = factor(data_179$class)),size=4) +
  geom_text(label=data_179$class,vjust=1.5) 


require(caret)
trans = preProcess(data_filtered[,-785], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC = predict(trans, data_358)
ggplot(mnist_pred,aes(x=PC$PC1, y=PC$PC2)) + 
  geom_point(shape=21,aes(fill=factor(data_358$class),color = factor(data_358$class)),size=4) +
  geom_text(label=data_358$class,vjust=1.5) 

PC = predict(trans, data_179)
ggplot(mnist_pred,aes(x=PC$PC1, y=PC$PC2)) + 
  geom_point(shape=21,aes(fill=factor(data_179$class),color = factor(data_179$class)),size=4) +
  geom_text(label=data_179$class,vjust=1.5) 


#=================== Logistic Regression ========================
trainIndex <- createDataPartition(data_final$class, p=0.60, list=FALSE)
data_train <- data_final[trainIndex,]
data_test <- data_final[-trainIndex,]
data_train$class = factor(data_train$class)
data_test$class = factor(data_test$class)
library(nnet)
log_all = multinom(factor(class)~.,data=data_train,MaxNWts = 10000)
summary(log_all)
confint(log_all)
exp(coef(log_all))
log_all_pred = predict(log_all, newdata = data_test)


MNIST.lda.values <- predict(MNIST.lda,data_train)
mnist_pred = as.data.frame(MNIST.lda.values$x)
train_log_lda = cbind(MNIST.lda.values$class,mnist_pred)
log_lda = multinom(MNIST.lda.values$class~., data = train_log_lda)
summary(log_lda)



MNIST.pca.values = predict(mnist.pca, newdata=data_train)
mnist_pred = as.data.frame(MNIST.pca.values)
train_log_lda = cbind(data_train$class,mnist_pred)
log_pca = multinom(data_train$class~PC1+PC2+PC4+PC5+PC6+PC7+PC1+PC8+PC9, data = train_log_lda)
summary(log_pca)



#=============================KNN =================================

