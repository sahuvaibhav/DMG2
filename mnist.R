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
trans = preProcess(data_filtered[,-785],method=c("BoxCox", "center","scale", "pca"))

PC = predict(trans, data_358)
ggplot(mnist_pred,aes(x=PC$PC1, y=PC$PC2)) + 
  geom_point(shape=21,aes(fill=factor(data_358$class),color = factor(data_358$class)),size=4) +
  geom_text(label=data_358$class,vjust=1.5) 

PC = predict(trans, data_179)
ggplot(mnist_pred,aes(x=PC$PC1, y=PC$PC2)) + 
  geom_point(shape=21,aes(fill=factor(data_179$class),color = factor(data_179$class)),size=4) +
  geom_text(label=data_179$class,vjust=1.5) 


#=================== Logistic Regression ========================
library(ROCR)

data_accuracy =data.frame()

row =1
for(i in seq(0,8,by=1)){
  for(j in seq(i+1,9,by=1)){
      print(paste(i,j,sep = "-"))
      data_temp = data_filtered[data_filtered$class %in% c(i,j),]
      trainIndex <- createDataPartition(data_temp$class, p=0.60, list=FALSE)
      data_temp_train <- data_temp[trainIndex,]
      data_temp_test <- data_temp[-trainIndex,]
      data_temp_train$class = factor(data_temp_train$class)
      data_temp_test$class = factor(data_temp_test$class)
      # Logistic Regression Using All Variables
      print('logAll')
      log_reg_all = glm(class~., data =data_temp_train, family="binomial" )
      prob <- predict(log_reg_all, newdata=data_temp_test, type="response")
      pred <- prediction(prob, data_temp_test$class)
      aucAll <- performance(pred, measure = "auc")@y.values[[1]]
      # Logistic Regression LDA(9)
      print('logLDA')
      lda_train_pred <- as.data.frame(predict(MNIST.lda,data_temp_train)$x)
      lda_train_pred = cbind(lda_train_pred,data_temp_train$class)
      names(lda_train_pred)[10] = 'class'
      log_reg_lda = glm(class~.,data = lda_train_pred, family = "binomial")
      
      lda_test_pred <- as.data.frame(predict(MNIST.lda,data_temp_test)$x)
      prob <- predict(log_reg_lda, newdata=lda_test_pred, type="response")
      pred <- prediction(prob, data_temp_test$class)
      aucLDA <- performance(pred, measure = "auc")@y.values[[1]]
      # Logistic Regression PCA(9)
      print('logPCA')
      data_temp_train$class = as.numeric(data_temp_train$class)
      data_temp_test$class = as.numeric(data_temp_test$class)
      PC_train = predict(trans, data_temp_train)
      PC_train_pred = cbind(PC_train[,c(1:9)], data_temp_train$class)
      names(PC_train_pred)[10] = 'class'
      log_reg_pca = glm(factor(class)~.,data = PC_train_pred, family = "binomial")
      PC_test_pred <- predict(trans,data_temp_test)
      prob <- predict(log_reg_pca, newdata=PC_test_pred, type="response")
      pred <- prediction(prob, data_temp_test$class)
      aucPCA <- performance(pred, measure = "auc")@y.values[[1]]
      #Store Accuracry for each class Pair
      data_accuracy[row,1] = i
      data_accuracy[row,2] = j
      data_accuracy[row,3] = aucAll
      data_accuracy[row,4] = aucLDA
      data_accuracy[row,5] = aucPCA
      
      row = row+1
  }
}

names(data_accuracy) = c("Class_1","Class_2","Acc_All","Acc_LDA","Acc_PCA")

write.csv(data_accuracy,"Log_reg_Acc.csv",row.names =F)

#===============================Knn==================================================
data_sample = data.frame()
for(i in seq(0,9,by=1)){
  n_rows = as.numeric(row.names(data_filtered[data_filtered$class == i,]))
  n_train = sample(as.numeric(row.names(data_filtered[data_filtered$class == i,])),50)
  n_test = sample(setdiff(n_rows,n_sample),50)
  
  data_sample_train  = rbind(data_sample_train,data_filtered[n_train,])
  data_sample_test  = rbind(data_sample_test,data_filtered[n_test,])
}

#No Transformation 
mnist_knn_1 <- knn(train = data_sample_train[,c(1:784)], test = data_sample_test[c(1:784)], cl = data_sample_train$class, k=1)
mnist_knn_3 <- knn(train = data_sample_train[,c(1:784)], test = data_sample_test[c(1:784)], cl = data_sample_train$class, k=3)
mnist_knn_5 <- knn(train = data_sample_train[,c(1:784)], test = data_sample_test[c(1:784)], cl = data_sample_train$class, k=5)
mnist_knn_7 <- knn(train = data_sample_train[,c(1:784)], test = data_sample_test[c(1:784)], cl = data_sample_train$class, k=7)

predict_1 <- confusionMatrix(data_sample_test$class, mnist_knn_1)
acc_1 = (predict_1$overall["Accuracy"])

predict_3 <- confusionMatrix(data_sample_test$class, mnist_knn_3)
acc_3 = (predict_3$overall["Accuracy"])

predict_5 <- confusionMatrix(data_sample_test$class, mnist_knn_5)
acc_5 = (predict_5$overall["Accuracy"])

predict_7 <- confusionMatrix(data_sample_test$class, mnist_knn_7)
acc_7 = (predict_7$overall["Accuracy"])

#LDA Knn

lda_knn_train_pred <- as.data.frame(predict(MNIST.lda,data_sample_train)$x)
lda_knn_test_pred <- as.data.frame(predict(MNIST.lda,data_sample_test)$x)

lda_knn_1 <- knn(train = lda_knn_train_pred, test = lda_knn_test_pred, cl = data_sample_train$class, k=1)
lda_knn_3 <- knn(train = lda_knn_train_pred, test = lda_knn_test_pred, cl = data_sample_train$class, k=3)
lda_knn_5 <- knn(train = lda_knn_train_pred, test = lda_knn_test_pred, cl = data_sample_train$class, k=5)
lda_knn_7 <- knn(train = lda_knn_train_pred, test = lda_knn_test_pred, cl = data_sample_train$class, k=7)

lda_predict_1 <- confusionMatrix(data_sample_test$class, mnist_knn_1)
lda_acc_1 = (lda_predict_1$overall["Accuracy"])

lda_predict_3 <- confusionMatrix(data_sample_test$class, mnist_knn_3)
lda_acc_3 = (lda_predict_3$overall["Accuracy"])

lda_predict_5 <- confusionMatrix(data_sample_test$class, mnist_knn_5)
lda_acc_5 = (lda_predict_5$overall["Accuracy"])

lda_predict_7 <- confusionMatrix(data_sample_test$class, mnist_knn_7)
lda_acc_7 = (lda_predict_7$overall["Accuracy"])


#PCA Knn
data_sample_train$class = as.numeric(data_sample_train$class)
data_sample_test$class = as.numeric(data_sample_test$class)
PC_knn_train = predict(trans, data_sample_train)
PC_knn_test = predict(trans, data_sample_test)


Pca_knn_1 <- knn(train = PC_knn_train, test = PC_knn_test, cl = data_sample_train$class, k=1)
Pca_knn_3 <- knn(train = PC_knn_train, test = PC_knn_test, cl = data_sample_train$class, k=3)
Pca_knn_5 <- knn(train = PC_knn_train, test = PC_knn_test, cl = data_sample_train$class, k=5)
Pca_knn_7 <- knn(train = PC_knn_train, test = PC_knn_test, cl = data_sample_train$class, k=7)

Pca_predict_1 <- confusionMatrix(data_sample_test$class, mnist_knn_1)
Pca_acc_1 = (lda_predict_1$overall["Accuracy"])

Pca_predict_3 <- confusionMatrix(data_sample_test$class, mnist_knn_3)
Pca_acc_3 = (lda_predict_3$overall["Accuracy"])

Pca_predict_5 <- confusionMatrix(data_sample_test$class, mnist_knn_5)
Pca_acc_5 = (lda_predict_5$overall["Accuracy"])

Pca_predict_7 <- confusionMatrix(data_sample_test$class, mnist_knn_7)
Pca_acc_7 = (lda_predict_7$overall["Accuracy"])

data_knn_acc = data.frame(c(1,3,5,7),c(acc_1,acc_3,acc_5,acc_7),c(lda_acc_1,lda_acc_3,lda_acc_5,lda_acc_7),c(Pca_acc_1,Pca_acc_3,Pca_acc_5,Pca_acc_7))
names(data_knn_acc) = c('k','NoTransformation',"LDA(9),","PCA(9)")

write.csv(data_knn_acc,"knn_Acc.csv",row.names =F)

#==============================Bayesian Classifier ============================================
data_nb_accuracy = data.frame()
for(i in seq(0,8,by=1)){
  for(j in seq(i+1,9,by=1)){
    print(paste(i,j,sep = "-"))
    data_temp = data_filtered[data_filtered$class %in% c(i,j),]
    trainIndex <- createDataPartition(data_temp$class, p=0.60, list=FALSE)
    data_nb_train <- data_temp[trainIndex,]
    data_nb_test <- data_temp[-trainIndex,]
    model <- naiveBayes(class ~ ., data = data_nb_train)
    predic_nb <- predict(model, data_nb_test[,1:784])
    acc_all = confusionMatrix(predic_nb, data_nb_test$class)$overall["Accuracy"]
    
    lda_nb_train_pred <- as.data.frame(predict(MNIST.lda,data_nb_train)$x)
    lda_nb_train_pred = cbind(lda_nb_train_pred,data_nb_train$class)
    names(lda_nb_train_pred)[10] = 'class'
    lda_nb_test_pred <- as.data.frame(predict(MNIST.lda,data_nb_test)$x)
    model_lda <- naiveBayes(class ~ ., data = lda_nb_train_pred)
    predic_nb <- predict(model_lda, lda_nb_test_pred)
    acc_lda = confusionMatrix(predic_nb, data_nb_test$class)$overall["Accuracy"]
    
    data_nb_train$class = as.numeric(data_nb_train$class)
    data_nb_test$class = as.numeric(data_nb_test$class)
    PC_nb_train = predict(trans, data_nb_train)
    PC_nb_train_pred = cbind(PC_nb_train[,c(1:9)], data_nb_train$class)
    names(PC_nb_train_pred)[10] = 'class'
    PC_nb_test_pred <- predict(trans,data_nb_test)
    model <- naiveBayes(class ~ ., data = PC_nb_train_pred)
    predic_nb <- predict(model, PC_nb_test_pred)
    acc_pca = confusionMatrix(predic_nb, PC_nb_test_pred$class)$overall["Accuracy"]
    
    data_nb_accuracy[row,1] = i
    data_nb_accuracy[row,2] = j
    data_nb_accuracy[row,3] = acc_all
    data_nb_accuracy[row,4] = acc_lda
    data_nb_accuracy[row,5] = acc_pca
    
    row = row+1
  }
}

names(data_nb_accuracy) = c("Class_1","Class_2","Acc_All","Acc_LDA","Acc_PCA")

write.csv(data_nb_accuracy,"NB_Acc.csv",row.names =F)

