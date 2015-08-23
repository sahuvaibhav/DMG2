library(e1071)
library(caret)
library(FSelector)
library(ggplot2)
library(cowplot)
library(party)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

setwd("F:\\Analytics\\ISB Study\\DM2\\DMG2\\")

data = read.table("mushroom.txt",sep = ',',na.strings = "NA", stringsAsFactors=T)
names(data) = c('class','capshape','capsurface','capcolor','bruises','odor','gillattachment',
                'gillspacing','gillsize','gillcolor','stalkshape','stalkroot','stalksurfaceabovering',
                'stalksurfacebelowring','stalkcolorabovering','stalkcolorbelowring','veiltype',
                'veilcolor','ringnumber','ringtype','sporeprintcolor','population','habitat')

attach(data)
nrow(data)
levels(data$class)
data[data$stalkroot == '?',c(12)] = names(which.max(table(data[data$stalkroot != '?',]$stalkroot)))

length(data[data$stalkroot == '?',]$stalkroot)
table(data$stalkroot)



p1 = ggplot(aes(x=class), data=data)+geom_histogram(aes(fill=class))
p2 = ggplot(aes(x=capshape), data=data)+geom_histogram(aes(fill=capshape))
p3 = ggplot(aes(x=capsurface), data=data)+geom_histogram(aes(fill=capsurface))
p4 = ggplot(aes(x=capcolor), data=data)+geom_histogram(aes(fill=capcolor))
p5 = ggplot(aes(x=bruises), data=data)+geom_histogram(aes(fill=bruises))
p6 = ggplot(aes(x=odor), data=data)+geom_histogram(aes(fill=odor))
plot_grid(p1,p2,p3,p4,p5,p6,align = 'h')

p1 = ggplot(aes(x=gillattachment), data=data)+geom_histogram(aes(fill=gillattachment))
p2 = ggplot(aes(x=gillspacing), data=data)+geom_histogram(aes(fill=gillspacing))
p3 = ggplot(aes(x=gillsize), data=data)+geom_histogram(aes(fill=gillsize))
p4 = ggplot(aes(x=gillcolor), data=data)+geom_histogram(aes(fill=gillcolor))
p5 = ggplot(aes(x=stalkshape), data=data)+geom_histogram(aes(fill=stalkshape))
p6 = ggplot(aes(x=stalkroot), data=data)+geom_histogram(aes(fill=stalkroot))
plot_grid(p1,p2,p3,p4,p5,p6,align = 'h')

p1 = ggplot(aes(x=stalksurfaceabovering), data=data)+geom_histogram(aes(fill=stalksurfaceabovering))
p2 = ggplot(aes(x=stalksurfacebelowring), data=data)+geom_histogram(aes(fill=stalksurfacebelowring))
p3 = ggplot(aes(x=stalkcolorabovering), data=data)+geom_histogram(aes(fill=stalkcolorabovering))
p4 = ggplot(aes(x=stalkcolorbelowring), data=data)+geom_histogram(aes(fill=stalkcolorbelowring))
p5 = ggplot(aes(x=veiltype), data=data)+geom_histogram(aes(fill=veiltype))
p6 = ggplot(aes(x=veilcolor), data=data)+geom_histogram(aes(fill=veilcolor))
plot_grid(p1,p2,p3,p4,p5,p6,align = 'h')

p1 = ggplot(aes(x=ringnumber), data=data)+geom_histogram(aes(fill=ringnumber))
p2 = ggplot(aes(x=ringtype), data=data)+geom_histogram(aes(fill=ringtype))
p3 = ggplot(aes(x=sporeprintcolor), data=data)+geom_histogram(aes(fill=sporeprintcolor))
p4 = ggplot(aes(x=population), data=data)+geom_histogram(aes(fill=population))
p5 = ggplot(aes(x=habitat), data=data)+geom_histogram(aes(fill=habitat))
plot_grid(p1,p2,p3,p4,p5,align = 'h')

#Calculate Information Gain

infoGain = information.gain(class~.,data=data)
infoGain.sorted = infoGain[order(infoGain$attr_importance,decreasing = T),, drop=F]

GainRatio = gain.ratio(class~.,data=data)
GainRatio.sorted = GainRatio[order(GainRatio$attr_importance),, drop=F]

#=================Train Test Split===================
# x1 = as.numeric(rownames(data[data$class  == 'e',]))
# y1 = sample(x1,replace=F,size = 0.6*length(x1))
# x2 = as.numeric(rownames(data[data$class  == 'p',]))
# y2 = sample(x2,replace=F,size = 0.6*length(x2))
# 
# xTrain = data[c(y1,y2),-1]
# yTrain = data[c(y1,y2),1]
# xTest = data[c(setdiff(x1,y1),setdiff(x2,y2)),-1]
# yTest = data[c(setdiff(x1,y1),setdiff(x2,y2)),1]
# 
# Train = data[c(y1,y2),]
# Test = data[c(setdiff(x1,y1),setdiff(x2,y2)),]


trainIndex <- createDataPartition(data$class, p=0.60, list=FALSE)
data_train <- data[trainIndex,]
data_test <- data[-trainIndex,]

#===========================Naive Bayes Classification ========================

model_all <- naiveBayes(class ~ ., data = data_train)
predictions <- predict(model_all, data_test[,2:23])
mush_nb_all = confusionMatrix(predictions, data_test$class)$overall["Accuracy"]


k_top_5 = rownames(head(infoGain.sorted,5))

data_train_5 = data_train[,c('class',k_top_5)]
data_test_5 = data_test[,c('class',k_top_5)]

model_5 <- naiveBayes(class ~ ., data = data_train_5)
predictions_5 <- predict(model_5, data_test[,2:6])
mush_nb_5 = confusionMatrix(predictions_5, data_test$class)$overall["Accuracy"]


k_top_10 = rownames(head(infoGain.sorted,10))

data_train_10 = data_train[,c('class',k_top_10)]
data_test_10 = data_test[,c('class',k_top_10)]

model_10 <- naiveBayes(class ~ ., data = data_train_10)
predictions_10 <- predict(model_10, data_test[,2:11])
mush_nb_10 = confusionMatrix(predictions_5, data_test$class)$overall["Accuracy"]


#==================================Decision Tree ================================================

lc_tree = rpart(class ~ ., data = data_train, method = "class",control = rpart.control(cp = 0.05,minsplit=5))
fancyRpartPlot(lc_tree)
printcp(lc_tree)
plotcp(lc_tree)
#new.fit <- prp(tree,snip=TRUE)$obj
ptree<- prune(lc_tree,cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree, uniform=TRUE,main="Pruned Classification Tree")
table(predict(ptree,data_test,type ='class'),data_test$class)
low_accu = mean(predict(lc_tree,data_test,type ='class') == data_test$class)
summary(lc_tree)

hc_tree = rpart(class ~ ., data = data_train, method = "class",control = rpart.control(cp = 0.0,minsplit=5))
fancyRpartPlot(hc_tree)
printcp(hc_tree)
plotcp(hc_tree)
#new.fit <- prp(tree,snip=TRUE)$obj
ptree<- prune(hc_tree,cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree, uniform=TRUE,main="Pruned Classification Tree")
table(predict(ptree,data_test,type ='class'),data_test$class)
high_accu = mean(predict(hc_tree,data_test,type ='class') == data_test$class)
summary(hc_tree)


#==================================== Knn ==================================================
myKnn = function(train,test,class,k){
  if(ncol(train)!=ncol(test)){
    print("Number of features in train and test datasets should be same.")
  }
  else if(nrow(train)!= length(class)){
    print("Number of classse and number of rows in train datasets should be same.")
  } else {
    n = ncol(test)
    y = apply(train,1,function(z){apply(test,1,function(x){sum(x  == z)/n})})
    
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

pred = myKnn(data_train[,c(2:23)],data_test[,c(2:23)],data_train$class,k=3)
accuracy_3 = confusionMatrix(pred, data_test$class)$overall["Accuracy"]

pred = myKnn(data_train[,c(2:23)],data_test[,c(2:23)],data_train$class,k=5)
accuracy_5 = confusionMatrix(pred, data_test$class)$overall["Accuracy"]

pred = myKnn(data_train[,c(2:23)],data_test[,c(2:23)],data_train$class,k=7)
accuracy_7 = confusionMatrix(pred, data_test$class)$overall["Accuracy"]

acc_knn = data.frame(c(1,3,5,7),c(accuracy_1,accuracy_3,accuracy_5,accuracy_7))
names(acc_knn)=c('K','accuracy')

write.csv(acc_knn,"mushroom_knn_acc.csv",row.names=F)