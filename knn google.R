rm(list=ls())
load("google_app_final.RData")
library(FNN)
library(ISLR)
df <- google_app
str(df)
sum(is.na(df))

# train and test
y=df$Installs_4c
x=df[,c(2,4,7,14)] #devo usare solo variabili numeriche 
test <- sample(1:length(y),size=70*length(y)/100) #70% test 30% training
train_X <- x[-test,]
test_X <- x[test,]
train_Y <- y[-test]
test_Y <- y[test]


### KNN classification

# k = 1
knn_pred <- knn(train = train_X, test = test_X, cl = train_Y, k = 1)
knn_pred=factor(knn_pred,ordered=T)
mean(test_Y != knn_pred)   # misclassification error
table(knn_pred, test_Y)
sum(diag(table(knn_pred, test_Y))/sum(table(knn_pred,test_Y)))

# k = 3
knn_pred <- knn(train = train_X, test = test_X, cl = train_Y, k = 3)
knn_pred=factor(knn_pred,ordered=T)
mean(test_Y != knn_pred)   # misclassification error
table(knn_pred, test_Y)
sum(diag(table(knn_pred, test_Y))/sum(table(knn_pred,test_Y)))

# k = 5
knn_pred <- knn(train = train_X, test = test_X, cl = train_Y, k = 5)
knn_pred=factor(knn_pred,ordered=T)
mean(test_Y != knn_pred)   # misclassification error
table(knn_pred, test_Y)
sum(diag(table(knn_pred, test_Y))/sum(table(knn_pred,test_Y)))


