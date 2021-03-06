rm(list=ls())
load("google_app_final.RData")
library(FNN)
library(ISLR)
df <- google_app
str(df)
sum(is.na(df))

# train and test
y=df$Installs_4c

# test
set.seed(123)
train.index <- sample(1:nrow(google_app) , size = nrow(google_app)*0.7)

# divido il test e train
train_X <- google_app[train.index,]
test_X <- google_app[-train.index,]
str(train_X)
continue <- c("Rating","Size", "Price","reviews.rate")
train.cont <- train_X[,continue]
test.cont <- test_X[,continue]
preProcValues <- preProcess(train.cont, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, train.cont)
testTransformed <- predict(preProcValues, test.cont)
train_Y <- y[train.index]
test_Y <- y[-train.index]

### KNN classification

# k = 1
knn_pred <- knn(train = trainTransformed, test = testTransformed, cl = train_Y, k = 1)
knn_pred=factor(knn_pred,ordered=T)
mean(test_Y != knn_pred)   # misclassification error
table(knn_pred, test_Y)
sum(diag(table(knn_pred, test_Y))/sum(table(knn_pred,test_Y)))

# k = 3
knn_pred <- knn(train = trainTransformed, test = testTransformed, cl = train_Y, k = 3)
knn_pred=factor(knn_pred,ordered=T)
mean(test_Y != knn_pred)   # misclassification error
table(knn_pred, test_Y)
sum(diag(table(knn_pred, test_Y))/sum(table(knn_pred,test_Y)))

# k = 10
knn_pred <- knn(train = trainTransformed, test = testTransformed, cl = train_Y, k = 2)
knn_pred=factor(knn_pred,ordered=T)
mean(test_Y != knn_pred)   # misclassification error
cm=table(knn_pred, test_Y)
1-sum(diag(table(knn_pred, test_Y))/sum(table(knn_pred,test_Y)))
(cm[3,1]+cm[3,2]+cm[4,1]+cm[4,2])/sum(cm[,1],cm[,2])
(cm[1,3]+cm[1,4]+cm[2,3]+cm[2,4])/sum(cm[,3],cm[,4])

#provo ad utilizzare solo due variabili
str(x)

k_i=c(1,2,3,4,5,10,20,50,100)
tasso=rep(0,length(k_i))
for(i in 1:length(k_i))
{
  cat(i)
  knn_pred <- knn(train = trainTransformed, test = testTransformed, cl = train_Y, k = k_i[i])
  knn_pred=factor(knn_pred,ordered=T)
  mean(test_Y != knn_pred)   # misclassification error
  table(knn_pred, test_Y)
  tasso[i]=sum(diag(table(knn_pred, test_Y))/sum(table(knn_pred,test_Y)))
}
plot(tasso,type="l")

knn_pred <- knn(train = trainTransformed, test = testTransformed, cl = train_Y, k = 2)
knn_pred=factor(knn_pred,ordered=T)
mean(test_Y != knn_pred)   # misclassification error
table(knn_pred, test_Y)
sum(diag(table(knn_pred, test_Y))/sum(table(knn_pred,test_Y)))

#usando solo price e reviews rate migliora 
#k=2 ottengo un discreto risultato

#provo con altre combinazioni di variabili e posso vedere che peggiora
#ad esempio
str(x)

#Seleziono dei subset di variabili esplicative


k_i=c(1,2,3,4,5,10,20,50,100)
tasso=rep(0,length(k_i))
for(i in 1:length(k_i))
{
  cat(i)
  knn_pred <- knn(train = trainTransformed, test = testTransformed, cl = train_Y, k = k_i[i])
  knn_pred=factor(knn_pred,ordered=T)
  mean(test_Y != knn_pred)   # misclassification error
  table(knn_pred, test_Y)
  tasso[i]=sum(diag(table(knn_pred, test_Y))/sum(table(knn_pred,test_Y)))
}
plot(tasso,type="l")

#Per ora 0.44 migliore k=5 4 var, k=1 (1,4), k1 (3,4)

knn_pred <- knn(train = trainTransformed[,c(1,4)], test = testTransformed[,c(3,4)], cl = train_Y, k = 2)
knn_pred=factor(knn_pred,ordered=T)
mean(test_Y != knn_pred)   # misclassification error
table(knn_pred, test_Y)
sum(diag(table(knn_pred, test_Y))/sum(table(knn_pred,test_Y)))

