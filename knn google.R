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
test <- sample(1:length(y),size=30*length(y)/100) #30% test 70% training
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

#provo ad utilizzare solo due variabili
str(x)

x=df[,c(7,14)]  #7 14 cioè prezzo e rev.rate mi sembrano migliori
test <- sample(1:length(y),size=30*length(y)/100) #30% test 70% training
train_X <- x[-test,]
test_X <- x[test,]
train_Y <- y[-test]
test_Y <- y[test]

k_i=c(1,2,3,4,5,10,20,50,100)
tasso=rep(0,length(k_i))
for(i in 1:length(k_i))
{
  cat(i)
  knn_pred <- knn(train = train_X, test = test_X, cl = train_Y, k = k_i[i])
  knn_pred=factor(knn_pred,ordered=T)
  mean(test_Y != knn_pred)   # misclassification error
  table(knn_pred, test_Y)
  tasso[i]=sum(diag(table(knn_pred, test_Y))/sum(table(knn_pred,test_Y)))
}
plot(tasso,type="l")

knn_pred <- knn(train = train_X, test = test_X, cl = train_Y, k = 1)
knn_pred=factor(knn_pred,ordered=T)
mean(test_Y != knn_pred)   # misclassification error
table(knn_pred, test_Y)
sum(diag(table(knn_pred, test_Y))/sum(table(knn_pred,test_Y)))

#usando solo price e reviews rate migliora 
#k=2 ottengo un discreto risultato

#provo con altre combinazioni di variabili e posso vedere che peggiora
#ad esempio
str(x)

x=df[,c(2,14)]  
test <- sample(1:length(y),size=30*length(y)/100) #30% test 70% training
train_X <- x[-test,]
test_X <- x[test,]
train_Y <- y[-test]
test_Y <- y[test]

k_i=c(1,2,3,4,5,10,20,50,100)
tasso=rep(0,length(k_i))
for(i in 1:length(k_i))
{
  cat(i)
  knn_pred <- knn(train = train_X, test = test_X, cl = train_Y, k = k_i[i])
  knn_pred=factor(knn_pred,ordered=T)
  mean(test_Y != knn_pred)   # misclassification error
  table(knn_pred, test_Y)
  tasso[i]=sum(diag(table(knn_pred, test_Y))/sum(table(knn_pred,test_Y)))
}
plot(tasso,type="l")

#provo con solo una var
str(x)

x=df[,c(2,4,7)]  
test <- sample(1:length(y),size=30*length(y)/100) #30% test 70% training
train_X <- x[-test,]
test_X <- x[test,]
train_Y <- y[-test]
test_Y <- y[test]

k_i=c(1,2,3,4,5,10,20,50,100)
tasso=rep(0,length(k_i))
for(i in 1:length(k_i))
{
  cat(i)
  knn_pred <- knn(train = train_X, test = test_X, cl = train_Y, k = k_i[i])
  knn_pred=factor(knn_pred,ordered=T)
  mean(test_Y != knn_pred)   # misclassification error
  table(knn_pred, test_Y)
  tasso[i]=sum(diag(table(knn_pred, test_Y))/sum(table(knn_pred,test_Y)))
}
plot(tasso,type="l")
