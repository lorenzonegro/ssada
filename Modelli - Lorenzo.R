rm(list=ls())
load("google_app_final.RData")

library(plyr)
library(MASS)
library(caret)
str(google_app)
?polr  # package MASS

# test
set.seed(123)
train.index <- sample(1:nrow(google_app) , size = nrow(google_app)*0.7)

# divido il test e train
train <- google_app[train.index,]
test <- google_app[-train.index,]
str(train)
continue <- c("Rating","Size", "Price","reviews.rate")
train.cont <- train[,continue]
test.cont <- test[,continue]
preProcValues <- preProcess(train.cont, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, train.cont)
testTransformed <- predict(preProcValues, test.cont)

# metto le standardizzate al posto delle 

train[,continue]<- trainTransformed
test[,continue] <- testTransformed

google_app=rbind(train,test)
risposta=google_app$Installs
esplicative=c("Rating","Size","Category","Type_Price","Content.Rating","Last.Updated","Android.Ver")
formula <- as.formula(paste("Installs ~ ", paste(esplicative, collapse = "+"), collapse = ""))

#le variabili numeriche danno problemi
# first model
mod <- polr(formula, data=google_app)
mod

# significant variables
dropterm(mod, test = "Chisq") #genere in questo caso produce molti warnings: troppi fattori?

# classification
summary(mod)
p.mod<- predict(mod)
confusionMatrix(p.mod, google_app$Installs)

#come si può vedere predico solo 4 categorie, le altre tutte zero. Meglio usare 4c
formula <- as.formula(paste("Installs_4c ~ ", paste(esplicative, collapse = "+"), collapse = ""))

#le variabili numeriche danno problemi
# first model
mod <- polr(formula, data=google_app)
mod

# significant variables
dropterm(mod, test = "Chisq") 

# classification
p.mod<- predict(mod)
cm=confusionMatrix(p.mod, google_app$Installs_4c) 
1-(sum(diag(cm$table))/sum(cm$table))
(cm$table[3,1]+cm$table[3,2]+cm$table[4,1]+cm$table[4,2])/sum(cm$table[,1],cm$table[,2])
(cm$table[1,3]+cm$table[1,4]+cm$table[2,3]+cm$table[2,4])/sum(cm$table[,3],cm$table[,4])
#Provo 6

formula <- as.formula(paste("Installs_6c ~ ", paste(esplicative, collapse = "+"), collapse = ""))

#le variabili numeriche danno problemi
# first model
mod <- polr(formula, data=google_app)
mod

# significant variables
dropterm(mod, test = "Chisq") 

# classification
p.mod<- predict(mod)
confusionMatrix(p.mod, google_app$Installs_6c) #Provo 6, va peggio

#Il migliore è quello con 4 categorie

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

