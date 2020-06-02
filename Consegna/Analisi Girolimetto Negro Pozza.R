# carico i dati 
library(glmnet)
library(caret)
library(xgboost)
library(penalizedLDA)
setwd("~/GitHub/ssada/Consegna")
load("google_app_final.RData")


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
summary(test)
# divido le variabili tra risposta ed esplicative
colnames(train)
espli.index <- c("Category","Rating","Size", "Price", "Content.Rating", "Genres","Android.Ver",
                 "Last.Updated","reviews.rate")
resp.index <- "Installs_4c"

# matrici di disegno

dummies <- dummyVars( ~ ., data = train[,espli.index])
x.train <- predict(dummies,train[,espli.index])
x.test <- predict(dummies,test[,espli.index])
y.train <-  train[,resp.index]
y.test <- test[,resp.index]



# Multinomiale con penalizzazione lasso
fit=glmnet(x.train,y.train,family="multinomial")
par(mfrow=c(4,4))
plot(fit,xvar="dev")
cvfit=cv.glmnet(x.train,y.train,family="multinomial",nfolds=4)
par(mfrow=c(1,1))
plot(cvfit)
pred=predict(cvfit,newx=x.test,type="class")
t<- table(pred, y.test)
t
t <- rbind(t[-3,], t[3,])
t
rownames(t) <- levels(y.test)
1-sum(diag(t))/sum(t)
# falsi positivi
sum(t[3:4,1:2])/sum(t[,1:2])
# falsi negativi
sum(t[1:2,3:4])/sum(t[,3:4])
rownames(t4) <- levels(y.test)

# modello xgboost
label.train = as.integer(y.train)-1
label.test = as.integer(y.test)-1
train_matrix <- xgb.DMatrix(data = x.train, label = label.train)
test_matrix <- xgb.DMatrix(data = x.test, label = label.test)
numberOfClasses <- length(unique(label.train))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses,
                   "max.depth" = 3,
                   "eta"= 0.1,
                   "min_child_weight"=1,
                   "subsample"=1, 
                   "colsample_bytree"=1)

nround    <- 2000 # number of XGBoost rounds
cv.nfold  <- 4

# Fit cv.nfold 
cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = TRUE,
                   prediction = TRUE,
                   showsd = T, stratified = T,
                   print_every_n = 10,
                   early_stopping_rounds = 20, maximize = F)
cv_model$best_iteration

# previsione con modello totale
# Stimo il modello
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = cv_model$best_iteration)

# Previsione con xgboost
# da non ho capito bene cosa butta fuori il predict
# prenso le probabilità ma non so in che formato
test_pred <- predict(bst_model, newdata = test_matrix)
test_prediction <- t(matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses)) 
pred3 <- rep(NA, length(y.test))
for( i  in 1:length(y.test))
{
  pred3[i] <- levels(y.test)[which.max(test_prediction[i,])] 
}

# matrice di confusione
t3<- table(pred3, y.test)
t3
t3 <- rbind(t3[-3,], t3[3,])
rownames(t3) <- levels(y.test)
t3
1-sum(diag(t3))/sum(t3)
# falsi positivi
sum(t3[3:4,1:2])/sum(t3[,1:2])
# falsi negativi
sum(t3[1:2,3:4])/sum(t3[,3:4])

# falsi positivi e negativi
sum(t3[1:2,3:4])/sum(t3[,3:4])
sum(t3[3:4,1:2])/sum(t3[,1:2])
# importanza delle variabili
# get the feature real names
names <-  colnames(x.train)
# compute feature importance matrix
importance_matrix = xgb.importance(feature_names = names, model = bst_model)
head(importance_matrix,20)

# stampo per avere un'idea 
xgb.plot.importance(importance_matrix[1:20,])
save(importance_matrix, file = "Importace.RData")

# Modelli a logit comulati con penalizzazione lasso 
library(ordinalNet)
cv.ordinal <- ordinalNetCV(x.train, y.train, tuneMethod="cvMisclass", standardize=F)
cv.ordinal.2 <- ordinalNetTune(
  x.train,
  y.train,
  lambdaVals = NULL,
  folds = NULL,
  nFolds = 3,
  printProgress = TRUE,
  warn = TRUE,
  alpha = 0.5,
)

cv.ordinal.2$lambdaVals
summary(cv.ordinal.2)
fit <- ordinalNet(x.train, y.train, family="cumulative", link="logit",
                   parallelTerms=TRUE, nonparallelTerms=FALSE, standardize = T)
summary(fit)
fit$coefs
fit4 <- ordinalNet(x.train, y.train, family="cumulative", link="logit",
                   reverse = T,parallelTerms=TRUE, nonparallelTerms=FALSE, lambdaVals = 0.004)
coef(fit4)
# Prendo i più e i meno importanti
best <- round(sort(coef(fit4)[which(coef(fit4)>=sort(coef(fit4), decreasing = T)[30])],decreasing = T),2)
wrost <- round(sort(coef(fit4)[which(coef(fit4)<=sort(coef(fit4))[30])]),2)
save(best, file= "BestCoef.RData")
save(wrost, file= "WrostCoef.RData")
best

pred4 <- predict(fit4,x.test)
pred42 <- rep(NA, length(y.test))
for( i  in 1:length(y.test))
{
  pred42[i] <- levels(y.test)[which.max(pred4[i,])] 
}
summary(x.test)
# matrice di confusione
t4<- table(pred42, y.test)
t4
t4 <- rbind(t4[-3,], t4[3,])
# flp e fln
# falsi positivi
sum(t4[3:4,1:2])/sum(t4[,1:2])
# falsi negativi
sum(t4[1:2,3:4])/sum(t4[,3:4])
rownames(t4) <- levels(y.test)


t4
1-sum(diag(t4))/sum(t4)
# Falsi positivi e (per secondi) falsi negativi
sum(t4[1:2,3:4])/sum(t4[1:2,])
sum(t4[3:4,1:2])/sum(t4[,1:2])

# tabella 
tabClass = function(class,classVer){
  err=matrix(0, length(levels(classVer)),length(levels(classVer)))
  colnames(err)=levels(classVer)
  rownames(err)=levels(classVer)
  for(i in 1:length(class)){
    jS = which(levels(classVer)==class[i])
    jV = which(levels(classVer)==classVer[i])
    err[jS,jV]=err[jS,jV]+1
  }
  falsi.err= 1-diag(err)/apply(err,2,sum)
  overall.err=1-sum(diag(err))/sum(err)
  class.err=1-diag(err)/apply(err,1,sum)
  return(list(errMatrix=err,class.err=class.err,falsi.err=falsi.err,overall.err=overall.err))
}
leve
tabella.comulati  <- tabClass(pred42, y.test)
tabella.comulati$errMatrix
tabella.comulati$class.err
tabella.comulati$falsi.err
tabella.comulati$overall.err

#################################################
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

#come si puÃ² vedere predico solo 4 categorie, le altre tutte zero. Meglio usare 4c
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

#Il migliore Ã¨ quello con 4 categorie

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

#####################################################à
# data dataset
library(caret)
# PLOTMSE ----
plotMSE = function(mse,labelMSE,spo=c(0,2,0,0),passo=-1,titolo="MSE - Mean Square Errors",xlabel="mse",...){
  def=par()$mar # def = 5.1 4.1 4.1 2.1
  par(mar=(def+spo))
  posY=(-1):(-length(mse))
  plot(mse,posY,type="n", xlab=xlabel, ylab = "", las=1, yaxt="n",...)
  if(passo>0) abline(v=seq(from=0, to=max(mse)+passo, by = passo), col="gray88", lty=5)
  segments(0, posY, mse, posY, lty=3)
  points(mse,posY,pch=19)
  pos=-which(mse == min(mse))
  points(rep(min(mse),length(pos)),pos, col=2, cex=1.5, pch="O")
  axis(2, at=posY,  labels=labelMSE, las=1)
  title(titolo)
  par(mar=def)
}

# confronto ----
tabClass = function(class,classVer){
  err=matrix(0, length(levels(classVer)),length(levels(classVer)))
  colnames(err)=levels(classVer)
  rownames(err)=levels(classVer)
  for(i in 1:length(class)){
    jS = which(levels(classVer)==class[i])
    jV = which(levels(classVer)==classVer[i])
    err[jS,jV]=err[jS,jV]+1
  }
  falsi.err= 1-diag(err)/apply(err,2,sum)
  overall.err=1-sum(diag(err))/sum(err)
  class.err=1-diag(err)/apply(err,1,sum)
  return(list(errMatrix=err,class.err=class.err,falsi.err=falsi.err,overall.err=overall.err))
}

load("google_app_final.RData")
esp <- c("Category", "Rating", 
         "Size", "Price", "Content.Rating", "Genres", "Last.Updated", 
         "reviews.rate", "Android.Ver")
data= cbind(y=google_app[,15], google_app[,esp])
continue <- c("Rating","Size", "Price","reviews.rate")

set.seed(123)
acaso=sample(1:NROW(data), round(NROW(data)*0.7))
sStima=data[acaso,]
sVer=data[-acaso,]

train.cont <- sStima[,continue]
test.cont <- sVer[,continue]
preProcValues <- preProcess(train.cont, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, train.cont)
testTransformed <- predict(preProcValues, test.cont)
sStima[,continue]<- trainTransformed
sVer[,continue] <- testTransformed

xStima = model.matrix(~., data = sStima[,-1])[, -1] # SENZA INTERCETTA
xVer = model.matrix(~., data = sVer[,-1])[, -1] # SENZA INTERCETTA

library(nnet)
gStima = class.ind(sStima[,1]) # Matrice Y delle dummy sulle classi del training set
gVer = class.ind(sVer[,1]) # Matrice Y delle dummy sulle classi del test set
fqual=as.formula(paste("y~",paste(names(sStima)[-1],collapse = "+"),collapse = NULL))
fquant=as.formula(paste("gStima~",paste(names(sStima)[-1],collapse = "+"),collapse = NULL))

# Modello lineare ----
m.lin=lm(fquant,data=sStima)
p.lin=predict(m.lin,newdata = sVer)
class.lin = apply(p.lin, 1, function(x) levels(sStima$y)[which.max(x)]) # Assegnazione della classe che assume il valore massimo del vettore delle previsioni

# ANALISI DISCRIMINANTE ----
library(MASS)
m.lda=lda(as.formula(paste("y~",paste(names(sStima)[-c(1,7)],collapse = "+"),collapse = NULL)), data=sStima[,-7])
p.lda=predict(m.lda,newdata = sVer)
class.lda = p.lda$class

# MARS ----
library(polspline)
library(earth)
m.mars=earth(x=sStima[,-c(1,7)], y=sStima[,1])
class.mars <- predict(m.mars,sVer[,-1], type ="class")

m.mars = polyclass(sStima[,1], sStima[,-c(1,7)]) # PuÃ² contenere anche 2 basi moltiplicate
p.mars = ppolyclass(sVer[,-1], m.mars)
classM.mars = as.factor(cpolyclass(sVer[,-1], m.mars))
class.mars = levels(sStima$y)[classM.mars]

# Random Forest ----
library(randomForest)
cb1=sample(1:NROW(sBil),round(NROW(sBil)*3/4)) 
cb2=setdiff(1:NROW(sBil),cb1)

B = 200
#q = round(ncol(sStima)/3)
parte1 = sample(1:nrow(sStima), nrow(sStima)/2)
parte2 = setdiff(1:nrow(sStima), parte1)
qI=seq(10,(NCOL(sStima)-9),length.out = 10)
errR=matrix(NA, length(qI),2)
for(i in 1:length(qI)){
  rf1 = randomForest(y ~., data = sStima[parte1,], nodesize = 1, mtry = qI[i], ntree = B)
  class.rf = predict(rf1, newdata = sStima[parte2,], type = "response") 
  n=tabClass(class.rf,sStima[parte2,]$y)
  errR[i,1] = qI[i]
  errR[i,2] = n$overall.err
  cat(i,sep="")
}

errR
plot(errR[,1],errR[,2], type="b", ylab = "err", xlab="mtry (F)", pch=19,cex=0.5)

points(errR[which.min(errR[,2]),1],min(errR[,2]),col=2)
q=errR[which.min(errR[,2]),1]
m.rf = randomForest(y ~., data = sStima, nodesize = 1, mtry = 2, ntree = B, trace=T)


plot(m.rf)+title("                          | mtry = 2")
varImpPlot(m.rf)+title("                                               | mtry = 2")

class.rf = predict(m.rf, newdata = sVer, type = "response") # Previsione sulle classi di risposta
p.rf = predict(m.rf, newdata = sVer, type = "prob") # Previsione sulle probabilitÃ 

Errore=c(tabClass(class.lin,sVer$y)$overall.err,
         tabClass(class.lda,sVer$y)$overall.err,
         tabClass(class.mars,sVer$y)$overall.err,
         tabClass(class.tree,sVer$y)$overall.err,
         tabClass(class.rf,sVer$y)$overall.err)

sum(tabClass(class.lin,sVer$y)$errMatrix)
Modelli=c("Modello lineare",
          "Analisi discriminante",
          "MARS",
          "Alberi di classificazione",
          "Random Forest")

ris = data.frame(Modello=Modelli,Errore_Totale=Errore)
ris
plotMSE(Errore,Modelli, xlabel = "Errata classificazione", titolo = "", spo=c(0,6,0,0), passo=0.05)
