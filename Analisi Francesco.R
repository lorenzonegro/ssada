# carico i dati 
library(glmnet)
library(caret)
library(xgboost)
library(penalizedLDA)
setwd("~/GitHub/ssada")
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

# LDA Penalizzata
y.lda <- as.integer(y.train)
cv.PLDA <- PenalizedLDA.cv(x.train, y.lda, lambdas =seq(from=0, to =10000,length.out = 10), K = NULL, nfold = 4, folds = NULL)

cv.PLDA$bestlambda
cv.PLDA$lambdas
# Best model 
modelLDA <- PenalizedLDA(x.train, y.lda, K = cv.PLDA$bestK,lambda=cv.PLDA$bestlambda)

plot(cv.PLDA)
pred=predict(modelLDA,x.test)
pred$ypred
unique(pred$ypred)
str(pred)
t.lda<- table(pred$ypred, y.test)
t.lda
rownames(t.lda) <- levels(y.test)
1-sum(diag(t.lda))/sum(t.lda)





# La variabile reviews rate miglioora la previsione di poco
# quindi non credo che dia tanto overfitting
marginal <- table(y.train)/sum(table(y.train))

# Previioni "Pesate"
pred.w <- rep(NA, length(y.test))
for( i  in 1:length(y.test))
{
  pred.w[i] <- levels(y.test)[which.max((pred[i,,1]-marginal)/marginal)] 
}

t2<- table(pred.w, y.test)
t2
t2 <- rbind(t2[-3,], t2[3,])
rownames(t2) <- levels(y.test)
sum(diag(t2))/sum(t2)
# peggiora ulteriormente quindi non bene

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
