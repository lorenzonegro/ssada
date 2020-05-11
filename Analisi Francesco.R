# carico i dati 
library(glmnet)
library(caret)
library(xgboost)
load("google_app_final.RData")

# test
set.seed(123)
train.index <- sample(1:nrow(google_app) , size = nrow(google_app)*0.7)

# divido il test e train
train <- google_app[train.index,]
test <- google_app[-train.index,]
str(train)


# divido le variabili tra risposta ed esplicative
colnames(train)
espli.index <- c("Category","Rating","Size", "Price", "Content.Rating", "Genres","Android.Ver",
                 "Last.Updated","Type_Price")
resp.index <- "Installs_4c"
# matrici di disegno

dummies <- dummyVars( ~ ., data = train[,espli.index])
x.train <- predict(dummies,train[,espli.index])
x.test <- predict(dummies,test[,espli.index])
y.train <-  train[,resp.index]
y.test <- test[,resp.index]

# Multinomiale con penalizzazione lasso
fit=glmnet(x.train,y.train,family="multinomial")par(mfrow=c(4,4))
plot(fit,xvar="dev")
cvfit=cv.glmnet(x.train,y.train,family="multinomial",nfolds=4)
par(mfrow=c(1,1))
plot(cvfit)
pred=predict(cvfit,newx=x.test,type="class")
t<- table(pred, y.test)
t
t <- rbind(t[-2,], t[2,])
t
rownames(t) <- levels(y.test)
sum(diag(t))/sum(t)

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
                   "max.depth" = 4,
                   "eta"= 0.15,
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
t3 <- rbind(t3[-2,], t3[2,])
rownames(t3) <- levels(y.test)
t3
1-sum(diag(t3))/sum(t3)

# importanza delle variabili
# get the feature real names
names <-  colnames(x.train)
# compute feature importance matrix
importance_matrix = xgb.importance(feature_names = names, model = bst_model)
head(importance_matrix,20)

# stampo per avere un'idea 
xgb.plot.importance(importance_matrix[1:20,])
