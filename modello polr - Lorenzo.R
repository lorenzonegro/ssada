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
