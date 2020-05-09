rm(list=ls())
load("google_app_final.RData")

library(plyr)
library(MASS)
library(caret)
str(google_app)
?polr  # package MASS
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
confusionMatrix(p.mod, google_app$Installs_4c) #Provo 6

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
