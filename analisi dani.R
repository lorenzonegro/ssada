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

m.mars = polyclass(sStima[,1], sStima[,-c(1,7)]) # Può contenere anche 2 basi moltiplicate
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
p.rf = predict(m.rf, newdata = sVer, type = "prob") # Previsione sulle probabilità

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
