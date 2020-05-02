setwd("~/GitHub/ssada")
load("google_app_final.RData")
boxplot(google_app$Reviews~google_app$Installs)
# scrivo il limite massimo
max.rev <- c(10^3,5*10^3,10^4,5*10^4,10^5,5*10^5,10^6,5*10^6,10^7,5*10^7,10^8,5*10^8,10^9)
# calcolo i rate senza penalizzazione
n.rev <- rep(NA,length(google_app$Reviews))
install.levels <- levels(google_app$Installs)

# mi creo la "numerosità campionaria" di ogni osservazione
for(i in 1:13)
{
  n.rev[google_app$Installs==install.levels[i]] <- max.rev[i]
}

# ora devo scrivere la log-verosimiglianza
# marginale per stimare i parametri della priori (una beta)
# La marginale è una beta-binomiale
dati=as.matrix(cbind(n.rev,google_app$Reviews))
logL <- function(param,dat=dati)
{
  sum( lbeta(dat[,2]+param[1], param[2]+dat[,1]-dat[,2]  ) - lbeta(param[1],param[2]) )
}

logL(c(0.1,0.8))

stima.a.b <- nlminb(c(1,1), function(x) -logL(x), lower = c(1e-6,1e-6))$par

# Stima con shrinkage 
reviews.rate <- (google_app$Reviews+stima.a.b[1])/( n.rev+stima.a.b[1]+stima.a.b[2])

# confronto la stima con shrinkage con quella classica 
par(mfrow = c(2,1))
boxplot(reviews.rate~google_app$Installs)
boxplot(google_app$Reviews/n.rev~google_app$Installs)
# dai boxplot si vede un po' di shrinkage per quelle con pochi download
# Controllo un po' più nel dettaglio
head(reviews.rate[google_app$Installs==levels(google_app$Installs)[1]])
head((google_app$Reviews/n.rev)[google_app$Installs==levels(google_app$Installs)[1]])
