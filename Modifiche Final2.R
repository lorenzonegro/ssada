gps <- read.csv("googleplaystore.csv")
# elimino la riga 10473
gps <- gps[-10473,]

# controllo i doppioni
app_name <- unique(gps$App)
table_app <- as.data.frame(table(gps$App))
app_one <- as.character(table_app$Var1[table_app$Freq==1])
app_multi <- as.character(table_app$Var1[table_app$Freq>1])

gps_all <- NULL
for(i in 1:length(app_multi)){
  dat <- gps[gps$App==app_multi[i], , drop=F]
  gps_par <- dat[1,,drop=F]
  gps_par$Reviews <- as.numeric(as.character(gps_par$Reviews))
  if(nrow(dat)==2){
    if(sum(dat[1,]!=dat[2,], na.rm = T)>2){
      gps_par <- dat
    }else{
      cate <- as.character(unique(dat$Category))
      if(length(cate)>1){
        gps_par$Category <- cate[which(cate!="FAMILY")][1]
      }
      gps_par$Reviews <- median(as.numeric(as.character(dat$Reviews)))
    }
  }else if(nrow(dat)>2){
    tab <- as.data.frame(table(dat$Category))
    tab <- tab[order(tab$Freq, decreasing =T),] 
    gps_par$Category <- tab$Var1[1]
    gps_par$Reviews <- median(as.numeric(as.character(dat$Reviews)))
  }
  gps_all <- rbind(gps_all, gps_par)
  cat(i)
  cat("\n")
}

# sono quelle che non erano doppioni in gps
gps_fin <- gps[gps$App%in%app_one,]

# dentro gps_all ci sono ancora doppioni 
#(l'ho fatto a posta perchè non sapevo come sistemarli in automatico, quindi lo faccio a mano)
table_app2 <- as.data.frame(table(as.character(gps_all$App)))
gps_all_fin <- gps_all[gps_all$App%in%table_app2$Var1[table_app2$Freq==1],]

control <- gps_all[gps_all$App%in%table_app2$Var1[table_app2$Freq>1],]
rownames(control) <- NULL
control_fin <- control[c(1,3,6,7,10,11,14,16,18,19,22,24,26,28,30),]

# sistemo il dataset finale
google_app <- rbind(gps_fin,gps_all_fin,control_fin)
rownames(google_app) <- NULL
google_app$Reviews <- as.numeric(as.character(google_app$Reviews))

# Variabile Installs ricodifica e accorpamenti
inst <- as.character(google_app$Installs)
# lev <- c("[0, 1K)","[1K, 5K)","[5K, 10K)","[10K, 50K)",
#          "[50K, 100K)","[100K, 500K)","[500K, 1M)", "[1M, 5M)", 
#          "[5M, 10M)", "[10M, 50M)","[50M, 100M)", 
#          "[100M, 500M)","[500M, 1B)", "[1B, +)")
lev <- c("[0, 1K)","[1K, 5K)","[5K, 10K)","[10K, 50K)",
         "[50K, 100K)","[100K, 500K)","[500K, 1M)", "[1M, 5M)", 
         "[5M, 10M)", "[10M, 50M)","[50M, 100M)", 
         "[100M, 500M)","[500M, +)")
reco_factor <- function(x){
  if(x=="1,000,000,000+"){
    return(lev[13])
  } else if(x=="500,000,000+"){
    return(lev[13])
  } else if(x=="100,000,000+"){
    return(lev[12])
  } else if(x=="50,000,000+"){
    return(lev[11])
  } else if(x=="10,000,000+"){
    return(lev[10])
  } else if(x=="5,000,000+"){
    return(lev[9])
  } else if(x=="1,000,000+"){
    return(lev[8])
  } else if(x=="500,000+"){
    return(lev[7])
  } else if(x=="100,000+"){
    return(lev[6])
  } else if(x=="50,000+"){
    return(lev[5])
  } else if(x=="10,000+"){
    return(lev[4])
  } else if(x=="5,000+"){
    return(lev[3])
  } else if(x=="1,000+"){
    return(lev[2])
  } else{
    return(lev[1])
  }
}
instal_corr <- as.character(sapply(inst,reco_factor))
google_app$Installs <- factor(instal_corr, lev, ordered = T)

# Modifica 2
# creo la nuova variabile
Size_Varies <- google_app$Size=="Varies with device"
summary(Size_Varies)

# ricodifico quelle che non variano
table(google_app$Size)
# Dato che un Mb ? mille Kb
# Mb
# Tengo memoria dei Mb
lunghezze <- nchar(as.character(google_app$Size))
Mega <- substr(as.character(google_app$Size),lunghezze,lunghezze)=="M"
head(Mega,50)

# Trasformo
google_app$Size <-gsub("M","", google_app$Size)
google_app$Size <-gsub("k","", google_app$Size)
google_app$Size[google_app$Size=="Varies with device"] <- NA
google_app$Size <- as.numeric(google_app$Size)
head(google_app$Size)
# Riscalo i Mega
google_app$Size[Mega==T] <- google_app$Size[Mega==T]*10^3
google_app$Size

# Modifica 5 
table(google_app$Price)
google_app$Price <- as.numeric(gsub("\\$","",google_app$Price))
table(google_app$Price)
hist(google_app$Price)
summary(google_app$Price)

# c'? charamente un outlier


# Modifica 7
table(google_app$Content.Rating)
google_app[google_app$Content.Rating=="Unrated",]

# decido di metterle "Everyone"
google_app$Content.Rating[google_app$Content.Rating=="Unrated"]<-"Everyone"
google_app$Content.Rating <- factor(google_app$Content.Rating)
table(google_app$Content.Rating)

# Modifica 10
# Osservazione: Ci sono due Nan - secondo me possono essere eliminati
table(google_app$Android.Ver)
da_elim <- c("2.2 - 7.1.1","4.0.3 - 7.1.1","4.1 - 7.1.1","5.0 - 6.0",
             "5.0 - 7.1.1","5.0 - 8.0","7.0 - 7.1.1")
google_app<-google_app[-which(google_app$Android.Ver %in% da_elim),]
google_app$Android.Ver <- factor(google_app$Android.Ver)

# unisco le classi 
mod1 <- c("1.0 and up", "1.5 and up","1.6 and up")
mod2 <- c("2.0 and up","2.0.1 and up","2.1 and up",
          "2.2 and up","2.3 and up","2.3.3 and up")
mod3 <- c("3.0 and up","3.1 and up","3.2 and up")
mod4 <- c("4.0 and up","4.0.3 and up","4.1 and up",
          "4.2 and up","4.3 and up","4.4 and up",
          "4.4W and up")
mod5<- c("5.0 and up","5.1 and up")
mod7 <- c("7.0 and up","7.1 and up")
old_mod <- list(mod1=mod1, mod2=mod2,mod3=mod3,mod4=mod4,mod5=mod5,mod7=mod7)
old_mod[2]
# Considerazione: Ha senso tenere le app tanto nuove?
new_mod <-c("1.0 and up","2.0 and up","3.0 and up",
            "4.0 and up","5.0 and up","7.0 and up") 

for( i in 1:6)
{
  google_app$Android.Ver[which(google_app$Android.Ver %in% old_mod[[i]])]<- new_mod[i]
}
table(google_app$Android.Ver)
# ricodifico i fattori
google_app$Android.Ver <- factor(google_app$Android.Ver)
# elimino gli Nan 
google_app <- google_app[-which(google_app$Android.Ver=="NaN"),]
# ho dovuto fare cos? perch? NaN era una modalit? non il classico NaN
google_app$Android.Ver <- factor(google_app$Android.Ver)
table(google_app$Android.Ver)

#Variabile 3

table(google_app$Type)
#Ho 1 NaN e 1 0, le elimino entrambe
eliminare=which(google_app$Type=="NaN")
eliminare=c(eliminare,which(google_app$Type=="0"))
eliminare
google_app[eliminare,]
google_app=google_app[-eliminare,]

#Controllo
which(google_app$Type=="NaN")
which(google_app$Type=="0")

#Variabile 6: Collassare Type e price
table(google_app$Price)
google_app$Price=gsub( "\\$","",google_app$Price)
table(google_app$Price)
google_app$Price=as.numeric(google_app$Price)
boxplot(google_app$Price[google_app$Price!=0])
hist(google_app$Price[google_app$Price!=0],xlim=c(0.00,100),nclass=1000)

Type_Price=rep(NA,length(google_app$Price))
for(i in (1:length(google_app$Price)))
{
  if(google_app$Price[i]==0)
  {
    Type_Price[i]="Free"
  }
  else
  {
    Type_Price[i]="Paid"
  }
}
Type_Price=as.factor(Type_Price)
google_app=cbind(google_app,Type_Price)


#Variabile 8
str(google_app$Genres) #120 Livelli
table(google_app$Genres)
google_app$Genres=as.character(google_app$Genres)
i=1
for(i in (1:length(google_app$Genres)))
{
  a=strsplit(google_app$Genres[i],";")[[1]]
  if(length(a)==2)
  {
    google_app$Genres[i]=a[2]
  }
  else
  {
    google_app$Genres[i]=a[1]
  }
}

table(google_app$Genres)
google_app$Genres[2]
google_app$Genres=as.factor(google_app$Genres)
str(google_app$Genres) #53 livelli

#Variabile 9
Sys.setlocale("LC_ALL","C") #devo cambiare formato in cui vengono letti i mesi in inglese
google_app$Last.Updated=as.Date(as.character(google_app$Last.Updated),format='%B %d, %Y')
google_app$Last.Updated

#Ricodifico category come genre
table(google_app$Category)
#tutte lower
google_app$Category=tolower(google_app$Category)
table(google_app$Category)
#alzo la prima lettera
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

google_app$Category=firstup(google_app$Category)
table(google_app$Category)

library(plyr)
google_app$Category <- revalue(google_app$Category,c("Art_and_design"="Art & Design",
                                                     "Auto_and_vehicles"="Auto & Vehicles",
                                                     "Books_and_reference"="Books & Reference",
                                                     "Food_and_drink"="Food & Drink",
                                                     "Health_and_fitness"="Health & Fitness",
                                                     "House_and_home"="House & Home",
                                                     "Libraries_and_demo"="Libraries & Demo",
                                                     "Maps_and_navigation"="Maps & Navigation",
                                                     "News_and_magazines"="News & Magazines",
                                                     "Travel_and_local"="Travel & Local",
                                                     "Video_players"="Video Players"))

table(google_app$Category)
length(which(google_app$Category==google_app$Genres))
google_app$Category=as.factor(google_app$Category)
google_app$App=factor(google_app$App)

#Android ver
#Modifica confrontando relative device fissi e device variati
#creo una nuova variabile che mi indica se la versione android cambia o meno
varies_with_device=rep(NA,length(google_app$Android.Ver))
for(i in (1:length(google_app$Android.Ver)))
{
  if(google_app$Android.Ver[i]=="Varies with device")
  {
    varies_with_device[i]="Yes"
  }
  else
  {
    varies_with_device[i]="No"
  }
}
table(varies_with_device)
table(google_app$Android.Ver)
google_app=cbind(google_app,varies_with_device)
google_app$varies_with_device=as.factor(google_app$varies_with_device) #2 livelli
str(google_app$varies_with_device)

google_app$Content.Rating=revalue(google_app$Content.Rating,c("Adults only 18+"="Adults",
                                                              "Mature 17+"="Adults",
                                                              "Everyone 10+"="Teen"))
table(google_app$Content.Rating)

# Eliminare quelle che costano più di 100 euro
google_app <- google_app[-which(google_app$Price>100),]



#imputare size con valore mediano di ogni genere
#Questa riga darla per sistemare i fattori nella variabile APP (solo una questione visiva perchè presenta 9647 fattori con 9627 righe)
google_app$App <- factor(google_app$App)

sum(is.na(google_app$Size))
#Input Size
generi <- levels(google_app$Genres)
for(i in 1:length(generi))
{
  med.gen <- median(google_app$Size[which(google_app$Genres==generi[i])], na.rm = TRUE)
  google_app$Size[which(google_app$Genres==generi[i] & is.na(google_app$Size))] <- med.gen
  cat(i)
}
med.gen

#Input Ratings
gen_lev <- levels(google_app$Genres)
for(i in 1:length(gen_lev)){
  input <- median(google_app$Rating[which(google_app$Genres==gen_lev[i])], na.rm = TRUE)
  google_app$Rating[which(google_app$Genres==gen_lev[i] & is.na(google_app$Rating))] <- input
  cat(i)
}

# scrivo il limite massimo
max.rev <- c(10^3,5*10^3,10^4,5*10^4,10^5,5*10^5,10^6,5*10^6,10^7,5*10^7,10^8,5*10^8,10^9)
# calcolo i rate senza penalizzazione
n.rev <- rep(NA,length(google_app$Reviews))
install.levels <- levels(google_app$Installs)

# mi creo la "numerosit? campionaria" di ogni osservazione
for(i in 1:13)
{
  n.rev[google_app$Installs==install.levels[i]] <- max.rev[i]
}

# ora devo scrivere la log-verosimiglianza
# marginale per stimare i parametri della priori (una beta)
# La marginale ? una beta-binomiale
dati=as.matrix(cbind(n.rev,google_app$Reviews))
logL <- function(param,dat=dati)
{
  sum( lbeta(dat[,2]+param[1], param[2]+dat[,1]-dat[,2]  ) - lbeta(param[1],param[2]) )
}

stima.a.b <- nlminb(c(1,1), function(x) -logL(x), lower = c(1e-6,1e-6))$par

# Stima con shrinkage 
reviews.rate <- (google_app$Reviews+stima.a.b[1])/( n.rev+stima.a.b[1]+stima.a.b[2])

# confronto la stima con shrinkage con quella classica 
par(mfrow = c(2,1))
boxplot(reviews.rate~google_app$Installs)
boxplot(google_app$Reviews/n.rev~google_app$Installs)
# dai boxplot si vede un po' di shrinkage per quelle con pochi download
# Controllo un po' pi? nel dettaglio
head(reviews.rate[google_app$Installs==levels(google_app$Installs)[1]])
head((google_app$Reviews/n.rev)[google_app$Installs==levels(google_app$Installs)[1]])

google_app$reviews.rate <- reviews.rate
str(google_app)

google_app <- google_app[,-1]
google_app <- google_app[,-11]
save(google_app,file="google_app_final.RData")

