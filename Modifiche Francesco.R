# Analisi esplorativa dataset 
# carico i dati 
dati <- read.csv("C:/Users/pozza/Documents/GitHub/ssada/google_app.csv", header = T)

# Analisi veloce
summary(dati)

# secondo me abbiamo solo sei variabili che comunque hanno una miriade di moldalità
# le ultime due possiamo modificarle in un fattore. Ossia assumere se possiamo 
# pensarle come il fatto che gli sviluppatori aggiornano o meno una versione in 
# base alle potenzialità del dataset. Anche la grandezza non puù essere lasciata cosi
# o riduciamo di molto le classi oppure quantitativa. Data di pubblicazione un 
# fattore che forse non riusciamo a tenere conto 

# Modifica 2
# creo la nuova variabile
Size_Varies <- dati$Size=="Varies with device"
summary(Size_Varies)

# ricodifico quelle che non variano
table(dati$Size)
# Dato che un Mb è mille Kb
# Mb
# Tengo memoria dei Mb
substr(dati$S)
lunghezze <- nchar(as.character(dati$Size))
Mega <- substr(as.character(dati$Size),lunghezze,lunghezze)=="M"
head(Mega,50)

# Trasformo
dati$Size <-gsub("M","", dati$Size)
dati$Size <-gsub("k","", dati$Size)
dati$Size[dati$Size=="Varies with device"] <- NA
dati$Size <- as.numeric(dati$Size)
head(dati$Size)
# Riscalo i Mega
dati$Size[Mega==T] <- dati$Size[Mega==T]*10^3
dati$Size

# Modifica 5 
table(dati$Price)
dati$Price <- as.numeric(gsub("\\$","",dati$Price))
table(dati$Price)
hist(dati$Price)
summary(dati$Price)

# c'è charamente un outlier


# Modifica 7
table(dati$Content.Rating)
dati[dati$Content.Rating=="Unrated",]

# decido di metterle "Everyone"
dati$Content.Rating[dati$Content.Rating=="Unrated"]<-"Everyone"
dati$Content.Rating <- factor(dati$Content.Rating)
table(dati$Content.Rating)

# Modifica 10
# Osservazione: Ci sono due Nan - secondo me possono essere eliminati
table(dati$Android.Ver)
da_elim <- c("2.2 - 7.1.1","4.0.3 - 7.1.1","4.1 - 7.1.1","5.0 - 6.0",
             "5.0 - 7.1.1","5.0 - 8.0","7.0 - 7.1.1")
dati<-dati[-which(dati$Android.Ver %in% da_elim),]
dati$Android.Ver <- factor(dati$Android.Ver)

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
  dati$Android.Ver[which(dati$Android.Ver %in% old_mod[[i]])]<- new_mod[i]
}
table(dati$Android.Ver)
# ricodifico i fattori
dati$Android.Ver <- factor(dati$Android.Ver)
# elimino gli Nan 
dati <- dati[-which(dati$Android.Ver=="NaN"),]
# ho dovuto fare così perché NaN era una modalità non il classico NaN
dati$Android.Ver <- factor(dati$Android.Ver)
table(dati$Android.Ver)

