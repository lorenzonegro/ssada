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

# Modifica 5 
table(dati$Price)
dati$Price <- as.numeric(gsub("\\$","",dati$Price))
table(dati$Price)
summary(dati$Price)

# Modifica 7
table(dati$Content.Rating)
dati[dati$Content.Rating=="Unrated",]

# decido di metterle "Everyone"
dati$Content.Rating[dati$Content.Rating=="Unrated"]<-"Everyone"
table(as.factor(dati$Content.Rating))
