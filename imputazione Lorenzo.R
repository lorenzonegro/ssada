#imputare size con valore mediano di ogni genere
load("google_app_final.RData")
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

sum(is.na(google_app$Size))
