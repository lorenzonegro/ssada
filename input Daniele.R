load("google_app_final.RData")
#Questa riga darla per sistemare i fattori nella variabile APP (solo una questione visiva perchè presenta 9647 fattori con 9627 righe)
google_app$App <- factor(google_app$App)

#Input Ratings
gen_lev <- levels(google_app$Genres)
for(i in 1:length(gen_lev)){
  input <- median(google_app$Rating[which(google_app$Genres==gen_lev[i])], na.rm = TRUE)
  google_app$Rating[which(google_app$Genres==gen_lev[i] & is.na(google_app$Rating))] <- input
  cat(i)
}
