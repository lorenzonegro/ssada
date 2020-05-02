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
  gps_par$Reviews <- as.numeric(gps_par$Reviews)
  if(nrow(dat)==2){
    if(sum(dat[1,]!=dat[2,], na.rm = T)>2){
      gps_par <- dat
    }else{
      cate <- as.character(unique(dat$Category))
      if(length(cate)>1){
        gps_par$Category <- cate[which(cate!="FAMILY")][1]
      }
      gps_par$Reviews <- median(as.numeric(dat$Reviews))
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
save(google_app, file="dataset_pulizia.RData")
write.csv(google_app, file = "google_app.csv", row.names = FALSE)
