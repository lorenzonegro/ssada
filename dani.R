# prova daniele
gps <- read.csv("googleplaystore.csv")
gpsr <- read.csv("googleplaystore_user_reviews.csv")

str(gps)
summary(gps$Last.Updated)
summary(gps$App)
boxplot(gps$Rating~gps$Installs)
unique(gps$Last.Updated)
table(as.numeric(substring(as.character(gps$Last.Updated), 
                    (nchar(as.character(gps$Last.Updated))+1)-4,nchar(as.character(gps$Last.Updated)))))
GPS2 <- gps[!(is.na(gps$Rating)),]
summary(GPS2$App)
length(unique(GPS2$App))
