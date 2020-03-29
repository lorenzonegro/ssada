#Pulizia e analisi preliminari dataset
rm(list=ls())
setwd("C:/Users/user1/Desktop/UNIVERSITA'/DATI AZIENDALI/googleplaystore")
gps=read.csv("googleplaystore.csv")
gpsr=read.csv("googleplaystore_user_reviews.csv")
head(gps)
str(gps)
hist(gps$Rating,xlim=c(0,5),nclass=100)
mean(gps$Rating)
#ciaos