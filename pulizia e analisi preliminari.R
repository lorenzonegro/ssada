#Pulizia e analisi preliminari dataset
rm(list=ls())
gps=read.csv("googleplaystore.csv")
gpsr=read.csv("googleplaystore_user_reviews.csv")
head(gps)
str(gps)
hist(gps$Rating,xlim=c(0,5),nclass=100)
mean(gps$Rating)
colSums(is.na(gps))
#ciaos