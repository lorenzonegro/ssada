load("google_app_final.RData")
#Esplorativa
#Category
#Generes
#Cont. rating
#Android ver
#Review

str(google_app$Installs)
hist(google_app$Reviews,nclass=100)
table(google_app$Installs)
table(google_app$Category)

table(google_app$Category[google_app$Installs>="[50.000.000, 100.000.000)"])

#Generi più presenti
top20=sort(table(google_app$Category),decreasing = T)[1:20]
nomi21=c(names(top20),"Others")
others.dim=dim(google_app)[1]-sum(as.numeric(top20))

top21=c(as.numeric(top20),others.dim)
names(top21)=nomi21

pie(as.numeric(top21),nomi21)
pie(as.numeric(top21),nomi21, col=hcl.colors(10,"Greens"),clockwise=T,
    main="FlowingData Pool",lty=0, cex=0.7)

#Implemento ciò che ha scritto Dani con le mie variabili

library(ggplot2)
library(lubridate)
countInst <- as.data.frame(table(google_app$Installs))

#Reviews
mu.rev=median(google_app$Reviews)
ggplot(google_app[google_app$Reviews<=mu.rev,]) + 
  geom_bar(aes(y=Installs, x = (..count..)/sum(..count..)), position="dodge")
ggplot(google_app[google_app$Reviews>mu.rev,]) + 
  geom_bar(aes(y=Installs, x = (..count..)/sum(..count..)), position="dodge")

#Per categoria e genere dobbiamo capire quali e in che modo indagare. MACRO CATEGORIE?

#Android ver
#Modifica confrontando relative device fissi e device variati

ggplot(google_app) + geom_bar(aes(x=Android.Ver, fill=Installs), position="dodge")
#Content rating
ggplot(google_app) + geom_bar(aes(x=Content.Rating, fill=Installs), position="dodge")

