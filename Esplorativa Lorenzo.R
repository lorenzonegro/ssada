rm(list=ls())
load("google_app_final.RData")
library(ggplot2)
library(lubridate)
library(grid)
library(gridExtra)
library(scales)
#Esplorativa
#Category
#Generes
#Cont. rating
#Android ver
#Review

str(google_app$Installs)
hist(google_app$Reviews,nclass=20)
table(google_app$Installs)
table(google_app$Category)

table(google_app$Category[google_app$Installs>="[50.000.000, 100.000.000)"])

#Generi pi? presenti
top20=sort(table(google_app$Category),decreasing = T)[1:20]
nomi21=c(names(top20),"Others")
others.dim=dim(google_app)[1]-sum(as.numeric(top20))

top21=c(as.numeric(top20),others.dim)
names(top21)=nomi21

pie(as.numeric(top21),nomi21)
pie(as.numeric(top21),nomi21, col=hcl.colors(10,"Greens"),clockwise=T,
    main="FlowingData Pool",lty=0, cex=0.7)

#Implemento ciò che ha scritto Dani con le mie variabili
library(grid)
library(ggplot2)
library(lubridate)
countInst <- as.data.frame(table(google_app$Installs))

#Reviews
mu.rev=median(google_app$Reviews)
mu.rev=1000
poco_rec=ggplot(google_app[google_app$Reviews<=mu.rev,]) + 
  geom_bar(aes(y=Installs, x = (..count..)/sum(..count..),fill=Installs), position="dodge")+
  ggtitle("<= 1000 Recensioni")+
  scale_x_continuous(breaks = c(0,0.15,0.30))+
  scale_y_discrete(limit = countInst$Var1)+
  theme(axis.title = element_blank(), plot.title = element_text(size=11, face="bold"),
        legend.position = "none")+
  scale_fill_hue()

molto_rec=ggplot(google_app[google_app$Reviews>mu.rev,]) + 
  geom_bar(aes(y=Installs, x = (..count..)/sum(..count..),fill=Installs), position="dodge")+
  ggtitle(">1000 Recensioni")+
  scale_x_continuous(breaks = c(0,0.15,0.30))+
  scale_y_discrete(limit = countInst$Var1)+
  theme(axis.title = element_blank(), plot.title = element_text(size=11, face="bold"),
        legend.position = "none")+
  scale_fill_hue()

grid.arrange(poco_rec,molto_rec, ncol=2,
             bottom = "Frequenze")
#Reviews confronto con installs
rev_down <- ggplot(google_app[google_app$Reviews<=mu.rev,]) + 
  geom_bar( aes(x=Installs, y = (..count..)/sum(..count..)), position="dodge", fill="#00b200") + 
  #geom_density(aes(x = as.numeric(Installs)),adjust=1.75, col="#004900") 
  ggtitle("Meno recensite")+
  theme(axis.title = element_blank(), plot.title = element_text(size=11, face="bold"))

rev_up <- ggplot(google_app[google_app$Reviews>mu.rev,]) + 
  geom_bar(aes(x=Installs, y = (..count..)/sum(..count..)), position="dodge", fill="#ff6b30")+
  scale_y_continuous(breaks = c(0,0.15,0.30))+
  scale_x_discrete(limit = countInst$Var1) + ggtitle("Molto recensite")+
  theme(axis.title = element_blank(), plot.title = element_text(size=11, face="bold"))
  #geom_density(aes(x = as.numeric(Installs)),adjust=1, col = "#923c1a")

grid.arrange(rev_down,rev_up, 
             top = "Confronto del numero di installazioni per app molto recensite o meno", 
             left = textGrob("Frequenze",rot = 90, vjust = 0.5, hjust = 0.5))


#Per categoria e genere dobbiamo capire quali e in che modo indagare. MACRO CATEGORIE?

ggplot(google_app) + geom_bar(aes(x=varies_with_device, fill=Installs), position="dodge")

#devo standardizzare all'interno delle due classi
varia=ggplot(google_app[google_app$varies_with_device=="Yes",]) + 
  geom_bar(aes(y=Installs, x = (..count..)/sum(..count..),fill=Installs), position="dodge")+
  ggtitle("Varia con device")+
  scale_x_continuous(breaks = c(0,0.15,0.30))+
  scale_y_discrete(limit = countInst$Var1)+
  theme(axis.title = element_blank(), plot.title = element_text(size=11, face="bold"),
        legend.position = "none")+
  scale_fill_hue()

non_varia=ggplot(google_app[google_app$varies_with_device=="No",]) + 
  geom_bar(aes(y=Installs, x = (..count..)/sum(..count..),fill=Installs), position="dodge")+
  ggtitle("Non varia con device")+
  scale_x_continuous(breaks = c(0,0.15,0.30))+
  scale_y_discrete(limit = countInst$Var1)+
  theme(axis.title = element_blank(), plot.title = element_text(size=11, face="bold"),
        legend.position = "none")+
  scale_fill_hue()

grid.arrange(varia, non_varia, ncol=2,
             bottom = "Frequenze")


#Content rating
ggplot(google_app) + geom_bar(aes(x=Content.Rating, fill=Installs), position="dodge")+
  #theme(axis.title = element_blank())+
  scale_fill_hue()+ylab("Count")+xlab("")

