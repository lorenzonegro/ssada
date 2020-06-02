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

#devtools::install_github("cardiomoon/moonBook")
#devtools::install_github("cardiomoon/webr")
# require(ggplot2)
# require(moonBook)
# require(webr)
# data <- tibble(value=as.numeric(top21),group=nomi21)
# data <- data %>% 
#   arrange(value) %>%
#   mutate(prop = value / sum(data$value) *100) %>% 
#   mutate(ypos = cumsum(prop)- 0.5*prop ) %>% 
#   mutate(group=factor(group, levels= rev(as.character(group)), ordered = T))
# 
# PieDonut(data, aes(pies=group,count=value), showRatioPie=F,r0=0,showPieName=FALSE, 
#         pieLabelSize =3, labelposition = 1,start=-pi/2)

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

library(ggplot2)
library(lubridate)
library(grid)
library(gridExtra)
library(scales)

load("google_app_final.RData")
countInst <- as.data.frame(table(google_app$Installs))
ggplot(google_app) + geom_bar(aes(y=Installs))


#### Type_Price #### 
ggplot(google_app) + geom_bar(aes(x=Type_Price, y=(..count..)/sum(..count..), fill=Installs), position="dodge")

obj_names <- c(
  `Free` = "Gratis",
  `Paid` = "A pagamento"
)

ty_free <- ggplot(google_app, aes()) + 
  geom_bar(aes(y=Installs, x = (..count..)/sum(..count..), fill=Installs), position="dodge") + 
  facet_wrap(google_app$Type, scales = "free_x", labeller = as_labeller(obj_names))+
  theme(axis.title = element_blank(), plot.title = element_text(size=11, face="bold"),
        legend.position = "none")+
  scale_fill_hue()

#Size
ggplot(google_app) + geom_boxplot(aes(y=Size, x=Installs, fill=Installs), outlier.size =0.15)+
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_hue()


#Price
prova <- which(google_app$Price>0 & google_app$Price<100)
ggplot(google_app[google_app$Price>0,]) + geom_boxplot(aes(y=Price, x=Installs, fill=Installs)) + coord_cartesian(ylim = c(0,20))

ggplot(google_app[prova,]) + geom_density(aes(x=Price, col=Installs))

google_app[google_app$Price>100,]




ggplot(google_app) + geom_bar(aes(fill=Installs,x=as.character(year(Last.Updated))), position="dodge")


#### Last update #### 
pippo <- year(google_app$Last.Updated)>=2018
obj_names <- c(
  `No` = "Non varia con device",
  `Yes` = "Varia con device"
)
ty_2018 <- ggplot(google_app) + 
  geom_bar( aes(y=Installs, x = (..count..)/sum(..count..), fill=Installs), position="dodge") + 
  #geom_density(aes(x = as.numeric(Installs)),adjust=1.75, col="#004900") + 
  facet_wrap(google_app$varies_with_device, scales = "free_x", labeller = as_labeller(obj_names))+
  theme(axis.title = element_blank(), plot.title = element_text(size=11, face="bold"),
        legend.position = "none")+
  scale_fill_hue()


# Rat
ggplot(google_app) + geom_boxplot(aes(y=Rating, x=Installs, fill=Installs))
ggplot(google_app) + geom_violin(aes(y=Rating, x=Installs, fill=Installs))
