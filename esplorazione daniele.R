library(ggplot2)
library(lubridate)
library(grid)
library(gridExtra)
library(scales)

load("google_app_final.RData")
countInst <- as.data.frame(table(google_app$Installs))
ggplot(google_app) + geom_bar(aes(y=Installs))


# Type_Price
ty_free <- ggplot(google_app[google_app$Type_Price=="Free",]) + 
  geom_bar( aes(x=Installs, y = (..count..)/sum(..count..)), position="dodge", fill="#00b200") + 
  geom_density(aes(x = as.numeric(Installs)),adjust=1.75, col="#004900") + ggtitle("Gratis")+
  theme(axis.title = element_blank(), plot.title = element_text(size=11, face="bold"))

ty_paid <- ggplot(google_app[google_app$Type_Price%in% c("Paid","Plus"),]) + 
  geom_bar(aes(x=Installs, y = (..count..)/sum(..count..)), position="dodge", fill="#ff6b30")+
  scale_y_continuous(breaks = c(0,0.15,0.30))+
  scale_x_discrete(limit = countInst$Var1) + ggtitle("A pagamento")+
  theme(axis.title = element_blank(), plot.title = element_text(size=11, face="bold"))+
  geom_density(aes(x = as.numeric(Installs)),adjust=1, col = "#923c1a")

grid.arrange(ty_free,ty_paid, 
             top = "Confronto del numero di installazioni per la tipologia di prezzo", 
             left = textGrob("Frequenze",rot = 90, vjust = 0.5, hjust = 0.5))


#Size
ggplot(google_app) + geom_boxplot(aes(y=Size, x=Installs, fill=Installs))+
  theme(legend.position = "none")
sum(is.na(google_app$Size))
#ATTENZIONE CI SONO NA!!!! eliminiamo le righe??

#Price
prova <- which(google_app$Price>0)
ggplot(google_app[prova,]) + geom_boxplot(aes(y=Price, x=Installs, fill=Installs)) +coord_cartesian(ylim = c(0,20))

ggplot(google_app) + geom_bar(aes(x=Installs,fill=as.character(year(Last.Updated))), position="dodge")




ggplot(google_app[prova,]) + geom_point(aes(y=Price, x=Size, col= Installs)) + coord_cartesian(ylim = c(0,50))
