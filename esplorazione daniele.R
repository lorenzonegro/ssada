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
#' Un app a pagamento tende ad avere un andamento decrescente e sembra che invece le app "gratis" 
#' invece siano considerevolmente più avvantaggiate nel numero di installazioni. 
#' Non si deve cadere nel tranello di considerare un app gratis meno reditizzia.
#' Infatti l'app viene considerata gratis nel momento dell'acquisto: molte di 
#' queste prevedono la possibilità di spendere denaro all'interno come ad esempio per app di gioco 
#' d'azzardo oppure giochi in cui vengono venduti oggetti estetici.

#Size
ggplot(google_app) + geom_boxplot(aes(y=Size, x=Installs, fill=Installs))+
  theme(legend.position = "none")
sum(is.na(google_app$Size))
table(google_app$Installs[is.na(google_app$Size)])
#ATTENZIONE CI SONO NA!!!! eliminiamo le righe??

#Price
prova <- which(google_app$Price>0 & google_app$Price<100)
ggplot(google_app[prova,]) + geom_boxplot(aes(y=Price, x=Installs, fill=Installs)) + coord_cartesian(ylim = c(0,20))

ggplot(google_app[prova,]) + geom_density(aes(x=Price, col=Installs))

google_app[google_app$Price>100,]




ggplot(google_app) + geom_bar(aes(fill=Installs,x=as.character(year(Last.Updated))), position="dodge")


#### Last update #### 
ty_2018 <- ggplot(google_app[year(google_app$Last.Updated)>=2018,]) + 
  geom_bar( aes(x=Installs, y = (..count..)/sum(..count..)), position="dodge", fill="#00b200") + 
  geom_density(aes(x = as.numeric(Installs)),adjust=1.75, col="#004900") + 
  ggtitle("Ultimo aggiornamento durante il 2018") +
  theme(axis.title = element_blank(), plot.title = element_text(size=11, face="bold"))

ty_20xx <- ggplot(google_app[year(google_app$Last.Updated)<2018,]) + 
  geom_bar(aes(x=Installs, y = (..count..)/sum(..count..)), position="dodge", fill="#ff6b30")+
  scale_y_continuous(breaks = c(0,0.15,0.30))+
  scale_x_discrete(limit = countInst$Var1) + ggtitle("Ultimo aggiornamento prima del 2018")+
  theme(axis.title = element_blank(), plot.title = element_text(size=11, face="bold"))+
  geom_density(aes(x = as.numeric(Installs)),adjust=1.5, col = "#923c1a")


grid.arrange(ty_2018,ty_20xx, 
             top = "Confronto del numero di installazioni per data dell'ultimo aggiornamento", 
             left = textGrob("Frequenze",rot = 90, vjust = 0.5, hjust = 0.5))
#' Si può notare che la "curva" delle installazioni in entrambi i casi parte un po' in discesa. 
#' Tuttavia mentre se l'app non ha ricevuto aggiornamenti dell'ultimo anno (2018) continua a 
#' decrescere il numero di installazioni, questo non avviene se l'app viene tenuta aggiornata. 
#' In quest'ultimo caso infatti la curva tende ad avere un secondo picco più grande intorno al 
#' milione di installazioni 



# Rat
ggplot(google_app) + geom_boxplot(aes(y=Rating, x=Installs, fill=Installs))
ggplot(google_app) + geom_violin(aes(y=Rating, x=Installs, fill=Installs))


ggplot(google_app) + geom_density(aes(x=Rating, col=Installs))

ggplot(google_app[prova,]) + geom_point(aes(y=Price, x=Size, col= Installs)) + coord_cartesian(ylim = c(0,50))
