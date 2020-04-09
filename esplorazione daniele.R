library(ggplot2)
library(lubridate)
load("google_app_final.RData")
countInst <- as.data.frame(table(google_app$Installs))
ggplot(google_app) + geom_bar(aes(y=Installs))
table(google_app$Installs)


# Type_Price
ggplot(google_app[google_app$Type_Price=="Free",]) + 
  geom_bar(aes(y=Installs, x = (..count..)/sum(..count..)), position="dodge")
ggplot(google_app[google_app$Type_Price=="Paid",]) + 
  geom_bar(aes(y=Installs, x = (..count..)/sum(..count..)), position="dodge")
ggplot(google_app[google_app$Type_Price=="Plus",]) + 
  geom_bar(aes(y=Installs, x = (..count..)/sum(..count..)), position="dodge")

ggplot(google_app) + 
  geom_bar(aes(y=Installs, fill=Type_Price), position=position_dodge2(width = 0.9, preserve = "single"))+
  facet_grid(rows=Type_Price)

ggplot(google_app) + 
  geom_bar(aes(x=Type_Price, fill=Installs), position=position_dodge2(width = 0.9, preserve = "single"))


#Size
ggplot(google_app) + geom_boxplot(aes(x=Size, y=Installs, fill=Installs))+
  theme(legend.position = "none")
sum(is.na(google_app$Size))
#ATTENZIONE CI SONO NA!!!! eliminiamo le righe??

#Price
prova <- which(google_app$Price>0)
ggplot(google_app[prova,]) + geom_boxplot(aes(x=Price, y=Installs, fill=Installs))

ggplot(google_app) + geom_bar(aes(x=year(Last.Updated), fill=Installs), position="dodge")




ggplot(google_app[prova,]) + geom_point(aes(y=Price, x=Size, col= Installs)) + coord_cartesian(ylim = c(0,50))
