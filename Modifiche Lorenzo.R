#Correzioni Lorenzo
#Variabile 3

table(gps$Type)
#Ho 1 NaN e 1 0, le elimino entrambe
eliminare=which(gps$Type=="NaN")
eliminare=c(eliminare,which(gps$Type=="0"))
eliminare
gps[eliminare,]
gps=gps[-eliminare,]

#Controllo
which(gps$Type=="NaN")
which(gps$Type=="0")

#Variabile 6: Collassare Type e price
table(gps$Price)
gps$Price=gsub( "\\$","",gps$Price)
table(gps$Price)

#Variabile 8
str(gps$Genres) #120 Livelli
table(gps$Genres)
gps$Genres=as.character(gps$Genres)
i=1
for(i in (1:length(gps$Genres)))
{
  a=strsplit(gps$Genres[i],";")[[1]]
  if(length(a)==2)
  {
    gps$Genres[i]=a[2]
  }
  else
  {
    gps$Genres[i]=a[1]
  }
}

table(gps$Genres)
gps$Genres[2]
gps$Genres=as.factor(gps$Genres)
str(gps$Genres) #53 livelli

#Variabile 9
Sys.setlocale("LC_ALL","C") #devo cambiare formato in cui vengono letti i mesi in inglese
gps$Last.Updated=as.Date(as.character(gps$Last.Updated),format='%B %d, %Y')
gps$Last.Updated
