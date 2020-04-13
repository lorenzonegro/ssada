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

length(which(google_app$Category==google_app$Genres))
