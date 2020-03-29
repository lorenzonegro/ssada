google_app <- read.csv("google_app.csv")

# Variabile Installs ricodifica e accorpamenti
inst <- as.character(google_app$Installs)
lev <- c("[0, 1.000)","[1.000, 5.000)","[5.000, 10.000)","[10.000, 50.000)",
         "[50.000, 100.000)","[100.000, 500.000)","[500.000, 1.000.000)", "[1.000.000, 5.000.000)", 
         "[5.000.000, 10.000.000)", "[10.000.000, 50.000.000)","[50.000.000, 100.000.000)", 
         "[100.000.000, 500.000.000)","[500.000.000, 1.000.000.000)", "[1.000.000.000, +)")
reco_factor <- function(x){
  if(x=="1,000,000,000+"){
    return(lev[14])
  } else if(x=="500,000,000+"){
    return(lev[13])
  } else if(x=="100,000,000+"){
    return(lev[12])
  } else if(x=="50,000,000+"){
    return(lev[11])
  } else if(x=="10,000,000+"){
    return(lev[10])
  } else if(x=="5,000,000+"){
    return(lev[9])
  } else if(x=="1,000,000+"){
    return(lev[8])
  } else if(x=="500,000+"){
    return(lev[7])
  } else if(x=="100,000+"){
    return(lev[6])
  } else if(x=="50,000+"){
    return(lev[5])
  } else if(x=="10,000+"){
    return(lev[4])
  } else if(x=="5,000+"){
    return(lev[3])
  } else if(x=="1,000+"){
    return(lev[2])
  } else{
    return(lev[1])
  }
}
instal_corr <- as.character(sapply(inst,reco_factor))
google_app$Installs <- factor(instal_corr, lev)