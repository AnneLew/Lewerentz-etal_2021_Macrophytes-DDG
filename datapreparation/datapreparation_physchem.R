#### PREPARATION ####
## Set WD
setwd("C:/Users/anl85ck/Desktop/PhD/5_Macrophytes-Bavaria/2_DDGasPackage/MacrophytesDDG")

## LOAD Packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)


############### Import Data ###############
###########################################

setwd("./rawdata/seen-chemie_all")
chem <- list.files(pattern="*.csv")
chem.names <- paste("Chem", strtrim(chem, 4), sep="")
for (i in 1:length(chem.names)) {
  assign(chem.names[i], read.csv(chem[i], skip=8 , dec=",", header=TRUE, sep=";",stringsAsFactors = FALSE, encoding = "UTF-8"))
}

## Read in Head of the table
lakes <- data.frame(categories=character(),data=character())

for (i in chem) {
  dat <- read.csv(i, nrows=6, dec=",", sep=";", skip=0, header=TRUE)[,1:2]
  names(dat) <- names(lakes)
  lakes <- rbind(lakes, dat)
}
#colnames(lakes)[1] <- "categories"
#colnames(lakes)[2] <- "data"
lakes <- subset(lakes, subset = lakes$categories %in% c("Gewässer:","Gew?sser:","Messstellen-Nr.:","Messstellen-Name:"))

lak=lakes[1:3,]
for (i in 2:length(chem.names)){
  lak <- cbind(lak,lakes[((i*3)-2):(i*3),2])
}
lake <- t(lak)
#L<-length(lake)
lake <- lake[2:55,] #!
colnames(lake)[colnames(lake)=="3"] <- "Messstellen-Name"
colnames(lake)[colnames(lake)=="4"] <- "Messstellen-Nr"
colnames(lake)[colnames(lake)=="5"] <- "Gewaesser"
rownames(lake) <- c(1:54)
lake <- lake[,c(3,2,1)]
lake.names <- lake[,1]

names0 <- names(Chem2138)
for (i in chem.names){
  names0 <- intersect(names0, names(eval(as.name(i))))
  print(names0)
}

# Chem.Attributes <- names0[2:75]
#Chem.Attributes[75]<-"SPAK.254.nm..1.m...0.0.m.Tiefe."
#Chem.Attributes[88]<-"Phaeopig..?g.l...0.0.m.Tiefe."
#Chem.Attributes[76]<-"Chloroph..?g.l...0.0.m.Tiefe."

Chem.Attributes <- c("NO3.N..mg.l...0.0.m.Tiefe.","SiO2..mg.l...0.0.m.Tiefe.","P.ges...mg.l...0.0.m.Tiefe.",
                     "O2.gel.f6.st..mg.l...0.0.m.Tiefe.","NH4.N..mg.l...0.0.m.Tiefe.","pH.Wert..vor.Ort.......0.0.m.Tiefe.",
                     "Wassertemp..vor.Ort....U.00B0.C...0.0.m.Tiefe.","LF..20..U.00B0.C..vor.Ort...U.00B5.S.cm...0.0.m.Tiefe.",
                     "N.ges...mg.l...0.0.m.Tiefe.","Chlorid..mg.l...0.0.m.Tiefe.","Sichttiefe..cm...0.0.m.Tiefe.","SPAK.254.nm..1.m...0.0.m.Tiefe.",

                     "NO3.N..mg.l...2.0.m.Tiefe.","SiO2..mg.l...2.0.m.Tiefe.","P.ges...mg.l...2.0.m.Tiefe.",
                     "O2.gel.f6.st..mg.l...2.0.m.Tiefe.","NH4.N..mg.l...2.0.m.Tiefe.","pH.Wert..vor.Ort.......2.0.m.Tiefe.",
                     "Wassertemp..vor.Ort....U.00B0.C...2.0.m.Tiefe.","LF..20..U.00B0.C..vor.Ort...U.00B5.S.cm...2.0.m.Tiefe.",
                     "N.ges...mg.l...2.0.m.Tiefe.","Chlorid..mg.l...2.0.m.Tiefe.",

                     "NO3.N..mg.l...4.0.m.Tiefe.","SiO2..mg.l...4.0.m.Tiefe.","P.ges...mg.l...4.0.m.Tiefe.",
                     "O2.gel.f6.st..mg.l...4.0.m.Tiefe.","NH4.N..mg.l...4.0.m.Tiefe.","pH.Wert..vor.Ort.......4.0.m.Tiefe.",
                     "Wassertemp..vor.Ort....U.00B0.C...4.0.m.Tiefe.","LF..20..U.00B0.C..vor.Ort...U.00B5.S.cm...4.0.m.Tiefe.",
                     "N.ges...mg.l...4.0.m.Tiefe.","Chlorid..mg.l...4.0.m.Tiefe.",

                     "NO3.N..mg.l...6.0.m.Tiefe.","SiO2..mg.l...6.0.m.Tiefe.","P.ges...mg.l...6.0.m.Tiefe.",
                     "O2.gel.f6.st..mg.l...6.0.m.Tiefe.","NH4.N..mg.l...6.0.m.Tiefe.","pH.Wert..vor.Ort.......6.0.m.Tiefe.",
                     "Wassertemp..vor.Ort....U.00B0.C...6.0.m.Tiefe.","LF..20..U.00B0.C..vor.Ort...U.00B5.S.cm...6.0.m.Tiefe.",
                     "N.ges...mg.l...6.0.m.Tiefe.","Chlorid..mg.l...6.0.m.Tiefe.")

Years <- unique(unique(format(as.Date(Chem2246$Datum),"%Y"))) ##Chem2246 l?ngester Datenstz mit allen Jahren

##### <BG ersetzt mit 0 ###  MACHT DAS SINN? evt eher Wert = 0.5 * BG || ABER: BG kenne ich nicht.




############### Berechnung von Mittelwerten pro See, Parameter, zT Jahr ###############
#######################################################################################

##MEAN for each year, lake and attribute!!
Chem.Mean.Year <- array(0, dim=c(length(chem.names),length(Years), length(Chem.Attributes)),
                        dimnames = list(chem.names,Years,Chem.Attributes))

for (j in chem.names){
  for (i in Chem.Attributes){
    if (!(i %in% colnames(eval(as.name(j))))){
      next
    }
    for (k in Years){
      myvec <- eval(as.name(j))
      myvec <- select(myvec, 1, i)
      myvec[[1]]<- format(as.Date(myvec[[1]]), "%Y")
      #Chem.Mean.Year[j,k,i] <- mean(myvec[myvec$Datum==k, i], na.rm = T)
      Chem.Mean.Year[j,k,i] <- ifelse(length(myvec[myvec$Datum==k, i])>=8,
                                      mean(myvec[myvec$Datum==k, i], na.rm = T), NA)
    }
  }
}

dimnames(Chem.Mean.Year)[[1]] <- lake.names
Chem.Mean.YearDF <- as.data.frame.table(Chem.Mean.Year, responseName = "value")
Chem.Mean.YearDF <- subset(Chem.Mean.YearDF, !is.na(Chem.Mean.YearDF$value))

setwd("C:/Users/anl85ck/Desktop/PhD/5_Macrophytes-Bavaria/2_DDGasPackage/MacrophytesDDG")
#write.csv(Chem.Mean.YearDF, file="./data/Chem.Mean.YearDF_ALL_DiffDepth.csv")

save(Chem.Mean.YearDF, file="./data/Chem.Mean.YearDF_ALL_DiffDepth.RData")








# ##SUMMERMEAN for each year, lake and attribute!!
# Chem.Mean.Apr_Aug <- array(0, dim=c(length(chem.names),length(Years), length(Chem.Attributes)),
#                            dimnames = list(chem.names,Years,Chem.Attributes))
#
# for (j in chem.names){
#   for (i in Chem.Attributes){
#     if (!(i %in% colnames(eval(as.name(j))))){
#       next
#     }
#     for (k in Years){
#       myvec <- eval(as.name(j))
#       myvec <- select(myvec, 1, i)
#       myvec[[1]]<- format(as.Date(myvec[[1]]), "%d/%m/%Y")
#       myvec <- myvec[month(dmy(myvec[[1]]))==4 |month(dmy(myvec[[1]]))==5 | month(dmy(myvec[[1]]))==6 |
#                        month(dmy(myvec[[1]]))==7 | month(dmy(myvec[[1]]))==8,]
#       Chem.Mean.Apr_Aug[j,k,i] <- ifelse(length(myvec[year(dmy(myvec$Datum))==k, i])>=4,
#                                          mean(myvec[year(dmy(myvec$Datum))==k, i], na.rm = T), NA)
#     }
#   }
# }
#
# setwd("C:/Users/anl85ck/Desktop/PhD/5_Macrophytes-Bavaria/3_WFD-Project")
# #save(Chem.Mean.Apr_Aug, file="./03_Results/Chem.Mean.Apr_Aug.rda")
#
# dimnames(Chem.Mean.Apr_Aug)[[1]] <- lake.names
# Chem.Mean.Apr_AugDF <- as.data.frame.table(Chem.Mean.Apr_Aug, responseName = "value")
# Chem.Mean.Apr_AugDF <- subset(Chem.Mean.Apr_AugDF, !is.na(Chem.Mean.Apr_AugDF$value))
# write.csv(Chem.Mean.Apr_AugDF, file="./01_Input/Chem.Mean.Apr_AugDF_ALL.csv")
#
#
