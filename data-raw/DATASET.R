## code to prepare `DATASET` dataset goes here

#### PREPARATION ####
## Set WD
setwd("C:/Users/anl85ck/Desktop/PhD/5_Macrophytes-Bavaria/2_DDGasPackage/MacrophytesDDG")

## LOAD Packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)


##########################################################################################
###################################   ABIOTIC DATA   #####################################
##### Calculation of mean annual values for selected physical chemical measurements ######
##########################################################################################

## Load data
setwd("./data-raw/abiotic") #In raw data, all values <BG (below detection limit) were replaced with 0 as I don't know all BG values


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

lakes <- subset(lakes, subset = lakes$categories %in% c("Gewässer:","Gew?sser:","Messstellen-Nr.:","Messstellen-Name:"))

## Read in data
lak=lakes[1:3,]
for (i in 2:length(chem.names)){
  lak <- cbind(lak,lakes[((i*3)-2):(i*3),2])
}
lake <- t(lak)
lake <- lake[2:55,]

## Rename colnames
colnames(lake)[colnames(lake)=="3"] <- "Messstellen-Name"
colnames(lake)[colnames(lake)=="4"] <- "Messstellen-Nr"
colnames(lake)[colnames(lake)=="5"] <- "Gewaesser"
rownames(lake) <- c(1:54)
lake <- lake[,c(3,2,1)]
lake.names <- lake[,1]

## Selection of Datasets of variables
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

Years <- unique(unique(format(as.Date(Chem2246$Datum),"%Y"))) ##Chem2246 This dataset provides all years

## Calculation of annual mean of variables per lake & year
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
      Chem.Mean.Year[j,k,i] <- ifelse(length(myvec[myvec$Datum==k, i])>=8, #CONDITION: 8 monthly values have to be available for builing a mean
                                      mean(myvec[myvec$Datum==k, i], na.rm = T), NA)
    }
  }
}

dimnames(Chem.Mean.Year)[[1]] <- lake.names
Chem.Mean.YearDF <- as.data.frame.table(Chem.Mean.Year, responseName = "value")
Chem.Mean.YearDF <- subset(Chem.Mean.YearDF, !is.na(Chem.Mean.YearDF$value))

## Save data
setwd("C:/Users/anl85ck/Desktop/PhD/5_Macrophytes-Bavaria/2_DDGasPackage/MacrophytesDDG")
usethis::use_data(Chem.Mean.YearDF, overwrite = TRUE)




##########################################################################################
###################################   BIOTIC DATA   ######################################
#################### Result: Community table for submerged macrophytes ###################
##########################################################################################


## Set WD
setwd("C:/Users/anl85ck/Desktop/PhD/5_Macrophytes-Bavaria/2_DDGasPackage/MacrophytesDDG")

## Load data
Makroph <- read.csv("./data-raw/Makrophyten_WRRL_05-17_nurMakrophytes.csv", header=TRUE, sep=";")

## Filter for unplausible datasets
Makroph <- Makroph %>%
  filter(!(Gewässer=="Chiemsee" & (YEAR==2011))) %>% filter(!(Gewässer=="Chiemsee" & YEAR==2012)) %>% # 1 plot per year -> wrong
  filter(!(Gewässer=="Chiemsee" & (YEAR==2014))) %>% filter(!(Gewässer=="Chiemsee" & (YEAR==2015))) %>%
  filter(!(Gewässer=="Chiemsee" & (YEAR==2017))) %>% filter(!(Gewässer=="Staffelsee - Sued" & (YEAR==2012))) %>%
  filter(!(Gewässer=="Gr. Alpsee" & (YEAR==2012))) %>% filter(!(Gewässer=="Gr. Alpsee" & (YEAR==2013))) %>%
  filter(!(Gewässer=="Pilsensee" & (YEAR==2015))) %>% filter(!(Gewässer=="Langbuergner See" & (YEAR==2014))) %>%
  filter(!(Gewässer=="Pelhamer See" & (YEAR==2017))) %>%  filter(!(Gewässer=="Weitsee" & (YEAR==2017))) %>%
  distinct()

## Rename values of depth
Makroph$Probestelle <- plyr::revalue(Makroph$Probestelle, c("0-1 m"="0-1", "1-2 m"="1-2", "2-4 m"="2-4",">4 m"="4-x" ))

## Selection of species that were determined until species level
Makroph <- Makroph %>%
  filter(str_detect(Taxon, " ")) %>%
  filter(Taxon != "Ranunculus, aquatisch")

## To get a dataset with all possible PLOTS
Makroph_dataset <- Makroph %>% group_by(Gewässer, MST_NR, YEAR) %>%
  select(Gewässer, MST_NR, YEAR, LAKE_TYPE2) %>% #3590 * Gew?sser, MST_NR, YEAR, Probestelle IIII 1013 *4 => 4052 m?sstens eigentlich mind sein
  distinct()
Probestelle <- tibble(Probestelle=c("4-x","0-1","1-2", "2-4")) # tibble(Probestelle)
Makroph_dataset <- merge(Makroph_dataset, Probestelle, by=NULL) #996 * 4 = 3984

## Select for Submerged species
Makroph_comm_S2 <- Makroph %>% group_by(Gewässer, MST_NR, DATUM, Probestelle, Taxon) %>%
  filter(Form=="S") %>%
  ungroup()%>% group_by(Gewässer, MST_NR, Probestelle, YEAR, Taxon) %>%
  summarise(Messwert = mean(Messwert)) %>% #get rid of double values for DATUM
  select(Gewässer, MST_NR, YEAR, Probestelle, Taxon, Messwert) %>%  #duplicated %>% which %>% #check for duplications
  spread(Taxon, Messwert)%>%
  select_if(~sum(!is.na(.)) > 0)

Makroph_comm_S <-  right_join(Makroph_comm_S2, Makroph_dataset, by=c("Gewässer", "MST_NR", "YEAR", "Probestelle"))
Makroph_comm_S$Tiefe <- plyr::revalue(Makroph_comm_S$Probestelle, c("0-1"="-0.5", "1-2"="-1.5", "2-4"="-3","4-x"="-5"))
Makroph_comm_S<-Makroph_comm_S%>%mutate(Tiefe=as.numeric(Tiefe))

## Save data
usethis::use_data(Makroph_comm_S, overwrite = TRUE)

