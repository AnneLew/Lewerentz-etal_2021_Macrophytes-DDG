## Set WD
setwd("C:/Users/anl85ck/Desktop/PhD/5_Macrophytes-Bavaria/2_DDGasPackage/MacrophytesDDG")

## LOAD Packages
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

## Load data
Makroph <- read.csv("./rawdata/Makrophyten_WRRL_05-17_nurMakrophytes.csv", header=TRUE, sep=";") #%>%

#Filter for unplausible datasets
Makroph <- Makroph %>%
  filter(!(Gewässer=="Chiemsee" & (YEAR==2011))) %>% filter(!(Gewässer=="Chiemsee" & YEAR==2012)) %>% # 1 plot per year -> wrong
  filter(!(Gewässer=="Chiemsee" & (YEAR==2014))) %>% filter(!(Gewässer=="Chiemsee" & (YEAR==2015))) %>%
  filter(!(Gewässer=="Chiemsee" & (YEAR==2017))) %>% filter(!(Gewässer=="Staffelsee - Sued" & (YEAR==2012))) %>%
  filter(!(Gewässer=="Gr. Alpsee" & (YEAR==2012))) %>% filter(!(Gewässer=="Gr. Alpsee" & (YEAR==2013))) %>%
  filter(!(Gewässer=="Pilsensee" & (YEAR==2015))) %>% filter(!(Gewässer=="Langbuergner See" & (YEAR==2014))) %>%
  filter(!(Gewässer=="Pelhamer See" & (YEAR==2017))) %>%  filter(!(Gewässer=="Weitsee" & (YEAR==2017))) %>%
  #subset(!(Gewässer %in% c("Rottachsee", "Seehamer See", "Walchensee"))) %>% ###beide sind keine nat?rlichen Seen, Walchensee hat keine nat?rl Dynamik
  distinct()

Makroph$Probestelle <- revalue(Makroph$Probestelle, c("0-1 m"="0-1", "1-2 m"="1-2", "2-4 m"="2-4",">4 m"="4-x" ))

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


### Submerged species
Makroph_comm_S2 <- Makroph %>% group_by(Gewässer, MST_NR, DATUM, Probestelle, Taxon) %>%
  filter(Form=="S") %>%
  ungroup()%>% group_by(Gewässer, MST_NR, Probestelle, YEAR, Taxon) %>%
  summarise(Messwert = mean(Messwert)) %>% #get rid of double values for DATUM
  select(Gewässer, MST_NR, YEAR, Probestelle, Taxon, Messwert) %>%  #duplicated %>% which %>% #check for duplications
  spread(Taxon, Messwert)%>%
  select_if(~sum(!is.na(.)) > 0)

Makroph_comm_S <-  right_join(Makroph_comm_S2, Makroph_dataset, by=c("Gewässer", "MST_NR", "YEAR", "Probestelle"))
Makroph_comm_S$Tiefe <- revalue(Makroph_comm_S$Probestelle, c("0-1"="-0.5", "1-2"="-1.5", "2-4"="-3","4-x"="-5"))

write.csv(Makroph_comm_S, "./data/Makroph_WRRL_05-17_COMM_S.csv")
