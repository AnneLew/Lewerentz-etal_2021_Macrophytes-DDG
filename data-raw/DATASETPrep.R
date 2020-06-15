## LOAD Packages
library(dplyr)
library(tidyr)
library(vegan)
library(stringr)

## Set WD
setwd("C:/Users/anl85ck/Desktop/PhD/5_Macrophytes-Bavaria/2_DDGasPackage/MacrophytesDDG")

### Data import
## Macrophytes
MakrophS_raw <- Makroph_comm_S  %>% ungroup() %>% select(-LAKE_TYPE2) %>% rename(Lake=Gewässer)%>%mutate(YEAR=as.factor(YEAR))
MakrophS_raw[is.na(MakrophS_raw)]<-0 #Replace all NA with zero


##Morphology data of lakes in Bavaria
# Morphology <- read.csv("C:/Users/anl85ck/Desktop/PhD/5_Macrophytes-Bavaria/3_WFD-Project/01_Input/lake-data.csv",skip=0 , dec=",",header=TRUE, sep=";")%>%
#   select(-SizeClass, -Region,-Depth,-Volume_hm3, -Type, -Type_simpl, -See_deutsch, -Source, -LastMapping, -Type2, -MNW_Sommer, -MW_Sommer, -MHW_Sommer, -Considered,MHWMNW_Diff, -VQ, -Rechtswert, -Hochwert) #Exclude parameters I don't use
#
# usethis::use_data(Morphology, overwrite = TRUE)

Morph <- Morphology %>% #Import morphological Dataset
  filter(Nat.artifi == "n") %>% #Exclude all artificial lakes
  filter(Lake !="Barmsee") %>%#Weil nur 1 Kartierung
  filter(Area_ha >=50) %>%#No WFD moniotoring
  filter(maxDepth_m > 10) %>% #Exclude shallow lakes
  select(-Nat.artifi) %>%#Exclude parameters I don't use
  rename(WLF = MHWMNW_Diff)

##Selection of macrophytes of lakes of interest (without artificial & shallow)
MakrophS_ALL <- merge(MakrophS_raw, Morph, by.x=c("Lake"), by.y=c("Name_Makro_short") )[,1:97]

## Chemistry
Chem_table <- Chem.Mean.YearDF %>% dplyr::rename(YEAR = Var2, Lake = Var1) #Import Chemical Dataset: Mean for the productive Summer Month between Apr - Aug
Chem_ALL <- Chem_table %>%  group_by(Lake, YEAR)%>% spread(key = Var3, value = value) #Transformation in Table

Chem_selection <- Chem_ALL %>% rowwise() %>%
  transmute(Lake, YEAR,
            #Selection of surface values
            Chloride = Chlorid..mg.l...0.0.m.Tiefe.,
            Conduct = LF..20..U.00B0.C..vor.Ort...U.00B5.S.cm...0.0.m.Tiefe.,
            Ntot = N.ges...mg.l...0.0.m.Tiefe.,
            NH4N = NH4.N..mg.l...0.0.m.Tiefe.,
            NO3N = NO3.N..mg.l...0.0.m.Tiefe.,
            O2diss = O2.gel.f6.st..mg.l...0.0.m.Tiefe.,
            Ptot = P.ges...mg.l...0.0.m.Tiefe.,
            pH = pH.Wert..vor.Ort.......0.0.m.Tiefe.,
            SiO2 = SiO2..mg.l...0.0.m.Tiefe.,
            Temp = Wassertemp..vor.Ort....U.00B0.C...0.0.m.Tiefe.,
            Transp = Sichttiefe..cm...0.0.m.Tiefe.,
            SAC = SPAK.254.nm..1.m...0.0.m.Tiefe. ,
            Tempsd = sd(c(Wassertemp..vor.Ort....U.00B0.C...0.0.m.Tiefe.,Wassertemp..vor.Ort....U.00B0.C...2.0.m.Tiefe.,Wassertemp..vor.Ort....U.00B0.C...4.0.m.Tiefe.,
                          Wassertemp..vor.Ort....U.00B0.C...6.0.m.Tiefe.), na.rm=TRUE))

Chem_uniform <- Chem_selection[rowSums(is.na(Chem_selection[,c(3:13,15)]))==0,] #Selection of datasets (lake&year) where all surface measurements (except SAK) are available at once
Chem_uniform_LOI <- inner_join(Chem_uniform, Morph, by=c("Lake"="Name_chem")) #Chem of Lakes of Interest

## New column to destingush between different Datasets with SAK (Spektraler Absorptionskoeffizient) and without SAK (="NoSAK")
Chem_uniform_LOI$dataset<-Chem_uniform_LOI$SAC
Chem_uniform_LOI$dataset[!is.na(Chem_uniform_LOI$dataset)]<-"SAK"
Chem_uniform_LOI$dataset[is.na(Chem_uniform_LOI$dataset)]<-"No"

## New column to destingush between different Datasets with WLF (Water level fluctuation) and without WLF (="NoWLF")
Chem_uniform_LOI$datasetWLF<-Chem_uniform_LOI$WLF
Chem_uniform_LOI$datasetWLF[!is.na(Chem_uniform_LOI$datasetWLF)]<-"WLF"
Chem_uniform_LOI$datasetWLF[is.na(Chem_uniform_LOI$datasetWLF)]<-"No"

## New column give information about WLF & SAK
Chem_uniform_LOI$datasettot<-NA
Chem_uniform_LOI$datasettot[(Chem_uniform_LOI$dataset=="SAK")&(Chem_uniform_LOI$datasetWLF=="WLF")]<-"LEVEL3"
Chem_uniform_LOI$datasettot[is.na(Chem_uniform_LOI$datasettot)]<-"LEVEL2"



############ CALCULATION Of DIVERSITY METRICES
SPlength <- length(MakrophS_ALL)-1
MakrophS_ALL$ALPHA <- specnumber(MakrophS_ALL[5:(SPlength)])
MakrophS_ALL$DENS_ALPHA <- rowSums(MakrophS_ALL[c(5:(SPlength))]^3)

# Combined dataset without differentiation in Transects - -"MST_NR" or Depth -"Probestelle"
Makroph_Lake_ALL <- MakrophS_ALL %>% group_by(Lake, YEAR) %>%
  summarise_at(vars(-"MST_NR", -"Probestelle"), mean, na.rm=TRUE) ##Mittelwerte der Abundance pro See #, -c((SPlengthS-3):(SPlengthS+4))
SPlength <- length(Makroph_Lake_ALL)-3
Makroph_Lake_ALL$GAMMA <- specnumber(Makroph_Lake_ALL[c(4:SPlength)])
Makroph_Lake_ALL$DENS_GAMMA <- rowSums(Makroph_Lake_ALL[c(4:SPlength)]^3)


## Create table to destinguish between Datasets of Macroph_ALL: without Chem ("NoChem"), "SAK","NoSAK"
DATASET <- left_join(Makroph_Lake_ALL, Chem_uniform_LOI, by.x=c("Lake", "YEAR"), by.y=c("Name_Makro_short","YEAR") )[,c(1:2,121,122,123)]
DATASET$dataset[is.na(DATASET$dataset)]<-"NoChem"
DATASET$datasetWLF[is.na(DATASET$datasetWLF)]<-"NoChem"
DATASET$datasettot[DATASET$dataset=="NoChem"]<-"LEVEL1"

DATASET$datasettotsimpl<- DATASET$datasettot

DATASET<-DATASET %>%
  mutate(datasettotsimpl=ifelse(datasettot=="LEVEL1","Biotic+Abiotic",ifelse(datasettot=="LEVEL2","Biotic",ifelse(datasettot=="LEVEL3","Biotic","NA"))))

MakrophS_ALL<-merge(MakrophS_ALL, DATASET, by=c("Lake", "YEAR")) #Add dataset information to Macrophyte table
Makroph_Lake_ALL<-merge(Makroph_Lake_ALL, DATASET, by=c("Lake", "YEAR")) #Add dataset information to Macrophyte lakes table


# ## Check for annual trend -> Makes no sense due to sampling strategy
# Makroph_YEAR<-Makroph_Lake_ALL %>% group_by(YEAR) %>% summarise_at(vars(-"Lake"), mean, na.rm=TRUE)
# Makroph_YEAR$GAMMA<-specnumber(Makroph_YEAR[c(2:93)])


## Diversity peak analysis
PEAK_ALL <- MakrophS_ALL %>% group_by(Lake, MST_NR, YEAR) %>%  filter(ALPHA == max(ALPHA)) %>% filter(ALPHA>0) #For each Transect filter the depth with the highest species number if there are species
# For each lake & year: Calculation of mean depth (=PEAK)
PEAK_LAKE_ALL <- PEAK_ALL %>% group_by(Lake, YEAR) %>%
  dplyr::summarise(AlphaPeakDepth = mean(Tiefe), AlphaPeakDepth_sd=sd(Tiefe), AlphaPeakRichness = mean(ALPHA), AlphaPeakRichness_sd=sd(ALPHA))

PEAK_LAKE_ALL <- merge(PEAK_LAKE_ALL, Makroph_Lake_ALL[c(1,2,98)], by=c("Lake", "YEAR")) #Add gamma diversity
PEAK_LAKE_Chem <- merge(PEAK_LAKE_ALL, Makroph_Lake_ALL[c(1,2,100,101,102)], by=c("Lake", "YEAR")) #Add dataset type

PEAK_LAKE_ALL$AlphaPeakDepth_sc <- scale(PEAK_LAKE_ALL$AlphaPeakDepth)[,1]
PEAK_LAKE_ALL$AlphaPeakRichness_sc <- scale(PEAK_LAKE_ALL$AlphaPeakRichness)[,1]
#histogram(PEAK_LAKE_ALL$AlphaPeakDepth_sc)
#histogram(PEAK_LAKE_ALL$AlphaPeakRichness_sc)

PEAK_LAKE_ALL<-merge(PEAK_LAKE_ALL, DATASET, by=c("Lake", "YEAR")) #Add dataset information to Peak table



### Depth dependent Analysis for each lake & year
Makroph_Lake_DepthS <- MakrophS_ALL %>% group_by(Lake, Probestelle, YEAR) %>% #Tiefe
  summarise_at(vars(-"MST_NR",-"dataset",-"datasetWLF", -"datasettot"), mean, na.rm=TRUE) #%>% ##Mittelwerte der Abundance pro Tiefenstufe und See
#summarise(mean=mean(),spnrALLsd = sd(spnrALL))
Makroph_Lake_DepthSD <- MakrophS_ALL %>% group_by(Lake, Probestelle, YEAR) %>% #Tiefe
  summarise(ALPHAsd = sd(ALPHA))
Makroph_Lake_DepthS <- merge(Makroph_Lake_DepthS, Makroph_Lake_DepthSD, by=c("Lake", "Probestelle", "YEAR"))

SPlength <- length(Makroph_Lake_DepthS)-5
Makroph_Lake_DepthS$GAMMA <- specnumber(Makroph_Lake_DepthS[c(4:SPlength )]) # Gamma richnes
Makroph_Lake_DepthS$DENS_GAMMA <- rowSums(Makroph_Lake_DepthS[c(4:SPlength)]^3) # Species density at gamma level
Makroph_Lake_DepthS$BETA <- Makroph_Lake_DepthS$GAMMA-Makroph_Lake_DepthS$ALPHA #Beta = Gamma - mean(Alpha)

Makroph_Lake_DepthS<-merge(Makroph_Lake_DepthS, DATASET, by=c("Lake", "YEAR")) #Add dataset information to Peak table



Makroph_Depth <- Makroph_Lake_DepthS %>% group_by(Probestelle) %>% summarise(mAlpha=mean(ALPHA), sdAlpha=sd(ALPHA),
                                                                             mBeta=mean(BETA), sdBeta=sd(BETA),
                                                                             mGamma=mean(GAMMA), sdGamma=sd(GAMMA), Tiefe=mean(Tiefe))

Peak_Gamma <- Makroph_Lake_DepthS  %>%  group_by(Lake, YEAR, dataset, datasetWLF, datasettot) %>% filter(GAMMA == max(GAMMA)) %>%
  select("Lake", "YEAR", "Tiefe", "GAMMA", "dataset", "datasetWLF", "datasettot") %>% summarise(GammaPeakDepth=mean(Tiefe), GammaPeakRichness=mean(GAMMA))

Peak_Gamma <- merge(Peak_Gamma,Makroph_Lake_ALL[c(1,2,98)], by=c("Lake", "YEAR"))

Peak_Beta <- Makroph_Lake_DepthS  %>%  group_by(Lake, YEAR, dataset, datasetWLF, datasettot) %>% filter(BETA == max(BETA)) %>%
  select("Lake", "YEAR", "Tiefe", "BETA", "dataset", "datasetWLF", "datasettot") %>% summarise(BetaPeakDepth=mean(Tiefe),
                                                                                                   BetaPeakRichness=mean(BETA))

Peak_Beta <- merge(Peak_Beta,Makroph_Lake_ALL[c(1,2,98)], by=c("Lake", "YEAR"))


PEAK<-merge(PEAK_LAKE_ALL, Peak_Gamma[c(1,2,6,7)], by=c("Lake", "YEAR"))
PEAK<-merge(PEAK, Peak_Beta[c(1,2,6,7)], by=c("Lake", "YEAR"))

Chem_uniform_LOIx <- inner_join(Chem_uniform_LOI, PEAK, by.x=c("Name_Makro_short", "YEAR"), by.y=c("Lake", "YEAR"))[1:30]
PEAK_Chem<-inner_join(Chem_uniform_LOI, PEAK, by.y=c("Lake", "YEAR"),by.x=c("Name_Makro_short", "YEAR")) #New dataset with chem information of LOI

PEAK_Chem_norm<-bind_cols(PEAK_Chem[c(1,2)], #Lake, YEAR, dataset
                          log(PEAK_Chem[c(3:15,17,18,19,23)]+1),#Chem & Morphometry
                          PEAK_Chem[c(27:37)]) #MBD, SPNRmean, Gamma, group, dataset

## Set WD
setwd("C:/Users/anl85ck/Desktop/PhD/5_Macrophytes-Bavaria/2_DDGasPackage/MacrophytesDDG")
##########################
usethis::use_data(MakrophS_ALL, overwrite = TRUE)
usethis::use_data(Makroph_Lake_DepthS, overwrite = TRUE)
usethis::use_data(Makroph_Depth, overwrite = TRUE)
usethis::use_data(Makroph_Lake_ALL, overwrite = TRUE)

usethis::use_data(PEAK, overwrite = TRUE)
usethis::use_data(Chem_uniform_LOIx, overwrite = TRUE)
usethis::use_data(PEAK_Chem_norm, overwrite = TRUE)
