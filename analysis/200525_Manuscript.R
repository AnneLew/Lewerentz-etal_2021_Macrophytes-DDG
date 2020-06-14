## Set WD
#setwd("C:/Users/anl85ck/Desktop/PhD/5_Macrophytes-Bavaria/3_WFD-Project")

## ggplot theme updates
source("C:/Users/anl85ck/Desktop/PhD/5_Macrophytes-Bavaria/3_WFD-Project/02_Themes/tidy_white_anne.R")

## LOAD Packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(vegan)
library(gamm4)
library(ggpubr)
library(stringr)
library(corrplot)
library(MacrophytesDDG)


### Datasets ####
MakrophS_ALL %>% summarise(NLake = n_distinct(Lake),NTrans = n_distinct(MST_NR),NDepth = n_distinct(Probestelle),NYEAR = n_distinct(YEAR))

MakrophS_ALL %>% filter(datasettot=="LEVEL3") %>% group_by(datasettot) %>% summarise(NLake = n_distinct(Lake),N = n_distinct(Lake,YEAR),
                                                                                     NTrans = n_distinct(MST_NR),NDepth = n_distinct(Probestelle),NYEAR = n_distinct(YEAR))

MakrophS_ALL %>% filter(datasettot=="LEVEL3"|datasettot=="LEVEL2") %>% summarise(NLake = n_distinct(Lake),N = n_distinct(Lake,YEAR),
                                                                                 NTrans = n_distinct(MST_NR),NDepth = n_distinct(Probestelle),NYEAR = n_distinct(YEAR))

ggplot(Makroph_Lake_ALL, aes(y=GAMMA))+geom_boxplot()
min(Makroph_Lake_ALL$GAMMA)
max(Makroph_Lake_ALL$GAMMA)

########## Normalverteilung? -> Nein
shapiro.test(PEAK$AlphaPeakDepth) #Normalverteilung: kann bei diesem Test eine Signifikanz (p < 0.05) festgestellt werden, so liegt keine Normalverteilung vor.
shapiro.test(PEAK$AlphaPeakRichness)
shapiro.test(PEAK$BetaPeakDepth)
shapiro.test(PEAK$BetaPeakRichness)
shapiro.test(PEAK$GammaPeakDepth)
shapiro.test(PEAK$GammaPeakRichness)
shapiro.test(PEAK$GAMMA)

### RESULTS QUESTION 1 ####
ggplot(data=Makroph_Depth)+
  geom_line(aes(x=(Tiefe), y=mAlpha), col="red")+
  #geom_errorbar(aes(x=(Tiefe),ymin=mAlpha-sdAlpha, ymax=mAlpha+sdAlpha), width=.1)+
  ylab("Alpha richness")+xlab(("Depth (m)"))+ scale_x_reverse()+
  geom_line(aes(x=(Tiefe), y=mBeta), col="blue")+
  #geom_errorbar(aes(x=(Tiefe),ymin=mBeta-sdBeta, ymax=mBeta+sdBeta), width=.1)+
  ylab("Richness")+xlab(("Depth (m)"))+
  geom_line(aes(x=(Tiefe), y=mGamma), col="purple")
  #geom_errorbar(aes(x=(Tiefe),ymin=mGamma-sdGamma, ymax=mGamma+sdGamma), width=.1)


A1<-ggplot(data=Makroph_Lake_DepthS)+
  geom_line(aes(x=Tiefe, y=ALPHA, group=interaction(Lake, YEAR)),col="grey")+ scale_x_reverse()+
  geom_line(data=Makroph_Depth,aes(x=(Tiefe), y=mAlpha))+
  geom_errorbar(data=Makroph_Depth,aes(x=(Tiefe),ymin=mAlpha-sdAlpha, ymax=mAlpha+sdAlpha), width=.1)+
  ylab("Alpha richness")+xlab(("Depth (m)"))
B1<-ggplot(data=Makroph_Lake_DepthS)+
  geom_line(aes(x=Tiefe, y=BETA, group=interaction(Lake, YEAR)),col="grey")+ scale_x_reverse()+
  geom_line(data=Makroph_Depth,aes(x=(Tiefe), y=mBeta))+
  geom_errorbar(data=Makroph_Depth,aes(x=(Tiefe),ymin=mBeta-sdBeta, ymax=mBeta+sdBeta), width=.1)+
  ylab("Beta richness")+xlab(("Depth (m)"))
C1<-ggplot(data=Makroph_Lake_DepthS)+
  geom_line(aes(x=Tiefe, y=GAMMA, group=interaction(Lake, YEAR)),col="grey")+ scale_x_reverse()+
  geom_line(data=Makroph_Depth,aes(x=(Tiefe), y=mGamma))+
  geom_errorbar(data=Makroph_Depth,aes(x=(Tiefe),ymin=mGamma-sdGamma, ymax=mGamma+sdGamma), width=.1)+
  ylab("Gamma richness")+xlab(("Depth (m)"))
ggarrange(A1,B1,C1,ncol=3,nrow=1,labels="auto")



# A<-ggplot(data=Makroph_Depth)+
#   geom_line(aes(x=(Tiefe), y=mAlpha))+
#   geom_errorbar(aes(x=(Tiefe),ymin=mAlpha-sdAlpha, ymax=mAlpha+sdAlpha), width=.1)+
#   ylab("Alpha richness")+xlab(("Depth (m)"))+ scale_x_reverse()
#
# B<-ggplot(data=Makroph_Depth)+
#   geom_line(aes(x=(Tiefe), y=mBeta))+
#   geom_errorbar(aes(x=(Tiefe),ymin=mBeta-sdBeta, ymax=mBeta+sdBeta), width=.1)+
#   ylab("Beta richness")+xlab(("Depth (m)"))+ scale_x_reverse()
#
# C<-ggplot(data=Makroph_Depth)+
#   geom_line(aes(x=(Tiefe), y=mGamma))+
#   geom_errorbar(aes(x=(Tiefe),ymin=mGamma-sdGamma, ymax=mGamma+sdGamma), width=.1)+
#   ylab("Gamma richness")+xlab(("Depth (m)"))+ scale_x_reverse()
#
# ggarrange(A,B,C,ncol=3,nrow=1,labels="auto")

Makroph_Depth

cor.test(Makroph_Lake_DepthS$ALPHA, Makroph_Lake_DepthS$BETA, method = "pearson")
cor.test(Makroph_Lake_DepthS$ALPHA, Makroph_Lake_DepthS$GAMMA, method = "pearson")
cor.test(Makroph_Lake_DepthS$BETA, Makroph_Lake_DepthS$GAMMA, method = "pearson")


#PATTERN ANALYSIS
PEAK_LAKE_ALL$DepthGroup<-if_else(PEAK_LAKE_ALL$AlphaPeakDepth>-1,"1",
                                  if_else(PEAK_LAKE_ALL$AlphaPeakDepth>-2,"2",
                                          if_else(PEAK_LAKE_ALL$AlphaPeakDepth>-4,"3","4")))

PEAKCLASS1<-PEAK_LAKE_ALL %>% group_by(DepthGroup) %>% summarise(n_distinct(interaction(Lake,YEAR)))
PEAKCLASS1

PEAK %>% group_by(GammaPeakDepth) %>% summarise(n_distinct(interaction(Lake,YEAR)))

PEAK %>% group_by(BetaPeakDepth) %>% summarise(n_distinct(interaction(Lake,YEAR)))

chisq.test(PEAKCLASS1$`n_distinct(interaction(Lake, YEAR))`)

###Alpha&Peak Plot
ggplot(data=Makroph_Lake_DepthS, aes(x=(Tiefe), y=ALPHA, col=factor(YEAR), group=interaction(Lake,YEAR)))+
  geom_line(aes(linetype=datasettot))+#interaction(dataset,datasetWLF)
  facet_wrap(~ Lake, ncol=7)+
  ylab("Alpha richness")+ xlab("")+labs(fill = "Pattern type")+ylim(0,15)+
  geom_errorbar(data=Makroph_Lake_DepthS,aes(ymin=ALPHA-ALPHAsd, ymax=ALPHA+ALPHAsd), width=.1)+
  geom_point(data=PEAK_LAKE_ALL, aes(x=(AlphaPeakDepth), y=AlphaPeakRichness, shape=datasettot))+ scale_x_reverse()
### GAMMA & Peak Plot
ggplot(Makroph_Lake_DepthS, aes(x=Tiefe, y=GAMMA, group=interaction(Lake,YEAR), col=factor(YEAR), linetype=datasettot))+
  geom_line()+facet_wrap(~ Lake, ncol=10)+
  ylab("Gamma richness")+ xlab("Depth (m)")+labs(fill = "Pattern type")+
  geom_point(data=PEAK, aes(x=GammaPeakDepth, y=GammaPeakRichness, shape=datasettot))+ scale_x_reverse()
### BETA & Peak Plot
ggplot(Makroph_Lake_DepthS, aes(x=Tiefe, y=BETA, group=interaction(Lake,YEAR), col=factor(YEAR), linetype=datasettot))+
  geom_line()+facet_wrap(~ Lake, ncol=10)+
  ylab("Beta richness")+ xlab("Depth (m)")+labs(fill = "Pattern type")+
  geom_point(data=PEAK, aes(x=BetaPeakDepth, y=BetaPeakRichness, shape=datasettot))+ scale_x_reverse()


## PEAK Plot
PPalpha<-ggplot(PEAK_LAKE_ALL, aes(y=AlphaPeakRichness, x=AlphaPeakDepth, col=datasettot))+
  geom_point()+# labs(col = "Pattern type")+
  #geom_text(aes(label=paste(Lake,YEAR)),hjust=0, vjust=0)+
  xlab("AlphaPeak Depth (m)")+ylab("AlphaPeak richness (N)")+
  theme(legend.position = c(0.9, 0.2))+ scale_x_reverse()

PPbeta<-ggplot(PEAK, aes(x=BetaPeakDepth, y=BetaPeakRichness, col=datasettot))+geom_point()+ scale_x_reverse()+
  xlab("BetaPeak Depth (m)")+ylab("BetaPeak richness (N)")

PPgamma<-ggplot(PEAK, aes(x=GammaPeakDepth, y=GammaPeakRichness, col=datasettot))+geom_point()+ scale_x_reverse()+
  xlab("GammaPeak Depth (m)")+ylab("GammaPeak richness (N)")

ggarrange(PPalpha,PPbeta,PPgamma, ncol=3,common.legend = T, legend="bottom", labels="auto")



p_format <- function(x, ndp=3)
{
  out <- format(round(as.numeric(x),ndp),ns=ndp,scientific=F,just="none")
  ifelse(out=="0.000","<0.01", out)
}

corrplot(cor(PEAK[c(7,3,16,14,5,17,15)], use="complete.obs"), type = "upper")
pval <- psych::corr.test(PEAK[c(7,3,16,14,5,17,15)], adjust="none")$p
pos <- expand.grid(1:ncol(pval), ncol(pval):1)
text(pos, p_format(pval))
corrplot(cor(PEAK[c(7,3,16,14,5,17,15)], use="complete.obs"), type="lower", add=T, tl.pos="d", cl.pos="n")

corrplot(cor(PEAK[c(7,3,16,14,5,17,15)], use="complete.obs"), type = "upper", method = "number", insig = "blank")

cor.test(PEAK$GAMMA, PEAK$AlphaPeakRichness, method = "pearson")
cor.test(PEAK$GAMMA, PEAK$BetaPeakRichness, method = "pearson")
cor.test(PEAK$GAMMA, PEAK$GammaPeakRichness, method = "pearson")

cor.test(PEAK$AlphaPeakDepth, PEAK$BetaPeakDepth, method = "pearson")
cor.test(PEAK$AlphaPeakDepth, PEAK$GammaPeakDepth, method = "pearson")
cor.test(PEAK$BetaPeakDepth, PEAK$GammaPeakDepth, method = "pearson")

cor.test(PEAK$AlphaPeakRichness, PEAK$BetaPeakRichness, method = "pearson")
cor.test(PEAK$AlphaPeakRichness, PEAK$GammaPeakRichness, method = "pearson")
cor.test(PEAK$BetaPeakRichness, PEAK$GammaPeakRichness, method = "pearson")


# # Plot
# corrplot(cor(M), type="upper", tl.pos="n")
#
# # Get positions & plot formatted p-values
# pos <- expand.grid(1:ncol(pval), ncol(pval):1)
# text(pos, p_format(pval))
#
# # lower tri
# corrplot(cor(M), type="lower", add=T, tl.pos="d", cl.pos="n")


##TEST for Represantativity #Permanova

PEAK$datasetreduced<- "LEVEL"
PEAK$datasetreduced[PEAK_LAKE_ALL$datasettot=="LEVEL1"]<-"LEVEL99"
PEAK$datasetreduced[PEAK_LAKE_ALL$datasettot=="LEVEL2"]<-"LEVEL99"
PEAK$datasetreduced[PEAK_LAKE_ALL$datasettot=="LEVEL3"]<-"LEVEL3"


#TEST Is LEVEL 3 ?quivalent to Rest of data
A<-scale(PEAK[,c(3,5)])

histogram(A[,1])
Adista<-vegdist(A, method="bray")

adonis2(A~datasetreduced, data=PEAK, by = NULL) #ALPHAPEAK
adonis2(PEAK[,c(13,14)]~datasetreduced, data=PEAK) #BETAPEAK
adonis2(PEAK[,c(15,16)]~datasetreduced, data=PEAK) #GAMMAPEAK


# data(dune)
# data(dune.env)
#
# # default test by terms
# adonis2(dune ~ Management*A1, data = dune.env)
#
# # overall tests
# adonis2(dune ~ Management*A1, data = dune.env, by = NULL)


## TRANSECT ALPHA RICHNESS PATTERN ALL LAKES PLOTS
ggplot(MakrophS_ALL, aes(x=Probestelle, y=ALPHA, col=factor(YEAR), group=interaction(Lake,YEAR,MST_NR), linetype=dataset))+
  geom_line(alpha=0.3)+
  ylab("Alpha richness all Transects")+ xlab("Depth (m)")+labs(fill = "Pattern type")+ylim(0,15)+
  facet_wrap(~ Lake, ncol=10)


##############################################################################################################################
##############################################################################################################################

## DATA OVERVIEW
detach(package:ggbiplot)
detach(package:plyr)
Chem_uniform_LOIx %>% filter(datasettot=="LEVEL3") %>% group_by(datasettot)%>%
  summarise(MArea = mean(Area_ha),MinArea = min(Area_ha),MaxArea = max(Area_ha),
            MDepth = mean(maxDepth_m),MinDepth = min(maxDepth_m),MaxDepth = max(maxDepth_m),
            MWLF = mean(WLF),MinWLF= min(WLF),MaxWLF = max(WLF)
            #MVol = mean(Volume_hm3),MinVol= min(Volume_hm3),MaxVol= max(Volume_hm3),
            #MCatch = mean(CatchmentArea_km2),MinCatch = min(CatchmentArea_km2),MaxCatch = max(CatchmentArea_km2),
            #MAltitude = mean(Altitude_masl),MinAlti = min(Altitude_masl),MaxAlto = max(Altitude_masl)
            )
Chem_uniform_LOIx %>% ungroup() %>%#filter(datasettot=="LEVEL3"|datasettot=="LEVEL2") %>% #group_by(datasettot)%>%
  dplyr::summarize(MArea = mean(Area_ha),MinArea = min(Area_ha),MaxArea = max(Area_ha),
            MDepth = mean(maxDepth_m),MinDepth = min(maxDepth_m),MaxDepth = max(maxDepth_m),
            MWLF = mean(WLF),MinWLF= min(WLF),MaxWLF = max(WLF)
            #MVol = mean(Volume_hm3),MinVol= min(Volume_hm3),MaxVol= max(Volume_hm3),
            #MCatch = mean(CatchmentArea_km2),MinCatch = min(CatchmentArea_km2),MaxCatch = max(CatchmentArea_km2),
            #MAltitude = mean(Altitude_masl),MinAlti = min(Altitude_masl),MaxAlto = max(Altitude_masl)
  )

#install.packages("finalfit")
#library(finalfit)
Chem_overview_SmallDataset<-Chem_uniform_LOIx %>% rowwise() %>% filter(datasettot=="LEVEL3") %>% group_by(datasettot) %>%
  summarise(MChl = mean(Chloride, na.rm = T),MinChlor = min(Chloride, na.rm = T),MaxChlor = max(Chloride, na.rm = T),
            MCond = mean(Conduct, na.rm = T),MinCond = min(Conduct, na.rm = T),MaxCond = max(Conduct, na.rm = T),
            MN_tot = mean(Ntot, na.rm = T),MinN_tot = min(Ntot, na.rm = T),MaxN_tot = max(Ntot, na.rm = T),
            MNH4N_0 = mean(NH4N, na.rm = T),MinNH4N = min(NH4N, na.rm = T),MaxNH4 = max(NH4N, na.rm = T),
            MNO3N_0 = mean(NO3N, na.rm = T),MinNO3N = min(NO3N, na.rm = T),MaxNO3N = max(NO3N, na.rm = T),
            MO2gel_0 = mean(O2diss, na.rm = T),MinO2gel = min(O2diss, na.rm = T),MaxO2gel = max(O2diss, na.rm = T),
            MPges_0 = mean(Ptot, na.rm = T),MinPges = min(Ptot, na.rm = T),MaxPges = max(Ptot, na.rm = T),
            MpH_0 = mean(pH, na.rm = T),MinpH = min(pH, na.rm = T),MaxpH = max(pH, na.rm = T),
            MSiO2_0 = mean(SiO2, na.rm = T),MinSiO2 = min(SiO2, na.rm = T),MaxSiO2 = max(SiO2, na.rm = T),
            MTemp_0 = mean(Temp, na.rm = T),MinTemp = min(Temp, na.rm = T),MaxTemp = max(Temp, na.rm = T),
            MTransparency = mean(Transp, na.rm = T),MinTransparency = min(Transp, na.rm = T),MaxTransparency = max(Transp, na.rm = T),
            MSAK.254 = mean(SAC, na.rm = T),MinSAK.254 = min(SAC, na.rm = T),MaxSAK.254 = max(SAC, na.rm = T),
            MTemp_0_6_sd = mean(Tempsd, na.rm = T),MinTemp_0_6_sd = min(Tempsd, na.rm = T),MaxTemp_0_6_sd = max(Tempsd, na.rm = T)
  )

write.csv2(Chem_overview_SmallDataset,"Chem_overview_SmallDataset.csv")

Chem_overview_MediumDataset<-Chem_uniform_LOIx %>% ungroup() %>%
  summarise(MChl = mean(Chloride, na.rm = T),MinChlor = min(Chloride, na.rm = T),MaxChlor = max(Chloride, na.rm = T),
             MCond = mean(Conduct, na.rm = T),MinCond = min(Conduct, na.rm = T),MaxCond = max(Conduct, na.rm = T),
             MN_tot = mean(Ntot, na.rm = T),MinN_tot = min(Ntot, na.rm = T),MaxN_tot = max(Ntot, na.rm = T),
             MNH4N_0 = mean(NH4N, na.rm = T),MinNH4N = min(NH4N, na.rm = T),MaxNH4 = max(NH4N, na.rm = T),
             MNO3N_0 = mean(NO3N, na.rm = T),MinNO3N = min(NO3N, na.rm = T),MaxNO3N = max(NO3N, na.rm = T),
             MO2gel_0 = mean(O2diss, na.rm = T),MinO2gel = min(O2diss, na.rm = T),MaxO2gel = max(O2diss, na.rm = T),
             MPges_0 = mean(Ptot, na.rm = T),MinPges = min(Ptot, na.rm = T),MaxPges = max(Ptot, na.rm = T),
             MpH_0 = mean(pH, na.rm = T),MinpH = min(pH, na.rm = T),MaxpH = max(pH, na.rm = T),
             MSiO2_0 = mean(SiO2, na.rm = T),MinSiO2 = min(SiO2, na.rm = T),MaxSiO2 = max(SiO2, na.rm = T),
             MTemp_0 = mean(Temp, na.rm = T),MinTemp = min(Temp, na.rm = T),MaxTemp = max(Temp, na.rm = T),
             MTransparency = mean(Transp, na.rm = T),MinTransparency = min(Transp, na.rm = T),MaxTransparency = max(Transp, na.rm = T),
             #MSAK.254 = mean(SAC, na.rm = T),MinSAK.254 = min(SAC, na.rm = T),MaxSAK.254 = max(SAC, na.rm = T),
             MTemp_0_6_sd = mean(Tempsd, na.rm = T),MinTemp_0_6_sd = min(Tempsd, na.rm = T),MaxTemp_0_6_sd = max(Tempsd, na.rm = T)
  )

write.csv2(Chem_overview_MediumDataset,"Chem_overview_MediumDataset.csv")



#BA<-sqrt(Chem_uniform_LOIx[c(3:13,15,17)])
#BA<-decostand(Chem_uniform_LOIx[c(3:13,15,17)], method="total")
BA<-log10(Chem_uniform_LOIx[c(3:13,15,17)]+1)
BAdist<-dist(BA)
adonis2(BA~datasettot, data=Chem_uniform_LOIx)
#betadisper(BAdist, group=Chem_uniform_LOIx$datasettot)
#permutest(BAdist, model="direct") #gleichen Mittelpunkt und die  gleiche Streuung (Varianz)


######################################################################
######################################################################
######################################################################
######################################################################
############################ DRIVERS #################################
######################################################################
######################################################################
######################################################################
######################################################################

#MEAN DATASET
PEAK_Chem_norm<-bind_cols(PEAK_Chem[c(1,2)], #Lake, YEAR, dataset
                          log(PEAK_Chem[c(3:15,17,18,19,23)]+1),#Chem & Morphometry
                          PEAK_Chem[c(27:37)]) #MBD, SPNRmean, Gamma, group, dataset

# C<-cor(PEAK_LAKE_ALL_Chem_norm[c(3:20,22,23)], use="complete.obs")
# corrplot(C, type = "upper", method = "number", insig = "blank")

C<-cor(PEAK_Chem_norm[c(3:16,19)], use="complete.obs")
corrplot(C, type = "upper", method = "number", insig = "blank")

plot(PEAK_Chem_norm$GAMMA,PEAK_Chem_norm$Area_ha)

### PCA mit CHEM
### PCA
################ PCA ###############################################
library(ggbiplot)

label_lake<- PEAK_Chem_norm[complete.cases(PEAK_Chem_norm[,c(3:16,19)]),] #,81
#group_lake <- as.factor(label_lake$datasettot)
lak.pca <- prcomp(na.omit(label_lake[,c(3:16,19)]),center = TRUE, scale. = TRUE)
print(lak.pca)
dev.off()
plot(lak.pca, type="l")
summary(lak.pca)

p1<- ggbiplot(lak.pca, choices = 1:2, obs.scale = 1, var.scale = 1,labels=label_lake$Lake, arrow.color = "#FF0000",
         #groups = group_lake_withoutSAK,
         ellipse = TRUE, cicle = TRUE) #+ xlim(-4,4) +ylim(-4,4)
p2<- ggbiplot(lak.pca, choices = 3:4, obs.scale = 1, var.scale = 1,labels=label_lake$Lake, arrow.color = "#FF0000",
         #groups = group_lake_withoutSAK,
         ellipse = TRUE, cicle = TRUE) #+ xlim(-4,4) +ylim(-4,4)
figure <- ggarrange(p1,p2,
                    #widths = c(2.3,2),heights = c(1.1,1,1,1.2),
                    labels = c("A","B"),
                    #label.x = c(0.18,0,0.18,0,0.18,0,0.18,0),label.y = c(0.8,0.8,1,1,1,1,1,1),
                    ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom", align = "hv")
figure

PCA <- (data.frame(label_lake$Lake))
PCA$YEAR <- label_lake$YEAR
PCA$Morph_PC1 <- lak.pca$x[,1]
PCA$Morph_PC2 <- lak.pca$x[,2]
PCA$Morph_PC3 <- lak.pca$x[,3]
PCA$Morph_PC4 <- lak.pca$x[,4]
PCA$Morph_PC5 <- lak.pca$x[,5]
names(PCA)[1]<-"Lake"


PEAK_PCA<-merge(PCA, PEAK_Chem_norm, by=c("Lake", "YEAR"))


### Figure about PCA loadings
print(lak.pca)

par(mfrow=c(1,5))
axis1<-barplot(lak.pca$rotation[,1], main="PC1", las=2,horiz = TRUE)
axis2<-barplot(lak.pca$rotation[,2], main="PC2", yaxt='n',horiz = TRUE)
axis3<-barplot(lak.pca$rotation[,3], main="PC3", yaxt='n',horiz = TRUE)
axis4<-barplot(lak.pca$rotation[,4], main="PC4", yaxt='n',horiz = TRUE)
axis4<-barplot(lak.pca$rotation[,5], main="PC5",yaxt='n',horiz = TRUE)


# GLM
fit <- glm(AlphaPeakDepth~Morph_PC1+Morph_PC2+Morph_PC3+Morph_PC4+Morph_PC5,data=PEAK_PCA,family=gaussian(link = "identity"))
fit <- glm(AlphaPeakDepth~Morph_PC1+Morph_PC2+Morph_PC3+Morph_PC4+Morph_PC5,data=PEAK_PCA,family=gaussian(link = "identity"))
fit2 <- glm(AlphaPeakDepth~Morph_PC2+Morph_PC3+Morph_PC4,data=PEAK_PCA,family=gaussian(link = "identity"))


anova(fit,fit2)
AIC(fit,fit2)



fm2 <- step(fit)
summary(fm2)

summary(fit) # display results
plot(fit)

fitR <- glm(AlphaPeakRichness~Morph_PC1+Morph_PC2+Morph_PC3+Morph_PC4+Morph_PC5,data=PEAK_PCA,family=gaussian(link = "identity"))
fitR2 <- glm(AlphaPeakRichness~Morph_PC1+Morph_PC2+Morph_PC3+Morph_PC4+Morph_PC5,data=PEAK_PCA,family=gaussian(link = "identity"))

anova(fitR,fitR2)
AIC(fit,fit2)

summary(fitR) # display results
plot(fitR)

#anova(fit,fit2)


#########################
fit <- glm(GammaPeakDepth~Morph_PC1+Morph_PC2+Morph_PC3+Morph_PC4+Morph_PC5,data=PEAK_PCA,family=gaussian(link = "identity"))
summary(fit) # display results
plot(fit)

fit <- glm(BetaPeakDepth~Morph_PC1+Morph_PC2+Morph_PC3+Morph_PC4+Morph_PC5,data=PEAK_PCA,family=gaussian(link = "identity"))
summary(fit) # display results
plot(fit)
#anova(fit,fit2)
#################### GAM ###################################

gam_AlphaPeakDepth <- gamm4(AlphaPeakDepth ~ s(Morph_PC1)+s(Morph_PC2)+s(Morph_PC3)+s(Morph_PC4)+s(Morph_PC5),
                   random= ~(1|Lake), # package gamm4
                   data=PEAK_PCA)

summary(gam_AlphaPeakDepth$gam)

par(mfrow=c(1,5))
plot(gam_AlphaPeakDepth$gam,residuals=TRUE, main = "PEAKDepth", shade = T,seWithMean=T)


gam_AlphaPeakDepth <- gamm4(AlphaPeakDepth ~ s(Morph_PC2)+s(Morph_PC3)+s(Morph_PC4)+s(Morph_PC5),
                            random= ~(1|Lake), # package gamm4
                            data=PEAK_PCA)

summary(gam_AlphaPeakDepth$gam)

gam_AlphaPeakDepth <- gamm4(AlphaPeakDepth ~ s(Morph_PC2)+s(Morph_PC3)+s(Morph_PC4),
                            random= ~(1|Lake), # package gamm4
                            data=PEAK_PCA)

summary(gam_AlphaPeakDepth$gam)

gam_AlphaPeakDepth <- gamm4((AlphaPeakDepth) ~ s(Morph_PC2)+s(Morph_PC4),
                            random= ~(1|Lake), # package gamm4
                            data=PEAK_PCA)

summary(gam_AlphaPeakDepth$gam)

par(mfrow=c(1,3))
plot(gam_AlphaPeakDepth$gam,residuals=TRUE, main = "PEAKDepth", shade = T,seWithMean=T)

gam_AlphaPeakRichness <- gamm4(AlphaPeakRichness ~ s(Morph_PC1)+s(Morph_PC2)+s(Morph_PC3)+s(Morph_PC4)+s(Morph_PC5),
                            random= ~(1|Lake), # package gamm4
                            data=PEAK_PCA)

summary(gam_AlphaPeakRichness$gam)

par(mfrow=c(1,5))
plot(gam_AlphaPeakRichness$gam,residuals=TRUE, main = "PEAKRichness", shade = T,seWithMean=T)


gam_AlphaPeakRichness <- gamm4(AlphaPeakRichness ~ s(Morph_PC1),
                               random= ~(1|Lake), # package gamm4
                               data=PEAK_PCA)

summary(gam_AlphaPeakRichness$gam)
par(mfrow=c(1,1))
plot(gam_AlphaPeakRichness$gam,residuals=TRUE, main = "PEAKRichness", shade = T,seWithMean=T)



install.packages("itsadug")
library(itsadug)
compareML(gam_AlphaPeakRichness$gam, gam_AlphaPeakDepth$gam)



##################################
###################################
dev.off()

row.names(PEAK_LAKE_ALL_Chem_norm)<-paste(PEAK_LAKE_ALL_Chem_norm$Lake, PEAK_LAKE_ALL_Chem_norm$YEAR)



ord <- PEAK_LAKE_ALL_Chem_norm[c(20,22)]
vec.sp<-envfit(ord, PEAK_LAKE_ALL_Chem_norm[c(3:16)], permu = 999,na.rm = TRUE)#26:38,
vec.sp
vec.sp.df<-as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r))
vec.sp.df$param<-rownames(vec.sp.df)
vec.sp.df <- vec.sp.df %>% filter(param %in% (c("N_tot","NO3N","P_tot","pH","Transparency","SAK.254","Area_ha")))#"Temp_0_6_sd",

dev.off()
ordiplot(ord)#, type = "text"
#points(y=PEAK_LAKE_ALL_Chem_norm$ALPHA_PEAKmeansc,x=PEAK_LAKE_ALL_Chem_norm$PEAKsc, col=factor(PEAK_LAKE_ALL_Chem_norm$groupsPEAK),
#       bg=factor(PEAK_LAKE_ALL_Chem_norm$groupsPEAK), pch=21, type = "p", cex=1)
plot(vec.sp, p.max = 0.05)

ggplot(PEAK_LAKE_ALL_Chem_norm, aes(x=ALPHA_PEAKmeansc, y=PEAKsc))+
  geom_point(aes())+  theme(legend.title=element_blank())+#scale_fill_discrete(name = "Groups")+
  geom_segment(data=vec.sp.df,aes(x=0,xend=2*ALPHA_PEAKmeansc,y=0,yend=2*PEAKsc),
               arrow = arrow(length = unit(0.5, "cm")),colour="black",inherit_aes=FALSE) +
  geom_text(data=vec.sp.df,aes(x=2.3*ALPHA_PEAKmeansc,y=2.3*PEAKsc,label=param),size=5)+
  coord_fixed()  + xlab("Maximum species number (scaled)")  + ylab("Depth of maximum number of species (scaled)")+
  theme(legend.position = c(0.9, 0.1))

# ggplot(MBD_LAKE_Morph_Chem_norm, aes(x=SPNRmeansc, y=MBDsc))+
#   geom_point()+  theme(legend.title=element_blank())+#scale_fill_discrete(name = "Groups")+
#   geom_segment(data=vec.sp.df,aes(x=0,xend=2*SPNRmeansc,y=0,yend=2*MBDsc),
#                arrow = arrow(length = unit(0.5, "cm")),colour="black",inherit_aes=FALSE) +
#   geom_text(data=vec.sp.df,aes(x=2.3*SPNRmeansc,y=2.3*MBDsc,label=param),size=5)+
#   coord_fixed() + xlab("Maximum species number (scaled)")  + ylab("Depth of maximum number of species (scaled)")
#
#
# ggplot(MBD_LAKE_Morph_Chem_norm, aes(x=SPNRmeansc, y=MBDsc))+
#   geom_point()+  theme(legend.title=element_blank())+#scale_fill_discrete(name = "Groups")+
#     coord_fixed() + xlab("Maximum species number (scaled)")  + ylab("Depth of maximum number of species (scaled)")

ord <- PEAK_LAKE_ALL_Chem_norm[c(20,22)]
vec.sp_SAK<-envfit(ord, PEAK_LAKE_ALL_Chem_norm[c(3,4,6:13,15,16)], permu = 999,na.rm = TRUE)#26:38,
vec.sp_SAK
vec.sp.df_SAK<-as.data.frame(vec.sp_SAK$vectors$arrows*sqrt(vec.sp_SAK$vectors$r))
vec.sp.df_SAK$param<-rownames(vec.sp.df_SAK)
vec.sp.df_SAK <- vec.sp.df_SAK %>% filter(param %in% (c("P_tot","pH","Transparency","Area_ha","Temp_0_6_sd")))#"Temp_0_6_sd",
ggplot(PEAK_LAKE_ALL_Chem_norm, aes(x=ALPHA_PEAKmeansc, y=PEAKsc))+
  geom_point(aes())+  theme(legend.title=element_blank())+#scale_fill_discrete(name = "Groups")+
  geom_segment(data=vec.sp.df_SAK,aes(x=0,xend=2*ALPHA_PEAKmeansc,y=0,yend=2*PEAKsc),
               arrow = arrow(length = unit(0.5, "cm")),colour="black",inherit_aes=FALSE) +
  geom_text(data=vec.sp.df_SAK,aes(x=2.3*ALPHA_PEAKmeansc,y=2.3*PEAKsc,label=param),size=5)+
  coord_fixed()  + xlab("Maximum species number (scaled)")  + ylab("Depth of maximum number of species (scaled)")+
  theme(legend.position = c(0.9, 0.1))



ord <- PEAK_LAKE_ALL_Chem_norm[c(21,23)]
vec.sp_SAK<-envfit(ord, PEAK_LAKE_ALL_Chem_norm[c(3,4,6:15,16,19)], permu = 999,na.rm = TRUE)#26:38,
vec.sp_SAK
vec.sp.df_SAK<-as.data.frame(vec.sp_SAK$vectors$arrows*sqrt(vec.sp_SAK$vectors$r))
vec.sp.df_SAK$param<-rownames(vec.sp.df_SAK)
vec.sp.df_SAK <- vec.sp.df_SAK %>% filter(param %in% (c("Conductivity","NH4N","NO3N","O2_diss","P_tot","SiO2","Transparency","SAK.254","MHWMNW_Diff","Area_ha","Temp_0_6_sd")))#"Temp_0_6_sd",
ggplot(PEAK_LAKE_ALL_Chem_norm, aes(x=ALPHA_PEAKmeansc, y=PEAKsc))+
  geom_point(aes())+  theme(legend.title=element_blank())+#scale_fill_discrete(name = "Groups")+
  geom_segment(data=vec.sp.df_SAK,aes(x=0,xend=2*ALPHA_PEAKmeansc,y=0,yend=2*PEAKsc),
               arrow = arrow(length = unit(0.5, "cm")),colour="black",inherit_aes=FALSE) +
  geom_text(data=vec.sp.df_SAK,aes(x=2.3*ALPHA_PEAKmeansc,y=2.3*PEAKsc,label=param),size=5)+
  coord_fixed()  + xlab("Maximum species number (scaled)")  + ylab("Depth of maximum number of species (scaled)")+
  theme(legend.position = c(0.9, 0.1))














##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
#### Time change


## Wie hat es sich ge?ndert?
library(ggpmisc)

T0<-ggplot(PEAK_LAKE_ALL%>%group_by(Lake)%>%filter(n_distinct(YEAR)>3), aes(x=factor(YEAR), y=GAMMA, group=factor(Lake))) +
  xlab("")+ylab("GAMMA richness")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_point(alpha = 0.3) +facet_wrap(vars(Lake),nrow=3) +
  geom_smooth(method = "lm", se = F) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x.npc = "left", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 3)+
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
                  label.x = 'left', label.y = 0.35, size = 3)



T1<-ggplot(PEAK_LAKE_ALL%>%group_by(Lake)%>%filter(n_distinct(YEAR)>3), aes(x=factor(YEAR), y=AlphaPeakDepth, group=factor(Lake)))+
  xlab("")+ylab("AlphaPeakDepth")+

  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_point(alpha = 0.3) +facet_wrap(vars(Lake),nrow=3) +
  geom_smooth(method = "lm", se = F) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x.npc = "left", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 3)+
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
                  label.x = 'left', label.y = -3.8, size = 3)



T2<-ggplot(PEAK_LAKE_ALL%>%group_by(Lake)%>%filter(n_distinct(YEAR)>3), aes(x=factor(YEAR), y=AlphaPeakRichness, group=factor(Lake)))+
  xlab("")+ylab("AlphaPeakRichness")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_point(alpha = 0.3) +facet_wrap(vars(Lake),nrow=3) +
  geom_smooth(method = "lm", se = F) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x.npc = "left", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 3)+
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
                  label.x = 'left', label.y = -3.8, size = 3)

ggarrange(T1,T2, nrow=2)

T3<-ggplot(PEAK%>%group_by(Lake)%>%filter(n_distinct(YEAR)>3), aes(x=factor(YEAR), y=BetaPeakDepth, group=factor(Lake)))+
  xlab("")+ylab("BetaPeakDepth")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_point(alpha = 0.3) +facet_wrap(vars(Lake),nrow=3) +
  geom_smooth(method = "lm", se = F) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x.npc = "left", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 3)+
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
                  label.x = 'left', label.y = -3.8, size = 3)

T4<-ggplot(PEAK%>%group_by(Lake)%>%filter(n_distinct(YEAR)>3), aes(x=factor(YEAR), y=BetaPeakRichness, group=factor(Lake)))+
  xlab("")+ylab("BetaPeakRichness")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_point(alpha = 0.3) +facet_wrap(vars(Lake),nrow=3) +
  geom_smooth(method = "lm", se = F) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x.npc = "left", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 3)+
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
                  label.x = 'left', label.y = -3.8, size = 3)

ggarrange(T3,T4, nrow=2)

T5<-ggplot(PEAK%>%group_by(Lake)%>%filter(n_distinct(YEAR)>3), aes(x=factor(YEAR), y=GammaPeakDepth, group=factor(Lake)))+
  xlab("")+ylab("GammaPeakDepth")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_point(alpha = 0.3) +facet_wrap(vars(Lake),nrow=3) +
  geom_smooth(method = "lm", se = F) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x.npc = "left", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 3)+
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
                  label.x = 'left', label.y = -3.8, size = 3)

T6<-ggplot(PEAK%>%group_by(Lake)%>%filter(n_distinct(YEAR)>3), aes(x=factor(YEAR), y=GammaPeakRichness, group=factor(Lake)))+
  xlab("")+ylab("GammaPeakRichness")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_point(alpha = 0.3) +facet_wrap(vars(Lake),nrow=3) +
  geom_smooth(method = "lm", se = F) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x.npc = "left", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 3)+
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
                  label.x = 'left', label.y = -3.8, size = 3)

ggarrange(T5,T6, nrow=2)


####################### MODELS ##################################
formula <- y ~ x

lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

GammaModel<-PEAK_LAKE_ALL%>%group_by(Lake)%>%filter(n_distinct(YEAR)>3) %>%
  do({
    mod = lm(GAMMA ~ YEAR, data = .)
    data.frame(Slope = coef(mod)[2],pValue=lmp(mod))
  }) %>% plyr::rename(c("Slope"="GammaSlope", "pValue"="GammaPValue"))

AlphaPeakRichnessModel<-PEAK_LAKE_ALL%>%group_by(Lake)%>%filter(n_distinct(YEAR)>3) %>%
  do({
    mod = lm(AlphaPeakRichness ~ YEAR, data = .)
    data.frame(Slope = coef(mod)[2],pValue=lmp(mod))
  }) %>% plyr::rename(c("Slope"="AlphaPeakRichnessSlope", "pValue"="AlphaPeakRichnessPValue"))

AlphaPeakDepthModel<-PEAK_LAKE_ALL%>%group_by(Lake)%>%filter(n_distinct(YEAR)>3) %>%
  do({
    mod = lm(AlphaPeakDepth ~ YEAR, data = .)
    data.frame(Slope = coef(mod)[2],pValue=lmp(mod))
  }) %>% plyr::rename(c("Slope"="AlphaPeakDepthSlope", "pValue"="AlphaPeakDepthPValue"))

AlphaPeakModel <- merge (AlphaPeakDepthModel, AlphaPeakRichnessModel, by="Lake")


BetaPeakRichnessModel<-PEAK%>%group_by(Lake)%>%filter(n_distinct(YEAR)>3) %>%
  do({
    mod = lm(BetaPeakRichness ~ YEAR, data = .)
    data.frame(Slope = coef(mod)[2],pValue=lmp(mod))
  }) %>% plyr::rename(c("Slope"="BetaPeakRichnessSlope", "pValue"="BetaPeakRichnessPValue"))

BetaPeakDepthModel<-PEAK%>%group_by(Lake)%>%filter(n_distinct(YEAR)>3) %>%
  do({
    mod = lm(BetaPeakDepth ~ YEAR, data = .)
    data.frame(Slope = coef(mod)[2],pValue=lmp(mod))
  }) %>% plyr::rename(c("Slope"="BetaPeakDepthSlope", "pValue"="BetaPeakDepthPValue"))

BetaPeakModel <- merge (BetaPeakDepthModel, BetaPeakRichnessModel, by="Lake")

GammaPeakRichnessModel<-PEAK%>%group_by(Lake)%>%filter(n_distinct(YEAR)>3) %>%
  do({
    mod = lm(GammaPeakRichness ~ YEAR, data = .)
    data.frame(Slope = coef(mod)[2],pValue=lmp(mod))
  }) %>% plyr::rename(c("Slope"="GammaPeakRichnessSlope", "pValue"="GammaPeakRichnessPValue"))

GammaPeakDepthModel<-PEAK%>%group_by(Lake)%>%filter(n_distinct(YEAR)>3) %>%
  do({
    mod = lm(GammaPeakDepth ~ YEAR, data = .)
    data.frame(Slope = coef(mod)[2],pValue=lmp(mod))
  }) %>% plyr::rename(c("Slope"="GammaPeakDepthSlope", "pValue"="GammaPeakDepthPValue"))

GammaPeakModel <- merge (GammaPeakDepthModel, GammaPeakRichnessModel, by="Lake")

PeakModel <- merge (AlphaPeakModel,BetaPeakModel,by="Lake")
PeakModel <- merge (PeakModel,GammaPeakModel,by="Lake")

Model <- merge (GammaModel,PeakModel,by="Lake")

sd(Model$GammaSlope)

ModelT<-Model %>% mutate(GammaTrend=ifelse(Model$GammaSlope>0.5*sd(Model$GammaSlope),"+",ifelse(Model$GammaSlope<(-0.5*sd(Model$GammaSlope)),"-","0"))) %>%
  mutate(GammapV=ifelse(GammaPValue<0.001,"***",ifelse(GammaPValue<0.01,"**",ifelse(GammaPValue<0.05,"*",ifelse(GammaPValue<0.1,".","NA")))))%>%

  mutate(AlphaPeakDepthTrend=ifelse(Model$AlphaPeakDepthSlope>0.5*sd(Model$AlphaPeakDepthSlope),"+",ifelse(Model$AlphaPeakDepthSlope<(-0.5*sd(Model$AlphaPeakDepthSlope)),"-","0"))) %>%
  mutate(AlphaPeakDepthpV=ifelse(AlphaPeakDepthPValue<0.001,"***",ifelse(AlphaPeakDepthPValue<0.01,"**",ifelse(AlphaPeakDepthPValue<0.05,"*",ifelse(AlphaPeakDepthPValue<0.1,".","NA")))))%>%

  mutate(AlphaPeakRichnessTrend=ifelse(Model$AlphaPeakRichnessSlope>0.5*sd(Model$AlphaPeakRichnessSlope),"+",ifelse(Model$AlphaPeakRichnessSlope<(-0.5*sd(Model$AlphaPeakRichnessSlope)),"-","0")))%>%
  mutate(AlphaPeakRichnesspV=ifelse(AlphaPeakRichnessPValue<0.001,"***",ifelse(AlphaPeakRichnessPValue<0.01,"**",ifelse(AlphaPeakRichnessPValue<0.05,"*",ifelse(AlphaPeakRichnessPValue<0.1,".","NA")))))%>%

  mutate(BetaPeakDepthTrend=ifelse(Model$BetaPeakDepthSlope>0.5*sd(Model$BetaPeakDepthSlope),"+",ifelse(Model$BetaPeakDepthSlope<(-0.5*sd(Model$BetaPeakDepthSlope)),"-","0"))) %>%
  mutate(BetaPeakDepthpV=ifelse(BetaPeakDepthPValue<0.001,"***",ifelse(BetaPeakDepthPValue<0.01,"**",ifelse(BetaPeakDepthPValue<0.05,"*",ifelse(BetaPeakDepthPValue<0.1,".","NA")))))%>%

  mutate(BetaPeakRichnessTrend=ifelse(Model$BetaPeakRichnessSlope>0.5*sd(Model$BetaPeakRichnessSlope),"+",ifelse(Model$BetaPeakRichnessSlope<(-0.5*sd(Model$BetaPeakRichnessSlope)),"-","0")))%>%
  mutate(BetaPeakRichnesspV=ifelse(BetaPeakRichnessPValue<0.001,"***",ifelse(BetaPeakRichnessPValue<0.01,"**",ifelse(BetaPeakRichnessPValue<0.05,"*",ifelse(BetaPeakRichnessPValue<0.1,".","NA")))))%>%

  mutate(GammaPeakDepthTrend=ifelse(Model$GammaPeakDepthSlope>0.5*sd(Model$GammaPeakDepthSlope),"+",ifelse(Model$GammaPeakDepthSlope<(-0.5*sd(Model$GammaPeakDepthSlope)),"-","0"))) %>%
  mutate(GammaPeakDepthpV=ifelse(GammaPeakDepthPValue<0.001,"***",ifelse(GammaPeakDepthPValue<0.01,"**",ifelse(GammaPeakDepthPValue<0.05,"*",ifelse(GammaPeakDepthPValue<0.1,".","NA")))))%>%


  mutate(GammaPeakRichnessTrend=ifelse(Model$GammaPeakRichnessSlope>0.5*sd(Model$GammaPeakRichnessSlope),"+",ifelse(Model$GammaPeakRichnessSlope<(-0.5*sd(Model$GammaPeakRichnessSlope)),"-","0")))%>%
  mutate(GammaPeakRichnesspV=ifelse(GammaPeakRichnessPValue<0.001,"***",ifelse(GammaPeakRichnessPValue<0.01,"**",ifelse(GammaPeakRichnessPValue<0.05,"*",ifelse(GammaPeakRichnessPValue<0.1,".","NA")))))

ModelT<-Model %>% mutate(GammaTrend=ifelse(Model$GammaSlope>0,"+",ifelse(Model$GammaSlope<0,"-","0"))) %>%
  mutate(GammapV=ifelse(GammaPValue<0.001,"***",ifelse(GammaPValue<0.01,"**",ifelse(GammaPValue<0.05,"*",ifelse(GammaPValue<0.1,".","NA")))))%>%

  mutate(AlphaPeakDepthTrend=ifelse(Model$AlphaPeakDepthSlope>0.5*0,"+",ifelse(Model$AlphaPeakDepthSlope<0,"-","0"))) %>%
  mutate(AlphaPeakDepthpV=ifelse(AlphaPeakDepthPValue<0.001,"***",ifelse(AlphaPeakDepthPValue<0.01,"**",ifelse(AlphaPeakDepthPValue<0.05,"*",ifelse(AlphaPeakDepthPValue<0.1,".","NA")))))%>%

  mutate(AlphaPeakRichnessTrend=ifelse(Model$AlphaPeakRichnessSlope>0,"+",ifelse(Model$AlphaPeakRichnessSlope<0,"-","0")))%>%
  mutate(AlphaPeakRichnesspV=ifelse(AlphaPeakRichnessPValue<0.001,"***",ifelse(AlphaPeakRichnessPValue<0.01,"**",ifelse(AlphaPeakRichnessPValue<0.05,"*",ifelse(AlphaPeakRichnessPValue<0.1,".","NA")))))%>%

  mutate(BetaPeakDepthTrend=ifelse(Model$BetaPeakDepthSlope>0,"+",ifelse(Model$BetaPeakDepthSlope<0,"-","0"))) %>%
  mutate(BetaPeakDepthpV=ifelse(BetaPeakDepthPValue<0.001,"***",ifelse(BetaPeakDepthPValue<0.01,"**",ifelse(BetaPeakDepthPValue<0.05,"*",ifelse(BetaPeakDepthPValue<0.1,".","NA")))))%>%

  mutate(BetaPeakRichnessTrend=ifelse(Model$BetaPeakRichnessSlope>0,"+",ifelse(Model$BetaPeakRichnessSlope<0,"-","0")))%>%
  mutate(BetaPeakRichnesspV=ifelse(BetaPeakRichnessPValue<0.001,"***",ifelse(BetaPeakRichnessPValue<0.01,"**",ifelse(BetaPeakRichnessPValue<0.05,"*",ifelse(BetaPeakRichnessPValue<0.1,".","NA")))))%>%

  mutate(GammaPeakDepthTrend=ifelse(Model$GammaPeakDepthSlope>0,"+",ifelse(Model$GammaPeakDepthSlope<0,"-","0"))) %>%
  mutate(GammaPeakDepthpV=ifelse(GammaPeakDepthPValue<0.001,"***",ifelse(GammaPeakDepthPValue<0.01,"**",ifelse(GammaPeakDepthPValue<0.05,"*",ifelse(GammaPeakDepthPValue<0.1,".","NA")))))%>%


  mutate(GammaPeakRichnessTrend=ifelse(Model$GammaPeakRichnessSlope>0,"+",ifelse(Model$GammaPeakRichnessSlope<0,"-","0")))%>%
  mutate(GammaPeakRichnesspV=ifelse(GammaPeakRichnessPValue<0.001,"***",ifelse(GammaPeakRichnessPValue<0.01,"**",ifelse(GammaPeakRichnessPValue<0.05,"*",ifelse(GammaPeakRichnessPValue<0.1,".","NA")))))


ModelTshort<-ModelT[c(1,16:29)] %>%
  unite(GammaLM, c("GammaTrend", "GammapV"))%>%
  unite(AlphaPeakDepthLM, c("AlphaPeakDepthTrend", "AlphaPeakDepthpV"))%>%
  unite(AlphaPeakRichnessLM, c("AlphaPeakRichnessTrend", "AlphaPeakRichnesspV"))%>%
  unite(BetaPeakDepthLM, c("BetaPeakDepthTrend", "BetaPeakDepthpV"))%>%
  unite(BetaPeakRichnessLM, c("BetaPeakRichnessTrend", "BetaPeakRichnesspV"))%>%
  unite(GammaPeakDepthLM, c("GammaPeakDepthTrend", "GammaPeakDepthpV"))%>%
  unite(GammaPeakRichnessLM, c("GammaPeakRichnessTrend", "GammaPeakRichnesspV"))

write.csv2(ModelTshort, "Trend.csv")

ModelT


######################################################################
library(tseries)
library(aTSA)
T<-as.vector(PEAK_LAKE_ALL%>%filter(Lake=="Starnberger See")%>%select(AlphaPeakDepth))
T<-as.vector(PEAK_LAKE_ALL%>%filter(Lake=="Chiemsee")%>%select(AlphaPeakDepth))
TY<-as.vector(PEAK_LAKE_ALL%>%filter(Lake=="Chiemsee")%>%select(YEAR,AlphaPeakDepth))
See<-T[,1]
kpss.test(See, null = "Trend")
adf.test(See,alternative = c("stationary"))
stationary.test(See)

ggplot(PEAK_LAKE_ALL%>%group_by(Lake)%>%filter(n_distinct(YEAR)>2), aes(y=ALPHA_PEAKmean, x=-PEAK))+xlim(0.5,4.5)+ylim(2,11)+#, col=factor(groupsPEAK)
  geom_point()+# labs(col = "Pattern type")+
  geom_path(aes(group=factor(Lake)))+
  geom_text(aes(label=paste(YEAR)),hjust=0, vjust=0)+
  xlab("mean Alpha richness peak (m)")+ylab("Maximal species richness (N)")+
  theme(legend.position = c(0.9, 0.2))



# MBD_LAKE_MULTIPLEYEARS <- PEAK_LAKE_ALL %>% filter(Lake %in% c(rle(PEAK_LAKE_ALL$Lake)$values[rle(PEAK_LAKE_ALL$Lake)$length>=3] ))
#
#
# ggplot(MBD_LAKE_MULTIPLEYEARS, aes(x=SPNRmean, y=MBD,group=factor(Lake)))+
#   geom_point(aes(col=factor(GROUPSMBD)), size=3)+
#   geom_path(arrow = arrow(angle = 15, ends = "last", type = "closed"))+xlab("Maximum species number")+ylab("Depth of maximum number of species (m)")+ labs(col = "Pattern type")+
#   #geom_text(aes(label=paste(Lake,YEAR)),hjust=0, vjust=0)+
#   theme(legend.position = c(0.9, 0.2))
detach(package:ggbiplot)
detach(package:plyr)
LAKECHANGE<-PEAK%>% dplyr::group_by(Lake) %>%
  summarise(NYEAR=n_distinct(YEAR),
            #AlphaPeakDepthMax=max(AlphaPeakDepth),AlphaPeakDepthMin=min(AlphaPeakDepth),
            AlphaPeakDepthmean=mean(AlphaPeakDepth),AlphaPeakDepthRange=max(AlphaPeakDepth)-min(AlphaPeakDepth), #AlphaPeakDepthsd=sd(AlphaPeakDepth),
            AlphaPeakRichnessnmean=mean(AlphaPeakRichness),AlphaPeakRichnessRange=max(AlphaPeakRichness)-min(AlphaPeakRichness),#AlphaPeakRichnessnsd=sd(AlphaPeakRichness)
            BetaPeakDepthmean=mean(BetaPeakDepth),BetaPeakDepthRange=max(BetaPeakDepth)-min(BetaPeakDepth), #BetaPeakDepthsd=sd(BetaPeakDepth),
            BetaPeakRichnessnmean=mean(BetaPeakRichness),BetaPeakRichnessRange=max(BetaPeakRichness)-min(BetaPeakRichness),#BetaPeakRichnessnsd=sd(BetaPeakRichness)
            GammaPeakDepthmean=mean(GammaPeakDepth),GammaPeakDepthRange=max(GammaPeakDepth)-min(GammaPeakDepth), #GammaPeakDepthsd=sd(GammaPeakDepth),
            GammaPeakRichnessnmean=mean(GammaPeakRichness),GammaPeakRichnessRange=max(GammaPeakRichness)-min(GammaPeakRichness)#GammaPeakRichnessnsd=sd(GammaPeakRichness)
            )%>%
  filter(NYEAR>3)

LAKECHANGE2<-LAKECHANGE %>%
  mutate(APeakDepthRangeClass=ifelse(AlphaPeakDepthRange<1,"0-1m",ifelse(AlphaPeakDepthRange<2,"1-2m",ifelse(AlphaPeakDepthRange<3,"2-3m",
                              ifelse(AlphaPeakDepthRange<4,"3-4m",ifelse(AlphaPeakDepthRange<5,"4-5m",99))))),
         BPeakDepthRangeClass=ifelse(BetaPeakDepthRange<1,"0-1m",ifelse(BetaPeakDepthRange<2,"1-2m",ifelse(BetaPeakDepthRange<3,"2-3m",
                              ifelse(BetaPeakDepthRange<4,"3-4m",ifelse(BetaPeakDepthRange<5,"4-5m",99))))),
         GPeakDepthRangeClass=ifelse(GammaPeakDepthRange<1,"0-1m",ifelse(GammaPeakDepthRange<2,"1-2m",ifelse(GammaPeakDepthRange<3,"2-3m",
                              ifelse(GammaPeakDepthRange<4,"3-4m",ifelse(GammaPeakDepthRange<5,"4-5m",99))))))

AA<-LAKECHANGE2%>%group_by(APeakDepthRangeClass)%>%summarise(AlphaPeakDepthRange=n_distinct(Lake))%>%rename(PeakDepthRangeClass=APeakDepthRangeClass)
BA<-LAKECHANGE2%>%group_by(BPeakDepthRangeClass)%>%summarise(BetaPeakDepthRange=n_distinct(Lake))%>%rename(PeakDepthRangeClass=BPeakDepthRangeClass)
GA<-LAKECHANGE2%>%group_by(GPeakDepthRangeClass)%>%summarise(GammaPeakDepthRange=n_distinct(Lake))%>%rename(PeakDepthRangeClass=GPeakDepthRangeClass)

AB<-full_join(AA,BA,by=c("PeakDepthRangeClass"))
ABG<-full_join(AB,GA,by=c("PeakDepthRangeClass"))
ABGs<-ABG %>% tidyr::gather("Type", "Nlakes", -PeakDepthRangeClass)

positions <- c("GammaPeakDepthRange","BetaPeakDepthRange", "AlphaPeakDepthRange" )
ggplot(ABGs,aes(x=(Type),y=Nlakes,fill=PeakDepthRangeClass))+
  #geom_bar(stat="identity") +
  geom_col(position = position_stack(reverse = TRUE))+
  xlab("")+ylab("Lakes (N)")+
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()+
  scale_x_discrete(limits = positions)



## ALL CHEM VALUES TIMECHANGE
Chem_0_6m_mean_STAFF <- Chem_uniform_LOIx #%>% filter(Lake %in% c("Staffelsee - Sued","Riegsee")) #,"Riegsee""Starnberger See", "Riegsee",
STAFF1<-ggplot(Chem_0_6m_mean_STAFF,aes(x=factor(YEAR), y=Chloride, group=(Lake), col=Lake))+
  geom_line()+geom_point()+xlab("")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
STAFF2<-ggplot(Chem_0_6m_mean_STAFF,aes(x=factor(YEAR), y=Conduct, group=(Lake), col=Lake))+
  geom_line()+geom_point()+xlab("")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
STAFF3<-ggplot(Chem_0_6m_mean_STAFF,aes(x=factor(YEAR), y=Ntot, group=(Lake), col=Lake))+
  geom_line()+geom_point()+xlab("")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
STAFF4<-ggplot(Chem_0_6m_mean_STAFF,aes(x=factor(YEAR), y=NH4N, group=(Lake), col=Lake))+
  geom_line()+geom_point()+xlab("")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
STAFF5<-ggplot(Chem_0_6m_mean_STAFF,aes(x=factor(YEAR), y=NO3N, group=(Lake), col=Lake))+
  geom_line()+geom_point()+xlab("")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
STAFF6<-ggplot(Chem_0_6m_mean_STAFF,aes(x=factor(YEAR), y=O2diss, group=(Lake), col=Lake))+
  geom_line()+geom_point()+xlab("")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
STAFF7<-ggplot(Chem_0_6m_mean_STAFF,aes(x=factor(YEAR), y=pH, group=(Lake), col=Lake))+
  geom_line()+geom_point()+xlab("")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
STAFF7A<-ggplot(Chem_0_6m_mean_STAFF,aes(x=factor(YEAR), y=Ptot, group=(Lake), col=Lake))+
  geom_line()+geom_point()+xlab("")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

STAFF8<-ggplot(Chem_0_6m_mean_STAFF,aes(x=factor(YEAR), y=SiO2, group=(Lake), col=Lake))+
  geom_line()+geom_point()+xlab("")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
STAFF9<-ggplot(Chem_0_6m_mean_STAFF,aes(x=factor(YEAR), y=Temp, group=(Lake), col=Lake))+
  geom_line()+geom_point()+xlab("")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
STAFF10<-ggplot(Chem_0_6m_mean_STAFF,aes(x=factor(YEAR), y=Transp, group=(Lake), col=Lake))+
  geom_line()+geom_point()+xlab("YEAR")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
STAFF11<-ggplot(Chem_0_6m_mean_STAFF,aes(x=factor(YEAR), y=SAC, group=(Lake), col=Lake))+
  geom_line()+geom_point()+xlab("YEAR")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
STAFF12<-ggplot(Chem_0_6m_mean_STAFF,aes(x=factor(YEAR), y=Tempsd, group=(Lake), col=Lake))+
  geom_line()+geom_point()+xlab("YEAR")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggarrange(STAFF1,STAFF2,STAFF3,STAFF4,STAFF5,STAFF6,STAFF7,STAFF7A,STAFF8,STAFF9,STAFF10,STAFF11,STAFF12, nrow = 5, ncol = 3, common.legend = TRUE, legend = "bottom", align = "hv")





### CHANGE ANALYSIS
PEAK_Change <- PEAK_LAKE_ALL %>% group_by(Lake) %>%
  mutate(DiffPEAK = PEAK - lag(PEAK), DiffY = YEAR - lag(YEAR), DiffALPHA_PEAKmean = ALPHA_PEAKmean - lag(ALPHA_PEAKmean)) %>%
  filter(DiffPEAK != "NA") %>%
  filter(DiffY >0) %>% mutate(AnnDiffPEAK=DiffPEAK/DiffY, AnnDiffSP=DiffALPHA_PEAKmean/DiffY)


ggplot(PEAK_Change, aes( y=DiffPEAK)) +geom_boxplot()
ggplot(PEAK_Change, aes( y=DiffALPHA_PEAKmean)) +geom_boxplot()

PEAK_Change

# CHANGE PLOT
C1<-ggplot(PEAK_Change, aes(x=factor(groupsPEAK), y=AnnDiffPEAK))+geom_boxplot()+ylab("Depth of maximum number of species - annual change")+xlab("Group")#+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
C2<-ggplot(PEAK_Change, aes(x=factor(groupsPEAK),y=AnnDiffSP))+geom_boxplot()+ylab("Maximum species number - annual change")+xlab("Group")#+  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
ggarrange(C1,C2, nrow = 1, ncol = 2, align = "h")



#Herberich Test
library(multcomp)
library(multcompView)
library(sandwich)
##FOR ANNUAL SPN @ LAKE Change
PEAK_Change$groupsPEAK <- as.factor(PEAK_Change$groupsPEAK)
aov2 = aov((DiffPEAK) ~ (groupsPEAK), data=PEAK_Change) #Fit an Analysis of Variance Model
Heteroaov1 <- glht(aov2,mcp(groupsPEAK="Tukey") , vcov=vcovHC)
summary(Heteroaov1)  #Studies sites do not significantly differ in ....
plot(Heteroaov1)

aov2 = aov((DiffSP) ~ (GROUPSMBD), data=MBD_Change) #Fit an Analysis of Variance Model
Heteroaov2 <- glht(aov2,mcp(GROUPSMBD="Tukey") , vcov=vcovHC)
summary(Heteroaov2)  #Studies sites do not significantly differ in ....
plot(Heteroaov2)



PEAK_Change %>% filter(AnnDiffPEAK>=0 && AnnDiffSP >=0)
PEAK_Change %>% filter(AnnDiffPEAK>=0 & AnnDiffSP <0)
PEAK_Change %>% filter(AnnDiffPEAK<0 & AnnDiffSP >=0)
PEAK_Change %>% filter(AnnDiffPEAK<0 & AnnDiffSP <0)

PEAK_ChangeTrend<-PEAK_Change %>% mutate(Trend= ifelse(AnnDiffPEAK>=0 & AnnDiffSP >=0,"++",
                                                       ifelse(AnnDiffPEAK>=0 & AnnDiffSP <0, "+-",
                                                              ifelse(AnnDiffPEAK<0 & AnnDiffSP >=0,"-+",
                                                                     ifelse(AnnDiffPEAK<0 & AnnDiffSP <0,"--","NA")))) )
PEAK_ChangeTrend$Trend

ggplot(PEAK_ChangeTrend, aes(x=Trend,y=GAMMA))+geom_boxplot()

ggplot()

PEAK_ChangeTrendmorph <- merge(PEAK_ChangeTrend, Morph, by.x = c("Lake"), by.y=c("Name_Makro_short"))

TREND<-PEAK_ChangeTrendmorph %>% group_by(Trend) %>% summarise_each(funs(mean))

ggplot(PEAK_ChangeTrendmorph, aes(x=Trend, y=log(Area_ha)))+geom_boxplot()

PEAK_ChangeTrendmorph %>% dplyr::select(Lake, YEAR, Trend) %>% spread("YEAR","Trend")

PEAK_ChangeTrendmorph %>% group_by(Trend) %>% summarise(n_distinct(Lake))

## CHEM CHANGE
Chem_change <- Chem_table %>% group_by(Lake, Var3) %>% arrange(Lake,Var3,YEAR)%>%
  filter(Var3!="Chloroph..?g.l...0.0.m.Tiefe.")%>%
  mutate(DiffVar = value - lag(value), DiffYC = YEAR - lag(YEAR)) %>%
  filter(DiffYC != "NA") %>% filter(DiffYC > 0) %>% dplyr::select(-X)



## Correlieren die ?nderungen auch ??
Chem_change_short <- Chem_change %>% mutate(AnnChange = DiffVar/DiffYC) %>% dplyr::select("Lake", "YEAR","Var3","AnnChange") %>%
  spread(Var3, AnnChange) %>% dplyr::select("Lake", "YEAR", contains("0.0.m.Tiefe"))


CHANGE <- inner_join(PEAK_ChangeTrend[c(1,2,12,17,18,19)], Chem_change_short, by=c("Lake", "YEAR"))


ggplot(CHANGE, aes(y=P.ges...mg.l...0.0.m.Tiefe., x=factor(Trend)))+geom_boxplot()
ggplot(CHANGE, aes(y=Wassertemp..vor.Ort....U.00B0.C...0.0.m.Tiefe., x=factor(Trend)))+geom_boxplot()
ggplot(CHANGE, aes(y=Sichttiefe..cm...0.0.m.Tiefe., x=factor(Trend)))+geom_boxplot()
ggplot(CHANGE, aes(y=Chlorid..mg.l...0.0.m.Tiefe., x=factor(Trend)))+geom_boxplot()
ggplot(CHANGE, aes(y=LF..20..U.00B0.C..vor.Ort...U.00B5.S.cm...0.0.m.Tiefe., x=factor(Trend)))+geom_boxplot()

ggplot(CHANGE, aes(x=AnnDiffPEAK,y=P.ges...mg.l...0.0.m.Tiefe.))+geom_point()
ggplot(CHANGE, aes(x=AnnDiffSP,y=P.ges...mg.l...0.0.m.Tiefe.))+geom_point()
ggplot(CHANGE, aes(x=AnnDiffPEAK,y=Sichttiefe..cm...0.0.m.Tiefe.))+geom_point()
ggplot(CHANGE, aes(x=AnnDiffSP,y=Sichttiefe..cm...0.0.m.Tiefe.))+geom_point()



C2 <- cor(CHANGE[c(4,5,7:18)], use="pairwise.complete.obs")
corrplot(C2, type = "upper", method = "number")




CHANGE_morph <- merge(CHANGE, Morph, by.x = c("Lake"), by.y = c("Name_Makro_short"))
c3 <- cor(CHANGE_morph[c(4,5,7:18,20,21,22)],use = "complete.obs")
corrplot(c3, type = "upper", method = "number")

