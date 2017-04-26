# ENEC 395 Undergraduate Research 
# Alicia Wood
# 3/20/17
# Correlates of Extinction Risk in European and North American Amphibians



### Setting my working directory to the folder where all of my data and code are stored

setwd('~/Desktop/Extinction-Risk')

library(dplyr)
library(tidyr)



### Reading in raw data files

NAfrogs = read.table("NorthAmericanFrogs.txt", header=T, sep = '\t', quote = "\"")
NApaired = read.table("NorthAmericanFrogsPaired.txt", header=T, sep='\t', quote = "\"")
EuroAmphib = read.table("EuropeanAmphibians.txt", header=T, sep = '\t', quote = "\"", 
                        na.strings = "DD")
Taxonomy = read.table("Taxonomy.txt", header=T, sep = '\t', quote = "\"")



### Simplifying and preparing data for analysis

#Creating Habitat Breadth columns in both datasets
EuroAmphib$Forest <- apply(EuroAmphib[,c(123:131)], 1, sum)
EuroAmphib$Forest[EuroAmphib$Forest > 0] <- 1
EuroAmphib$Savanna <- apply(EuroAmphib[,c(132:133)], 1, sum)
EuroAmphib$Savanna[EuroAmphib$Savanna > 0] <- 1
EuroAmphib$Shrubland <- apply(EuroAmphib[,c(134:141)], 1, sum)
EuroAmphib$Shrubland[EuroAmphib$Shrubland > 0] <- 1
EuroAmphib$Grassland <- apply(EuroAmphib[,c(142:148)], 1, sum)
EuroAmphib$Grassland[EuroAmphib$Grassland > 0] <- 1
EuroAmphib$Wetlands <- apply(EuroAmphib[,c(149:164)], 1, sum)
EuroAmphib$Wetlands[EuroAmphib$Wetlands > 0] <- 1
EuroAmphib$RockyAreas[EuroAmphib$Habitat_RockyAreas_inland_cliffs_moutain_peaks == 1] <- 1
EuroAmphib$RockyAreas[EuroAmphib$Habitat_RockyAreas_inland_cliffs_moutain_peaks == 0] <- 0
EuroAmphib$Caves <- apply(EuroAmphib[,c(166:167)], 1, sum)
EuroAmphib$Caves[EuroAmphib$Caves > 0] <- 1
EuroAmphib$Deserts <- apply(EuroAmphib[,c(168:170)], 1, sum)
EuroAmphib$Deserts[EuroAmphib$Deserts > 0] <- 1
EuroAmphib$ArtificialTerrestrial <- apply(EuroAmphib[,c(171:176)], 1, sum)
EuroAmphib$ArtificialTerrestrial[EuroAmphib$ArtificialTerrestrial > 0] <- 1
EuroAmphib$Other[EuroAmphib$Habitat_Other_dunes == 1] <- 1
EuroAmphib$Other[EuroAmphib$Habitat_Other_dunes == 0] <- 0
EuroAmphib$HabitatBreadth <- apply(EuroAmphib[,c(255:264)], 1, sum)
NAfrogs$HabitatBreadth <- apply(NAfrogs[,c(22:33)], 1, sum)

#Condensing mean max and min Clutch Size into 1 mean Clutch Size column for NA dataset
NAfrogs$ClutchSize <- ifelse(is.na(NAfrogs$MeanClutchSize), ifelse(is.na(NAfrogs$MaxClutchSize), 
                      ifelse(is.na(NAfrogs$MinClutchSize), NA, NAfrogs$MinClutchSize),
                      ifelse(is.na(NAfrogs$MinClutchSize), NAfrogs$MaxClutchSize, 
                      rowMeans(NAfrogs[,c("MinClutchSize", "MaxClutchSize")]))), 
                      NAfrogs$MeanClutchSize)

#Simplifying NAfrogs SVL data into 1 Mean SVL column
NAfrogs$SVLunspecified <- ifelse(is.na(NAfrogs$MeanSVL_Unspecified_mm), 
                                 ifelse(is.na(NAfrogs$MaxSVL_Unspecified_mm),
                                        ifelse(is.na(NAfrogs$MinSVL_Unspecified_mm), NA, 
                                               NAfrogs$MinSVL_Unspecified_mm), 
                                        ifelse(is.na(NAfrogs$MinSVL_Unspecified_mm), 
                                               NAfrogs$MaxSVL_Unspecified_mm, 
                                               rowMeans(NAfrogs[,c("MinSVL_Unspecified_mm", 
                                                                   "MaxSVL_Unspecified_mm")]))), 
                                 NAfrogs$MeanSVL_Unspecified_mm)
NAfrogs$SVLfemale <- ifelse(is.na(NAfrogs$MeanSVL_Female_mm), 
                            ifelse(is.na(NAfrogs$MaxSVL_Female_mm), 
                                   ifelse(is.na(NAfrogs$MinSVL_Female_mm), NA, 
                                          NAfrogs$MinSVL_Female_mm), 
                                   ifelse(is.na(NAfrogs$MinSVL_Female_mm), 
                                          NAfrogs$MaxSVL_Female_mm, 
                                          rowMeans(NAfrogs[,c("MinSVL_Female_mm", 
                                                              "MaxSVL_Female_mm")]))), 
                            NAfrogs$MeanSVL_Female_mm)
NAfrogs$SVLmale <- ifelse(is.na(NAfrogs$MeanSVL_Male_mm), 
                          ifelse(is.na(NAfrogs$MaxSVL_Male_mm), 
                                 ifelse(is.na(NAfrogs$MinSVL_Male_mm), NA, 
                                        NAfrogs$MinSVL_Male_mm), 
                                 ifelse(is.na(NAfrogs$MinSVL_Male_mm), NAfrogs$MaxSVL_Male_mm, 
                                        rowMeans(NAfrogs[,c("MinSVL_Male_mm", 
                                                            "MaxSVL_Male_mm")]))), 
                          NAfrogs$MeanSVL_Male_mm)
NAfrogs$SVLMFmean <- rowMeans(NAfrogs[,c("SVLfemale", "SVLmale")])
NAfrogs$SVL <- ifelse(!is.na(NAfrogs$SVLunspecified) & !is.na(NAfrogs$SVLfemale) & 
                        !is.na(NAfrogs$SVLmale), rowMeans(NAfrogs[,c("SVLunspecified", 
                                                                     "SVLMFmean")]), 
                      ifelse(is.na(NAfrogs$SVLunspecified), 
                             ifelse(is.na(NAfrogs$SVLfemale), 
                                    ifelse(is.na(NAfrogs$SVLmale), NA, NAfrogs$SVLmale), 
                                    ifelse(is.na(NAfrogs$SVLmale), NAfrogs$SVLfemale, 
                                           NAfrogs$SVLMFmean)), NAfrogs$SVLunspecified))



### Organizing Threat Data

# Creating Continuous Threat Status Values in European Dataset
EuroAmphib$threat[EuroAmphib$IUCN_Status_LeastConcern == 1] <- 1
EuroAmphib$threat[EuroAmphib$IUCN_Status_NearThreatened == 1] <- 2
EuroAmphib$threat[EuroAmphib$IUCN_Status_Vulnerable == 1] <- 3
EuroAmphib$threat[EuroAmphib$IUCN_Status_Endangered == 1] <- 4
EuroAmphib$threat[EuroAmphib$IUCN_Status_CriticallyEndangered == 1] <- 5
# Creating Threat status ordinal values
threat = data.frame(IUCN_Threat_Status = c('LC', 'NT', 'VU', 'EN', 'CR', 'EW'),
                    threat = 1:6)



### Joining in taxonomy and threat datasets

NAfrogs <- merge(NAfrogs, Taxonomy, by = "SpeciesName") %>% left_join(threat)
EuroAmphib <- merge(EuroAmphib, Taxonomy, by = "SpeciesName")



### Dividing European dataset into Anura and Urodela order datasets

Efrogs <- EuroAmphib[EuroAmphib$Order == 'Anura',]
Esalamanders <- EuroAmphib[EuroAmphib$Order == 'Urodela',]



##### Plots and Linear Models

## North American and European Anura Comparison
pdf('plots/NA_Eur_anura.pdf', height = 8, width = 10)
par(mfrow=c(2,2), oma = c(0,2,2,0), cex.lab = 1.2, cex.axis = 1.2)
# SVL
plot(NAfrogs$SVL, NAfrogs$threat, xlab = "Mean SVL(mm)", ylab = "", 
     main = "Body Size", col = "darkgreen", pch = 17, cex = 1.3)
lm_svl_na = lm(NAfrogs$threat ~ NAfrogs$SVL)
abline(lm_svl_na, col = "darkgreen")
summary(lm_svl_na)
points(Efrogs$SVL_Unspecified_mm, Efrogs$threat, 
       col = "mediumpurple1", pch = 19, cex = 1.3)
lm_svl_e = lm(Efrogs$threat ~ Efrogs$SVL_Unspecified_mm)
abline(lm_svl_e, col = "mediumpurple1")
summary(lm_svl_e)
legend("topright", c("North American", "European"), pch = c(17,19), 
       col = c("darkgreen", "mediumpurple1"), cex = 1.3, bty = "n")
mtext("(A)", 3, adj = 0, line = 1)
# Clutch Size
plot(log(NAfrogs$ClutchSize), NAfrogs$threat, xlab = "ln(Mean Clutch Size)", ylab = "", 
     main = "Fecundity", col = "darkgreen", pch = 17, cex = 1.3)
lm_cs_na = lm(NAfrogs$threat ~ log(NAfrogs$ClutchSize))
abline(lm_cs_na, col = "darkgreen")
summary(lm_cs_na)
points(log(Efrogs$Number_of_eggs_or_offspring), Efrogs$threat, 
       col = "mediumpurple1", pch = 19, cex = 1.3)
lm_cs_e = lm(Efrogs$threat ~ log(Efrogs$Number_of_eggs_or_offspring))
abline(lm_cs_e, col = "mediumpurple1")
summary(lm_cs_e)
mtext("(B)", 3, adj = 0, line = 1)
# Habitat Breadth
plot(NAfrogs$HabitatBreadth, NAfrogs$threat, xlab = "Habitat Breadth", ylab = "", 
     main = "Habitat Specificity", col = "darkgreen", pch = 17, cex = 1.3)
lm_hb_na = lm(NAfrogs$threat ~ NAfrogs$HabitatBreadth)
abline(lm_hb_na, col = "darkgreen")
summary(lm_hb_na)
points(Efrogs$HabitatBreadth, Efrogs$threat, 
       col = "mediumpurple1", pch = 19, cex = 1.3)
lm_hb_e = lm(Efrogs$threat ~ Efrogs$HabitatBreadth)
abline(lm_hb_e, col = "mediumpurple1")
summary(lm_hb_e)
mtext("(C)", 3, adj = 0, line = 1)
# Minimum Elevation
plot(NAfrogs$MinElevation_m, NAfrogs$threat, xlab = "Minimum Elevation (m)", 
     ylab = "", main = "Minimum Elevation", 
     col = "darkgreen", pch = 17, cex = 1.3)
lm_me_na = lm(NAfrogs$threat ~ NAfrogs$MinElevation_m)
abline(lm_me_na, col = "darkgreen")
summary(lm_me_na)
points(Efrogs$Altitude_min, Efrogs$threat, col = "mediumpurple1", pch = 19, cex = 1.3)
lm_me_e = lm(Efrogs$threat ~ Efrogs$Altitude_min)
abline(lm_me_e, col = "mediumpurple1")
summary(lm_me_e)
mtext("(D)", 3, adj = 0, line = 1)
mtext("Threat Status", side = 2, outer = TRUE)
dev.off()

# Multi-Variable Linear Model North American Frogs
Multi_lm_na = lm(NAfrogs$threat ~ NAfrogs$SVL + NAfrogs$ClutchSize + 
                   NAfrogs$HabitatBreadth + NAfrogs$MinElevation_m)
summary(Multi_lm_na)
# Multi-Variable Linear Model European Frogs
Multi_lm_e = lm(Efrogs$threat ~ Efrogs$SVL_Unspecified_mm + 
                  Efrogs$Number_of_eggs_or_offspring + 
                  Efrogs$HabitatBreadth + Efrogs$Altitude_min)
summary(Multi_lm_e)


## European Anura and Urodela Comparison
pdf('plots/EurF_EurS.pdf', height = 8, width = 10)
par(mfrow=c(2,2), oma = c(0,2,2,0), cex.lab = 1.2, cex.axis = 1.2)
# SVL
plot(Efrogs$SVL_Unspecified_mm, Efrogs$threat, xlab = "Mean SVL (mm)", ylab = "", 
     col = "mediumpurple1", main = "Body Size", pch = 17, cex = 1.3)
lm_svl_f = lm(Efrogs$threat ~ Efrogs$SVL_Unspecified_mm)
abline(lm_svl_f, col = "mediumpurple1")
summary(lm_svl_f)
points(Esalamanders$SVL_Unspecified_mm, Esalamanders$threat, 
       col = "royalblue3", pch = 19, cex = 1.3)
lm_svl_s = lm(Esalamanders$threat ~ Esalamanders$SVL_Unspecified_mm)
abline(lm_svl_s, col = "royalblue3")
summary(lm_svl_s)
legend("topright", c("Anura", "Urodela"), pch = c(17,19), 
       col = c("mediumpurple1", "royalblue3"), cex = 1.3, bty = "n")
mtext("(A)", 3, adj = 0, line = 1)
# Clutch Size
plot(log(Efrogs$Number_of_eggs_or_offspring), Efrogs$threat, xlab = "ln(Mean Clutch Size)", 
     ylab = "", col = "mediumpurple1", main = "Fecundity", pch = 17, cex = 1.3)
lm_cs_f = lm(Efrogs$threat ~ log(Efrogs$Number_of_eggs_or_offspring))
abline(lm_cs_f, col = "mediumpurple1")
summary(lm_cs_f)
points(log(Esalamanders$Number_of_eggs_or_offspring), Esalamanders$threat, 
       col = "royalblue3", pch = 19, cex = 1.3)
lm_cs_s = lm(Esalamanders$threat ~ log(Esalamanders$Number_of_eggs_or_offspring))
abline(lm_cs_s, col = "royalblue3")
summary(lm_cs_s)
mtext("(B)", 3, adj = 0, line = 1)
# Habitat Breadth
plot(Efrogs$HabitatBreadth, Efrogs$threat, xlab = "Habitat Breadth", 
     ylab = "", col = "mediumpurple1", main = "Habitat Specificity", pch = 17, cex = 1.3)
lm_hb_f = lm(Efrogs$threat ~ Efrogs$HabitatBreadth)
abline(lm_hb_f, col = "mediumpurple1")
summary(lm_hb_f)
points(Esalamanders$HabitatBreadth, Esalamanders$threat, 
       col = "royalblue3", pch = 19, cex = 1.3)
lm_hb_s = lm(Esalamanders$threat ~ Esalamanders$HabitatBreadth)
abline(lm_hb_s, col = "royalblue3")
summary(lm_hb_s)
mtext("(C)", 3, adj = 0, line = 1)
# Minimum Elevation
plot(Efrogs$Altitude_min, Efrogs$threat, xlab = "Minimum Elevation (m)", 
     ylab = "", col = "mediumpurple1", 
     main = "Minimum Elevation", pch = 17, cex = 1.3)
lm_me_f = lm(Efrogs$threat ~ Efrogs$Altitude_min)
abline(lm_me_f, col = "mediumpurple1")
summary(lm_me_f)
points(Esalamanders$Altitude_min, Esalamanders$threat, 
       col = "royalblue3", pch = 19, cex = 1.3)
lm_me_s = lm(Esalamanders$threat ~ Esalamanders$Altitude_min)
abline(lm_me_s, col = "royalblue3")
summary(lm_me_s)
mtext("(D)", 3, adj = 0, line = 1)
mtext("Threat Status", outer = TRUE, side = 2)
dev.off()

# Multi-Variable Linear Model European Frogs
Multi_lm_e = lm(Efrogs$threat ~ Efrogs$SVL_Unspecified_mm + 
                  Efrogs$Number_of_eggs_or_offspring + 
                  Efrogs$HabitatBreadth + Efrogs$Altitude_min)
summary(Multi_lm_e)
# Multi-Variable Linear Model European Salamanders
Multi_lm_s = lm(Esalamanders$threat ~ Esalamanders$SVL_Unspecified_mm + 
                  Esalamanders$Number_of_eggs_or_offspring + 
                  Esalamanders$HabitatBreadth + 
                  Esalamanders$Altitude_min)
summary(Multi_lm_s)



#### Paired Analysis

#preparing the paired dataset
NApaired$HabitatBreadth1 <- apply(NApaired[,c(24:35)], 1, sum)
NApaired$HabitatBreadth2 <- apply(NApaired[,c(63:74)], 1, sum)

NApaired$ClutchSize1 <- ifelse(is.na(NApaired$MeanClutchSize1), 
                               ifelse(is.na(NApaired$MaxClutchSize1), 
                                      ifelse(is.na(NApaired$MinClutchSize1), NA, 
                                             NApaired$MinClutchSize1), 
                                      ifelse(is.na(NApaired$MinClutchSize1), 
                                             NApaired$MaxClutchSize1, 
                                             rowMeans(NApaired[,c("MinClutchSize1", 
                                                                  "MaxClutchSize1")]))), 
                               NApaired$MeanClutchSize1)
NApaired$ClutchSize2 <- ifelse(is.na(NApaired$MeanClutchSize2), 
                               ifelse(is.na(NApaired$MaxClutchSize2), 
                                      ifelse(is.na(NApaired$MinClutchSize2), NA, 
                                             NApaired$MinClutchSize2), 
                                      ifelse(is.na(NApaired$MinClutchSize2), 
                                             NApaired$MaxClutchSize2, 
                                             rowMeans(NApaired[,c("MinClutchSize2", 
                                                                  "MaxClutchSize2")]))), 
                               NApaired$MeanClutchSize2)

NApaired$SVLunspecified1 <- ifelse(is.na(NApaired$MeanSVL_Unspecified_mm1), 
                                   ifelse(is.na(NApaired$MaxSVL_Unspecified_mm1), 
                                          ifelse(is.na(NApaired$MinSVL_Unspecified_mm1), NA, 
                                                 NApaired$MinSVL_Unspecified_mm1), 
                                          ifelse(is.na(NApaired$MinSVL_Unspecified_mm1), 
                                                 NApaired$MaxSVL_Unspecified_mm1, 
                                                 rowMeans(NApaired[,c("MinSVL_Unspecified_mm1", 
                                                                      "MaxSVL_Unspecified_mm1")]))), 
                                   NApaired$MeanSVL_Unspecified_mm1)
NApaired$SVLfemale1 <- ifelse(is.na(NApaired$MeanSVL_Female_mm1), 
                              ifelse(is.na(NApaired$MaxSVL_Female_mm1), 
                                     ifelse(is.na(NApaired$MinSVL_Female_mm1), NA, 
                                            NApaired$MinSVL_Female_mm1), 
                                     ifelse(is.na(NApaired$MinSVL_Female_mm1), 
                                            NApaired$MaxSVL_Female_mm1, 
                                            rowMeans(NApaired[,c("MinSVL_Female_mm1", 
                                                                 "MaxSVL_Female_mm1")]))), 
                              NApaired$MeanSVL_Female_mm1)
NApaired$SVLmale1 <- ifelse(is.na(NApaired$MeanSVL_Male_mm1), 
                            ifelse(is.na(NApaired$MaxSVL_Male_mm1), 
                                   ifelse(is.na(NApaired$MinSVL_Male_mm1), NA, 
                                          NApaired$MinSVL_Male_mm1), 
                                   ifelse(is.na(NApaired$MinSVL_Male_mm1), 
                                          NApaired$MaxSVL_Male_mm1, 
                                          rowMeans(NApaired[,c("MinSVL_Male_mm1", 
                                                               "MaxSVL_Male_mm1")]))), 
                            NApaired$MeanSVL_Male_mm1)
NApaired$SVLMFmean1 <- rowMeans(NApaired[,c("SVLfemale1", "SVLmale1")])
NApaired$SVL1 <- ifelse(!is.na(NApaired$SVLunspecified1) & !is.na(NApaired$SVLfemale1) & 
                          !is.na(NApaired$SVLmale1),
                        rowMeans(NApaired[,c("SVLunspecified1", "SVLMFmean1")]), 
                        ifelse(is.na(NApaired$SVLunspecified1), 
                               ifelse(is.na(NApaired$SVLfemale1), 
                                      ifelse(is.na(NApaired$SVLmale1), NA, 
                                             NApaired$SVLmale1), 
                                      ifelse(is.na(NApaired$SVLmale1), 
                                             NApaired$SVLfemale1, NApaired$SVLMFmean1)), 
                               NApaired$SVLunspecified1))
NApaired$SVLunspecified2 <- ifelse(is.na(NApaired$MeanSVL_Unspecified_mm2), 
                                   ifelse(is.na(NApaired$MaxSVL_Unspecified_mm2), 
                                          ifelse(is.na(NApaired$MinSVL_Unspecified_mm2), NA, 
                                                 NApaired$MinSVL_Unspecified_mm2), 
                                          ifelse(is.na(NApaired$MinSVL_Unspecified_mm2), 
                                                 NApaired$MaxSVL_Unspecified_mm2, 
                                                 rowMeans(NApaired[,c("MinSVL_Unspecified_mm2", 
                                                                      "MaxSVL_Unspecified_mm2")]))), 
                                   NApaired$MeanSVL_Unspecified_mm2)
NApaired$SVLfemale2 <- ifelse(is.na(NApaired$MeanSVL_Female_mm2), 
                              ifelse(is.na(NApaired$MaxSVL_Female_mm2), 
                                     ifelse(is.na(NApaired$MinSVL_Female_mm2), NA, 
                                            NApaired$MinSVL_Female_mm2), 
                                     ifelse(is.na(NApaired$MinSVL_Female_mm2), 
                                            NApaired$MaxSVL_Female_mm2, 
                                            rowMeans(NApaired[,c("MinSVL_Female_mm2", 
                                                                 "MaxSVL_Female_mm2")]))), 
                              NApaired$MeanSVL_Female_mm2)
NApaired$SVLmale2 <- ifelse(is.na(NApaired$MeanSVL_Male_mm2), 
                            ifelse(is.na(NApaired$MaxSVL_Male_mm2), 
                                   ifelse(is.na(NApaired$MinSVL_Male_mm2), NA, 
                                          NApaired$MinSVL_Male_mm2), 
                                   ifelse(is.na(NApaired$MinSVL_Male_mm2), 
                                          NApaired$MaxSVL_Male_mm2, 
                                          rowMeans(NApaired[,c("MinSVL_Male_mm2", 
                                                               "MaxSVL_Male_mm2")]))), 
                            NApaired$MeanSVL_Male_mm2)
NApaired$SVLMFmean2 <- rowMeans(NApaired[,c("SVLfemale2", "SVLmale2")])
NApaired$SVL2 <- ifelse(!is.na(NApaired$SVLunspecified2) & !is.na(NApaired$SVLfemale2) & 
                          !is.na(NApaired$SVLmale2), 
                        rowMeans(NApaired[,c("SVLunspecified2", "SVLMFmean2")]), 
                        ifelse(is.na(NApaired$SVLunspecified2), 
                               ifelse(is.na(NApaired$SVLfemale2), 
                                      ifelse(is.na(NApaired$SVLmale2), NA, 
                                             NApaired$SVLmale2), 
                                      ifelse(is.na(NApaired$SVLmale2), NApaired$SVLfemale2, 
                                             NApaired$SVLMFmean2)), NApaired$SVLunspecified2))

## Wilcoxon Tests
wilcox.test(NApaired$SVL1, NApaired$SVL2, paired = TRUE)
wilcox.test(NApaired$ClutchSize1, NApaired$ClutchSize2, paired = TRUE)
wilcox.test(NApaired$HabitatBreadth1, NApaired$HabitatBreadth2, paired = TRUE)
wilcox.test(NApaired$MinElevation_m1, NApaired$MinElevation_m2, paired = TRUE)

## Creating Ratio Columns
NApaired$SVLratio = NApaired$SVL1 / NApaired$SVL2
NApaired$ClutchSizeratio = NApaired$ClutchSize1 / NApaired$ClutchSize2
NApaired$HabitatBreadthratio = NApaired$HabitatBreadth1 / NApaired$HabitatBreadth2
NApaired$MinElevationratio = (NApaired$MinElevation_m1 + 1) / (NApaired$MinElevation_m2 + 1)

## Paired Ratio Boxplots
pdf('plots/pairedratio.pdf', height = 8, width = 10)
par(cex.lab = 1.3, cex.axis = 1.2)
boxplot(log(NApaired[, c('SVLratio', 'ClutchSizeratio', 
                     'HabitatBreadthratio', "MinElevationratio")]), 
        names = c("Mean SVL (mm)", "Mean Clutch Size", 
                  "Habitat Breadth", "Minimum Elevation (m)"), 
        ylab = "Log( non-threatened / threatened )")
abline(h=0, col = "blue", cex = 1.2)
text(x=1, y=4, labels = "P = 0.417", cex = 1.3)
text(x=2, y=4, labels = "P = 0.073", cex = 1.3)
text(x=3, y=4, labels = "P = 0.023", cex = 1.3)
text(x=4, y=4, labels = "P = 0.018", cex = 1.3)
dev.off()
