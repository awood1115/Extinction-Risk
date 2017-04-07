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
EuroAmphib$threat[EuroAmphib$IUCN_Status_Vulnerable == 1] <- 2
EuroAmphib$threat[EuroAmphib$IUCN_Status_NearThreatened == 1] <- 3
EuroAmphib$threat[EuroAmphib$IUCN_Status_Endangered == 1] <- 5
EuroAmphib$threat[EuroAmphib$IUCN_Status_CriticallyEndangered == 1] <- 6
# Creating Threat status ordinal values
threat = data.frame(IUCN_Threat_Status = c('LC', 'VU', 'NT', 'TH', 'EN', 'CR', 'EW'),
                    threat = 1:7)



### Joining in taxonomy and threat datasets

NAfrogs <- merge(NAfrogs, Taxonomy, by = "SpeciesName") %>% left_join(threat)
EuroAmphib <- merge(EuroAmphib, Taxonomy, by = "SpeciesName")



### Dividing European dataset into Anura and Urodela order datasets

Efrogs <- EuroAmphib[EuroAmphib$Order == 'Anura',]
Esalamanders <- EuroAmphib[EuroAmphib$Order == 'Urodela',]



##### Plots and Linear Models

## North American and European Anura Comparison
par(mfrow=c(2,2), oma = c(0,0,2,0))
# SVL
plot(NAfrogs$SVL, NAfrogs$threat, xlab = "Mean SVL(mm)", ylab = "Threat Status", 
     main = "Mean SVL", col = "darkgreen", pch = 16)
lm_svl_na = lm(NAfrogs$threat ~ NAfrogs$SVL)
abline(lm_svl_na, col = "darkgreen")
summary(lm_svl_na)
points(Efrogs$SVL_Unspecified_mm, Efrogs$threat, xlab = "Mean SVL (mm)", ylab = "Threat Status", 
       col = "mediumpurple3", pch = 16)
lm_svl_e = lm(Efrogs$threat ~ Efrogs$SVL_Unspecified_mm)
abline(lm_svl_e, col = "mediumpurple3")
summary(lm_svl_e)
legend(115,7.5, c("North American", "European"), pch = 16, 
       col = c("darkgreen", "mediumpurple3"), cex = 0.9, bty = "n", y.intersp = 0.5)
# Clutch Size
plot(NAfrogs$ClutchSize, NAfrogs$threat, xlab = "Mean Clutch Size", ylab = "Threat Status", 
     main = "Mean Clutch Size", col = "darkgreen", pch = 16)
lm_cs_na = lm(NAfrogs$threat ~ NAfrogs$ClutchSize)
abline(lm_cs_na, col = "darkgreen")
summary(lm_cs_na)
points(Efrogs$Number_of_eggs_or_offspring, Efrogs$threat, xlab = "Mean Clutch Size", 
       ylab = "Threat Status", col = "mediumpurple3", pch = 16)
lm_cs_e = lm(Efrogs$threat ~ Efrogs$Number_of_eggs_or_offspring)
abline(lm_cs_e, col = "mediumpurple3")
summary(lm_cs_e)
legend(19000,7.5, c("North American", "European"), pch = 16, 
       col = c("darkgreen", "mediumpurple3"), cex = 0.9, bty = "n", y.intersp = 0.5)
# Habitat Breadth
plot(NAfrogs$HabitatBreadth, NAfrogs$threat, xlab = "Habitat Breadth", ylab = "Threat Status", 
     main = "Habitat Breadth", col = "darkgreen", pch = 16)
lm_hb_na = lm(NAfrogs$threat ~ NAfrogs$HabitatBreadth)
abline(lm_hb_na, col = "darkgreen")
summary(lm_hb_na)
points(Efrogs$HabitatBreadth, Efrogs$threat, xlab = "Habitat Breadth", ylab = "Threat Status", 
       col = "mediumpurple3", pch = 16)
lm_hb_e = lm(Efrogs$threat ~ Efrogs$HabitatBreadth)
abline(lm_hb_e, col = "mediumpurple3")
summary(lm_hb_e)
legend(6.3,7.5, c("North American", "European"), pch = 16, 
       col = c("darkgreen", "mediumpurple3"), cex = 0.9, bty = "n", y.intersp = 0.5)
# Minimum Elevation
plot(NAfrogs$MinElevation_m, NAfrogs$threat, xlab = "Minimum Elevation (m)", 
     ylab = "Threat Status", main = "Minimum Elevation", 
     col = "darkgreen", pch = 16)
lm_me_na = lm(NAfrogs$threat ~ NAfrogs$MinElevation_m)
abline(lm_me_na, col = "darkgreen")
summary(lm_me_na)
points(Efrogs$Altitude_min, Efrogs$threat, xlab = "Minimum Altitude (m)", ylab = "Threat Status", 
       col = "mediumpurple3", pch = 16)
lm_me_e = lm(Efrogs$threat ~ Efrogs$Altitude_min)
abline(lm_me_e, col = "mediumpurple3")
summary(lm_me_e)
legend(1700,7.5, c("North American", "European"), pch = 16, 
       col = c("darkgreen", "mediumpurple3"), cex = 0.9, bty = "n", y.intersp = 0.5)
mtext("North American and European Anura Extinction Risk Correlates", outer = TRUE, cex = 1.3)

# Multi-Variable Linear Model North American Frogs
Multi_lm_na = lm(NAfrogs$threat ~ NAfrogs$SVL + NAfrogs$ClutchSize + NAfrogs$HabitatBreadth + 
                   NAfrogs$MinElevation_m)
summary(Multi_lm_na)
# Multi-Variable Linear Model European Frogs
Multi_lm_e = lm(Efrogs$threat ~ Efrogs$SVL_Unspecified_mm + Efrogs$Number_of_eggs_or_offspring + 
                  Efrogs$HabitatBreadth + Efrogs$Altitude_min)
summary(Multi_lm_e)

## European Anura and Urodela Comparison
par(mfrow=c(2,2), oma = c(0,0,2,0))
# SVL
plot(Efrogs$SVL_Unspecified_mm, Efrogs$threat, xlab = "Mean SVL (mm)", ylab = "Threat Status", 
     col = "mediumpurple3", main = "Mean SVL", pch = 16)
lm_svl_f = lm(Efrogs$threat ~ Efrogs$SVL_Unspecified_mm)
abline(lm_svl_f, col = "mediumpurple3")
summary(lm_svl_f)
points(Esalamanders$SVL_Unspecified_mm, Esalamanders$threat, xlab = "Mean SVL (mm)", 
       ylab = "Threat Status", col = "palevioletred4", pch = 16)
lm_svl_s = lm(Esalamanders$threat ~ Esalamanders$SVL_Unspecified_mm)
abline(lm_svl_s, col = "palevioletred4")
summary(lm_svl_s)
legend(103,5, c("North American", "European"), pch = 16, 
       col = c("mediumpurple3", "palevioletred4"), cex = 0.9, bty = "n", y.intersp = 0.5)
# Clutch Size
plot(Efrogs$Number_of_eggs_or_offspring, Efrogs$threat, xlab = "Mean Clutch Size", 
     ylab = "Threat Status", col = "mediumpurple3", 
     main = "Clutch Size", pch = 16)
lm_cs_f = lm(Efrogs$threat ~ Efrogs$Number_of_eggs_or_offspring)
abline(lm_cs_f, col = "mediumpurple3")
summary(lm_cs_f)
points(Esalamanders$Number_of_eggs_or_offspring, Esalamanders$threat, xlab = "Mean Clutch Size", 
       ylab = "Threat Status", col = "palevioletred4", pch = 16)
lm_cs_s = lm(Esalamanders$threat ~ Esalamanders$Number_of_eggs_or_offspring)
abline(lm_cs_s, col = "palevioletred4")
summary(lm_cs_s)
legend(17000,4, c("North American", "European"), pch = 16, 
       col = c("mediumpurple3", "palevioletred4"), cex = 0.9, bty = "n", y.intersp = 0.5)
# Habitat Breadth
plot(Efrogs$HabitatBreadth, Efrogs$threat, xlab = "Habitat Breadth", 
     ylab = "Threat Status", col = "mediumpurple3", 
     main = "Habitat Breadth", pch = 16)
lm_hb_f = lm(Efrogs$threat ~ Efrogs$HabitatBreadth)
abline(lm_hb_f, col = "mediumpurple3")
summary(lm_hb_f)
points(Esalamanders$HabitatBreadth, Esalamanders$threat, xlab = "Habitat Breadth", 
       ylab = "Threat Status", col = "palevioletred4", pch = 16)
lm_hb_s = lm(Esalamanders$threat ~ Esalamanders$HabitatBreadth)
abline(lm_hb_s, col = "palevioletred4")
summary(lm_hb_s)
legend(3,5, c("North American", "European"), pch = 16, 
       col = c("mediumpurple3", "palevioletred4"), cex = 0.9, bty = "n", y.intersp = 0.5)
# Minimum Elevation
plot(Efrogs$Altitude_min, Efrogs$threat, xlab = "Minimum Altitude (m)", 
     ylab = "Threat Status", col = "mediumpurple3", 
     main = "Minimum Elevation", pch = 16)
lm_me_f = lm(Efrogs$threat ~ Efrogs$Altitude_min)
abline(lm_me_f, col = "mediumpurple3")
summary(lm_me_f)
points(Esalamanders$Altitude_min, Esalamanders$threat, xlab = "Minimum Altitude (m)", 
       ylab = "Threat Status", col = "palevioletred4", pch = 16)
lm_me_s = lm(Esalamanders$threat ~ Esalamanders$Altitude_min)
abline(lm_me_s, col = "palevioletred4")
summary(lm_me_s)
legend(550,3.3, c("North American", "European"), pch = 16, 
       col = c("mediumpurple3", "palevioletred4"), cex = 0.9, bty = "n", y.intersp = 0.5)
mtext("European Anura and Urodela Extinction Risk Correlates", outer = TRUE, cex = 1.3)

# Multi-Variable Linear Model European Frogs
Multi_lm_e = lm(Efrogs$threat ~ Efrogs$SVL_Unspecified_mm + 
                  Efrogs$Number_of_eggs_or_offspring + 
                  Efrogs$HabitatBreadth + Efrogs$Altitude_min)
summary(Multi_lm_e)
# Multi-Variable Linear Model European Frogs
Multi_lm_s = lm(Esalamanders$threat ~ Esalamanders$SVL_Unspecified_mm + 
                  Esalamanders$Number_of_eggs_or_offspring + Esalamanders$HabitatBreadth + 
                  Esalamanders$Altitude_min)
summary(Multi_lm_s)

## North American Anaxyrus and Rana Analyses
Rana = NAfrogs[NAfrogs$Genus == 'Rana',]
Anaxyrus = NAfrogs[NAfrogs$Genus == 'Anaxyrus',]
par(mfrow=c(2,2), oma = c(0,0,2,0))
# SVL
plot(Rana$SVL, Rana$threat, xlab = "Mean SVL(mm)", ylab = "Threat Status", 
     main = "Mean SVL", col = "skyblue3", pch = 16)
lm_svl_r = lm(Rana$threat ~ Rana$SVL)
abline(lm_svl_r, col = "skyblue3")
summary(lm_svl_r)
points(Anaxyrus$SVL, Anaxyrus$threat, xlab = "Mean SVL(mm)", ylab = "Threat Status", 
       col = "purple", pch = 16)
lm_svl_a = lm(Anaxyrus$threat ~ Anaxyrus$SVL)
abline(lm_svl_a, col = "purple")
summary(lm_svl_a)
legend(125,5.5, c("Rana", "Anaxyrus"), pch = 16, 
       col = c("skyblue3", "purple"), cex = 0.9, bty = "n")
# Clutch Size
plot(Rana$ClutchSize, Rana$threat, xlab = "Mean Clutch Size", ylab = "Threat Status", 
     main = "Mean Clutch Size", 
     col = "skyblue3", pch = 16)
lm_cs_r = lm(Rana$threat ~ Rana$ClutchSize)
abline(lm_cs_r, col = "skyblue3")
summary(lm_cs_r)
points(Anaxyrus$ClutchSize, Anaxyrus$threat, xlab = "Mean Clutch Size", ylab = "Threat Status", 
       col = "purple", pch = 16)
lm_cs_a = lm(Anaxyrus$threat ~ Anaxyrus$ClutchSize)
abline(lm_cs_a, col = "purple")
summary(lm_cs_a)
legend(8000,5.5, c("Rana", "Anaxyrus"), pch = 16, 
       col = c("skyblue3", "purple"), cex = 0.9, bty = "n")
# Habitat Breadth
plot(Rana$HabitatBreadth, Rana$threat, xlab = "Habitat Breadth", ylab = "Threat Status", 
     main = "Habitat Breadth", 
     col = "skyblue3", pch = 16)
lm_hb_r = lm(Rana$threat ~ Rana$HabitatBreadth)
abline(lm_hb_r, col = "skyblue3")
summary(lm_hb_r)
points(Anaxyrus$HabitatBreadth, Anaxyrus$threat, xlab = "Habitat Breadth", 
       ylab = "Threat Status", col = "purple", pch = 16)
lm_hb_a = lm(Anaxyrus$threat ~ Anaxyrus$HabitatBreadth)
abline(lm_hb_a, col = "purple")
summary(lm_hb_a)
legend(5.5,5.5, c("Rana", "Anaxyrus"), pch = 16, 
       col = c("skyblue3", "purple"), cex = 0.9, bty = "n")
# Minimum Elevation
plot(Rana$MinElevation_m, Rana$threat, xlab = "Minimum Elevation (m)", ylab = "Threat Status", 
     main = "Minimum Elevation", 
     col = "skyblue3", pch = 16)
lm_me_r = lm(Rana$threat ~ Rana$MinElevation_m)
abline(lm_me_r, col = "skyblue3")
summary(lm_me_r)
points(Anaxyrus$MinElevation_m, Anaxyrus$threat, xlab = "Minimum Elevation (m)", 
       ylab = "Threat Status", col = "purple", pch = 16)
lm_me_a = lm(Anaxyrus$threat ~ Anaxyrus$MinElevation_m)
abline(lm_me_a, col = "purple")
summary(lm_me_a)
legend(700,5.8, c("Rana", "Anaxyrus"), pch = 16, 
       col = c("skyblue3", "purple"), cex = 0.9, bty = "n")
mtext("North American Anaxyrus and Rana Extinction Risk Correlates", outer = TRUE, cex = 1.3)

# Multi-Variable Linear Model North American Rana
Multi_lm_r = lm(Rana$threat ~ Rana$SVL + Rana$ClutchSize + Rana$HabitatBreadth + 
                  Rana$MinElevation_m)
summary(Multi_lm_r)
# Multi-Variable Linear Model North American Anaxyrus
Multi_lm_a = lm(Anaxyrus$threat ~ Anaxyrus$SVL + Anaxyrus$ClutchSize + 
                  Anaxyrus$HabitatBreadth + Anaxyrus$MinElevation_m)
summary(Multi_lm_a)



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



### Diet
diet = data.frame(diet = c('Ins', 'Mol', 'Can'), dietid = 1:3)

NAthreatgroups <- group_by(NAfrogs, threat) %>%
  summarise(Ins = sum(Insectivorous, na.rm = TRUE), 
            Mol = sum(Moluscivorous, na.rm = TRUE), 
            Can = sum(Cannibalism, na.rm = TRUE)) %>%
  gather(key = diet, value = n, Ins:Can) %>%
  left_join(diet) %>% data.frame()
plot(NAthreatgroups$dietid, NAthreatgroups$threat, pch = 16, 
     cex = NAthreatgroups$n/8, col = "darkgreen")
