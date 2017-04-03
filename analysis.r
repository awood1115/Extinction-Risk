# ENEC 395 Undergraduate Research 
# Alicia Wood
# 3/20/17
# Correlates of Extinction Risk in European and North American Amphibians


### Setting my working directory to the folder where all of my data and code are stored

setwd('~/Desktop/Extinction-Risk')

library(dplyr)


### Reading in raw data files

NAfrogs = read.table("NorthAmericanFrogs.txt", header=T, sep = '\t', quote = "\"")
NApaired = read.table("NorthAmericanFrogsPaired.txt", header=T, sep='\t', quote = "\"")
EuroAmphib = read.table("EuropeanAmphibians.txt", header=T, sep = '\t', quote = "\"", na.strings = "DD")
Taxonomy = read.table("Taxonomy.txt", header=T, sep = '\t', quote = "\"")

# Creating threat binary column in the European Dataset
EuroAmphib$ThreatBinary <- NA
EuroAmphib$ThreatBinary[EuroAmphib$IUCN_Status_Vulnerable == 1 | EuroAmphib$IUCN_Status_Endangered == 1 | EuroAmphib$IUCN_Status_NearThreatened == 1 | EuroAmphib$IUCN_Status_CriticallyEndangered == 1] <- 1
EuroAmphib$ThreatBinary[EuroAmphib$IUCN_Status_LeastConcern == 1] <- 0
# Creating Continuous Threat Status Variable
EuroAmphib$threat[EuroAmphib$IUCN_Status_LeastConcern == 1] <- 1
EuroAmphib$threat[EuroAmphib$IUCN_Status_Vulnerable == 1] <- 2
EuroAmphib$threat[EuroAmphib$IUCN_Status_NearThreatened == 1] <- 3
EuroAmphib$threat[EuroAmphib$IUCN_Status_Endangered == 1] <- 5
EuroAmphib$threat[EuroAmphib$IUCN_Status_CriticallyEndangered == 1] <- 6


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

EuroAmphib$HabitatBreadth <- apply(EuroAmphib[,c(256,265)], 1, sum)
NAfrogs$HabitatBreadth <- apply(NAfrogs[,c(22:33)], 1, sum)

#Condensing mean max and min Clutch Size into 1 mean Clutch Size column for NA dataset
NAfrogs$ClutchSize <- ifelse(is.na(NAfrogs$MeanClutchSize), ifelse(is.na(NAfrogs$MaxClutchSize), ifelse(is.na(NAfrogs$MinClutchSize), NA, NAfrogs$MinClutchSize), ifelse(is.na(NAfrogs$MinClutchSize), NAfrogs$MaxClutchSize, rowMeans(NAfrogs[,c("MinClutchSize", "MaxClutchSize")]))), NAfrogs$MeanClutchSize)
#Europe dataset already in 1 column named number_of_eggs_or_offspring

#Condensing mean max and min SVL unspecified into 1 column for NA dataset
NAfrogs$SVLunspecified <- ifelse(is.na(NAfrogs$MeanSVL_Unspecified_mm), ifelse(is.na(NAfrogs$MaxSVL_Unspecified_mm), ifelse(is.na(NAfrogs$MinSVL_Unspecified_mm), NA, NAfrogs$MinSVL_Unspecified_mm), ifelse(is.na(NAfrogs$MinSVL_Unspecified_mm), NAfrogs$MaxSVL_Unspecified_mm, rowMeans(NAfrogs[,c("MinSVL_Unspecified_mm", "MaxSVL_Unspecified_mm")]))), NAfrogs$MeanSVL_Unspecified_mm)
#Condensing mean max and min SVL female into 1 column
NAfrogs$SVLfemale <- ifelse(is.na(NAfrogs$MeanSVL_Female_mm), ifelse(is.na(NAfrogs$MaxSVL_Female_mm), ifelse(is.na(NAfrogs$MinSVL_Female_mm), NA, NAfrogs$MinSVL_Female_mm), ifelse(is.na(NAfrogs$MinSVL_Female_mm), NAfrogs$MaxSVL_Female_mm, rowMeans(NAfrogs[,c("MinSVL_Female_mm", "MaxSVL_Female_mm")]))), NAfrogs$MeanSVL_Female_mm)
#Condensing mean max and min SVL male into 1 column
NAfrogs$SVLmale <- ifelse(is.na(NAfrogs$MeanSVL_Male_mm), ifelse(is.na(NAfrogs$MaxSVL_Male_mm), ifelse(is.na(NAfrogs$MinSVL_Male_mm), NA, NAfrogs$MinSVL_Male_mm), ifelse(is.na(NAfrogs$MinSVL_Male_mm), NAfrogs$MaxSVL_Male_mm, rowMeans(NAfrogs[,c("MinSVL_Male_mm", "MaxSVL_Male_mm")]))), NAfrogs$MeanSVL_Male_mm)
#Condensing male female and unspecified SVL into 1 final mean SVL column
NAfrogs$SVLMFmean <- rowMeans(NAfrogs[,c("SVLfemale", "SVLmale")])
NAfrogs$SVL <- ifelse(!is.na(NAfrogs$SVLunspecified) & !is.na(NAfrogs$SVLfemale) & !is.na(NAfrogs$SVLmale), rowMeans(NAfrogs[,c("SVLunspecified", "SVLMFmean")]), ifelse(is.na(NAfrogs$SVLunspecified), ifelse(is.na(NAfrogs$SVLfemale), ifelse(is.na(NAfrogs$SVLmale), NA, NAfrogs$SVLmale), ifelse(is.na(NAfrogs$SVLmale), NAfrogs$SVLfemale, NAfrogs$SVLMFmean)), NAfrogs$SVLunspecified))
#Mean SVL alredy in 1 column in Euro dastaset named SVL_unspecified_mm



# Threat status ordinal values
threat = data.frame(IUCN_Threat_Status = c('LC', 'VU', 'NT', 'TH', 'EN', 'CR', 'EW'),
                    threat = 1:7)
# Joining in taxon info
NAfrogs <- merge(NAfrogs, Taxonomy, by = "SpeciesName") %>% left_join(threat)
EuroAmphib <- merge(EuroAmphib, Taxonomy, by = "SpeciesName")

##### Plots

### North American Frogs

# Single Trait Linear Models
plot(NAfrogs$SVL, NAfrogs$threat, xlab = "Mean SVL(mm)", ylab = "Threat Status", main = "Threat Status vs. Mean SVL", col = "darkgreen", pch = 16)
lmsvl = lm(NAfrogs$threat ~ NAfrogs$SVL)
abline(lmsvl)
summary(lmsvl)
plot(NAfrogs$ClutchSize, NAfrogs$threat, xlab = "Mean Clutch Size", ylab = "Threat Status", main = "Threat Status vs. Mean Clutch Size", col = "mediumpurple3", pch = 16)
lmcs = lm(NAfrogs$threat ~ NAfrogs$ClutchSize)
abline(lmcs)
summary(lmcs)
plot(NAfrogs$HabitatBreadth, NAfrogs$threat, xlab = "Habitat Breadth", ylab = "Threat Status", main = "Threat Status vs. Habitat Breadth", col = "skyblue3", pch = 16)
lmbh = lm(NAfrogs$threat ~ NAfrogs$HabitatBreadth)
abline(lmbh)
summary(lmbh)
# Multi-Variable Linear Model
MultiLM = lm(NAfrogs$threat ~ NAfrogs$SVL + NAfrogs$ClutchSize + NAfrogs$HabitatBreadth)
summary(MultiLM)

### North American Rana
Rana = NAfrogs[NAfrogs$Genus == 'Rana',]
plot(Rana$SVL, Rana$threat, xlab = "Mean SVL(mm)", ylab = "Threat Status", main = "Rana Threat Status vs. Mean SVL", col = "darkgreen", pch = 16)
lmsvlr = lm(Rana$threat ~ Rana$SVL)
abline(lmsvlr)
summary(lmsvlr)
plot(Rana$ClutchSize, Rana$threat, xlab = "Mean Clutch Size", ylab = "Threat Status", main = "Rana Threat Status vs. Mean Clutch Size", col = "mediumpurple3", pch = 16)
lmcsr = lm(Rana$threat ~ Rana$ClutchSize)
abline(lmcsr)
summary(lmcsr)
plot(Rana$HabitatBreadth, Rana$threat, xlab = "Habitat Breadth", ylab = "Threat Status", main = "Rana Threat Status vs. Habitat Breadth", col = "skyblue3", pch = 16)
lmbhr = lm(Rana$threat ~ Rana$HabitatBreadth)
abline(lmbhr)
summary(lmbhr)
MultiLMr = lm(Rana$threat ~ Rana$SVL + Rana$ClutchSize + Rana$HabitatBreadth)
summary(MultiLMr)

# North American Anaxyrus
plot(Anaxyrus$SVL, Anaxyrus$threat, xlab = "Mean SVL(mm)", ylab = "Threat Status", main = "Anaxyrus Threat Status vs. Mean SVL", col = "darkgreen", pch = 16)
lmsvla = lm(Anaxyrus$threat ~ Anaxyrus$SVL)
abline(lmsvla)
summary(lmsvla)
plot(Anaxyrus$ClutchSize, Anaxyrus$threat, xlab = "Mean Clutch Size", ylab = "Threat Status", main = "Anaxyrus Threat Status vs. Mean Clutch Size", col = "mediumpurple3", pch = 16)
lmcsa = lm(Anaxyrus$threat ~ Anaxyrus$ClutchSize)
abline(lmcsa)
summary(lmcsa)
plot(Anaxyrus$HabitatBreadth, Anaxyrus$threat, xlab = "Habitat Breadth", ylab = "Threat Status", main = "Anaxyrus Threat Status vs. Habitat Breadth", col = "skyblue3", pch = 16)
lmbha = lm(Anaxyrus$threat ~ Anaxyrus$HabitatBreadth)
abline(lmbha)
summary(lmbha)
MultiLMa = lm(Anaxyrus$threat ~ Anaxyrus$SVL + Anaxyrus$ClutchSize + Anaxyrus$HabitatBreadth)
summary(MultiLMa)


### Next group by order then plot European frogs and salamanders
plot(EuroAmphib$SVL_Unspecified_mm, EuroAmphib$threat, xlab = "Mean SVL (mm)", ylab = "Threat Status", main = "Threat Status vs Mean SVL", col = "darkgreen", pch = 16)


#### Paired Analysis

#preparing the paired dataset
NApaired$HabitatBreadth1 <- apply(NApaired[,c(24:35)], 1, sum)
NApaired$HabitatBreadth2 <- apply(NApaired[,c(63:74)], 1, sum)

NApaired$ClutchSize1 <- ifelse(is.na(NApaired$MeanClutchSize1), ifelse(is.na(NApaired$MaxClutchSize1), ifelse(is.na(NApaired$MinClutchSize1), NA, NApaired$MinClutchSize1), ifelse(is.na(NApaired$MinClutchSize1), NApaired$MaxClutchSize1, rowMeans(NApaired[,c("MinClutchSize1", "MaxClutchSize1")]))), NApaired$MeanClutchSize1)
NApaired$ClutchSize2 <- ifelse(is.na(NApaired$MeanClutchSize2), ifelse(is.na(NApaired$MaxClutchSize2), ifelse(is.na(NApaired$MinClutchSize2), NA, NApaired$MinClutchSize2), ifelse(is.na(NApaired$MinClutchSize2), NApaired$MaxClutchSize2, rowMeans(NApaired[,c("MinClutchSize2", "MaxClutchSize2")]))), NApaired$MeanClutchSize2)

NApaired$SVLunspecified1 <- ifelse(is.na(NApaired$MeanSVL_Unspecified_mm1), ifelse(is.na(NApaired$MaxSVL_Unspecified_mm1), ifelse(is.na(NApaired$MinSVL_Unspecified_mm1), NA, NApaired$MinSVL_Unspecified_mm1), ifelse(is.na(NApaired$MinSVL_Unspecified_mm1), NApaired$MaxSVL_Unspecified_mm1, rowMeans(NApaired[,c("MinSVL_Unspecified_mm1", "MaxSVL_Unspecified_mm1")]))), NApaired$MeanSVL_Unspecified_mm1)
NApaired$SVLfemale1 <- ifelse(is.na(NApaired$MeanSVL_Female_mm1), ifelse(is.na(NApaired$MaxSVL_Female_mm1), ifelse(is.na(NApaired$MinSVL_Female_mm1), NA, NApaired$MinSVL_Female_mm1), ifelse(is.na(NApaired$MinSVL_Female_mm1), NApaired$MaxSVL_Female_mm1, rowMeans(NApaired[,c("MinSVL_Female_mm1", "MaxSVL_Female_mm1")]))), NApaired$MeanSVL_Female_mm1)
NApaired$SVLmale1 <- ifelse(is.na(NApaired$MeanSVL_Male_mm1), ifelse(is.na(NApaired$MaxSVL_Male_mm1), ifelse(is.na(NApaired$MinSVL_Male_mm1), NA, NApaired$MinSVL_Male_mm1), ifelse(is.na(NApaired$MinSVL_Male_mm1), NApaired$MaxSVL_Male_mm1, rowMeans(NApaired[,c("MinSVL_Male_mm1", "MaxSVL_Male_mm1")]))), NApaired$MeanSVL_Male_mm1)
NApaired$SVLMFmean1 <- rowMeans(NApaired[,c("SVLfemale1", "SVLmale1")])
NApaired$SVL1 <- ifelse(!is.na(NApaired$SVLunspecified1) & !is.na(NApaired$SVLfemale1) & !is.na(NApaired$SVLmale1), rowMeans(NApaired[,c("SVLunspecified1", "SVLMFmean1")]), ifelse(is.na(NApaired$SVLunspecified1), ifelse(is.na(NApaired$SVLfemale1), ifelse(is.na(NApaired$SVLmale1), NA, NApaired$SVLmale1), ifelse(is.na(NApaired$SVLmale1), NApaired$SVLfemale1, NApaired$SVLMFmean1)), NApaired$SVLunspecified1))
NApaired$SVLunspecified2 <- ifelse(is.na(NApaired$MeanSVL_Unspecified_mm2), ifelse(is.na(NApaired$MaxSVL_Unspecified_mm2), ifelse(is.na(NApaired$MinSVL_Unspecified_mm2), NA, NApaired$MinSVL_Unspecified_mm2), ifelse(is.na(NApaired$MinSVL_Unspecified_mm2), NApaired$MaxSVL_Unspecified_mm2, rowMeans(NApaired[,c("MinSVL_Unspecified_mm2", "MaxSVL_Unspecified_mm2")]))), NApaired$MeanSVL_Unspecified_mm2)
NApaired$SVLfemale2 <- ifelse(is.na(NApaired$MeanSVL_Female_mm2), ifelse(is.na(NApaired$MaxSVL_Female_mm2), ifelse(is.na(NApaired$MinSVL_Female_mm2), NA, NApaired$MinSVL_Female_mm2), ifelse(is.na(NApaired$MinSVL_Female_mm2), NApaired$MaxSVL_Female_mm2, rowMeans(NApaired[,c("MinSVL_Female_mm2", "MaxSVL_Female_mm2")]))), NApaired$MeanSVL_Female_mm2)
NApaired$SVLmale2 <- ifelse(is.na(NApaired$MeanSVL_Male_mm2), ifelse(is.na(NApaired$MaxSVL_Male_mm2), ifelse(is.na(NApaired$MinSVL_Male_mm2), NA, NApaired$MinSVL_Male_mm2), ifelse(is.na(NApaired$MinSVL_Male_mm2), NApaired$MaxSVL_Male_mm2, rowMeans(NApaired[,c("MinSVL_Male_mm2", "MaxSVL_Male_mm2")]))), NApaired$MeanSVL_Male_mm2)
NApaired$SVLMFmean2 <- rowMeans(NApaired[,c("SVLfemale2", "SVLmale2")])
NApaired$SVL2 <- ifelse(!is.na(NApaired$SVLunspecified2) & !is.na(NApaired$SVLfemale2) & !is.na(NApaired$SVLmale2), rowMeans(NApaired[,c("SVLunspecified2", "SVLMFmean2")]), ifelse(is.na(NApaired$SVLunspecified2), ifelse(is.na(NApaired$SVLfemale2), ifelse(is.na(NApaired$SVLmale2), NA, NApaired$SVLmale2), ifelse(is.na(NApaired$SVLmale2), NApaired$SVLfemale2, NApaired$SVLMFmean2)), NApaired$SVLunspecified2))

## Calculating Pair Differences
NApaired$SVLDiff <- NApaired$SVL2 - NApaired$SVL1
NApaired$ClutchSizeDiff <- NApaired$ClutchSize2 - NApaired$ClutchSize1
NApaired$HabitatBreadthDiff <- NApaired$HabitatBreadth2 - NApaired$HabitatBreadth1

## Wilcoxon Tests
wilcox.test(NApaired$SVLDiff)
wilcox.test(NApaired$ClutchSizeDiff)
wilcox.test(NApaired$HabitatBreadthDiff)











# example data visualizations
boxplot(NAfrogs$MeanClutchSize ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$MaxClutchSize ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$MinClutchSize ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$MeanSVL_Unspecified_mm ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$MeanSVL_Female_mm ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$MeanSVL_Male_mm ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$MaxSVL_Male_mm ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$MaxSVL_Female_mm ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$MaxSVL_Unspecified_mm ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$MinSVL_Unspecified_mm ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$MinSVL_Female_mm ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$MinSVL_Male_mm ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$Insectivorous ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$Cannibalism ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$Moluscivorous ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$MaxElevation_m ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$MinElevation_m ~ NAfrogs$ThreatBinary)
boxplot(NAfrogs$HabitatBreadth ~ NAfrogs$ThreatBinary)



boxplot(EuroAmphib$SVL_Unspecified_mm ~ EuroAmphib$ThreatBinary)
boxplot(EuroAmphib$SVL_Female_mm ~ EuroAmphib$ThreatBinary)
boxplot(EuroAmphib$SVL_Male_mm ~ EuroAmphib$ThreatBinary)
boxplot(EuroAmphib$Number_of_eggs_or_offspring ~ EuroAmphib$ThreatBinary)
boxplot(EuroAmphib$AdultDiet_Insectivorous ~ EuroAmphib$ThreatBinary)
boxplot(EuroAmphib$AdultDIet_Moluscivorous ~ EuroAmphib$ThreatBinary)
boxplot(EuroAmphib$AdultDiet_Cannibalism ~ EuroAmphib$ThreatBinary)
boxplot(EuroAmphib$AdultDiet_Herbivorous ~ EuroAmphib$ThreatBinary)
boxplot(EuroAmphib$Altitude_min ~ EuroAmphib$ThreatBinary)
boxplot(EuroAmphib$Altitude_max ~ EuroAmphib$ThreatBinary)
boxplot(EuroAmphib$HabitatBreadth ~ EuroAmphib$ThreatBinary)


# paired tests: e.g., Wilcoxon rank sum test (wilcox.test)

# need to rearrange data for paired comparison where you 

  