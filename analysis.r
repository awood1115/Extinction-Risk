# ENEC 395 Undergraduate Research 
# Alicia Wood
# 3/20/17
# Correlates of Extinction Risk in European and North American Amphibians


### Setting my working directory to the folder where all of my data and code are stored

setwd('~/Desktop/Extinction-Risk')

### Reading in raw data files

NAfrogs = read.table("NorthAmericanFrogs.txt", header=T, sep = '\t', quote = "\"")
EuroAmphib = read.table("EuropeanAmphibians.txt", header=T, sep = '\t', quote = "\"", na.strings = "DD")
Taxonomy = read.table("Taxonomy.txt", header=T, sep = '\t', quote = "\"")

# Creating threat binary column in the European Dataset
EuroAmphib$ThreatBinary <- NA
EuroAmphib$ThreatBinary[EuroAmphib$IUCN_Status_Vulnerable == 1 | EuroAmphib$IUCN_Status_Endangered == 1 | EuroAmphib$IUCN_Status_NearThreatened == 1 | EuroAmphib$IUCN_Status_CriticallyEndangered == 1] <- 1
EuroAmphib$ThreatBinary[EuroAmphib$IUCN_Status_LeastConcern == 1] <- 0

#Creating Habitat Breadth columns in both datasets
EuroAmphib$Habitat_Forest <- apply(EuroAmphib[,c(123:131)], 1, sum)
EuroAmphib$Habitat_Forest[EuroAmphib$Habitat_Forest > 0] <- 1
EuroAmphib$Habitat_Savanna <- apply(EuroAmphib[,c(132:133)], 1, sum)
EuroAmphib$Habitat_Savanna[EuroAmphib$Habitat_Savanna > 0] <- 1
EuroAmphib$Habitat_Shrubland <- apply(EuroAmphib[,c(134:141)], 1, sum)
EuroAmphib$Habitat_Shrubland[EuroAmphib$Habitat_Shrubland > 0] <- 1
EuroAmphib$Habitat_Grassland <- apply(EuroAmphib[,c(142:148)], 1, sum)
EuroAmphib$Habitat_Grassland[EuroAmphib$Habitat_Grassland > 0] <- 1
EuroAmphib$Habitat_Wetlands <- apply(EuroAmphib[,c(149:164)], 1, sum)
EuroAmphib$Habitat_Wetlands[EuroAmphib$Habitat_Wetlands > 0] <- 1
EuroAmphib$Habitat_RockyAreas[EuroAmphib$Habitat_RockyAreas_inland_cliffs_moutain_peaks == 1] <- 1
EuroAmphib$Habitat_RockyAreas[EuroAmphib$Habitat_RockyAreas_inland_cliffs_moutain_peaks == 0] <- 0
EuroAmphib$Habitat_Caves <- apply(EuroAmphib[,c(166:167)], 1, sum)
EuroAmphib$Habitat_Caves[EuroAmphib$Habitat_Caves > 0] <- 1
EuroAmphib$Habitat_Deserts <- apply(EuroAmphib[,c(168:170)], 1, sum)
EuroAmphib$Habitat_Deserts[EuroAmphib$Habitat_Deserts > 0] <- 1
EuroAmphib$Habitat_ArtificialTerrestrial <- apply(EuroAmphib[,c(171:176)], 1, sum)
EuroAmphib$Habitat_ArtificialTerrestrial[EuroAmphib$Habitat_ArtificialTerrestrial > 0] <- 1
EuroAmphib$Habitat_Other[EuroAmphib$Habitat_Other_dunes == 1] <- 1
EuroAmphib$Habitat_Other[EuroAmphib$Habitat_Other_dunes == 0] <- 0

EuroAmphib$HabitatBreadth <- apply(EuroAmphib[,c(256,265)], 1, sum)
NAfrogs$HabitatBreadth <- apply(NAfrogs[,c(22:33)], 1, sum)

str(EuroAmphib$HabitatBreadth)
str(NAfrogs$HabitatBreadth)

# Joining in taxon info
NAfrogs <- merge(NAfrogs, Taxonomy, by = "SpeciesName")
EuroAmphib <- merge(EuroAmphib, Taxonomy, by = "SpeciesName")

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


# paired tests: e.g., Wilcoxon rank sum test (wilcox.test)

# need to rearrange data for paired comparison where you 

  