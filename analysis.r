# ENEC 395 Undergraduate Research 
# Alicia Wood
# 3/20/17
# Title

### Loading in necessary libraries

#library(dplyr)
#library(tools)

### Setting my working directory to the folder where all of my data and code are stored

setwd('~/Desktop/Extinction-Risk')

### Reading in raw data files

frogs = read.table("NorthAmericanFrogs.txt", header=T, sep = '\t', quote = "\"")

# example data visualizations
boxplot(frogs$MeanClutchSize)
boxplot(frogs$MeanClutchSize ~ frogs$IUCN_Threat_Status)

# paired tests: e.g., Wilcoxon rank sum test (wilcox.test)

# need to rearrange data for paired comparison where you 

  