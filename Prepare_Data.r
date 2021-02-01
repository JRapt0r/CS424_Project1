# Jonathon Repta -- CS 424

# Load libraries
library(ggplot2)

# Read in data
energy <- read.table(file = "https://www.evl.uic.edu/aej/424/annual_generation_state.csv", sep = ",", header = TRUE)

# convert year to "numeric"
energy$properYear <- as.numeric(energy$YEAR)

# convert "Megawatthours" to numeric
energy$megawatts <- gsub(pattern=",","", ignore.case = TRUE, energy$GENERATION..Megawatthours)

# Filter negative megawatthour values
energy$megawatts <- subset(energy$megawatts, energy$megawatts >= 0)

# Unify state totals by giving them the same case
energy$STATE<-toupper(energy$STATE)

# Filter missing identifiers
energy$STATE <- subset(energy$STATE, energy$STATE != "  ")

# Convert STATE, TYPE OF PRODUCER, and ENERGY SOURCE to categorical values
energy$STATE <- as.factor(energy$STATE)
energy$TYPE.OF.PRODUCER <- as.factor(energy$TYPE.OF.PRODUCER)
energy$ENERGY.SOURCE <- as.factor(energy$ENERGY.SOURCE)

energy$megawatts <- subset(energy$megawatts, energy$megawatts >= 0)

# Remove irrelevant energy sources
energy$ENERGY.SOURCE <- subset(energy$ENERGY.SOURCE, energy$ENERGY.SOURCE != "Other")
energy$ENERGY.SOURCE <- subset(energy$ENERGY.SOURCE, energy$ENERGY.SOURCE != "Other Gases")
energy$ENERGY.SOURCE <- subset(energy$ENERGY.SOURCE, energy$ENERGY.SOURCE != "Other Biomass")
energy$ENERGY.SOURCE <- subset(energy$ENERGY.SOURCE, energy$ENERGY.SOURCE != "Pumped Storage")