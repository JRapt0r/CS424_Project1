# Jonathon Repta -- CS 424

# Load libraries
library(ggplot2)

# Read in data
energy <- read.table(file = "https://www.evl.uic.edu/aej/424/annual_generation_state.csv", sep = ",", header = TRUE)

# convert year to "numeric"
energy$YEAR <- as.numeric(energy$YEAR)

# convert "Megawatthours" to numeric
energy$GENERATION..Megawatthours. <- as.numeric(gsub(pattern=",","", ignore.case = TRUE, energy$GENERATION..Megawatthours))

# Filter negative megawatthour values
energy <- subset(energy, energy$GENERATION..Megawatthours >= 0)

# Unify state totals by giving them the same case
energy$STATE<-toupper(energy$STATE)

# Filter missing identifiers
energy <- subset(energy, energy$STATE != "  ")

# Convert STATE, TYPE OF PRODUCER, and ENERGY SOURCE to categorical values
energy$STATE <- as.factor(energy$STATE)
energy$TYPE.OF.PRODUCER <- as.factor(energy$TYPE.OF.PRODUCER)
energy$ENERGY.SOURCE <- as.factor(energy$ENERGY.SOURCE)

# Remove irrelevant energy sources
energy <- subset(energy, energy$ENERGY.SOURCE != "Other")
energy <- subset(energy, energy$ENERGY.SOURCE != "Other Gases")
energy <- subset(energy, energy$ENERGY.SOURCE != "Other Biomass")
energy <- subset(energy, energy$ENERGY.SOURCE != "Pumped Storage")

# Give columns legible names
names(energy) <- c("year", "state", "producerType", "energySource", "megaWattHours")