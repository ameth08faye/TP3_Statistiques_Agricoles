# Title: TP3 - ISEP2 - Statistiques Agricoles
# Files: "Base_TP2"
# Last update: April 08 2024
# Author : ONANENA AMANA Jeanne De La Fleche and Ameth FAYE 

# Define folder structure
root <- "/Users/HP/Desktop/ISEP2CoursR2024/TP_Stat_Agri/TP3 Stat_Agri"
Datawork <- file.path(root, "Rendu TP3 Stat_Agri")
datapath <- file.path(Datawork, "Base")
scripts <- file.path(Datawork, "R scripts")
outputs <- file.path(Datawork, "Outputs")

# Define input file path
inputfile <- file.path(datapath, "data_base.csv")


# Import required libraries
library(haven)
library(tidyverse)
library(base)
library(dplyr)
library(ggplot2)


# Import master dataset
data_production <- read.csv(inputfile)

# Run the task-specific master do-files
if (TRUE) {
  source(file.path(scripts, "2.1_Préparation et Nettoyage des données.R"))
}
if (TRUE) {
  source(file.path(scripts, "2.2 Analyse de la productivité.R"))
}
if (TRUE) {
  source(file.path(scripts, "2.3 Analyse des intrants.R"))
}
if (TRUE) {
  source(file.path(scripts, "2.4 Analyse des intrants (bis)"))
}
