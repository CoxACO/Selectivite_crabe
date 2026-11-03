####################################################################
# Analyse de selectivite des trekos à crabe de mangrove
# corentin.vignard@ird.fr
# 01_formatage_donnees.R
####################################################################

### Chargement des packages
# install.packages('readxl','lubridate','tidyverse','ggplot2','hms','TropFishR')
library(readxl)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(hms)
library(TropFishR)

### Chargement des données
data_crabe <- read.csv("data/Corepeche_crabe_avec_taille_Mahajanga.csv")
str(data_crabe)

### Formatage des données
data_crabe$taille_de_crabe <- as.numeric(data_crabe$taille_de_crabe)
data_crabe$date_premiere_sortie_de_peche <- format(data_crabe$Date, "%Y%d") # Variable jour-mois
