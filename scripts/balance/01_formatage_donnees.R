####################################################################
# Analyse de selectivite des balances à crabe de mangrove
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

### Chargement des données (2 bases à fusionner)
data_crabe_antsatrana <- read_excel("data/Sélectivité_2022_04_23.xlsx", sheet = "Saisie", na = c("NA",""), guess_max = 3000)
data_crabe_baly <- read_excel("data/Sélectivité_Antsatrana_Soalala_2023_01_09.xlsx", sheet = "Saisie", na = c("NA",""), guess_max = 3000)
data_crabe_baly <- data_crabe_baly |> filter(Lieu == "baly")

### Formatage des données
# Renommage des variables/modalites
data_crabe_baly <- data_crabe_baly |> rename("Fiche" = "Fiche N°")
data_crabe_baly <- data_crabe_baly |> rename("Taille" = "Taille (mm)")
data_crabe_baly <- data_crabe_baly |> rename("Site_peche" = "Site pêche")
data_crabe_baly <- data_crabe_baly |> rename("N_calee" = "N° Calée")
data_crabe_baly <- data_crabe_baly |> rename("Calee_debut" = "Calée_Début")
data_crabe_baly <- data_crabe_baly |> rename("Calee_fin" = "Calée_Fin")
data_crabe_baly <- data_crabe_baly |> rename("Levee_debut" = "Levée_Début")
data_crabe_baly <- data_crabe_baly |> rename("Levee_fin" = "Levée_Fin")
data_crabe_baly <- data_crabe_baly |> rename("Balance_N" = "Balance N°")
data_crabe_baly <- data_crabe_baly |> mutate(Poids = NA)
data_crabe_baly$Maillage[which(data_crabe_baly$Maillage == "temoin")] <- "Temoin"
data_crabe_baly$Maillage[which(data_crabe_baly$Maillage == "55")] <- "55_mm"
data_crabe_baly$Maillage[which(data_crabe_baly$Maillage == "65")] <- "65_mm"
data_crabe_baly$Maillage[which(data_crabe_baly$Maillage == "75")] <- "75_mm"
data_crabe_baly$Poids <- as.numeric(data_crabe_baly$Poids)
# Uniformisation du format heure
data_crabe_baly$Levee_fin[695] # Mauvais format
data_crabe_baly$Levee_fin[695:698] <- "13h02"
data_crabe_baly$Calee_debut <- lubridate::hm(data_crabe_baly$Calee_debut)
data_crabe_baly$Calee_fin <- lubridate::hm(data_crabe_baly$Calee_fin)
data_crabe_baly$Levee_debut <- lubridate::hm(data_crabe_baly$Levee_debut)
data_crabe_baly$Levee_fin <- lubridate::hm(data_crabe_baly$Levee_fin)
data_crabe_antsatrana$Calee_debut <- lubridate::hms(format(data_crabe_antsatrana$Calee_debut, "%H:%M:%S"))
data_crabe_antsatrana$Calee_fin <- lubridate::hms(format(data_crabe_antsatrana$Calee_fin, "%H:%M:%S"))
data_crabe_antsatrana$Levee_debut <- lubridate::hms(format(data_crabe_antsatrana$Levee_debut, "%H:%M:%S"))
data_crabe_antsatrana$Levee_fin <- lubridate::hms(format(data_crabe_antsatrana$Levee_fin, "%H:%M:%S"))
# Suppression des colonnes
data_crabe_baly <- data_crabe_baly[,-1]
data_crabe_antsatrana <- data_crabe_antsatrana[, -c(13:20)]
# Verification avant fusion
names(data_crabe_antsatrana)
names(data_crabe_baly)
str(data_crabe_antsatrana)
str(data_crabe_baly)

# Fusion des 2 bases
data_crabe <- rbind(data_crabe_antsatrana, data_crabe_baly)

data_crabe$jour_mois <- format(data_crabe$Date, "%d-%m") # Variable jour-mois
data_crabe$Balance_N <- as.numeric(data_crabe$Balance_N)

# Nommage des variables
names(data_crabe)[13] <- "Temps_calee"
names(data_crabe)[14] <- "Temps_levee"
names(data_crabe)[15] <- "Fin_levee_debut_calee"
names(data_crabe)[16] <- "Debut_levee_fin_calee"
# Re-calcul des temps
data_crabe$Temps_calee <- as.numeric(data_crabe$Calee_fin, "minutes") - as.numeric(data_crabe$Calee_debut, "minutes")
data_crabe$Temps_levee <- as.numeric(data_crabe$Levee_fin, "minutes") - as.numeric(data_crabe$Levee_debut, "minutes")
data_crabe$Fin_levee_debut_calee <- as.numeric(data_crabe$Levee_fin, "minutes") - as.numeric(data_crabe$Calee_debut, "minutes")
data_crabe$Debut_levee_fin_calee <- as.numeric(data_crabe$Levee_debut, "minutes") - as.numeric(data_crabe$Calee_fin, "minutes")

data_crabe$Temps_peche <- as.numeric(data_crabe$Fin_levee_debut_calee - data_crabe$Debut_levee_fin_calee)
data_crabe$Temps_peche1 <- as.numeric(data_crabe$Levee_debut - data_crabe$Calee_debut)/60
data_crabe$Temps_peche2 <- as.numeric(data_crabe$Levee_fin - data_crabe$Calee_fin)/60
data_crabe$Temps_peche3 <- (data_crabe$Temps_peche1 + data_crabe$Temps_peche2)/2

data_crabe$Calee_debut <- format(data_crabe$Calee_debut, format = "%H:%M:%S")
data_crabe$Calee_fin <- format(data_crabe$Calee_fin, format = "%H:%M:%S")
data_crabe$Levee_debut <- format(data_crabe$Levee_debut, format = "%H:%M:%S")
data_crabe$Levee_fin <- format(data_crabe$Levee_fin, format = "%H:%M:%S")

# Verification
names(data_crabe)
str(data_crabe)

table(data_crabe$Balance_N, useNA = "ifany") # Certains N° balances NA
table(data_crabe$Fiche, useNA = "ifany") # Aucunes fiches NA
table(data_crabe$N_calee, useNA = "ifany") # Certains N° calee NA : a supprimer car impossible de reconstituer

# Identification des calees
data_crabe <- data_crabe %>% mutate(ID_calee = paste(Date, Nom, N_calee, sep = "_"))

# Separation des 2 bases uniformisees
data_crabe_antsatrana <- data_crabe |> filter(Lieu == "Antsatrana")
data_crabe_baly <- data_crabe |> filter(Lieu == "baly")

# Verification du nombre de calees par rapport au rapport
length(unique(data_crabe_antsatrana$ID_calee)) # OK : même nombre de calées que les rapports
data_crabe_antsatrana |> group_by(jour_mois) |> summarise(nb_calee_jour = n_distinct(ID_calee)) # OK

# Verification du nombre d'equipes par rapport au rapport
data_crabe_antsatrana |> group_by(jour_mois) |> summarise(nb_equipe_jour = n_distinct(Nom)) # OK

# Verification du nombre de crabes par rapport au rapport
sum(!is.na(data_crabe_antsatrana$Taille)) # OK
sum(!is.na(data_crabe_antsatrana$Sexe)) # OK
nrow(data_crabe_antsatrana |> filter(Taille < 110)) # OK (mais erreur dans la note technique p.10)

### Creation des tableaux de donnees pour analyse
# Selection des donnees pertinentes et fiables
selectivite_crabe <- dplyr::select(data_crabe, -c(Fiche, Calee_debut, Calee_fin, Levee_debut, Levee_fin, 
                                                  Balance_N, Prof_debut, Prof_milieu, Prof_fin))
selectivite_crabe <- selectivite_crabe |> filter(!(Nom == "Nicolas" & as.Date(Date) == as.Date("2022-03-16 UTC"))) # Suppression des calee NA
# selectivite_crabe <- selectivite_crabe |> filter(Taille <= 130) # Eventuel filtre sur la taille
sum(!is.na(selectivite_crabe$Taille)) # Nombre de crabes apres selection

# Calcul du nombre de crabes par maillage
nb_crabes_maillage <- selectivite_crabe |> filter(!is.na(Taille)) |> group_by(as.factor(Maillage)) |> summarise(n = n()) # OK
nb_crabes_maillage

# Calcul du nombre de maillages par calee : effort de pêche
liste_maillage_calee <- selectivite_crabe |> distinct(ID_calee, Maillage)
liste_calee_zone <- selectivite_crabe |> distinct(ID_calee, Lieu)
n_calees <- liste_maillage_calee |> count(Maillage)
n_calees
n_maillage_calee <- liste_maillage_calee |> group_by(ID_calee) |> summarise(n_maillage_calee = n())
n_maillage_calee

# Creation des classes de taille
intervalle <- 10
selectivite_crabe <- selectivite_crabe |> mutate(classe_taille = cut_width(Taille, width = intervalle, boundary = 0))
selectivite_crabe <- selectivite_crabe |> mutate(classe_taille_mid = ceiling(Taille/intervalle) * intervalle - intervalle/2)
# Vecteur des classes de taille
classe_taille_mid <- seq(min(selectivite_crabe$classe_taille_mid, na.rm = T), max(selectivite_crabe$classe_taille_mid, na.rm = T), by = intervalle)

# Calcul du nombre de crabes par calee, classe de taille, sexe et maillage
captures <- selectivite_crabe |> filter(!is.na(classe_taille)) |> count(ID_calee, classe_taille_mid, Maillage, Sexe) # Seulement les captures (sans NA)
captures_long_format <- liste_maillage_calee |> tidyr::crossing(classe_taille_mid = classe_taille_mid, Sexe = c("m", "f")) |>
  left_join(captures, by = c("ID_calee", "Maillage", "classe_taille_mid", "Sexe")) |> mutate(n = coalesce(n, 0L)) |> # Avec les balances sans capture (avec 0)
  left_join(liste_calee_zone, by = "ID_calee")

# Matrice des captures completes par classe de taille × maillage x sexe
captures_matrix <- captures_long_format |> group_by(classe_taille_mid, Maillage) |>
                                           summarise(n = sum(n), .groups = "drop") |>
                                           tidyr::pivot_wider(names_from = Maillage, values_from = n, values_fill = 0) |>
                                           arrange(classe_taille_mid) |>
                                           dplyr::select(classe_taille_mid, Temoin, `55_mm`, `65_mm`, `75_mm`) |>
                                           tibble::column_to_rownames("classe_taille_mid") |>
                                           as.matrix()

captures_matrix_m <- captures_long_format |> filter(Sexe == "m") |> group_by(classe_taille_mid, Maillage) |>
                                             summarise(n = sum(n), .groups = "drop") |>
                                             tidyr::pivot_wider(names_from = Maillage, values_from = n, values_fill = 0) |>
                                             arrange(classe_taille_mid) |>
                                             dplyr::select(classe_taille_mid, Temoin, `55_mm`, `65_mm`, `75_mm`) |>
                                             tibble::column_to_rownames("classe_taille_mid") |>
                                             as.matrix()

captures_matrix_f <- captures_long_format |> filter(Sexe == "f") |> group_by(classe_taille_mid, Maillage) |>
                                             summarise(n = sum(n), .groups = "drop") |>
                                             tidyr::pivot_wider(names_from = Maillage, values_from = n, values_fill = 0) |>
                                             arrange(classe_taille_mid) |>
                                             dplyr::select(classe_taille_mid, Temoin, `55_mm`, `65_mm`, `75_mm`) |>
                                             tibble::column_to_rownames("classe_taille_mid") |>
                                             as.matrix()

# Verification de la somme totale d'individus
sum(captures_matrix[,1:4]) 
sum(captures_matrix_m[,1:4])
sum(captures_matrix_f[,1:4])
rowSums(captures_matrix)

# Matrice des captures Temoin/Experimentale par classe de taille x sexe x zone (fonction)
captures_matrice_temoin_exp <- function(maille_exp, sexe = NULL, lieu = NULL, maille_temoin = "Temoin") 
{
# Identification des calees ayant utilisé la maille experimentale
calees <- captures_long_format |> filter(Maillage == maille_exp) |> distinct(ID_calee) |> pull(ID_calee)
# Construction du tableau des captures avec temoin et maille experimentale
captures <- captures_long_format |> filter(ID_calee %in% calees, Maillage %in% c(maille_exp, maille_temoin))
# Selection par sexe/zone si besoin
if (!is.null(sexe)) {captures <- captures |> filter(Sexe == sexe)}
if (!is.null(lieu)) {captures <- captures |> filter(Lieu == lieu)}
captures |> group_by(classe_taille_mid, Maillage) |> summarise(n = sum(n), .groups = "drop") |> 
                    tidyr::pivot_wider(names_from = Maillage, values_from = n, values_fill = 0) |>
                    arrange(classe_taille_mid) |>
                    dplyr::select(classe_taille_mid, experimental = all_of(maille_exp), Temoin = all_of(maille_temoin)) |>
                    as.matrix()
}

# Maille 55 mm
captures_matrix_55     <- captures_matrice_temoin_exp("55_mm")
captures_matrix_55_m   <- captures_matrice_temoin_exp("55_mm", sexe = "m")
captures_matrix_55_f   <- captures_matrice_temoin_exp("55_mm", sexe = "f")

captures_matrix_55_A   <- captures_matrice_temoin_exp("55_mm", lieu = "Antsatrana")
captures_matrix_55_m_A <- captures_matrice_temoin_exp("55_mm", sexe = "m", lieu = "Antsatrana")
captures_matrix_55_f_A <- captures_matrice_temoin_exp("55_mm", sexe = "f", lieu = "Antsatrana")

captures_matrix_55_B   <- captures_matrice_temoin_exp("55_mm", lieu = "baly")
captures_matrix_55_m_B <- captures_matrice_temoin_exp("55_mm", sexe = "m", lieu = "baly")
captures_matrix_55_f_B <- captures_matrice_temoin_exp("55_mm", sexe = "f", lieu = "baly")

# Maille 65 mm
captures_matrix_65     <- captures_matrice_temoin_exp("65_mm")
captures_matrix_65_m   <- captures_matrice_temoin_exp("65_mm", sexe = "m")
captures_matrix_65_f   <- captures_matrice_temoin_exp("65_mm", sexe = "f")

captures_matrix_65_A   <- captures_matrice_temoin_exp("65_mm", lieu = "Antsatrana")
captures_matrix_65_m_A <- captures_matrice_temoin_exp("65_mm", sexe = "m", lieu = "Antsatrana")
captures_matrix_65_f_A <- captures_matrice_temoin_exp("65_mm", sexe = "f", lieu = "Antsatrana")

captures_matrix_65_B   <- captures_matrice_temoin_exp("65_mm", lieu = "baly")
captures_matrix_65_m_B <- captures_matrice_temoin_exp("65_mm", sexe = "m", lieu = "baly")
captures_matrix_65_f_B <- captures_matrice_temoin_exp("65_mm", sexe = "f", lieu = "baly")

# Maille 75 mm
captures_matrix_75     <- captures_matrice_temoin_exp("75_mm")
captures_matrix_75_m   <- captures_matrice_temoin_exp("75_mm", sexe = "m")
captures_matrix_75_f   <- captures_matrice_temoin_exp("75_mm", sexe = "f")

captures_matrix_75_A   <- captures_matrice_temoin_exp("75_mm", lieu = "Antsatrana")
captures_matrix_75_m_A <- captures_matrice_temoin_exp("75_mm", sexe = "m", lieu = "Antsatrana")
captures_matrix_75_f_A <- captures_matrice_temoin_exp("75_mm", sexe = "f", lieu = "Antsatrana")

captures_matrix_75_B   <- captures_matrice_temoin_exp("75_mm", lieu = "baly")
captures_matrix_75_m_B <- captures_matrice_temoin_exp("75_mm", sexe = "m", lieu = "baly")
captures_matrix_75_f_B <- captures_matrice_temoin_exp("75_mm", sexe = "f", lieu = "baly")

# Verification
sum(!is.na(selectivite_crabe$classe_taille))   
sum(captures_matrix[, 1:4])     

sum(captures_matrix_55[,2],captures_matrix_65[,2],captures_matrix_75[,2])  
sum(!is.na(selectivite_crabe |> filter(Maillage != "Temoin") |> pull(classe_taille))) 

sum(captures_matrix_55_m[,2],captures_matrix_65_m[,2],captures_matrix_75_m[,2])
sum(!is.na(selectivite_crabe |> filter(Maillage != "Temoin" & Sexe == "m") |> pull(classe_taille))) 

sum(captures_matrix_55_f[,2],captures_matrix_65_f[,2],captures_matrix_75_f[,2])
sum(!is.na(selectivite_crabe |> filter(Maillage != "Temoin" & Sexe == "f") |> pull(classe_taille)))

sum(captures_matrix_55_A[,2],captures_matrix_65_A[,2],captures_matrix_75_A[,2])
sum(!is.na(selectivite_crabe |> filter(Maillage != "Temoin" & Lieu == "Antsatrana") |> pull(classe_taille)))

sum(captures_matrix_55_B[,2],captures_matrix_65_B[,2],captures_matrix_75_B[,2])
sum(!is.na(selectivite_crabe |> filter(Maillage != "Temoin" & Lieu == "baly") |> pull(classe_taille)))

sum(captures_matrix_55_m_B[,2],captures_matrix_65_m_B[,2],captures_matrix_75_m_B[,2])
sum(!is.na(selectivite_crabe |> filter(Maillage != "Temoin" & Lieu == "baly" & Sexe == "m") |> pull(classe_taille)))

### Quelques representations graphiques
hist(selectivite_crabe$Taille, breaks = 20)

ggplot() + geom_bar(data = selectivite_crabe |> filter(!is.na(Taille)), aes(x = classe_taille_mid, fill = Sexe), position = "dodge")
ggplot() + geom_bar(data = selectivite_crabe |> filter(!is.na(Taille)), aes(x = classe_taille_mid, fill = Lieu), position = "dodge")

ggplot() + geom_histogram(data = selectivite_crabe |> filter(!is.na(Taille)), aes(x = Taille, fill = Maillage), binwidth = 5, position = "dodge")
ggplot() + geom_histogram(data = selectivite_crabe |> filter(!is.na(Taille)), aes(x = Taille), binwidth = 5) + facet_wrap(~ Maillage)
# ggplot() + geom_bar(data = selectivite_crabe |> filter(Maillage == "Temoin"), aes(x = classe_taille_mid))
# ggplot() + geom_bar(data = selectivite_crabe |> filter(Maillage == "55_mm"), aes(x = classe_taille_mid))
# ggplot() + geom_bar(data = selectivite_crabe |> filter(Maillage == "65_mm"), aes(x = classe_taille_mid))
# ggplot() + geom_bar(data = selectivite_crabe |> filter(Maillage == "75_mm"), aes(x = classe_taille_mid))
