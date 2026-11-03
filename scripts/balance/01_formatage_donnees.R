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

### Chargement des données
data_crabe <- read_excel("data/Sélectivité_2022_04_23.xlsx", sheet = "Saisie", na = c("NA",""), guess_max = 3000)
str(data_crabe)

### Formatage des données
data_crabe$jour_mois <- format(data_crabe$Date, "%d-%m") # Variable jour-mois
data_crabe$Balance_N <- as.numeric(data_crabe$Balance_N)

# Nommage des variables
names(data_crabe)[13] <- "Temps_calee"
names(data_crabe)[14] <- "Temps_levee"
names(data_crabe)[15] <- "Fin_levee_debut_calee"
names(data_crabe)[16] <- "Debut_levee_fin_calee"
# Re-calcul des temps
data_crabe$Temps_calee <- difftime(data_crabe$Calee_fin, data_crabe$Calee_debut, units = "mins") |> as.numeric()
data_crabe$Temps_levee <- difftime(data_crabe$Levee_fin, data_crabe$Levee_debut, units = "mins") |> as.numeric()
data_crabe$Fin_levee_debut_calee <- difftime(data_crabe$Levee_fin, data_crabe$Calee_debut, units = "mins") |> as.numeric()
data_crabe$Debut_levee_fin_calee <- difftime(data_crabe$Levee_debut, data_crabe$Calee_fin, units = "mins") |> as.numeric()
data_crabe$Temps_peche <- as.numeric(data_crabe$Fin_levee_debut_calee - data_crabe$Debut_levee_fin_calee)
data_crabe$Temps_peche1 <- as.numeric(data_crabe$Levee_debut - data_crabe$Calee_debut)
data_crabe$Temps_peche2 <- as.numeric(data_crabe$Levee_fin - data_crabe$Calee_fin)
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
table(data_crabe$N_calee, useNA = "ifany") # Certains N° calee NA : a voir ?

# Identifiant de calee
data_crabe <- data_crabe %>% mutate(ID_calee = paste(Date, Nom, N_calee, sep = "_"))

# Verification du nombre de calees
length(unique(data_crabe$ID_calee)) # OK : même nombre de calées que les rapports
data_crabe |> group_by(jour_mois) |> summarise(nb_calee_jour = n_distinct(ID_calee)) # OK

# Verification du nombre d'equipes
data_crabe |> group_by(jour_mois) |> summarise(nb_equipe_jour = n_distinct(Nom)) # OK

# Verification du nombre de crabes
sum(!is.na(data_crabe$Taille)) # OK
sum(!is.na(data_crabe$Sexe)) # OK
nrow(data_crabe |> filter(Taille < 110)) # OK (mais erreur dans la note technique p.10)

### Creation des tableaux de donnees pour analyse
# Selection des donnees pertinentes et fiables
selectivite_crabe <- dplyr::select(data_crabe, -c(Fiche, Calee_debut, Calee_fin, Levee_debut, Levee_fin, 
                                                  Debut_Lat, Debut_Long, Fin_Lat, Fin_Long,
                                                  Balance_N, Prof_debut, Prof_milieu, Prof_fin))
selectivite_crabe <- selectivite_crabe |> filter(!(Nom == "Nicolas" & as.Date(Date) == as.Date("2022-03-16 UTC"))) # Suppression des calee NA
# selectivite_crabe <- selectivite_crabe |> filter(Taille <= 130) # Eventuel filtre sur la taille
sum(!is.na(selectivite_crabe$Taille)) # Nombre de crabes apres selection

# Calcul du nombre de crabes par maillage
nb_crabes_maillage <- selectivite_crabe |> filter(!is.na(Taille)) |> group_by(as.factor(Maillage)) |> summarise(n = n()) # OK
nb_crabes_maillage

# Calcul du nombre de maillages par calee : effort de pêche
liste_maillage_calee <- selectivite_crabe |> distinct(ID_calee, Maillage)
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
  left_join(captures, by = c("ID_calee", "Maillage", "classe_taille_mid", "Sexe")) |> mutate(n = coalesce(n, 0L)) # Avec les balances sans capture (avec 0)

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

# Matrice des captures Temoin/Experimentale par classe de taille x sexe (fonction)
captures_matrice_temoin_exp <- function(maille_exp, sexe = NULL, maille_temoin = "Temoin") 
{
# Identification des calees ayant utilisé la maille experimentale
calees <- captures_long_format |> filter(Maillage == maille_exp) |> distinct(ID_calee) |> pull(ID_calee)
# Construction du tableau des captures avec temoin et maille experimentale
captures <- captures_long_format |> filter(ID_calee %in% calees, Maillage %in% c(maille_exp, maille_temoin))
# Selection par sexe si necessaire
if (!is.null(sexe)) {captures <- captures |> filter(Sexe == sexe)}
captures |> group_by(classe_taille_mid, Maillage) |> summarise(n = sum(n), .groups = "drop") |> 
                    tidyr::pivot_wider(names_from = Maillage, values_from = n, values_fill = 0) |>
                    arrange(classe_taille_mid) |>
                    dplyr::select(classe_taille_mid, all_of(c(maille_exp, maille_temoin))) |>
                    rename("experimental" = all_of(maille_exp)) |> 
                    as.matrix()
}

captures_matrix_55 <- captures_matrice_temoin_exp("55_mm")
captures_matrix_55_m <- captures_matrice_temoin_exp("55_mm", sexe = "m")
captures_matrix_55_f <- captures_matrice_temoin_exp("55_mm", sexe = "f")

captures_matrix_65 <- captures_matrice_temoin_exp("65_mm")
captures_matrix_65_m <- captures_matrice_temoin_exp("65_mm", sexe = "m")
captures_matrix_65_f <- captures_matrice_temoin_exp("65_mm", sexe = "f")

captures_matrix_75 <- captures_matrice_temoin_exp("75_mm")
captures_matrix_75_m <- captures_matrice_temoin_exp("75_mm", sexe = "m")
captures_matrix_75_f <- captures_matrice_temoin_exp("75_mm", sexe = "f")

# Verification
sum(captures_matrix[,1]) 
sum(captures_matrix_55[,2])

sum(captures_matrix_m[,1])
sum(captures_matrix_55_m[,2])

### Quelques representations graphiques
hist(selectivite_crabe$Taille, breaks = 20)

ggplot() + geom_bar(data = selectivite_crabe |> filter(!is.na(Taille)), aes(x = classe_taille_mid, fill = Sexe), position = "dodge")
ggplot() + geom_histogram(data = selectivite_crabe |> filter(!is.na(Taille)), aes(x = Taille, fill = Maillage), binwidth = 5, position = "dodge")
ggplot() + geom_histogram(data = selectivite_crabe |> filter(!is.na(Taille)), aes(x = Taille), binwidth = 5) + facet_wrap(~ Maillage)
# ggplot() + geom_bar(data = selectivite_crabe |> filter(Maillage == "Temoin"), aes(x = classe_taille_mid))
# ggplot() + geom_bar(data = selectivite_crabe |> filter(Maillage == "55_mm"), aes(x = classe_taille_mid))
# ggplot() + geom_bar(data = selectivite_crabe |> filter(Maillage == "65_mm"), aes(x = classe_taille_mid))
# ggplot() + geom_bar(data = selectivite_crabe |> filter(Maillage == "75_mm"), aes(x = classe_taille_mid))
