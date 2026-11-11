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
data_crabe <- read.csv("data/Corepeche_crabe_avec_taille_Mahajanga_avec_nom_pecheur.csv")
names(data_crabe)
str(data_crabe)

### Formatage des données
data_crabe$taille_de_crabe <- as.numeric(data_crabe$taille_de_crabe) # Mise au format numerique
# Selection des donnees pertinentes
selectivite_crabe <- data_crabe |> dplyr::filter(village == "Ampitsopitsoka") |>  # Zone d'etude
                                   # dplyr::filter(!taille_de_crabe %in% c(46,47,49) & taille_de_crabe <= 100) |> # Filtre sur la taille du crabe : choix methodologique fort !
                                   # dplyr::filter(taille_de_crabe <= 109) |> # Filtre sur la taille du crabe : choix methodologique fort !
                                   dplyr::select(-c(id_enquete, date_expedition, nombre_participant, poids_de_crabes_consommes, nombre_de_crabes_consommes, 
                                                                poids_destination_collecte_1, prix_destination_collecte_1, poids_destination_collecte_2, prix_destination_collecte_2,
                                                                poids_destination_marche_local, prix_destination_marche_local, nombre_de_pirogue_sortie_de_peche_1, nombre_de_pirogue_sortie_de_peche_2,
                                                                nombre_de_pirogue_sortie_de_peche_3, nombre_de_pirogue_sortie_de_peche_4, echantillon_trie_ou_pas, les_tailles_absentes,
                                                                autres_tailles_absentes, poids_total_de_l_echantillon, destination_de_crabe)) # Suppression des colonnes superflues
## Verification de la base
table(selectivite_crabe$premier_engin_sortie_de_peche_1)
table(selectivite_crabe$premier_engin_sortie_de_peche_2)
table(selectivite_crabe$premier_engin_sortie_de_peche_3) # Certains engins avec balance75 : a corriger
selectivite_crabe$premier_engin_sortie_de_peche_3[which(selectivite_crabe$premier_engin_sortie_de_peche_3 == "Balance75")] <- "Treko71"
table(selectivite_crabe$premier_engin_sortie_de_peche_4)

# Lien entre engins de peche et sorties de peche
table(selectivite_crabe$premier_engin_sortie_de_peche_1, selectivite_crabe$premier_engin_sortie_de_peche_2) 
table(selectivite_crabe$premier_engin_sortie_de_peche_1, selectivite_crabe$premier_engin_sortie_de_peche_3) 
table(selectivite_crabe$premier_engin_sortie_de_peche_1, selectivite_crabe$premier_engin_sortie_de_peche_4)
table(selectivite_crabe$premier_engin_sortie_de_peche_2, selectivite_crabe$premier_engin_sortie_de_peche_3)
table(selectivite_crabe$premier_engin_sortie_de_peche_2, selectivite_crabe$premier_engin_sortie_de_peche_4)
table(selectivite_crabe$premier_engin_sortie_de_peche_3, selectivite_crabe$premier_engin_sortie_de_peche_4)

# Verification sur les dates
table(selectivite_crabe$date_premiere_sortie_de_peche, selectivite_crabe$date_deuxieme_sortie_de_peche)
table(selectivite_crabe$date_premiere_sortie_de_peche, selectivite_crabe$date_troisieme_sortie_de_peche)
table(selectivite_crabe$date_premiere_sortie_de_peche, selectivite_crabe$date_quatrieme_sortie_de_peche)
table(selectivite_crabe$date_deuxieme_sortie_de_peche, selectivite_crabe$date_troisieme_sortie_de_peche)
table(selectivite_crabe$date_deuxieme_sortie_de_peche, selectivite_crabe$date_quatrieme_sortie_de_peche)
table(selectivite_crabe$date_troisieme_sortie_de_peche, selectivite_crabe$date_quatrieme_sortie_de_peche)

# Verification sur le nombre d'engins de peche
table(selectivite_crabe$nombre_premier_engin_sortie_de_peche_1) # Erreur de saisie : 50 pieges par sortie
selectivite_crabe$nombre_premier_engin_sortie_de_peche_1[which(selectivite_crabe$nombre_premier_engin_sortie_de_peche_1 == 40)] <- 50
selectivite_crabe$nombre_premier_engin_sortie_de_peche_1[which(selectivite_crabe$nombre_premier_engin_sortie_de_peche_1 == 1)] <- 50

# Verification sur le nom des pecheurs
table(selectivite_crabe$nom_pecheur)
selectivite_crabe$nom_pecheur[which(selectivite_crabe$nom_pecheur == "Tella")] <- "tella" # Harmonisation des noms de pecheurs
selectivite_crabe$nom_pecheur[which(selectivite_crabe$nom_pecheur == "Olga")] <- "olga" # Harmonisation des noms de pecheurs

# Lien entre pecheurs et engins de peche
table(selectivite_crabe$nom_pecheur, selectivite_crabe$premier_engin_sortie_de_peche_1) # Deux pecheurs avec 2 types de treko : erreur de saisie
selectivite_crabe$premier_engin_sortie_de_peche_1[which(selectivite_crabe$nom_pecheur == "faeza" & selectivite_crabe$premier_engin_sortie_de_peche_1 == "Treko71")] <- "Treko"
selectivite_crabe$premier_engin_sortie_de_peche_1[which(selectivite_crabe$nom_pecheur == "hafalia" & selectivite_crabe$premier_engin_sortie_de_peche_1 == "Treko")] <- "Treko71"
table(selectivite_crabe$nom_pecheur, selectivite_crabe$premier_engin_sortie_de_peche_1) # OK

# Nombre de crabes totaux mesures
sum(!is.na(selectivite_crabe$taille_de_crabe))

### Creation des tableaux de donnees pour analyse

# Correspondance pecheur/engin
engin <- selectivite_crabe |> distinct(nom_pecheur, Maillage = premier_engin_sortie_de_peche_1)

# Identification des sorties
selectivite_crabe <- selectivite_crabe %>% mutate(ID_calee = paste(date_premiere_sortie_de_peche, nombre_sortie_de_peche_1, sep = "_"))
unique(selectivite_crabe$ID_calee)
nb_pecheurs_avec_donnees <- selectivite_crabe |> group_by(date_premiere_sortie_de_peche, nombre_sortie_de_peche_1, premier_engin_sortie_de_peche_1) |> summarise(nb_pecheurs_avec_donnees = n_distinct(nom_pecheur), .groups = "drop") 
nb_pecheurs_avec_donnees # 5*2 pecheurs echantillonnes a chaque sortie, parfois aucune capture = aucune ligne 

# Creation des classes de taille
intervalle <- 10
selectivite_crabe <- selectivite_crabe |> mutate(classe_taille = cut_width(taille_de_crabe, width = intervalle, boundary = 0),
                                                 classe_taille_mid = floor(taille_de_crabe / intervalle) * intervalle + intervalle / 2)
# Vecteur des classes de taille
classe_taille_mid <- seq(min(selectivite_crabe$classe_taille_mid, na.rm = T), max(selectivite_crabe$classe_taille_mid, na.rm = T), by = intervalle)

# Calcul du nombre de crabes par classe de taille, sexe et maillage
captures <- selectivite_crabe |> filter(!is.na(classe_taille)) |> count(classe_taille_mid, premier_engin_sortie_de_peche_1, sexe_du_crabe)
captures_long_format <- selectivite_crabe |> filter(!is.na(classe_taille)) |> count(nom_pecheur, classe_taille_mid, sexe_du_crabe) |> 
                                             tidyr::complete(nom_pecheur, classe_taille_mid, sexe_du_crabe, fill = list(n = 0)) |> left_join(engin, by = "nom_pecheur")

# Matrice des captures completes par classe de taille × maillage x sexe
captures_matrix <- captures_long_format |> group_by(classe_taille_mid, Maillage) |>
                                           summarise(n = sum(n), .groups = "drop") |>
                                           tidyr::pivot_wider(names_from = Maillage, values_from = n, values_fill = 0) |>
                                           arrange(classe_taille_mid) |>
                                           dplyr::select(classe_taille_mid, `Treko`, `Treko71`) |>
                                           tibble::column_to_rownames("classe_taille_mid") |>
                                           as.matrix()

captures_matrix_m <- captures_long_format |> filter(sexe_du_crabe == "Male") |> group_by(classe_taille_mid, Maillage) |>
                                             summarise(n = sum(n), .groups = "drop") |>
                                             tidyr::pivot_wider(names_from = Maillage, values_from = n, values_fill = 0) |>
                                             arrange(classe_taille_mid) |>
                                             dplyr::select(classe_taille_mid, `Treko`, `Treko71`) |>
                                             tibble::column_to_rownames("classe_taille_mid") |>
                                             as.matrix()

captures_matrix_f <- captures_long_format |> filter(sexe_du_crabe == "Femelle") |> group_by(classe_taille_mid, Maillage) |>
                                             summarise(n = sum(n), .groups = "drop") |>
                                             tidyr::pivot_wider(names_from = Maillage, values_from = n, values_fill = 0) |>
                                             arrange(classe_taille_mid) |>
                                             dplyr::select(classe_taille_mid, `Treko`, `Treko71`) |>
                                             tibble::column_to_rownames("classe_taille_mid") |>
                                             as.matrix()

# Verification de la somme totale d'individus
sum(captures_matrix[,1:2])
sum(captures_matrix[,1]) 
sum(captures_matrix[,2]) 
sum(captures_matrix_m[,1])
sum(captures_matrix_m[,2])
sum(captures_matrix_f[,1])
sum(captures_matrix_f[,2])
sum(captures_matrix_m[,1:2])
sum(captures_matrix_f[,1:2])
sum(captures_matrix_m[,1:2],captures_matrix_f[,1:2])
