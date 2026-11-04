####################################################################
# Analyse descriptive des trekos à crabe de mangrove
# corentin.vignard@ird.fr
# 02_analyse_descriptive_donnees.R
####################################################################

### Execution du script precedent
source("scripts/treko/01_formatage_donnees.R")

### Statistiques descriptives simples par engin et par sexe
# Taille minimale
min(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko" & sexe_du_crabe == "Male") |> pull(taille_de_crabe))
min(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko" & sexe_du_crabe == "Femelle") |> pull(taille_de_crabe))
min(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko71" & sexe_du_crabe == "Male") |> pull(taille_de_crabe))
min(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko71" & sexe_du_crabe == "Femelle") |> pull(taille_de_crabe))
# Taille maximale
max(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko" & sexe_du_crabe == "Male") |> pull(taille_de_crabe))
max(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko" & sexe_du_crabe == "Femelle") |> pull(taille_de_crabe))
max(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko71" & sexe_du_crabe == "Male") |> pull(taille_de_crabe))
max(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko71" & sexe_du_crabe == "Femelle") |> pull(taille_de_crabe))
# Taille moyenne
mean(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko" & sexe_du_crabe == "Male") |> pull(taille_de_crabe))
mean(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko" & sexe_du_crabe == "Femelle") |> pull(taille_de_crabe))
mean(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko71" & sexe_du_crabe == "Male") |> pull(taille_de_crabe))
mean(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko71" & sexe_du_crabe == "Femelle") |> pull(taille_de_crabe))
tapply(selectivite_crabe$taille_de_crabe, selectivite_crabe$premier_engin_sortie_de_peche_1, mean)
tapply(selectivite_crabe$taille_de_crabe, selectivite_crabe$premier_engin_sortie_de_peche_1, sd)
# Nombre de crabes
length(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko" & sexe_du_crabe == "Male") |> pull(taille_de_crabe))
length(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko" & sexe_du_crabe == "Femelle") |> pull(taille_de_crabe))
length(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko71" & sexe_du_crabe == "Male") |> pull(taille_de_crabe))
length(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko71" & sexe_du_crabe == "Femelle") |> pull(taille_de_crabe))
# Nombre de crabes par rapport a la taille legale
length(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko" & taille_de_crabe < 110) |> pull(taille_de_crabe))
length(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko" & taille_de_crabe >= 110) |> pull(taille_de_crabe))
length(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko71" & taille_de_crabe < 110) |> pull(taille_de_crabe))
length(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko71" & taille_de_crabe >= 110) |> pull(taille_de_crabe))
# length(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko" & taille_de_crabe < 85) |> pull(taille_de_crabe))*100/2818
# length(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko" & taille_de_crabe >= 85) |> pull(taille_de_crabe))*100/2818
# length(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko71" & taille_de_crabe < 85) |> pull(taille_de_crabe))*100/1366
# length(selectivite_crabe |> filter(premier_engin_sortie_de_peche_1 == "Treko71" & taille_de_crabe >= 85) |> pull(taille_de_crabe))*100/1366

### Test statistique Wilcoxon
wilcox.test(taille_de_crabe ~ premier_engin_sortie_de_peche_1, data = selectivite_crabe) # Si p >= 0.05 : non-significatif
tapply(selectivite_crabe$taille_de_crabe, selectivite_crabe$premier_engin_sortie_de_peche_1, mean)

### Quelques representations graphiques
hist(selectivite_crabe$taille_de_crabe, breaks = 20)

ggplot() + geom_bar(data = selectivite_crabe |> filter(!is.na(taille_de_crabe)), aes(x = taille_de_crabe, fill = sexe_du_crabe), position = "dodge")

ggplot() + geom_histogram(data = selectivite_crabe |> filter(!is.na(taille_de_crabe)), aes(x = taille_de_crabe, fill = premier_engin_sortie_de_peche_1), binwidth = 5, position = "dodge") + 
  geom_vline(xintercept = 110, linetype = "dashed", linewidth = 0.6) + theme_minimal() +
  labs(x = "Taille (mm)", y = "Nombre de crabes", title = "Distribution de la taille des crabes selon l'engin") + 
  scale_fill_manual(name = "", labels = c("Temoin", "Experimental"), values = c("#0072B2", "#E69F00")) + 
  scale_x_continuous(breaks = seq(0, 200, by = 20)) + theme(axis.title = element_text(size = 10, face = "bold"),
                                                            plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
                                                            legend.text = element_text(size = 10, face = "bold"),
                                                            legend.position = "bottom")

ggplot() + geom_histogram(data = selectivite_crabe |> filter(!is.na(taille_de_crabe)), aes(x = taille_de_crabe, fill = premier_engin_sortie_de_peche_1), binwidth = 5, position = "dodge") + 
  geom_vline(xintercept = 110, linetype = "dashed", linewidth = 0.6) +
  facet_wrap(~ sexe_du_crabe) + theme_minimal() +
  labs(x = "Taille (mm)", y = "Nombre de crabes", title = "Distribution de la taille des crabes selon l'engin et le sexe") + 
  scale_fill_manual(name = "", labels = c("Temoin", "Experimental"), values = c("#0072B2", "#E69F00")) + 
  scale_x_continuous(breaks = seq(0, 200, by = 20)) + theme(axis.title = element_text(size = 10, face = "bold"),
                                                            plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
                                                            legend.text = element_text(size = 10, face = "bold"),
                                                            legend.position = "bottom")

ggplot() + geom_histogram(data = selectivite_crabe |> filter(!is.na(taille_de_crabe), premier_engin_sortie_de_peche_1 == "Treko"), aes(x = taille_de_crabe), binwidth = 5, position = "dodge") + 
  geom_vline(xintercept = 110, linetype = "dashed", linewidth = 0.6) + facet_wrap(~ nom_pecheur) + theme_minimal() +
  labs(x = "Taille (mm)", y = "Nombre de crabes", title = "Distribution de la taille des crabes selon les pecheurs (Temoin)") + 
  scale_x_continuous(breaks = seq(0, 200, by = 20)) + theme(axis.title = element_text(size = 10, face = "bold"),
                                                            plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
                                                            legend.text = element_text(size = 10, face = "bold"),
                                                            legend.position = "bottom")

ggplot() + geom_histogram(data = selectivite_crabe |> filter(!is.na(taille_de_crabe), premier_engin_sortie_de_peche_1 == "Treko71"), aes(x = taille_de_crabe), binwidth = 5, position = "dodge") + 
  geom_vline(xintercept = 110, linetype = "dashed", linewidth = 0.6) + facet_wrap(~ nom_pecheur) + theme_minimal() +
  labs(x = "Taille (mm)", y = "Nombre de crabes", title = "Distribution de la taille des crabes selon les pecheurs (Experimental)") +  
  scale_x_continuous(breaks = seq(0, 200, by = 20)) + theme(axis.title = element_text(size = 10, face = "bold"),
                                                            plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
                                                            legend.text = element_text(size = 10, face = "bold"),
                                                            legend.position = "bottom")


# Diagnostic visuel pour connaitre le type de courbe
rapport <- captures_matrix[,2]/captures_matrix[,1]
# ok  <- captures_matrix[, 1] >= 0 # Toutes les classes
ok  <- captures_matrix[, 1] >= 10
plot(classe_taille_mid[ok], rapport[ok], type = "b", pch = 16, lty = 1, xlab = "Classe de taille", ylab = "Experimental/temoin")
