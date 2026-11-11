####################################################################
# Analyse comparative des tailles de crabe - Ampitsopitsoka
# corentin.vignard@ird.fr
# 02_analyse_descriptive_donnees.R
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
data_ampitsopitsoka <- read.csv("data/data-1794322820722.csv")
data_treko <- read.csv("data/Corepeche_crabe_avec_taille_Mahajanga_avec_nom_pecheur.csv")
names(data_treko)
names(data_ampitsopitsoka)
str(data_treko)

### Formatage des données
data_treko$type <- "selectivite"
data_ampitsopitsoka$type <- "suivi"
names(data_ampitsopitsoka)[10] <- names(data_treko)[10]
data_treko <- data_treko |> filter(village == "Ampitsopitsoka")
table(data_ampitsopitsoka$premier_engin_sortie_de_peche_1)

### Comparaison avec differents engins
## Balance classique non-selective (avant 2025)
data_ampitsopitsoka_balance <- data_ampitsopitsoka |> filter(premier_engin_sortie_de_peche_1 == "Balance")
data_ampitsopitsoka_balance_av_2025 <- data_ampitsopitsoka_balance |> filter(year(date_premiere_sortie_de_peche) < 2025)
data_ampitsopitsoka_balance_av_2025_sept_oct <- data_ampitsopitsoka_balance_av_2025 |> filter(month(date_premiere_sortie_de_peche) %in% c(9,10))
table(data_ampitsopitsoka_balance_av_2025_sept_oct$premier_engin_sortie_de_peche_1)
table(month(data_ampitsopitsoka_balance_av_2025_sept_oct$date_premiere_sortie_de_peche))
hist(data_ampitsopitsoka_balance_av_2025_sept_oct$taille_de_crabe)

ggplot() + geom_histogram(data = data_ampitsopitsoka_balance_av_2025_sept_oct |> filter(!is.na(taille_de_crabe)), aes(x = taille_de_crabe), fill = "lightblue", color = "black", binwidth = 10) + 
  geom_vline(xintercept = 110, linetype = "dashed", linewidth = 0.6, color = "black") + 
  geom_vline(aes(xintercept = mean(data_ampitsopitsoka_balance_av_2025_sept_oct$taille_de_crabe, na.rm = TRUE), color = "Moyenne"), linetype = "solid", linewidth = 1) +
  geom_vline(aes(xintercept = median(data_ampitsopitsoka_balance_av_2025_sept_oct$taille_de_crabe, na.rm = TRUE), color = "Médiane"), linetype = "solid", linewidth = 1) +
  theme_minimal() + labs(x = "Taille (mm)", y = "Nombre de crabes", title = "Distribution de la taille des crabes issus de la balance traditionnelle") + 
  scale_color_manual(name = "", values = c("Moyenne" = "orange", "Médiane" = "brown")) +
  scale_x_continuous(breaks = seq(0, 200, by = 10)) + theme(axis.title = element_text(size = 11, face = "bold"),
                                                            plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
                                                            strip.text = element_text(size = 11, face = "bold"),
                                                            legend.text = element_text(size = 11, face = "bold"),
                                                            legend.position = "bottom")

prop_sup110_balance_trad <- sum(data_ampitsopitsoka_balance_av_2025_sept_oct$taille_de_crabe >= 110)/length(data_ampitsopitsoka_balance_av_2025_sept_oct$taille_de_crabe)*100

## Balance selective (2026)
data_ampitsopitsoka_balance <- data_ampitsopitsoka |> filter(premier_engin_sortie_de_peche_1 == "Balance")
data_ampitsopitsoka_balance_2026 <- data_ampitsopitsoka_balance |> filter(year(date_premiere_sortie_de_peche) == 2026)
data_ampitsopitsoka_balance_2026_sept_oct <- data_ampitsopitsoka_balance_2026 |> filter(month(date_premiere_sortie_de_peche) %in% c(9,10))
table(data_ampitsopitsoka_balance_2026_sept_oct$premier_engin_sortie_de_peche_1)
table(month(data_ampitsopitsoka_balance_2026_sept_oct$date_premiere_sortie_de_peche))
hist(data_ampitsopitsoka_balance_2026_sept_oct$taille_de_crabe)

ggplot() + geom_histogram(data = data_ampitsopitsoka_balance_2026_sept_oct |> filter(!is.na(taille_de_crabe)), aes(x = taille_de_crabe), fill = "lightblue", color = "black", binwidth = 10) + 
  geom_vline(xintercept = 110, linetype = "dashed", linewidth = 0.6, color = "black") + 
  geom_vline(aes(xintercept = mean(data_ampitsopitsoka_balance_2026_sept_oct$taille_de_crabe, na.rm = TRUE), color = "Moyenne"), linetype = "solid", linewidth = 1) +
  geom_vline(aes(xintercept = median(data_ampitsopitsoka_balance_2026_sept_oct$taille_de_crabe, na.rm = TRUE), color = "Médiane"), linetype = "solid", linewidth = 1) +
  theme_minimal() + labs(x = "Taille (mm)", y = "Nombre de crabes", title = "Distribution de la taille des crabes issus de la balance selective") + 
  scale_color_manual(name = "", values = c("Moyenne" = "orange", "Médiane" = "brown")) +
  scale_x_continuous(breaks = seq(0, 200, by = 10)) + theme(axis.title = element_text(size = 11, face = "bold"),
                                                            plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
                                                            strip.text = element_text(size = 11, face = "bold"),
                                                            legend.text = element_text(size = 11, face = "bold"),
                                                            legend.position = "bottom")

prop_sup110_balance_selec <- sum(data_ampitsopitsoka_balance_2026_sept_oct$taille_de_crabe >= 110)/length(data_ampitsopitsoka_balance_2026_sept_oct$taille_de_crabe)*100

## Treko classique
data_ampitsopitsoka_treko <- data_ampitsopitsoka |> filter(premier_engin_sortie_de_peche_1 == "Treko")
data_ampitsopitsoka_treko_av_2026 <- data_ampitsopitsoka_treko |> filter(year(date_premiere_sortie_de_peche) < 2026)
data_ampitsopitsoka_treko_av_2026_sept_oct <- data_ampitsopitsoka_treko_av_2026 |> filter(month(date_premiere_sortie_de_peche) %in% c(9,10))
table(data_ampitsopitsoka_treko_av_2026_sept_oct$premier_engin_sortie_de_peche_1)
table(month(data_ampitsopitsoka_treko_av_2026_sept_oct$date_premiere_sortie_de_peche))
hist(data_ampitsopitsoka_treko_av_2026_sept_oct$taille_de_crabe)

ggplot() + geom_histogram(data = data_ampitsopitsoka_treko_av_2026_sept_oct |> filter(!is.na(taille_de_crabe)), aes(x = taille_de_crabe), fill = "lightblue", color = "black", binwidth = 10) + 
  geom_vline(xintercept = 110, linetype = "dashed", linewidth = 0.6, color = "black") + 
  geom_vline(aes(xintercept = mean(data_ampitsopitsoka_treko_av_2026_sept_oct$taille_de_crabe, na.rm = TRUE), color = "Moyenne"), linetype = "solid", linewidth = 1) +
  geom_vline(aes(xintercept = median(data_ampitsopitsoka_treko_av_2026_sept_oct$taille_de_crabe, na.rm = TRUE), color = "Médiane"), linetype = "solid", linewidth = 1) +
  theme_minimal() + labs(x = "Taille (mm)", y = "Nombre de crabes", title = "Distribution de la taille des crabes issus du treko traditionnel") + 
  scale_color_manual(name = "", values = c("Moyenne" = "orange", "Médiane" = "brown")) +
  scale_x_continuous(breaks = seq(0, 200, by = 10)) + theme(axis.title = element_text(size = 11, face = "bold"),
                                                            plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
                                                            strip.text = element_text(size = 11, face = "bold"),
                                                            legend.text = element_text(size = 11, face = "bold"),
                                                            legend.position = "bottom")

prop_sup110_treko_trad <-sum(data_ampitsopitsoka_treko_av_2026_sept_oct$taille_de_crabe >= 110)/length(data_ampitsopitsoka_treko_av_2026_sept_oct$taille_de_crabe)*100

## Treko experimental experimentation
data_treko_exp <- data_treko |> filter(premier_engin_sortie_de_peche_1 == "Treko71")
hist(data_treko_exp$taille_de_crabe)

ggplot() + geom_histogram(data = data_treko_exp |> filter(!is.na(taille_de_crabe)), aes(x = taille_de_crabe), fill = "lightblue", color = "black", binwidth = 10) + 
  geom_vline(xintercept = 110, linetype = "dashed", linewidth = 0.6, color = "black") + 
  geom_vline(aes(xintercept = mean(data_treko_exp$taille_de_crabe, na.rm = TRUE), color = "Moyenne"), linetype = "solid", linewidth = 1) +
  geom_vline(aes(xintercept = median(data_treko_exp$taille_de_crabe, na.rm = TRUE), color = "Médiane"), linetype = "solid", linewidth = 1) +
  theme_minimal() + labs(x = "Taille (mm)", y = "Nombre de crabes", title = "Distribution de la taille des crabes issus du treko experimental - experimentation 2026") + 
  scale_color_manual(name = "", values = c("Moyenne" = "orange", "Médiane" = "brown")) +
  scale_x_continuous(breaks = seq(0, 200, by = 10)) + theme(axis.title = element_text(size = 11, face = "bold"),
                                                            plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
                                                            strip.text = element_text(size = 11, face = "bold"),
                                                            legend.text = element_text(size = 11, face = "bold"),
                                                            legend.position = "bottom")

prop_sup110_treko_exp <-sum(data_treko_exp$taille_de_crabe >= 110)/length(data_treko_exp$taille_de_crabe)*100

## Treko traditionnel experimentation
data_treko_tem <- data_treko |> filter(premier_engin_sortie_de_peche_1 == "Treko")
hist(data_treko_tem$taille_de_crabe)

ggplot() + geom_histogram(data = data_treko_tem |> filter(!is.na(taille_de_crabe)), aes(x = taille_de_crabe), fill = "lightblue", color = "black", binwidth = 10) + 
  geom_vline(xintercept = 110, linetype = "dashed", linewidth = 0.6, color = "black") + 
  geom_vline(aes(xintercept = mean(data_treko_tem$taille_de_crabe, na.rm = TRUE), color = "Moyenne"), linetype = "solid", linewidth = 1) +
  geom_vline(aes(xintercept = median(data_treko_tem$taille_de_crabe, na.rm = TRUE), color = "Médiane"), linetype = "solid", linewidth = 1) +
  theme_minimal() + labs(x = "Taille (mm)", y = "Nombre de crabes", title = "Distribution de la taille des crabes issus du treko traditionnel - experimentation 2026") + 
  scale_color_manual(name = "", values = c("Moyenne" = "orange", "Médiane" = "brown")) +
  scale_x_continuous(breaks = seq(0, 200, by = 10)) + theme(axis.title = element_text(size = 11, face = "bold"),
                                                            plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
                                                            strip.text = element_text(size = 11, face = "bold"),
                                                            legend.text = element_text(size = 11, face = "bold"),
                                                            legend.position = "bottom")

prop_sup110_treko_tem <- sum(data_treko_tem$taille_de_crabe >= 110)/length(data_treko_tem$taille_de_crabe)*100


df <- data.frame("Engin" = c("Balance traditionnelle", "Balance selective","Treko traditionnel", "Treko experimental", "Treko traditionnel"),
           "Periode" = c("Sept-Oct 2021-2024", "Sept 2026","Sept-Oct 2021-2025", "Sept-Oct 2026", "Sept-Oct 2026"),
           "Nombre de crabes" = c(nrow(data_ampitsopitsoka_balance_av_2025_sept_oct), nrow(data_ampitsopitsoka_balance_2026_sept_oct), nrow(data_ampitsopitsoka_treko_av_2026_sept_oct), nrow(data_treko_exp), nrow(data_treko_tem)),
           "Taille moyenne (mm)" = c(mean(data_ampitsopitsoka_balance_av_2025_sept_oct$taille_de_crabe, na.rm = TRUE), mean(data_ampitsopitsoka_balance_2026_sept_oct$taille_de_crabe, na.rm = TRUE),mean(data_ampitsopitsoka_treko_av_2026_sept_oct$taille_de_crabe, na.rm = TRUE), mean(data_treko_exp$taille_de_crabe, na.rm = TRUE), mean(data_treko_tem$taille_de_crabe, na.rm = TRUE)),
           "Proportion de plus de 110 mm" = c(prop_sup110_balance_trad, prop_sup110_balance_selec, prop_sup110_treko_trad, prop_sup110_treko_exp, prop_sup110_treko_tem))
df

# Test statistique Kolmogorov-Smirnov # Si p >= 0.05 : non-significatif
ks.test(data_ampitsopitsoka_balance_av_2025_sept_oct$taille_de_crabe, data_ampitsopitsoka_treko_av_2026_sept_oct$taille_de_crabe)
ks.test(data_ampitsopitsoka_balance_av_2025_sept_oct$taille_de_crabe, data_treko_tem$taille_de_crabe)
ks.test(data_ampitsopitsoka_treko_av_2026_sept_oct$taille_de_crabe, data_treko_tem$taille_de_crabe)

# Test statistique Wilcoxon # Si p >= 0.05 : non-significatif
wilcox.test(data_ampitsopitsoka_balance_av_2025_sept_oct$taille_de_crabe, data_ampitsopitsoka_treko_av_2026_sept_oct$taille_de_crabe)
wilcox.test(data_ampitsopitsoka_balance_av_2025_sept_oct$taille_de_crabe, data_treko_tem$taille_de_crabe) 
wilcox.test(data_ampitsopitsoka_treko_av_2026_sept_oct$taille_de_crabe, data_treko_tem$taille_de_crabe) 
wilcox.test(data_ampitsopitsoka_balance_av_2025_sept_oct$taille_de_crabe, data_treko_exp$taille_de_crabe) 
wilcox.test(data_ampitsopitsoka_treko_av_2026_sept_oct$taille_de_crabe, data_treko_exp$taille_de_crabe)
