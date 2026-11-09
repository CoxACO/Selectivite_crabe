####################################################################
# Analyse de selectivite des balances à crabe de mangrove : exemple
# corentin.vignard@ird.fr
# 02_analyse_donnees.R
####################################################################

source("scripts/balance/01_formatage_donnees.R")

### Analyse de l'effet zone (Lien) entre Baly et Antsatrana
captures_df_temoin_exp <- function(maille_exp, maille_temoin = "Temoin")
{
  calees <- captures_long_format |> filter(Maillage == maille_exp) |> distinct(ID_calee) |> pull(ID_calee)
  captures_long_format |> filter(ID_calee %in% calees, Maillage %in% c(maille_exp, maille_temoin)) |>
    group_by(Lieu, classe_taille_mid, Maillage) |> summarise(n = sum(n), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Maillage, values_from = n, values_fill = 0) |>
    rename("experimental" = all_of(maille_exp)) |>
    as.data.frame()
}

res <- lapply(c("55_mm", "65_mm", "75_mm"), function(m) {
  d   <- captures_df_temoin_exp(m)
  m1  <- glm(cbind(experimental, Temoin) ~ classe_taille_mid * Lieu, family = binomial, data = d)
  m0  <- glm(cbind(experimental, Temoin) ~ classe_taille_mid + Lieu, family = binomial, data = d)
  m00 <- glm(cbind(experimental, Temoin) ~ classe_taille_mid,        family = binomial, data = d)
  data.frame(maille = m,
             p_interaction = anova(m0, m1,  test = "Chisq")$`Pr(>Chi)`[2],
             p_lieu        = anova(m00, m0, test = "Chisq")$`Pr(>Chi)`[2])
})

res <- do.call(rbind, res)
res
# maille p_interaction    p_lieu
# 1  55_mm     0.9092226 0.3270054
# 2  65_mm     0.8264250 0.8325829
# 3  75_mm     0.2164594 0.9526673
# Pas de difference signaficative du lieu : suggere une analyse confondue sur les 2 zones

###############################################################################
### Analyse generale de la forme des donnees
# Construction du tableau de maillage
taille_maillage <- data.frame(maillage = c("Temoin","55_mm","65_mm","75_mm"),
                              taille_mm = c(35,55,65,75)) # Taille de la maille temoin
# Effort de peche en nombre de calees par maillage par rapport au temoin
rel.power <- n_calees$n[match(taille_maillage$maillage, n_calees$Maillage)]/n_calees$n[n_calees$Maillage == "Temoin"]

# Diagnostic visuel pour connaitre le type de courbe
i_t <- which(taille_maillage$maillage == "Temoin")
captures_eff_corrige <- sweep(captures_matrix, 2, rel.power, "/") # Captures standardisees par l'effort
rapport <- captures_eff_corrige[, -i_t]/captures_eff_corrige[, i_t] # Rapport captures exp./temoin ≈ r_k(l) / r_témoin(l)
# ok  <- captures_matrix[, i_t] >= 0 # Toutes les classes
ok  <- captures_matrix[, i_t] >= 10 # Classes avec plus de 10 obs.
matplot(classe_taille_mid[ok], rapport[ok, ], type = "b", pch = 16, lty = 1, xlab = "Classe de taille", ylab = "Maille/temoin (corrige)")

### Modelisation SELECT avec le package 'TropFishR'
library(TropFishR)
## Modelisation forme en cloche, conjointe (toutes les maillages), courbes relatives à la maille ??

# Liste des parametres (necessaires pour select_Millar())
select_paramaters <- list(midLengths       = classe_taille_mid,
                          meshSizes        = taille_maillage$taille_mm,
                          CatchPerNet_mat  = captures_matrix)

# x0 = NULL par defaut (valeurs de départ calculées par la méthode de Millar & Holst (1997))
norm_loc <- select_Millar(select_paramaters, x0 = NULL, rtype = "norm.loc", rel.power)
norm_loc
norm_sca <- select_Millar(select_paramaters, x0 = NULL, rtype = "norm.sca", rel.power)
norm_sca
log_norm <- select_Millar(select_paramaters, x0 = NULL, rtype = "lognorm", rel.power)
log_norm 

c(norm.loc = norm_loc$value, norm.sca = norm_sca$value, lognorm = log_norm$value) #  -log vraisemblance : plus c'est faible, mieux le modele est ajuste
c(norm.loc = norm_loc$out[4]/norm_loc$out[6], norm.sca = norm_sca$out[4]/norm_sca$out[6], lognorm = log_norm$out[4]/log_norm$out[6])

###############################################################################
## Modelisation forme logistique, separee : chaque maille experimentale contre le temoin
# Fonction pour liste des parametres 
select_paramaters_logi <- function(df) 
{
  list(midLengths = df[, 1],
       numCodend = df[, 2],
       numCover = df[, 3],
       CatchPerNet_mat = cbind(df[, 3], df[, 2]),
       meshSizes = c(1, 2))
} 
# Creation des listes (necessaires pour select_Millar())
select_55 <- select_paramaters_logi(captures_matrix_55)
select_55_m <- select_paramaters_logi(captures_matrix_55_m)
select_55_f <- select_paramaters_logi(captures_matrix_55_f)

select_65 <- select_paramaters_logi(captures_matrix_65)
select_65_m <- select_paramaters_logi(captures_matrix_65_m)
select_65_f <- select_paramaters_logi(captures_matrix_65_f)

select_75 <- select_paramaters_logi(captures_matrix_75)
select_75_m <- select_paramaters_logi(captures_matrix_75_m)
select_75_f <- select_paramaters_logi(captures_matrix_75_f)

# Modelisation
# x0 : initialisation des parametres : a, b et p (efficacite relative par rapport au temoin) : a ajuster
logi_55 <- select_Millar(select_55, x0 = c(-6, 0.1, -0.2), rtype = "tt.logistic") # Maille 55 male et femelle
logi_55$par
logi_55_m <- select_Millar(select_55_m, x0 = c(-4, 0.05, -0.1), rtype = "tt.logistic") # Maille 55 male
logi_55_m$par
logi_55_f <- select_Millar(select_55_f, x0 = c(-12, 0.15, 0), rtype = "tt.logistic") # Maille 55 femelle
logi_55_f$par

logi_65 <- select_Millar(select_65, x0 = c(-6, 0.05, 0), rtype = "tt.logistic") # Maille 65 male et femelle
logi_65$par 
logi_65_m <- select_Millar(select_65_m, x0 = c(-5, 0.06, 0.1), rtype = "tt.logistic") # Maille 65 male
logi_65_m$par
logi_65_f <- select_Millar(select_65_f, x0 = c(-6, 0.06, 0.5), rtype = "tt.logistic") # Maille 65 femelle
logi_65_f$par

logi_75 <- select_Millar(select_75, x0 =c(-8, 0.05, 0.4), rtype = "tt.logistic") # Maille 75 male et femelle
logi_75$par
logi_75_m <- select_Millar(select_75_m, x0 = c(-7, 0.07, 0.3), rtype = "tt.logistic")  # Maille 75 male 
logi_75_m$par
logi_75_f <- select_Millar(select_75_f, x0 = c(-8, 0.05, 0.5), rtype = "tt.logistic") # Maille 75 femelle
logi_75_f$par 

# Affichage de la courbe et des residus
plot(logi_55, plotlens = seq(min(select_55$midLengths), max(select_55$midLengths), 0.5), deviance_plot = FALSE)
legend("topleft", c("Control", "Experimental"), lty = 1:2, col = 1:2)
plot(logi_55, selectivity_plot = FALSE) 

# Affichage des estimates : L50, SR et p (efficacite relative de peche)
logi_55$estimates
logi_55_m$estimates
logi_55_f$estimates

logi_65$estimates
logi_65_m$estimates
logi_65_f$estimates

logi_75$estimates
logi_75_m$estimates
logi_75_f$estimates

###############################################################################
### Modelisation SELECT avec p fixe (non estime) a 0.5
# Fonction codee manuellement
ajust_SELECT <- function(tab, L50_0, SR_0 = 15, p) 
 {taille <- tab[, 1]
  captures_exp <- tab[, 2]
  captures_tem <- tab[, 3]
  nll <- function(par) { # Fonction de vraisemblance négative
    L50 <- par[1] ; SR <- exp(par[2]) # 2 parametres à estimer
    r <- plogis(2*log(3)*(taille-L50)/SR) # Courbe logistique de sélectivité
    phi <- p*r/(p*r+(1-p)) # Correction par la maille témoin
    -sum(captures_exp*log(pmax(phi, 1e-12))+captures_tem*log(pmax(1 - phi, 1e-12))) # Calcul de la vraisemblance négative
  }
  fit <- optim(c(L50_0, log(SR_0)), nll, method = "BFGS", hessian = TRUE) # Estimation des parametres (optim minimise la vraisemblance negative = maximise la vraisemblance)
  se <- sqrt(diag(solve(fit$hessian)))   # Erreurs standards
  L50 <- fit$par[1]
  SR <- exp(fit$par[2])
  a <- -2 * log(3) * L50 / SR
  b <- 2 * log(3) / SR
  r <- plogis(a + b * taille) # Probabilites predites
  phi <- p * r / (p * r + (1 - p))
  dev <- 2 * (captures_exp * log(pmax(captures_exp / ((captures_exp + captures_tem) * phi), 1e-12)) + captures_tem * log(pmax(captures_tem / ((captures_exp + captures_tem) * (1 - phi)), 1e-12)))
  dev_residuals <- sign(captures_exp / (captures_exp + captures_tem) - phi) * sqrt(pmax(dev, 0)) # Residus de deviance
  list(L50 = L50,
       se_L50 = se[1],
       SR = SR,
       se_SR = SR * se[2],
       a = a,
       b = b,
       nll = fit$value,
       aic = 2 * fit$value + 2 * 2,
       dev_residuals = dev_residuals,
       deviance = sum(dev_residuals^2, na.rm = TRUE),
       ddl = sum(tab[, 2] + tab[, 3] > 0) - 2,
       converge = fit$convergence == 0)}

logi_55_pfixe <- ajust_SELECT(captures_matrix_55, L50_0 = 60, p = 0.5)
logi_55_pfixe_m <- ajust_SELECT(captures_matrix_55_m, L50_0 = 60, p = 0.5)
logi_55_pfixe_f <- ajust_SELECT(captures_matrix_55_f, L50_0 = 60, p = 0.5)
logi_55_pfixe$L50
logi_55_pfixe_m$L50
logi_55_pfixe_f$L50

logi_65_pfixe <- ajust_SELECT(captures_matrix_65, L50_0 = 80, p = 0.5)
logi_65_pfixe_m <- ajust_SELECT(captures_matrix_65_m, L50_0 = 80, p = 0.5)
logi_65_pfixe_f <- ajust_SELECT(captures_matrix_65_f, L50_0 = 80, p = 0.5)
logi_65_pfixe$L50
logi_65_pfixe_m$L50
logi_65_pfixe_f$L50

logi_75_pfixe <- ajust_SELECT(captures_matrix_75, L50_0 = 90, p = 0.5)
logi_75_pfixe_m <- ajust_SELECT(captures_matrix_75_m, L50_0 = 90, p = 0.5)
logi_75_pfixe_f <- ajust_SELECT(captures_matrix_75_f, L50_0 = 90, p = 0.5)
logi_75_pfixe$L50
logi_75_pfixe_m$L50
logi_75_pfixe_f$L50

###############################################################################
### Verification, comparaison des modeles et representation graphique
## Modelisation logistique p estime / p fixe
# Affectation des tableaux d'entree et resultats
tab <- as.data.frame(captures_matrix_55)
res_pfixe <- logi_55_pfixe
res_pestime <- logi_55

# Calcul de la part de crabes dans la maille experimentale
tab$n_tot <- tab$experimental + tab$Temoin
tab$part <- tab$experimental / tab$n_tot
# Fonction de calcul de la part
part_logi <- function(tab, L50, SR, p) 
{
  taille <- tab[,1]
  r <- plogis(2*log(3)*(taille - L50)/SR)
  p*r/(p*r+1-p)
}

# Calcul
part_pestime <- part_logi(tab, res_pestime$estimates["L50","par"], res_pestime$estimates["SR","par"], res_pestime$estimates["p","par"])
part_pfixe <- part_logi(tab, res_pfixe$L50, res_pfixe$SR, 0.5)

# Graphique
plot(tab[,1], tab$part, pch = 16, cex = sqrt(tab$n_tot)/3, ylim = c(0, 1), xlab = "Taille (mm)", ylab = "Part de la maille")
lines(tab[,1], part_pestime, col = "red")
lines(tab[,1], part_pfixe, col = "blue")
abline(h = 0.5, lty = 3)
legend("topleft", c("p estimé", "p fixe à 0,5"), col = c("red", "blue"), lty = 1)

# Comparaison graphique des residus de deviance
par(mfrow = c(2, 1))
plot(sign(res_pestime$Dev.resids[, 2]) * sqrt(rowSums(res_pestime$Dev.resids^2)), type = "h", col = "red", ylim = c(-3, 3)) 
abline(h = 0)
plot(res_pfixe$dev_residuals, type = "h", col = "blue", ylim = c(-3, 3))
abline(h = 0)
par(mfrow = c(1, 1))

# Qualite d'ajustement du modele : deviance/nbre de degres de liberte (aux alentours de 1 si le modele est bon, si > 1.5 : sur-dispersion)
res_pfixe$deviance/res_pfixe$ddl
res_pestime$out["Deviance",1]/res_pestime$out["d.o.f.",1]

# Comparaison de modele par AIC : si delta AIC > 2, le modele avec AIC le plus bas est prefere
aic_pestime <- 2 * res_pestime$value + 2 * length(res_pestime$par)   # k = 3
aic_pfixe   <- res_pfixe$aic                                         # k = 2
c(p_estime = aic_pestime, p_fixe = aic_pfixe, delta = aic_pestime - aic_pfixe)

## Modelisation logistique male / femelle
# Affectation des tableaux d'entree et resultats
tab_all <- as.data.frame(captures_matrix_65)
tab_m <- as.data.frame(captures_matrix_65_m)
tab_f <- as.data.frame(captures_matrix_65_f)

res_pfixe_all <- logi_65_pfixe
res_pfixe_m <- logi_65_pfixe_m
res_pfixe_f <- logi_65_pfixe_f

# Fonction de calcul de la courbe logistique
courbe_logi <- function(tab, L50, SR)
{
  taille <- tab[,1]
  plogis(2 * log(3) * (taille - L50) / SR)
}

# Calcul de la courbe logistique (et intervalle de confiance sur L50)
logi_pfixe_all <- courbe_logi(tab_all, res_pfixe_all$L50, res_pfixe_all$SR)
logi_pfixe_all_inf <- courbe_logi(tab_all, res_pfixe_all$L50 - 1.96 * res_pfixe_all$se_L50, res_pfixe_all$SR)
logi_pfixe_all_sup <- courbe_logi(tab_all, res_pfixe_all$L50 + 1.96 * res_pfixe_all$se_L50, res_pfixe_all$SR)

logi_pfixe_m <- courbe_logi(tab_m, res_pfixe_m$L50, res_pfixe_m$SR)
logi_pfixe_m_inf <- courbe_logi(tab_m, res_pfixe_m$L50 - 1.96 * res_pfixe_m$se_L50, res_pfixe_m$SR)
logi_pfixe_m_sup <- courbe_logi(tab_m, res_pfixe_m$L50 + 1.96 * res_pfixe_m$se_L50, res_pfixe_m$SR)

logi_pfixe_f <- courbe_logi(tab_f, res_pfixe_f$L50, res_pfixe_f$SR)
logi_pfixe_f_inf <- courbe_logi(tab_f, res_pfixe_f$L50 - 1.96 * res_pfixe_f$se_L50, res_pfixe_f$SR)
logi_pfixe_f_sup <- courbe_logi(tab_f, res_pfixe_f$L50 + 1.96 * res_pfixe_f$se_L50, res_pfixe_f$SR)

# Graphique
plot(tab_all[, 1], logi_pfixe_all, type = "l", col = "darkgreen", xlab = "Taille (mm)", ylab = "Probabilité de rétention")
lines(tab_all[, 1], logi_pfixe_all_inf, col = "darkgreen", lty = 2)
lines(tab_all[, 1], logi_pfixe_all_sup, col = "darkgreen", lty = 2)

lines(tab_m[, 1], logi_pfixe_m, col = "red")
lines(tab_m[, 1], logi_pfixe_m_inf, col = "red", lty = 2)
lines(tab_m[, 1], logi_pfixe_m_sup, col = "red", lty = 2)

lines(tab_f[, 1], logi_pfixe_f, col = "blue")
lines(tab_f[, 1], logi_pfixe_f_inf, col = "blue", lty = 2)
lines(tab_f[, 1], logi_pfixe_f_sup, col = "blue", lty = 2)

abline(h = 0.5, lty = 3)
abline(v = res_pfixe_all$L50, col = "darkgreen")
abline(v = res_pfixe_m$L50, col = "red")
abline(v = res_pfixe_f$L50, col = "blue")
legend("topleft", legend = c("Indifferencie", "Male", "Femelle"), col = c("darkgreen", "red", "blue"), lty = 1)

# Test statistique de Wald pour comparaison des coefficients
wald <- function(m, f, par = "L50") {
  diff   <- m[[par]] - f[[par]]
  se     <- sqrt(m[[paste0("se_", par)]]^2 + f[[paste0("se_", par)]]^2)
  stat_z <- diff / se
  c(diff = diff, se = se, stat_z = stat_z, p_value = 2 * pnorm(-abs(stat_z)))
}

wald(res_pfixe_m, res_pfixe_f, "L50") # Si p >= 0.05 : non-significatif
# wald(res_pfixe_m, res_pfixe_f, "SR")

# Test statistique Wilcoxon
wilcox.test(Taille ~ Sexe, data = selectivite_crabe |> filter(!is.na(Sexe))) # Si p >= 0.05 : non-significatif
tapply(selectivite_crabe$Taille, selectivite_crabe$Sexe, median)

###############################################################################
### Prise en compte de la variabilite inter-calees : Bootstrap par calee => incertitude sur L50/SR (et l'ensemble de la courbe)
#
# Principee : on « refait l'expérience » B fois : on tire au hasard AVEC remise des calées entières. Ex : une calée tirée 2 fois compte double, une calée non tirée ne compte pas. 
# On réajuste le modèle sur ce jeu de données reconstitué et on garde L50, SR et la courbe. La dispersion des B résultats mesure l'incertitude, variabilité entre calées comprise.

## Parametrage
maillages <- c("Temoin", "55_mm", "65_mm", "75_mm")
exp_m     <- c("55_mm", "65_mm", "75_mm")
L50_0     <- c("55_mm" = logi_55_pfixe$L50, "65_mm" = logi_65_pfixe$L50, "75_mm" = logi_75_pfixe$L50) # Points de départ de l'optimisation
B         <- 1000 # Nombre de tirages
set.seed(1) # Ppour retrouver les mêmes résultats

## Preparation des donnees et objets
classes <- sort(unique(na.omit(captures_long_format$classe_taille_mid)))
calees  <- sort(unique(liste_maillage_calee$ID_calee))
nc <- length(calees) # Nombre de calées

# Captures par calée × maillage × classe
cap <- captures_long_format |> dplyr::filter(!is.na(classe_taille_mid), as.character(Maillage) %in% maillages, ID_calee %in% calees) |>
  dplyr::group_by(ID_calee, Maillage, classe_taille_mid) |>
  dplyr::summarise(n = sum(n), .groups = "drop")

# A : tableau en 3 dimensions : calée × classe de taille × maillage (contient les captures)
A <- array(0, dim = c(nc, length(classes), length(maillages)), dimnames = list(calees, classes, maillages))
A[cbind(match(cap$ID_calee, calees),
        match(cap$classe_taille_mid, classes),
        match(as.character(cap$Maillage), maillages))] <- cap$n

# P : presence de la balance du maillage par calee (TRUE / FALSE)
ok_l <- as.character(liste_maillage_calee$Maillage) %in% maillages
P <- matrix(FALSE, nc, length(maillages), dimnames = list(calees, maillages))
P[cbind(match(liste_maillage_calee$ID_calee[ok_l], calees),
        match(as.character(liste_maillage_calee$Maillage[ok_l]), maillages))] <- TRUE

# Am : matrice « calées × classes » par maillage (pour la boucle)
Am <- lapply(setNames(maillages, maillages), function(m) A[, , m])

# Verification
colSums(P)
liste_maillage_calee |> group_by(Maillage) |> count(n())
sapply(Am, sum)

## Ajustement sur les vraies donnees (même calcul que la boucle, mais avec toutes les calées comptées une fois)
ajust_vrai <- lapply(setNames(exp_m, exp_m), function(m) {
  pm  <- P[, m] * 1 # poids 1 si la maille est présente, 0 sinon
  tab <- cbind(classes, colSums(Am[[m]] * pm), # Captures de la maille
               colSums(Am[["Temoin"]] * pm)) # Captures du témoin (mêmes calées)
  ajust_SELECT(tab, L50_0 = L50_0[m], p = 0.5)
})
sapply(ajust_vrai, function(f) c(L50 = f$L50, SR = f$SR)) # Verification

## Bootstrap
grille  <- seq(min(classes), max(classes), by = 1) # Gamme de taille
L50 <- SR <- matrix(NA_real_, B, length(exp_m), dimnames = list(NULL, exp_m))
courbes <- setNames(lapply(exp_m, function(m) matrix(NA_real_, B, length(grille))), exp_m)

# Boucle de tirage
for (b in 1:B) {
  w <- tabulate(sample(nc, nc, replace = TRUE), nbins = nc)   # Tirage des calées avec remise : w[i] = nombre de fois où la calée i est tirée
  for (m in exp_m) {
    pm <- w * P[, m]  # Poids : nombre de tirages (0 si la maille n'était pas dans la calée)
    tab <- cbind(classes,     # Tableau de captures du tirage : chaque calée est comptée pm fois (matrice × vecteur : chaque ligne i est multipliée par pm[i])
                 colSums(Am[[m]] * pm), # Captures de la maille
                 colSums(Am[["Temoin"]] * pm)) # Captures du témoin
    f <- try(ajust_SELECT(tab, L50_0 = L50_0[m], p = 0.5), silent = TRUE) # Réajustement du modèle sur ce tirage (p fixé à 0,5)
    if (!inherits(f, "try-error") && isTRUE(f$converge) && # Stockage du résultat si plausible (sinon : NA, tirage écarté)
        all(is.finite(c(f$L50, f$SR))) && f$SR < 200 && f$L50 > min(classes) && f$L50 < max(classes)) {
      L50[b, m] <- f$L50
      SR[b, m]  <- f$SR
      courbes[[m]][b, ] <- plogis(2 * log(3) * (grille - f$L50) / f$SR) # Courbe de rétention du tirage
    }
  }
  if (b %% 100 == 0) cat("tirage", b, "sur", B, "\n") # Suivi de l'avancement
}

## Resultats
ic <- function(x) quantile(x, c(0.025, 0.5, 0.975), na.rm = TRUE) # IC 95 % + médiane

colSums(is.na(L50)) # Nombre de tirages écartés par maille
round(apply(L50, 2, ic), 1) # IC 95 % de L50
round(apply(SR,  2, ic), 1) # IC 95 % de SR
round(apply(L50, 2, sd, na.rm = T), 1) # sd 
round(apply(SR, 2, sd, na.rm = T), 1) # sd

# Différences de L50 entre mailles (calculées tirage par tirage, donc avec le même tirage de calées)
dif <- cbind("65-55" = L50[, "65_mm"] - L50[, "55_mm"], "75-65" = L50[, "75_mm"] - L50[, "65_mm"], "75-55" = L50[, "75_mm"] - L50[, "55_mm"])
round(apply(dif, 2, ic), 1) # si l'intervalle exclut 0 : différence confirmée

# Rapport écart-type du bootstrap / erreur-type du modèle : proche de 1 = variabilité entre calées pèse peu / > 1,5-2 = erreurs-types du modèle trop optimistes
apply(L50, 2, sd, na.rm = TRUE)/sapply(ajust_vrai, function(f) f$se_L50)

# Courbes avec intervalles de confiance
cols <- c("55_mm" = "darkgreen", "65_mm" = "red", "75_mm" = "blue")
plot(NA, xlim = range(grille), ylim = c(0, 1), xlab = "Largeur de carapace (mm)", ylab = "Probabilité de rétention")
for (m in exp_m) {
  # À chaque taille : 2,5 % et 97,5 % des courbes = IC (point par point)
  bande <- apply(courbes[[m]], 2, quantile, c(0.025, 0.975), na.rm = TRUE)
  polygon(c(grille, rev(grille)), c(bande[1, ], rev(bande[2, ])),
          col = adjustcolor(cols[m], 0.25), border = NA)
  # Courbe estimée sur les vraies données
  lines(grille, plogis(2 * log(3) * (grille - ajust_vrai[[m]]$L50) / ajust_vrai[[m]]$SR),
        col = cols[m], lwd = 2)
  abline(v = c(ajust_vrai[[m]]$L50))
}
abline(h = c(0.5, 1), lty = 3)
legend("topleft", exp_m, col = cols, lwd = 2)
