####################################################################
# Analyse de selectivite des trekos à crabe de mangrove
# corentin.vignard@ird.fr
# 02_analyse_descriptive_donnees.R
####################################################################

### Execution du script precedent
source("scripts/treko/02_analyse_descriptive_donnees.R")

###############################################################################
### Analyse generale de la forme des donnees
classe_taille_mid <- as.numeric(row.names(captures_matrix))
rapport <- captures_matrix[,2]/(captures_matrix[,1]+captures_matrix[,2])
ok  <- captures_matrix[, 1] >= 0 # Toutes les classes
# ok  <- captures_matrix[, 1] >= 10
plot(classe_taille_mid[ok], rapport[ok], type = "b", pch = 16, lty = 1, 
     cex.axis = 1, cex.lab = 1.1,
     xlab = "Taille (mm)", ylab = "Rapport", main = "Part du temoin sur le total (nombre de crabes capturés)")

###############################################################################
### Modelisation SELECT avec le package 'TropFishR'
library(TropFishR)
## Modelisation forme en cloche, conjointe (toutes les maillages), courbes relatives, decalees par rapport au temoin
# Approche non adaptee au treko car le temoin est censé ne pas avoir de maillage (non selectif)
# Liste des parametres (necessaires pour select_Millar())
rel.power <- c(1, 1) # Effort de peche equivalent
select_paramaters <- list(midLengths       = classe_taille_mid,
                          meshSizes        = c(5, 71), # Taille de maille du temoin et experimental 
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

# captures_matrix[,2]/(captures_matrix[,1])/0.7

select <- list(midLengths = classe_taille_mid,
                          meshSizes = c(1, 2),              
                          CatchPerNet_mat = captures_matrix) 

select_m <- list(midLengths = classe_taille_mid,
                          meshSizes = c(1, 2),              
                          CatchPerNet_mat = captures_matrix_m) 

select_f <- list(midLengths = classe_taille_mid,
                          meshSizes = c(1, 2),              
                          CatchPerNet_mat = captures_matrix_f) 

# Modelisation
# x0 : initialisation des parametres : a, b et p (efficacite relative par rapport au temoin) : a ajuster
logi <- select_Millar(select, x0 = c(-7, 0.08, -0.2), rtype = "tt.logistic")
logi$par

logi_m <- select_Millar(select_m, x0 = c(-10, 0.15, -0.6), rtype = "tt.logistic")
logi_m$par

logi_f <- select_Millar(select_f, x0 = c(-7, 0.08, -0.3), rtype = "tt.logistic")
logi_f$par

# Affichage de la courbe et des residus
plot(logi, plotlens = seq(min(select$midLengths), max(select$midLengths), 0.5), deviance_plot = FALSE)
legend("topleft", c("Control", "Experimental"), lty = 1:2, col = 1:2)
plot(logi, selectivity_plot = FALSE) 

# Affichage des estimates : L50, SR et p (efficacite relative de peche)
logi$estimates
logi_m$estimates
logi_f$estimates

###############################################################################
### Modelisation SELECT avec p fixe (non estime) a 0.5
# Fonction codee manuellement
ajust_SELECT <- function(tab, L50_0, SR_0, p) 
{taille <- as.numeric(row.names(tab))
captures_exp <- tab[, 2]
captures_tem <- tab[, 1]
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
     ddl = sum(tab[, 2] + tab[, 1] > 0) - 2,
     converge = fit$convergence == 0)}

logi_pfixe <- ajust_SELECT(captures_matrix, L50_0 = 80, SR_0 = 25, p = 0.5)
logi_pfixe_m <- ajust_SELECT(captures_matrix_m, L50_0 = 80, SR_0 = 19, p = 0.5)
logi_pfixe_f <- ajust_SELECT(captures_matrix_f, L50_0 = 85, SR_0 = 30, p = 0.5)
logi_pfixe$L50
logi_pfixe_m$L50
logi_pfixe_f$L50

###############################################################################
### Verification, comparaison des modeles et representation graphique
## Modelisation logistique p estime / p fixe
# Affectation des tableaux d'entree et resultats
tab <- as.data.frame(captures_matrix)
res_pfixe <- logi_pfixe
res_pestime <- logi

# Calcul de la part de crabes dans la maille experimentale
tab$n_tot <- tab$Treko + tab$Treko71
tab$part <- tab$Treko71 / tab$n_tot
# Fonction de calcul de la part
part_logi <- function(tab, L50, SR, p) 
{
  taille <- as.numeric(row.names(tab))
  r <- plogis(2*log(3)*(taille - L50)/SR)
  p*r/(p*r+1-p)
}

# Calcul
part_pestime <- part_logi(tab, res_pestime$estimates["L50","par"], res_pestime$estimates["SR","par"], res_pestime$estimates["p","par"])
part_pfixe <- part_logi(tab, res_pfixe$L50, res_pfixe$SR, 0.5)

# Graphique
plot(as.numeric(row.names(tab)), tab$part, pch = 16, cex = sqrt(tab$n_tot)/3, ylim = c(0, 1), xlab = "Taille (mm)", ylab = "Part de la maille")
lines(as.numeric(row.names(tab)), part_pestime, col = "red")
lines(as.numeric(row.names(tab)), part_pfixe, col = "blue")
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

