# ==========================================================================
# MOD08 - Simulation interactive des lois discretes
# Pole Formation UIMM Centre-Val de Loire - BTS Industriel - Mathematiques
# Auteur : S. Jaubert
#
# Objectif
# --------
# Simuler des donnees selon les lois de Bernoulli, Binomiale et Poisson,
# et les representer graphiquement pour projection en seance.
# Chaque graphique superpose :
#   - la frequence empirique obtenue par simulation (barres orange),
#   - la probabilite theorique donnee par la loi (points/segments bleus).
#
# Utilisation
# -----------
# 1. Ouvrir ce fichier dans RStudio.
# 2. Executer tout le script (source) : les fonctions sont chargees et
#    le menu se lance automatiquement en mode interactif.
# 3. Si le menu ne se lance pas, taper dans la console : menu_principal()
#
# Aucun package externe requis (R de base uniquement : rbinom, dbinom,
# rpois, dpois).
# ==========================================================================

# ---------------------------------------------------------------------
# 1. Parametres graphiques (lisibilite en projection)
# ---------------------------------------------------------------------

COULEUR_THEO   <- "#1B4F72"  # bleu fonce : probabilite theorique
COULEUR_EMP    <- "#E67E22"  # orange : frequence simulee
COULEUR_ACCENT <- "#C0392B"  # rouge : loi de comparaison (Poisson)

par_projection <- function() {
  par(cex.main = 1.9, cex.lab = 1.5, cex.axis = 1.3, font.main = 2,
      mar = c(5, 5, 4, 2), lwd = 2)
}

# ---------------------------------------------------------------------
# 2. Fonctions utilitaires de saisie (console)
# ---------------------------------------------------------------------

pause <- function(msg = "\nAppuie sur Entree pour revenir au menu...") {
  invisible(readline(prompt = msg))
}

lire_entier <- function(msg, min = 0, max = Inf) {
  repeat {
    saisie <- readline(prompt = msg)
    val <- suppressWarnings(as.numeric(saisie))
    if (!is.na(val) && val >= min && val <= max && val == round(val)) {
      return(as.integer(val))
    }
    cat(sprintf("Valeur invalide (entier attendu entre %s et %s). Recommence.\n",
                format(min), format(max)))
  }
}

lire_proba <- function(msg) {
  repeat {
    saisie <- readline(prompt = msg)
    val <- suppressWarnings(as.numeric(saisie))
    if (!is.na(val) && val > 0 && val < 1) return(val)
    cat("La probabilite doit etre un nombre strictement compris entre 0 et 1. Recommence.\n")
  }
}

lire_lambda <- function(msg) {
  repeat {
    saisie <- readline(prompt = msg)
    val <- suppressWarnings(as.numeric(saisie))
    if (!is.na(val) && val > 0) return(val)
    cat("Lambda doit etre un nombre strictement positif. Recommence.\n")
  }
}

# Boucle generique : trace un graphique, puis propose de rejouer un
# nouveau tirage (r), de changer les parametres (p) ou de revenir au
# menu principal (m). Permet de montrer en direct la variabilite
# d'echantillonnage (deux tirages avec les memes parametres ne donnent
# jamais exactement la meme frequence empirique).
boucle_resultat <- function(dessiner) {
  repeat {
    dessiner()
    cat("\n[r] Nouveau tirage (memes parametres)   [p] Changer les parametres   [m] Retour au menu\n")
    suite <- tolower(trimws(readline(prompt = "Ton choix : ")))
    if (suite == "p") return("parametres")
    if (suite == "m") return("menu")
    # toute autre entree (y compris "r" ou vide) relance un tirage
  }
}

# ---------------------------------------------------------------------
# 3. Loi de Bernoulli B(p)
# ---------------------------------------------------------------------

tracer_bernoulli <- function(p, n_sim) {
  tirages <- rbinom(n_sim, size = 1, prob = p)

  freq_emp   <- c(mean(tirages == 0), mean(tirages == 1))
  proba_theo <- c(1 - p, p)

  moyenne_theo    <- p
  ecart_type_theo <- sqrt(p * (1 - p))
  moyenne_emp     <- mean(tirages)
  ecart_type_emp  <- sd(tirages)

  par_projection()
  ymax <- max(proba_theo, freq_emp) * 1.25
  barplot(rbind(proba_theo, freq_emp), beside = TRUE,
          names.arg = c("0 (echec)", "1 (succes)"),
          col = c(COULEUR_THEO, COULEUR_EMP), border = NA,
          ylim = c(0, ymax),
          main = sprintf("Loi de Bernoulli B(%.3g)  -  %d tirages simules", p, n_sim),
          xlab = "Issue", ylab = "Probabilite / Frequence")
  legend("topright", legend = c("Theorique", "Simule"),
         fill = c(COULEUR_THEO, COULEUR_EMP), border = NA, bty = "n", cex = 1.2)

  cat("\n")
  cat(sprintf("Esperance   theorique E(X) = %.4f   |   moyenne simulee    = %.4f\n",
              moyenne_theo, moyenne_emp))
  cat(sprintf("Ecart-type  theorique s(X) = %.4f   |   ecart-type simule  = %.4f\n",
              ecart_type_theo, ecart_type_emp))
}

simuler_bernoulli <- function() {
  repeat {
    cat("\n--- Loi de Bernoulli B(p) ---\n")
    p     <- lire_proba("Probabilite de succes p (entre 0 et 1) : ")
    n_sim <- lire_entier("Nombre de tirages a simuler : ", min = 1, max = 1000000)
    action <- boucle_resultat(function() tracer_bernoulli(p, n_sim))
    if (action == "menu") return(invisible(NULL))
  }
}

# ---------------------------------------------------------------------
# 4. Loi Binomiale B(n ; p)
# ---------------------------------------------------------------------

tracer_binomiale <- function(n, p, n_sim) {
  tirages <- rbinom(n_sim, size = n, prob = p)
  k <- 0:n
  proba_theo <- dbinom(k, size = n, prob = p)
  freq_emp   <- as.numeric(table(factor(tirages, levels = k))) / n_sim

  moyenne_theo    <- n * p
  ecart_type_theo <- sqrt(n * p * (1 - p))
  moyenne_emp     <- mean(tirages)
  ecart_type_emp  <- sd(tirages)

  par_projection()
  ymax <- max(proba_theo, freq_emp) * 1.25
  bp <- barplot(freq_emp, names.arg = k,
                col = adjustcolor(COULEUR_EMP, alpha.f = 0.55), border = NA,
                ylim = c(0, ymax),
                main = sprintf("Loi Binomiale B(%d ; %.3g)  -  %d tirages simules", n, p, n_sim),
                xlab = "k (nombre de succes)", ylab = "Probabilite / Frequence")
  segments(bp, 0, bp, proba_theo, col = COULEUR_THEO, lwd = 3)
  points(bp, proba_theo, pch = 19, col = COULEUR_THEO, cex = 1.4)
  legend("topright", legend = c("Frequence simulee", "Probabilite theorique"),
         fill = c(adjustcolor(COULEUR_EMP, alpha.f = 0.55), NA),
         border = c(NA, NA), pch = c(NA, 19), col = c(NA, COULEUR_THEO),
         lty = c(NA, 1), lwd = c(NA, 3), bty = "n", cex = 1.1)

  cat("\n")
  cat(sprintf("Esperance   theorique E(X) = %.4f   |   moyenne simulee    = %.4f\n",
              moyenne_theo, moyenne_emp))
  cat(sprintf("Ecart-type  theorique s(X) = %.4f   |   ecart-type simule  = %.4f\n",
              ecart_type_theo, ecart_type_emp))
}

simuler_binomiale <- function(n_fixe = NULL, p_fixe = NULL) {
  repeat {
    cat("\n--- Loi Binomiale B(n ; p) ---\n")
    n <- if (!is.null(n_fixe)) n_fixe else lire_entier("Nombre d'epreuves n : ", min = 1, max = 100000)
    p <- if (!is.null(p_fixe)) p_fixe else lire_proba("Probabilite de succes p (entre 0 et 1) : ")
    n_sim <- lire_entier("Nombre de repetitions du schema a simuler : ", min = 1, max = 1000000)
    action <- boucle_resultat(function() tracer_binomiale(n, p, n_sim))
    if (action == "menu") return(invisible(NULL))
    if (!is.null(n_fixe) || !is.null(p_fixe)) return(invisible(NULL))  # exemple fige : pas de re-saisie de n/p
  }
}

# ---------------------------------------------------------------------
# 5. Loi de Poisson P(lambda)
# ---------------------------------------------------------------------

tracer_poisson <- function(lambda, n_sim) {
  tirages <- rpois(n_sim, lambda = lambda)
  k_max <- max(10, ceiling(lambda + 5 * sqrt(lambda)))
  k <- 0:k_max
  proba_theo <- dpois(k, lambda = lambda)
  freq_emp   <- as.numeric(table(factor(tirages, levels = k))) / n_sim

  moyenne_theo    <- lambda
  ecart_type_theo <- sqrt(lambda)
  moyenne_emp     <- mean(tirages)
  ecart_type_emp  <- sd(tirages)

  par_projection()
  ymax <- max(proba_theo, freq_emp) * 1.25
  bp <- barplot(freq_emp, names.arg = k,
                col = adjustcolor(COULEUR_EMP, alpha.f = 0.55), border = NA,
                ylim = c(0, ymax),
                main = sprintf("Loi de Poisson P(%.3g)  -  %d periodes simulees", lambda, n_sim),
                xlab = "k (nombre d'evenements)", ylab = "Probabilite / Frequence")
  segments(bp, 0, bp, proba_theo, col = COULEUR_THEO, lwd = 3)
  points(bp, proba_theo, pch = 19, col = COULEUR_THEO, cex = 1.4)
  legend("topright", legend = c("Frequence simulee", "Probabilite theorique"),
         fill = c(adjustcolor(COULEUR_EMP, alpha.f = 0.55), NA),
         border = c(NA, NA), pch = c(NA, 19), col = c(NA, COULEUR_THEO),
         lty = c(NA, 1), lwd = c(NA, 3), bty = "n", cex = 1.1)

  cat("\n")
  cat(sprintf("Esperance   theorique E(X) = %.4f   |   moyenne simulee    = %.4f\n",
              moyenne_theo, moyenne_emp))
  cat(sprintf("Ecart-type  theorique s(X) = %.4f   |   ecart-type simule  = %.4f\n",
              ecart_type_theo, ecart_type_emp))
}

simuler_poisson <- function(lambda_fixe = NULL) {
  repeat {
    cat("\n--- Loi de Poisson P(lambda) ---\n")
    lambda <- if (!is.null(lambda_fixe)) lambda_fixe else lire_lambda("Parametre lambda (nombre moyen d'evenements) : ")
    n_sim  <- lire_entier("Nombre de periodes a simuler : ", min = 1, max = 1000000)
    action <- boucle_resultat(function() tracer_poisson(lambda, n_sim))
    if (action == "menu") return(invisible(NULL))
    if (!is.null(lambda_fixe)) return(invisible(NULL))  # exemple fige : pas de re-saisie de lambda
  }
}

# ---------------------------------------------------------------------
# 6. Comparaison Binomiale / Poisson (approximation "loi des petits nombres")
# ---------------------------------------------------------------------

tracer_comparaison <- function(n, p) {
  lambda <- n * p
  k_max <- min(n, max(10, ceiling(lambda + 5 * sqrt(lambda))))
  k <- 0:k_max
  proba_binom   <- dbinom(k, size = n, prob = p)
  proba_poisson <- dpois(k, lambda = lambda)

  par_projection()
  ymax <- max(proba_binom, proba_poisson) * 1.25
  bp <- barplot(proba_binom, names.arg = k,
                col = adjustcolor(COULEUR_THEO, alpha.f = 0.55), border = NA,
                ylim = c(0, ymax),
                main = sprintf("B(%d ; %.3g)  vs  P(np)  -  lambda = np = %.3g", n, p, lambda),
                xlab = "k", ylab = "Probabilite")
  points(bp, proba_poisson, pch = 19, col = COULEUR_ACCENT, cex = 1.3)
  lines(bp, proba_poisson, col = COULEUR_ACCENT, lwd = 3)
  legend("topright", legend = c("Binomiale B(n ; p)", "Poisson P(np)"),
         fill = c(adjustcolor(COULEUR_THEO, alpha.f = 0.55), NA),
         border = c(NA, NA), pch = c(NA, 19), col = c(NA, COULEUR_ACCENT),
         lty = c(NA, 1), lwd = c(NA, 3), bty = "n", cex = 1.1)

  ecart_max <- max(abs(proba_binom - proba_poisson))

  cat("\n")
  if (n >= 30 && p <= 0.1 && lambda <= 10) {
    cat("Conditions d'approximation verifiees (n >= 30, p <= 0,1, lambda = np <= 10).\n")
  } else {
    cat("Attention : conditions d'approximation NON verifiees (n >= 30, p <= 0,1, lambda = np <= 10).\n")
    cat(sprintf("Ici : n = %d, p = %.4f, lambda = %.4f. L'approximation peut etre mediocre.\n", n, p, lambda))
  }
  cat(sprintf("Ecart maximal entre les deux lois sur k = 0..%d : %.6f\n", k_max, ecart_max))
  cat(sprintf("Esperance (les deux lois)   = %.4f\n", lambda))
  cat(sprintf("Ecart-type Binomiale        = %.4f\n", sqrt(n * p * (1 - p))))
  cat(sprintf("Ecart-type Poisson          = %.4f\n", sqrt(lambda)))
}

comparer_binomiale_poisson <- function() {
  repeat {
    cat("\n--- Approximation Binomiale -> Poisson ---\n")
    n <- lire_entier("Nombre d'epreuves n (grand, ex : >= 30) : ", min = 1, max = 1000000)
    p <- lire_proba("Probabilite de succes p (petite, ex : <= 0,1) : ")
    cat("\n[r] Redessiner   [p] Changer les parametres   [m] Retour au menu\n")
    repeat {
      tracer_comparaison(n, p)
      cat("\n[p] Changer les parametres   [m] Retour au menu\n")
      suite <- tolower(trimws(readline(prompt = "Ton choix : ")))
      if (suite == "p") break
      if (suite == "m") return(invisible(NULL))
    }
  }
}

# ---------------------------------------------------------------------
# 7. Exemples directement issus du support de cours (reproductibles)
# ---------------------------------------------------------------------
# Ces deux exemples reprennent exactement les parametres des diagrammes
# en batons du support MOD08_Lois_Discretes_Cours_v1_0.pdf :
#   - Binomiale B(10 ; 0,3)  : E(X) = 3, sigma(X) ~ 1,45   (page 8)
#   - Poisson   P(3)         : E(X) = 3, sigma(X) ~ 1,73   (page 10)
# Utile pour projeter d'abord la version theorique du cours, puis
# montrer la convergence de la simulation vers cette meme courbe.

exemples_du_cours <- function() {
  repeat {
    cat("\n==============================================\n")
    cat(" Exemples repris du support de cours MOD08\n")
    cat("==============================================\n")
    cat("1. Binomiale B(10 ; 0,3)  (cf. support, page 8)\n")
    cat("2. Poisson P(3)           (cf. support, page 10)\n")
    cat("0. Retour au menu\n")
    choix <- trimws(readline(prompt = "Ton choix : "))
    if (choix == "1") simuler_binomiale(n_fixe = 10L, p_fixe = 0.3)
    else if (choix == "2") simuler_poisson(lambda_fixe = 3)
    else if (choix == "0") return(invisible(NULL))
    else cat("Choix invalide.\n")
  }
}

# ---------------------------------------------------------------------
# 8. Menu principal
# ---------------------------------------------------------------------

menu_principal <- function() {
  repeat {
    cat("\n==============================================\n")
    cat(" MOD08 - Simulation des lois discretes\n")
    cat(" Pole Formation UIMM Centre-Val de Loire\n")
    cat("==============================================\n")
    cat("1. Loi de Bernoulli B(p)\n")
    cat("2. Loi Binomiale B(n ; p)\n")
    cat("3. Loi de Poisson P(lambda)\n")
    cat("4. Comparaison Binomiale / Poisson (approximation)\n")
    cat("5. Exemples repris du support de cours\n")
    cat("0. Quitter\n")
    choix <- trimws(readline(prompt = "Ton choix : "))

    if (choix == "1") simuler_bernoulli()
    else if (choix == "2") simuler_binomiale()
    else if (choix == "3") simuler_poisson()
    else if (choix == "4") comparer_binomiale_poisson()
    else if (choix == "5") exemples_du_cours()
    else if (choix == "0") { cat("Fin.\n"); break }
    else cat("Choix invalide.\n")
  }
}

# ---------------------------------------------------------------------
# Lancement automatique en session interactive (RStudio)
# ---------------------------------------------------------------------

if (interactive()) {
  cat("\nScript charge. Tape menu_principal() si le menu ne s'affiche pas.\n")
  menu_principal()
}
