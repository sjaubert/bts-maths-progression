# ==========================================================================
# MOD08 - Simulation interactive des lois discretes (application Shiny)
# Pole Formation UIMM Centre-Val de Loire - BTS Industriel - Mathematiques
# Auteur : S. Jaubert
#
# Objectif
# --------
# Simuler des donnees selon les lois de Bernoulli, Binomiale et Poisson,
# et les representer graphiquement en temps reel via des curseurs.
# Chaque graphique superpose :
#   - la frequence empirique obtenue par simulation (barres orange),
#   - la probabilite theorique donnee par la loi (points/segments bleus).
#
# Utilisation
# -----------
# 1. Ouvrir ce fichier (app.R) dans RStudio.
# 2. Cliquer sur "Run App" (ou executer : shiny::runApp("app.R")).
# 3. Un serveur R local demarre et l'application s'ouvre dans le navigateur
#    ou dans la fenetre RStudio.
#
# Packages requis : shiny (et bslib, installe automatiquement avec shiny).
# Installation si besoin : install.packages("shiny")
# ==========================================================================

library(shiny)

# ---------------------------------------------------------------------
# 1. Constantes graphiques
# ---------------------------------------------------------------------

COULEUR_THEO   <- "#1B4F72"  # bleu fonce : probabilite theorique
COULEUR_EMP    <- "#E67E22"  # orange : frequence simulee
COULEUR_ACCENT <- "#C0392B"  # rouge : loi de comparaison (Poisson)

theme_uimm <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1B4F72",
  secondary = "#E67E22"
)

par_graphique <- function() {
  par(cex.main = 1.6, cex.lab = 1.25, cex.axis = 1.1, font.main = 2,
      mar = c(5, 5, 4, 2), lwd = 2)
}

# ---------------------------------------------------------------------
# 2. Interface utilisateur (UI)
# ---------------------------------------------------------------------

ui <- fluidPage(
  theme = theme_uimm,

  titlePanel(
    div(
      style = "display:flex; align-items:baseline; gap:14px;",
      span("MOD08 — Simulation des lois discrètes", style = "font-weight:700;"),
      span("Pôle Formation UIMM Centre-Val de Loire", style = "font-size:0.55em; color:#7a7a7a;")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      selectInput(
        "loi", "Loi à simuler :",
        choices = c(
          "Bernoulli B(p)"                          = "bernoulli",
          "Binomiale B(n ; p)"                       = "binomiale",
          "Poisson P(lambda)"                        = "poisson",
          "Comparaison Binomiale / Poisson"          = "comparaison"
        )
      ),

      conditionalPanel(
        condition = "input.loi == 'bernoulli'",
        sliderInput("p_bernoulli", "Probabilité de succès p :",
                    min = 0.01, max = 0.99, value = 0.30, step = 0.01)
      ),

      conditionalPanel(
        condition = "input.loi == 'binomiale' || input.loi == 'comparaison'",
        sliderInput("n_binom", "Nombre d'épreuves n :",
                    min = 1, max = 300, value = 10, step = 1),
        sliderInput("p_binom", "Probabilité de succès p :",
                    min = 0.01, max = 0.99, value = 0.30, step = 0.01)
      ),

      conditionalPanel(
        condition = "input.loi == 'poisson'",
        sliderInput("lambda_poisson", "Paramètre lambda :",
                    min = 0.1, max = 40, value = 3, step = 0.1)
      ),

      conditionalPanel(
        condition = "input.loi != 'comparaison'",
        sliderInput("n_sim", "Nombre de tirages simulés :",
                    min = 10, max = 50000, value = 1000, step = 10)
      ),

      conditionalPanel(
        condition = "input.loi != 'comparaison'",
        actionButton("rejouer", "Nouveau tirage", icon = icon("dice"),
                     class = "btn-primary", width = "100%")
      ),

      conditionalPanel(
        condition = "input.loi == 'binomiale' || input.loi == 'poisson'",
        br(),
        actionButton("exemple_cours", "Charger l'exemple du cours",
                     class = "btn-outline-secondary", width = "100%")
      ),

      hr(),
      helpText(
        "Bleu = probabilité théorique. Orange = fréquence obtenue ",
        "par simulation. Chaque curseur ou clic sur «Nouveau tirage» ",
        "relance immédiatement le calcul."
      )
    ),

    mainPanel(
      width = 9,
      plotOutput("graphique", height = "560px"),
      div(style = "margin-top:10px;", verbatimTextOutput("stats"))
    )
  )
)

# ---------------------------------------------------------------------
# 3. Logique serveur
# ---------------------------------------------------------------------

server <- function(input, output, session) {

  # Bouton "Charger l'exemple du cours" : reprend les diagrammes du
  # support MOD08_Lois_Discretes_Cours_v1_0.pdf
  #   - Binomiale B(10 ; 0,3)  (page 8)
  #   - Poisson   P(3)         (page 10)
  observeEvent(input$exemple_cours, {
    if (input$loi == "binomiale") {
      updateSliderInput(session, "n_binom", value = 10)
      updateSliderInput(session, "p_binom", value = 0.3)
    } else if (input$loi == "poisson") {
      updateSliderInput(session, "lambda_poisson", value = 3)
    }
  })

  # Simulation reactive : depend des parametres ET du bouton "Nouveau
  # tirage", pour permettre de rejouer un tirage a parametres fixes.
  donnees <- reactive({
    input$rejouer  # dependance explicite (rejoue sans changer les parametres)

    if (input$loi == "bernoulli") {
      p <- input$p_bernoulli
      n_sim <- input$n_sim
      tirages <- rbinom(n_sim, size = 1, prob = p)
      list(type = "bernoulli", p = p, n_sim = n_sim, tirages = tirages)

    } else if (input$loi == "binomiale") {
      n <- input$n_binom
      p <- input$p_binom
      n_sim <- input$n_sim
      tirages <- rbinom(n_sim, size = n, prob = p)
      list(type = "binomiale", n = n, p = p, n_sim = n_sim, tirages = tirages)

    } else if (input$loi == "poisson") {
      lambda <- input$lambda_poisson
      n_sim <- input$n_sim
      tirages <- rpois(n_sim, lambda = lambda)
      list(type = "poisson", lambda = lambda, n_sim = n_sim, tirages = tirages)

    } else {
      list(type = "comparaison", n = input$n_binom, p = input$p_binom)
    }
  })

  output$graphique <- renderPlot({
    d <- donnees()
    par_graphique()

    if (d$type == "bernoulli") {

      freq_emp   <- c(mean(d$tirages == 0), mean(d$tirages == 1))
      proba_theo <- c(1 - d$p, d$p)
      ymax <- max(proba_theo, freq_emp) * 1.25

      barplot(rbind(proba_theo, freq_emp), beside = TRUE,
              names.arg = c("0 (échec)", "1 (succès)"),
              col = c(COULEUR_THEO, COULEUR_EMP), border = NA,
              ylim = c(0, ymax),
              main = sprintf("Loi de Bernoulli B(%.3g)  —  %d tirages simulés", d$p, d$n_sim),
              xlab = "Issue", ylab = "Probabilité / Fréquence")
      legend("topright", legend = c("Théorique", "Simulé"),
             fill = c(COULEUR_THEO, COULEUR_EMP), border = NA, bty = "n", cex = 1.1)

    } else if (d$type == "binomiale") {

      k <- 0:d$n
      proba_theo <- dbinom(k, size = d$n, prob = d$p)
      freq_emp   <- as.numeric(table(factor(d$tirages, levels = k))) / d$n_sim
      ymax <- max(proba_theo, freq_emp) * 1.25

      bp <- barplot(freq_emp, names.arg = k,
                    col = adjustcolor(COULEUR_EMP, alpha.f = 0.55), border = NA,
                    ylim = c(0, ymax),
                    main = sprintf("Loi Binomiale B(%d ; %.3g)  —  %d tirages simulés", d$n, d$p, d$n_sim),
                    xlab = "k (nombre de succès)", ylab = "Probabilité / Fréquence")
      segments(bp, 0, bp, proba_theo, col = COULEUR_THEO, lwd = 3)
      points(bp, proba_theo, pch = 19, col = COULEUR_THEO, cex = 1.3)
      legend("topright", legend = c("Fréquence simulée", "Probabilité théorique"),
             fill = c(adjustcolor(COULEUR_EMP, alpha.f = 0.55), NA),
             border = c(NA, NA), pch = c(NA, 19), col = c(NA, COULEUR_THEO),
             lty = c(NA, 1), lwd = c(NA, 3), bty = "n", cex = 1.05)

    } else if (d$type == "poisson") {

      k_max <- max(10, ceiling(d$lambda + 5 * sqrt(d$lambda)))
      k <- 0:k_max
      proba_theo <- dpois(k, lambda = d$lambda)
      freq_emp   <- as.numeric(table(factor(d$tirages, levels = k))) / d$n_sim
      ymax <- max(proba_theo, freq_emp) * 1.25

      bp <- barplot(freq_emp, names.arg = k,
                    col = adjustcolor(COULEUR_EMP, alpha.f = 0.55), border = NA,
                    ylim = c(0, ymax),
                    main = sprintf("Loi de Poisson P(%.3g)  —  %d périodes simulées", d$lambda, d$n_sim),
                    xlab = "k (nombre d'événements)", ylab = "Probabilité / Fréquence")
      segments(bp, 0, bp, proba_theo, col = COULEUR_THEO, lwd = 3)
      points(bp, proba_theo, pch = 19, col = COULEUR_THEO, cex = 1.3)
      legend("topright", legend = c("Fréquence simulée", "Probabilité théorique"),
             fill = c(adjustcolor(COULEUR_EMP, alpha.f = 0.55), NA),
             border = c(NA, NA), pch = c(NA, 19), col = c(NA, COULEUR_THEO),
             lty = c(NA, 1), lwd = c(NA, 3), bty = "n", cex = 1.05)

    } else {

      n <- d$n; p <- d$p
      lambda <- n * p
      k_max <- min(n, max(10, ceiling(lambda + 5 * sqrt(lambda))))
      k <- 0:k_max
      proba_binom   <- dbinom(k, size = n, prob = p)
      proba_poisson <- dpois(k, lambda = lambda)
      ymax <- max(proba_binom, proba_poisson) * 1.25

      bp <- barplot(proba_binom, names.arg = k,
                    col = adjustcolor(COULEUR_THEO, alpha.f = 0.55), border = NA,
                    ylim = c(0, ymax),
                    main = sprintf("B(%d ; %.3g)  vs  P(np)  —  lambda = np = %.3g", n, p, lambda),
                    xlab = "k", ylab = "Probabilité")
      points(bp, proba_poisson, pch = 19, col = COULEUR_ACCENT, cex = 1.25)
      lines(bp, proba_poisson, col = COULEUR_ACCENT, lwd = 3)
      legend("topright", legend = c("Binomiale B(n ; p)", "Poisson P(np)"),
             fill = c(adjustcolor(COULEUR_THEO, alpha.f = 0.55), NA),
             border = c(NA, NA), pch = c(NA, 19), col = c(NA, COULEUR_ACCENT),
             lty = c(NA, 1), lwd = c(NA, 3), bty = "n", cex = 1.05)
    }
  })

  output$stats <- renderPrint({
    d <- donnees()

    if (d$type == "bernoulli") {
      moyenne_theo <- d$p
      ecart_theo   <- sqrt(d$p * (1 - d$p))
      moyenne_emp  <- mean(d$tirages)
      ecart_emp    <- sd(d$tirages)
      cat(sprintf("Esperance   theorique E(X) = %.4f   |   moyenne simulee    = %.4f\n", moyenne_theo, moyenne_emp))
      cat(sprintf("Ecart-type  theorique s(X) = %.4f   |   ecart-type simule  = %.4f\n", ecart_theo, ecart_emp))

    } else if (d$type == "binomiale") {
      moyenne_theo <- d$n * d$p
      ecart_theo   <- sqrt(d$n * d$p * (1 - d$p))
      moyenne_emp  <- mean(d$tirages)
      ecart_emp    <- sd(d$tirages)
      cat(sprintf("Esperance   theorique E(X) = %.4f   |   moyenne simulee    = %.4f\n", moyenne_theo, moyenne_emp))
      cat(sprintf("Ecart-type  theorique s(X) = %.4f   |   ecart-type simule  = %.4f\n", ecart_theo, ecart_emp))

    } else if (d$type == "poisson") {
      moyenne_theo <- d$lambda
      ecart_theo   <- sqrt(d$lambda)
      moyenne_emp  <- mean(d$tirages)
      ecart_emp    <- sd(d$tirages)
      cat(sprintf("Esperance   theorique E(X) = %.4f   |   moyenne simulee    = %.4f\n", moyenne_theo, moyenne_emp))
      cat(sprintf("Ecart-type  theorique s(X) = %.4f   |   ecart-type simule  = %.4f\n", ecart_theo, ecart_emp))

    } else {
      n <- d$n; p <- d$p; lambda <- n * p
      k_max <- min(n, max(10, ceiling(lambda + 5 * sqrt(lambda))))
      k <- 0:k_max
      ecart_max <- max(abs(dbinom(k, n, p) - dpois(k, lambda)))
      if (n >= 30 && p <= 0.1 && lambda <= 10) {
        cat("Conditions d'approximation verifiees (n >= 30, p <= 0,1, lambda = np <= 10).\n")
      } else {
        cat("Attention : conditions d'approximation NON verifiees (n >= 30, p <= 0,1, lambda = np <= 10).\n")
      }
      cat(sprintf("Ecart maximal entre les deux lois sur k = 0..%d : %.6f\n", k_max, ecart_max))
      cat(sprintf("Esperance (les deux lois)   = %.4f\n", lambda))
      cat(sprintf("Ecart-type Binomiale        = %.4f\n", sqrt(n * p * (1 - p))))
      cat(sprintf("Ecart-type Poisson          = %.4f\n", sqrt(lambda)))
    }
  })
}

# ---------------------------------------------------------------------
# 4. Lancement de l'application
# ---------------------------------------------------------------------

shinyApp(ui = ui, server = server)
