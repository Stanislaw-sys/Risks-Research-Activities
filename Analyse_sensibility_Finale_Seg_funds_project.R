# =============================================================================
# ANALYSE DE SENSIBILITÉ COMPLÈTE (7 FACTEURS)
# =============================================================================
# Couvre :
# 1. Mortalité (Stress Additif)
# 2. Déchéance (Stress Additif)
# 3. Frais (MER)
# 4. Taux d'intérêt
# 5. Marchés boursiers (Volatilité)
# 6. Durée du contrat
# 7. Taux de retrait
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# Sécurité : Vérifier que les fonctions de simulation sont chargées
# Sinon, il faut les exécuter dans le fichier de script principal Notebook_version_2025_12_16.R
if (!exists("simulate_GMWB"))
  stop("Veuillez charger les fonctions de simulation (simulate_GMWB, etc.) d'abord.")

# -----------------------------------------------------------------------------
# 0. PARAMETRES INITIALS
# -----------------------------------------------------------------------------

# Seed global pour reproductibilité
seed <- 2025
set.seed(seed)

cat("=== INITIALISATION DES PARAMÈTRES ===\n")
cat("Seed utilisé: 2025\n\n")

# Paramètres de marché
A_0 <- 100000
S_0 <- 1
r_0 <- 0.05
sigma_S <- 0.2  # Volatilité ANNUELLE

# Paramètres Vasicek Canada
a <-  0.068736
b <-  0.044013
sigma <- 0.01426517

# Paramètres démographiques
entry_Age <- 55
retirement_Age <- 65
max_Age <- 115
withdrawal_Years <- 30

# Table de mortalité
mortality_rate <- readRDS("Donnees_importees/MortalityRate.RDS")

# Taux de déchéance
lapse_rates <- list(GMWB = 0.078, GLWB = 0.034, GMIB = 0.039)
withdrawal_Rate <- 0.03

#---------------------------------------------
# Calcul de la rente viagère à 65 ans, s = 0
#---------------------------------------------
T_r <- 115 - retirement_Age
ages_Qx <- seq(40, 115, 1)
params <- list(
  a = a,
  b = b,
  sigma = sigma,
  max_Age = 115
)
a65_t0 <- a_x_s(
  x = retirement_Age,
  s = 0,
  Qx = mortality_rate,
  r_s = r_0,
  # taux court initial
  Obligation_fun = theoretical_Bond_Price,
  a = a,
  b = b,
  sigma = sigma,
  max_Age = 115
  #params = params
)

(guaranteed_Income_Rate <- 1 / a65_t0) # = 0.06940535

phi <- 0.02
nb_Sim <- 10000  # Réduit pour tester, remettez à 10000 pour les résultats finaux

# -----------------------------------------------------------------------------
# 1. MOTEUR DE SENSIBILITÉ
# -----------------------------------------------------------------------------

# A. Mise à jour des paramètres
update_parameters_harmonized <- function(param_name, value)
{
  # Valeurs par défaut (Globales)
  cfg <- list(
    sigma_S = if (exists("sigma_S"))
      sigma_S
    else
      0.2,
    r_0 = if (exists("r_0"))
      r_0
    else
      0.05,
    phi = if (exists("phi"))
      phi
    else
      0.02,
    withdrawal_Rate = if (exists("withdrawal_Rate"))
      withdrawal_Rate
    else
      0.04,
    entry_Age = if (exists("entry_Age"))
      entry_Age
    else
      55,
    withdrawal_Years = if (exists("withdrawal_Years"))
      withdrawal_Years
    else
      30,
    max_Age = if (exists("max_Age"))
      max_Age
    else
      115,
    # Stress additifs (0 par défaut)
    stress_mortality = 0,
    stress_lapse = 0
  )
  
  # Logique de mise à jour
  switch(
    param_name,
    "Volatilité"       = {
      cfg$sigma_S <- value
    },
    "Taux d'intérêt"   = {
      cfg$r_0 <- value
    },
    "Frais (MER)"      = {
      cfg$phi <- value
    },
    "Taux de retrait"  = {
      cfg$withdrawal_Rate <- value
    },
    "Durée du contrat" = {
      cfg$withdrawal_Years <- value
      # Pour le GLWB/GMIB, l'horizon max doit être cohérent
      cfg$max_Age <- cfg$entry_Age + (retirement_Age - cfg$entry_Age) + value
    },
    "Stress Mortalité" = {
      cfg$stress_mortality <- value
    },
    "Stress Déchéance" = {
      cfg$stress_lapse <- value
    },
    stop(paste("Paramètre inconnu :", param_name))
  )
  return(cfg)
}

# B. Création des tables (Stress Additif)
create_tables_harmonized <- function(entry_Age,
                                     product_type,
                                     stress_mort,
                                     stress_lapse)
{
  base_lapse <- switch(
    product_type,
    "GMWB" = lapse_rates$GMWB,
    "GLWB" = lapse_rates$GLWB,
    "GMIB" = lapse_rates$GMIB
  )
  
  Age_id <- paste("Age", entry_Age)
  start_idx <- which(names(mortality_rate) == Age_id)
  if (length(start_idx) == 0)
    start_idx <- 1
  q_x_d_base <- as.numeric(mortality_rate[start_idx:length(mortality_rate)])
  q_x_w_base <- rep(base_lapse, length(q_x_d_base))
  
  # Application Stress Additif (Borné 0-1)
  q_x_d_adj <- pmax(pmin(q_x_d_base + stress_mort, 0.999), 0)
  q_x_w_adj <- pmax(pmin(q_x_w_base + stress_lapse, 0.999), 0)
  
  return(list(q_x_d = q_x_d_adj, q_x_w = q_x_w_adj))
}

# C. Compilation des résultats
compile_results_harmonized <- function(results_list, param_name) {
  if (length(results_list) == 0)
    return(NULL)
  df_rows <- list()
  for (key in names(results_list)) {
    item <- results_list[[key]]
    val <- item$param_value
    res <- item$results
    get_mean <- function(obj)
      if (is.list(obj))
        obj$mean_Value
    else
      obj
    
    if (!is.null(res$GMWB))
      df_rows[[length(df_rows) + 1]] <- data.frame(
        Parameter = param_name,
        Value = val,
        Product = "GMWB",
        Mean_Value = get_mean(res$GMWB),
        stringsAsFactors = F
      )
    if (!is.null(res$GLWB))
      df_rows[[length(df_rows) + 1]] <- data.frame(
        Parameter = param_name,
        Value = val,
        Product = "GLWB",
        Mean_Value = get_mean(res$GLWB),
        stringsAsFactors = F
      )
    if (!is.null(res$GMIB))
      df_rows[[length(df_rows) + 1]] <- data.frame(
        Parameter = param_name,
        Value = val,
        Product = "GMIB",
        Mean_Value = get_mean(res$GMIB),
        stringsAsFactors = F
      )
  }
  return(do.call(rbind, df_rows))
}

# D. Wrapper d'exécution
run_sensitivity_harmonized <- function(param_name, param_values, nb_sim = 10000)
{
  cat(sprintf("Calcul : %s...\n", param_name))
  results_list <- list()
  
  for (val in param_values) {
    if (abs(val) < 1e-6)
      val <- 0
    cfg <- update_parameters_harmonized(param_name, val)
    
    # Durées
    yrs_ret <- retirement_Age - cfg$entry_Age
    tot_gmwb <- cfg$withdrawal_Years + yrs_ret
    tot_glwb <- cfg$max_Age - cfg$entry_Age
    tot_gmib <- yrs_ret
    
    prod_res <- list()
    tbl <- create_tables_harmonized(cfg$entry_Age,
                                    "GMWB",
                                    cfg$stress_mortality,
                                    cfg$stress_lapse)
    
    # GMWB
    if (length(tbl$q_x_d) >= tot_gmwb)
      prod_res$GMWB <- simulate_GMWB(
        A_0,
        S_0,
        cfg$r_0,
        cfg$entry_Age,
        retirement_Age,
        cfg$withdrawal_Years,
        a,
        b,
        sigma,
        cfg$sigma_S,
        nb_sim,
        cfg$phi,
        tbl$q_x_d[1:tot_gmwb],
        tbl$q_x_w[1:tot_gmwb],
        cfg$withdrawal_Rate,
        2025
      )
    
    # GLWB
    tbl_glwb <- create_tables_harmonized(cfg$entry_Age,
                                         "GLWB",
                                         cfg$stress_mortality,
                                         cfg$stress_lapse)
    if (length(tbl_glwb$q_x_d) >= tot_glwb)
      prod_res$GLWB <- simulate_GLWB(
        A_0,
        S_0,
        cfg$r_0,
        cfg$entry_Age,
        retirement_Age,
        cfg$max_Age,
        a,
        b,
        sigma,
        cfg$sigma_S,
        nb_sim,
        cfg$phi,
        tbl_glwb$q_x_d[1:tot_glwb],
        tbl_glwb$q_x_w[1:tot_glwb],
        cfg$withdrawal_Rate,
        2025
      )
    
    # GMIB (g calcule implicitement avec r_0 et q_x_d stresses)
    tbl_gmib <- create_tables_harmonized(cfg$entry_Age,
                                         "GMIB",
                                         cfg$stress_mortality,
                                         cfg$stress_lapse)
    if (length(tbl_gmib$q_x_d) >= tot_gmib)
      prod_res$GMIB <- simulate_GMIB_modular(
        A_0,
        S_0,
        cfg$r_0,
        cfg$entry_Age,
        retirement_Age,
        a,
        b,
        sigma,
        cfg$sigma_S,
        nb_sim,
        cfg$phi,
        tbl_gmib$q_x_d[1:tot_gmib],
        tbl_gmib$q_x_w[1:tot_gmib],
        12,
        cfg$max_Age,
        2025,
        theoretical_Bond_Price
      )
    
    results_list[[as.character(val)]] <- list(param_value = val, results =
                                                prod_res)
  }
  return(compile_results_harmonized(results_list, param_name))
}

# -----------------------------------------------------------------------------
# 2. EXÉCUTION DES 7 ANALYSES
# -----------------------------------------------------------------------------
nb_sim <- 10000 # Augmenter pour le rapport final

# 1. Mortalité (Stress Additif : -2% à +2%)
df_mort <- run_sensitivity_harmonized("Stress Mortalité", seq(-0.02, 0.02, by =
                                                                0.005), nb_sim)

# 2. Déchéance (Stress Additif : -5% à +5%)
df_laps <- run_sensitivity_harmonized("Stress Déchéance", seq(-0.05, 0.05, by =
                                                                0.01), nb_sim)

# Ici, je ramène les taux de déchéance qui deviennent négatifs après le stress à 0

# 3. Frais (MER) (0.5% à 3.5%)
df_fees <- run_sensitivity_harmonized("Frais (MER)", seq(0.005, 0.035, by =
                                                           0.005), nb_sim)

# 4. Taux d'intérêt (1% à 9%)
df_rate <- run_sensitivity_harmonized("Taux d'intérêt", seq(0.01, 0.09, by =
                                                              0.01), nb_sim)

# 5. Marchés boursiers (Volatilité : 10% à 30%)
df_vol  <- run_sensitivity_harmonized("Volatilité", seq(0.10, 0.30, by =
                                                          0.05), nb_sim)

# 6. Durée du contrat (15 à 35 ans)
df_dur  <- run_sensitivity_harmonized("Durée du contrat", seq(15, 35, by =
                                                                5), nb_sim)

# 7. Taux de retrait (2% à 6%)
df_with <- run_sensitivity_harmonized("Taux de retrait", seq(0.02, 0.06, by =
                                                               0.01), nb_sim)

# Fusion globale
all_results <- rbind(df_mort, df_laps, df_fees, df_rate, df_vol, df_dur, df_with)

# -----------------------------------------------------------------------------
# 3. VISUALISATION
# -----------------------------------------------------------------------------
plot_sens <- function(data, p_name, x_lab) {
  ggplot(subset(data, Parameter == p_name),
         aes(x = Value, y = Mean_Value, color = Product)) +
    geom_line(linewidth = 1) + geom_point(size = 2) + theme_minimal() +
    labs(
      #title=paste("Sensibilité :", p_name),
      x = x_lab,
      y = "Valeur Risque-Neutre ($)"
    ) +
    scale_y_continuous(labels = scales::dollar)
}


plot_sens <- function(data, p_name, x_lab) {
  ggplot(subset(data, Parameter == p_name),
         aes(x = Value, y = Mean_Value, color = Product)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    theme_minimal() +
    labs(
      # title = paste("Sensibilité :", p_name),
      x = x_lab,
      y = "Valeur Risque-Neutre ($)",
      color = "Produit"   # <- titre de la légende
    ) +
    scale_y_continuous(labels = scales::dollar)
}


# Affichage des 7 graphiques
## NB : Les stress sur les taux de mortalité et de déchéance sont appliqués de
## manière additive. Les probabilités résultantes sont bornées dans l’intervalle
## [0,1]. Les valeurs négatives de stress correspondent à des scénarios de
## réduction du risque et n’impliquent jamais des probabilités négatives.

print(plot_sens(all_results, "Stress Mortalité", "Stress Additif (qx + stress)"))
print(plot_sens(all_results, "Stress Déchéance", "Stress Additif (qw + stress)"))
print(plot_sens(all_results, "Frais (MER)", "Frais annuels (phi)"))
print(plot_sens(all_results, "Taux d'intérêt", "Taux sans risque (r0)"))
print(plot_sens(all_results, "Volatilité", "Volatilité (sigma)"))
print(plot_sens(all_results, "Durée du contrat", "Années de retrait"))
print(plot_sens(all_results, "Taux de retrait", "Taux garanti"))


# =============================================================================
# SAUVEGARDE ET RAPPORT DE SYNTHÈSE (Harmonisé)
# =============================================================================

# Dossier de résultats
results_dir <- "sensitivity_analysis_results"
if (!dir.exists(results_dir))
  dir.create(results_dir)

# -----------------------------------------------------------------------------
# 1. SAUVEGARDE DES DONNÉES
# -----------------------------------------------------------------------------

# Sauvegarde globale (plus pratique qu'un fichier par paramètre)
saveRDS(all_results,
        file.path(results_dir, "complete_sensitivity_analysis.rds"))
cat(
  sprintf(
    "Résultats complets sauvegardés dans : %s/complete_sensitivity_analysis.rds\n",
    results_dir
  )
)

# Sauvegarde dans le .Rproject
params_list <- unique(all_results$Parameter)
for (p in params_list) {
  # Nettoyage du nom pour le fichier (enlève parenthèses et espaces)
  safe_name <- gsub("[^a-zA-Z0-9]", "_", p)
  sub_df <- subset(all_results, Parameter == p)
  saveRDS(sub_df, file.path(results_dir, paste0("sensitivity_", safe_name, ".rds")))
}

# -----------------------------------------------------------------------------
# 2. CALCUL DES ÉLASTICITÉS ET CLASSEMENT
# -----------------------------------------------------------------------------
# Note : Pour les Stress (base = 0), l'élasticité %/% ne fonctionne pas (division par 0).
# Nous calculons ici la "Sensibilité Normalisée" :
# Variation du prix ($) pour une variation standardisée du paramètre.

calculate_ranking <- function(df) {
  df %>%
    group_by(Parameter, Product) %>%
    summarise(
      # Valeur Min et Max observée dans la plage
      Min_Val = min(Mean_Value),
      Max_Val = max(Mean_Value),
      # Amplitude de l'impact ($)
      Impact_Range_Dollar = Max_Val - Min_Val,
      # Valeur moyenne centrale (approx)
      Avg_Val = mean(Mean_Value),
      .groups = 'drop'
    ) %>%
    # On normalise l'impact par rapport à la valeur moyenne du produit (Impact %)
    mutate(Relative_Impact = Impact_Range_Dollar / Avg_Val) %>%
    group_by(Parameter) %>%
    # On fait la moyenne des impacts sur les 3 produits pour classer le paramètre
    summarise(Global_Sensitivity = mean(Relative_Impact),
              .groups = 'drop') %>%
    arrange(desc(Global_Sensitivity))
}

ranking_df <- calculate_ranking(all_results)

# -----------------------------------------------------------------------------
# 3. GRAPHIQUE DE SYNTHÈSE (TORNADO CHART SIMPLIFIÉ)
# -----------------------------------------------------------------------------

p_rank <- ggplot(ranking_df, aes(x = reorder(Parameter, Global_Sensitivity), y = Global_Sensitivity)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + # Barres horizontales
  theme_minimal() +
  labs(
    title = "Classement des Risques (Impact Relatif Moyen)",
    subtitle = "Mesuré par l'amplitude (Max - Min) relative à la valeur du contrat",
    x = "Paramètre",
    y = "Sensibilité Globale (Impact %)"
  ) +
  scale_y_continuous(labels = scales::percent)

print(p_rank)
ggsave(
  file.path(results_dir, "ranking_sensitivity.png"),
  p_rank,
  width = 8,
  height = 6
)

# -----------------------------------------------------------------------------
# 4. RAPPORT TEXTUEL (Sortie Console)
# -----------------------------------------------------------------------------

cat("\n=== RAPPORT DE SYNTHÈSE ===\n")
cat("Classement des paramètres du plus impactant au moins impactant :\n\n")

for (i in 1:nrow(ranking_df)) {
  cat(
    sprintf(
      "%d. %s (Impact moyen : %.2f%%)\n",
      i,
      ranking_df$Parameter[i],
      ranking_df$Global_Sensitivity[i] * 100
    )
  )
}

cat("\nNote d'interprétation :\n")
cat("- Les 'Stress' (Mortalité/Déchéance) montrent l'impact maximal sur la plage testée.\n")
cat("- Ce classement combine l'impact sur les 3 produits (GMWB, GLWB, GMIB).\n")
cat("- Pour le GLWB spécifiquement, le risque de mortalité sera plus élevé que cette moyenne.\n")



# =============================================================================
# EXPORTATION DES GRAPHIQUES EN PDF (CARRÉS 7x7)
# =============================================================================


pdf_dir <- "Graphs_Sensibility_Analysis"

if (!dir.exists(pdf_dir)) {
  dir.create(pdf_dir, recursive = TRUE)
}

# Création du dossier pour les PDFs s'il n'existe pas
pdf_dir <- file.path(results_dir, "Graphs_Sensibility_Analysis")
if (!dir.exists(pdf_dir))
  dir.create(pdf_dir)

cat("\n=== EXPORTATION DES GRAPHIQUES EN PDF ===\n")

# 1. Export des 7 graphiques de sensibilité
params_list <- unique(all_results$Parameter)

for (p in params_list) {
  # Nettoyage du nom pour le fichier (enlève espaces et parenthèses)
  safe_name <- gsub("[^a-zA-Z0-9]", "_", p)
  filename <- file.path(pdf_dir, paste0("Sensibilite_", safe_name, ".pdf"))
  
  # Génération du graphique
  p_temp <- plot_sens(all_results, p, x_lab = switch(
    p,
    "Stress Mortalité" = "Stress Additif (qx + stress)",
    "Stress Déchéance" = "Stress Additif (qw + stress)",
    "Frais (MER)" = "Frais annuels (phi)",
    "Taux d'intérêt" = "Taux sans risque (r0)",
    "Volatilité" = "Volatilité (sigma)",
    "Durée du contrat" = "Années de retrait",
    "Taux de retrait" = "Taux garanti",
    p # Valeur par défaut
  ))
  
  # Sauvegarde en PDF carré (7x7)
  ggsave(
    filename,
    plot = p_temp,
    width = 7,
    height = 7,
    device = "pdf"
  )
  cat(sprintf("Sauvegardé : %s\n", filename))
}

# 2. Export du graphique de classement (Tornado)
if (exists("p_rank")) {
  ggsave(
    file.path(pdf_dir, "Classement_Risques.pdf"),
    plot = p_rank,
    width = 7,
    height = 7,
    device = "pdf"
  )
  cat("Sauvegardé : Classement_Risques.pdf\n")
}


# =============================================================================
# 4. ANALYSE : SENSIBILITÉ À L'ÂGE D'ADHÉSION
# =============================================================================
# On suppose que l'âge de la retraite reste fixe à 65 ans.
# Donc :
# Age 40 -> 25 ans d'accumulation
# Age 60 -> 5 ans d'accumulation

cat("\n=== ANALYSE : ÂGE D'ADHÉSION ===\n")

# Fonction wrapper spécifique pour gérer la durée variable d'accumulation
run_age_sensitivity <- function(ages_list)
{
  results_list <- list()
  param_name <- "Âge d'adhésion"
  
  for (age in ages_list) {
    # Recalcul de la durée d'accumulation (Retraite à 65 ans fixe)
    new_entry_Age <- age
    # Si on commence à 60 ans, la retraite est dans 5 ans.
    # On met à jour la config globale temporairement
    cfg <- update_parameters_harmonized("Taux d'intérêt", r_0) # Reset valeurs de base
    cfg$entry_Age <- new_entry_Age
    
    # Recalcul des durées basées sur cet âge
    yrs_ret <- retirement_Age - new_entry_Age
    tot_gmwb <- withdrawal_Years + yrs_ret
    tot_glwb <- max_Age - new_entry_Age
    tot_gmib <- yrs_ret
    
    prod_res <- list()
    tbl <- create_tables_harmonized(new_entry_Age, "GMWB", 0, 0)
    
    # Simulation (On passe les nouvelles durées)
    if (length(tbl$q_x_d) >= tot_gmwb)
      prod_res$GMWB <- simulate_GMWB(
        A_0,
        S_0,
        r_0,
        new_entry_Age,
        retirement_Age,
        withdrawal_Years,
        a,
        b,
        sigma,
        sigma_S,
        nb_sim,
        phi,
        tbl$q_x_d[1:tot_gmwb],
        tbl$q_x_w[1:tot_gmwb],
        withdrawal_Rate,
        2025
      )
    
    tbl_glwb <- create_tables_harmonized(new_entry_Age, "GLWB", 0, 0)
    if (length(tbl_glwb$q_x_d) >= tot_glwb)
      prod_res$GLWB <- simulate_GLWB(
        A_0,
        S_0,
        r_0,
        new_entry_Age,
        retirement_Age,
        max_Age,
        a,
        b,
        sigma,
        sigma_S,
        nb_sim,
        phi,
        tbl_glwb$q_x_d[1:tot_glwb],
        tbl_glwb$q_x_w[1:tot_glwb],
        withdrawal_Rate,
        2025
      )
    
    tbl_gmib <- create_tables_harmonized(new_entry_Age, "GMIB", 0, 0)
    if (length(tbl_gmib$q_x_d) >= tot_gmib)
      prod_res$GMIB <- simulate_GMIB_modular(
        A_0,
        S_0,
        r_0,
        new_entry_Age,
        retirement_Age,
        a,
        b,
        sigma,
        sigma_S,
        nb_sim,
        phi,
        tbl_gmib$q_x_d[1:tot_gmib],
        tbl_gmib$q_x_w[1:tot_gmib],
        12,
        max_Age,
        2025,
        theoretical_Bond_Price
      )
    
    results_list[[as.character(age)]] <- list(param_value = age, results =
                                                prod_res)
  }
  return(compile_results_harmonized(results_list, param_name))
}

# Test
ages_to_test <- seq(40, 60, 5)
df_age <- run_age_sensitivity(ages_to_test)

# Visualisation Âge
p_age <- plot_sens(df_age, "Âge d'adhésion", "Âge à la signature du contrat")
print(p_age)
saveRDS(df_age, file.path(results_dir, "sensitivity_age.rds"))


# =============================================================================
# 5. SCÉNARIOS COMPOSITES (STRESS TESTS)
# =============================================================================
cat("\n=== ANALYSE : SCÉNARIOS COMPOSITES ===\n")

# Définition des scénarios
run_composite_scenario <- function(scenario_name,
                                   r_val,
                                   sigma_val,
                                   stress_mort,
                                   stress_lapse)
{
  # 1. Mise à jour manuelle des paramètres pour le scénario
  cfg <- list(
    r_0 = r_val,
    sigma_S = sigma_val,
    stress_mortality = stress_mort,
    stress_lapse = stress_lapse,
    entry_Age = 55 # Base
  )
  
  # Durées standards
  yrs_ret <- retirement_Age - cfg$entry_Age
  tot_gmwb <- withdrawal_Years + yrs_ret
  tot_glwb <- max_Age - cfg$entry_Age
  tot_gmib <- yrs_ret
  
  prod_res <- list()
  
  # Simulation
  tbl <- create_tables_harmonized(cfg$entry_Age,
                                  "GMWB",
                                  cfg$stress_mortality,
                                  cfg$stress_lapse)
  prod_res$GMWB <- simulate_GMWB(
    A_0,
    S_0,
    cfg$r_0,
    cfg$entry_Age,
    retirement_Age,
    withdrawal_Years,
    a,
    b,
    sigma,
    cfg$sigma_S,
    nb_sim,
    phi,
    tbl$q_x_d[1:tot_gmwb],
    tbl$q_x_w[1:tot_gmwb],
    withdrawal_Rate,
    2025
  )
  
  tbl_glwb <- create_tables_harmonized(cfg$entry_Age,
                                       "GLWB",
                                       cfg$stress_mortality,
                                       cfg$stress_lapse)
  prod_res$GLWB <- simulate_GLWB(
    A_0,
    S_0,
    cfg$r_0,
    cfg$entry_Age,
    retirement_Age,
    max_Age,
    a,
    b,
    sigma,
    cfg$sigma_S,
    nb_sim,
    phi,
    tbl_glwb$q_x_d[1:tot_glwb],
    tbl_glwb$q_x_w[1:tot_glwb],
    withdrawal_Rate,
    2025
  )
  
  tbl_gmib <- create_tables_harmonized(cfg$entry_Age,
                                       "GMIB",
                                       cfg$stress_mortality,
                                       cfg$stress_lapse)
  prod_res$GMIB <- simulate_GMIB_modular(
    A_0,
    S_0,
    cfg$r_0,
    cfg$entry_Age,
    retirement_Age,
    a,
    b,
    sigma,
    cfg$sigma_S,
    nb_sim,
    phi,
    tbl_gmib$q_x_d[1:tot_gmib],
    tbl_gmib$q_x_w[1:tot_gmib],
    12,
    max_Age,
    2025,
    theoretical_Bond_Price
  )
  
  # Formatage résultat
  df_res <- data.frame(
    Scenario = scenario_name,
    GMWB = prod_res$GMWB$mean_Value,
    GLWB = prod_res$GLWB$mean_Value,
    GMIB = prod_res$GMIB$mean_Value
  )
  return(df_res)
}

# Scénario 1 : "Crise Financière" (Taux bas, Volatilité haute)
# L'assureur souffre sur le hedging et sur l'actualisation
scen1 <- run_composite_scenario(
  "Crise Financière",
  r_val = 0.02,
  sigma_val = 0.30,
  stress_mort = 0,
  stress_lapse = 0
)

# Scénario 2 : "Rétention & Longévité" (Déchéance faible, Mortalité faible, Taux bas)
# Pire cas actuariel pour GLWB : Les gens vivent vieux, gardent leur contrat, et les taux sont bas.
scen2 <- run_composite_scenario(
  "Rétention & Longévité",
  r_val = 0.03,
  sigma_val = 0.20,
  stress_mort = -0.01,
  stress_lapse = -0.03
)

# Scénario Base (Pour comparaison)
scen_base <- run_composite_scenario(
  "Scénario de Base",
  r_val = 0.05,
  sigma_val = 0.20,
  stress_mort = 0,
  stress_lapse = 0
)

# Fusion
df_composite <- rbind(scen_base, scen1, scen2)
print(df_composite)
saveRDS(df_composite,
        file.path(results_dir, "composite_scenarios.rds"))


# =============================================================================
# 7. ANALYSES SPÉCIFIQUES : PÉRIODE D'ACCUMULATION
# =============================================================================
cat("\n=== ANALYSE : MÉTRIQUES D'ACCUMULATION ===\n")

# Analyse 1 : Probabilité que la Garantie soit "Dans la monnaie" (ITM) à la retraite
# C'est-à-dire : Prob(G_Tr > A_Tr). Si c'est le cas, l'option a de la valeur.
check_accumulation_metrics <- function(nb_sim = 10000)
{
  itm_count <- 0
  ruin_count <- 0
  sum_fees <- 0
  
  T_acc <- retirement_Age - entry_Age
  
  for (k in 1:nb_sim) {
    # Simulation simplifiée finale accumulation (Merton/BS direct pour rapidité)
    # S_T = S_0 * exp(...)
    growth_factor <- exp((r_0 - phi - 0.5 * sigma_S^2) * T_acc + sigma_S *
                           rnorm(1, 0, sqrt(T_acc)))
    A_final <- A_0 * growth_factor
    
    # Approx Step-up : Le max du brownien (formule analytique possible, mais simu ici)
    # On utilise une approximation conservatrice : G_Tr >= A_final.
    # Pour être précis, G_Tr est souvent > A_final grâce aux step-ups passés.
    # Ici on vérifie juste si le fonds a performé sous le capital garanti initial (A0)
    
    if (A_final < A_0)
      itm_count <- itm_count + 1 # Garantie plancher active
    
    # Frais payés (approx)
    fees_paid <- A_0 * (1 - exp(-phi * T_acc)) # Frais cumulés
    sum_fees <- sum_fees + fees_paid
  }
  
  return(list(
    Prob_ITM_Initial = itm_count / nb_sim,
    Avg_Fees_Paid = sum_fees / nb_sim
  ))
}

acc_metrics <- check_accumulation_metrics()
cat(
  sprintf(
    "Probabilité que le Fonds soit sous le Capital Initial à la retraite :
            %.2f%%\n",
    acc_metrics$Prob_ITM_Initial * 100
  )
)
cat(
  sprintf(
    "Frais moyens prélevés durant l'accumulation : $%.2f\n",
    acc_metrics$Avg_Fees_Paid
  )
)


# =============================================================================
# 6. GRAPHIQUE D'ÉVOLUTION DU FONDS (TRAJECTOIRES COMPARATIVES)
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr) # Pour pivot_longer

# Fonction de simulation d'une trajectoire unique pour un produit donné
simulate_single_trajectory <- function(product_type, seed = 2025, params)
{
  set.seed(seed)
  
  # Récupération des paramètres
  r <- params$r_0
  mu <- r # Risque-neutre
  sigma <- params$sigma_S
  phi <- params$phi
  dt <- 1 / 12 # Pas mensuel
  
  # Durées
  T_acc <- params$retirement_Age - params$entry_Age
  T_dec_max <- 35 # On observe 35 ans après la retraite (jusqu'à 100 ans)
  N_steps_acc <- T_acc * 12
  N_steps_dec <- T_dec_max * 12
  Total_steps <- N_steps_acc + N_steps_dec
  
  # Vecteurs
  Time <- seq(0, (Total_steps) * dt, by = dt)
  S_t <- numeric(length(Time))
  A_t <- numeric(length(Time))
  G_t <- numeric(length(Time)) # Base de garantie
  W_t <- numeric(length(Time)) # Flux (Retraits)
  
  # Initialisation
  S_t[1] <- params$S_0
  A_t[1] <- params$A_0
  G_t[1] <- params$A_0
  W_t[1] <- 0
  
  # --- BOUCLE TEMPORELLE ---
  for (i in 1:Total_steps) {
    # 1. Évolution du Marché (Geometric Brownian Motion)
    dW <- rnorm(1, mean = 0, sd = sqrt(dt))
    S_t[i + 1] <- S_t[i] * exp((r - 0.5 * sigma^2) * dt + sigma * dW)
    
    # Performance brute du fonds
    perf <- S_t[i + 1] / S_t[i]
    
    # 2. Gestion selon la phase
    t_curr <- i * dt
    is_accumulation <- (t_curr <= T_acc)
    
    if (is_accumulation) {
      # --- Phase ACCUMULATION ---
      # Croissance nette des frais
      A_t[i + 1] <- A_t[i] * perf * exp(-phi * dt)
      W_t[i + 1] <- 0
      
      # Step-up (Ratchet) Annuel
      if (i %% 12 == 0) {
        G_t[i + 1] <- max(G_t[i], A_t[i + 1])
      } else {
        G_t[i + 1] <- G_t[i]
      }
      
    } else {
      # --- Phase DÉCAISSEMENT ---
      # D'abord on applique la performance du marché
      fund_pre_withdrawal <- A_t[i] * perf * exp(-phi * dt)
      
      # Calcul du retrait selon le produit
      withdrawal <- 0
      
      if (product_type == "GMWB") {
        # Retrait fixe sur 20 ans (par exemple 5% de la base garantie à la retraite)
        rate <- 0.05
        withdrawal_yearly <- G_t[N_steps_acc + 1] * rate
        withdrawal <- withdrawal_yearly / 12
        
        # Arrêt si la période garantie est finie (ex: 20 ans)
        if (t_curr > T_acc + 20)
          withdrawal <- 0
        
        G_t[i + 1] <- max(G_t[i] - withdrawal, 0) # La base GMWB baisse avec les retraits
        
      } else if (product_type == "GLWB") {
        # Retrait viager (par exemple 4% à vie)
        rate <- 0.04
        withdrawal_yearly <- G_t[N_steps_acc + 1] * rate
        withdrawal <- withdrawal_yearly / 12
        
        # G_t reste constant (Base protégée à vie)
        G_t[i + 1] <- G_t[i]
        
      } else if (product_type == "GMIB") {
        # Annuitisation à la retraite
        # Le fonds A_t tombe à 0 (échangé contre la rente)
        # Le flux est la rente viagère
        
        # Calcul de la rente (simplifié pour le graph)
        # Supposons un taux de conversion g = 6% (garanti)
        rate <- 0.06
        annuity_yearly <- max(G_t[N_steps_acc + 1] * rate, 0) # Ou basée sur A_t si A_t > G_t
        withdrawal <- annuity_yearly / 12
        
        G_t[i + 1] <- 0 # Plus de base "de retrait", c'est une rente
        fund_pre_withdrawal <- 0 # Fonds liquidé
      }
      
      # Application du retrait sur le fonds
      if (product_type != "GMIB") {
        A_t[i + 1] <- max(fund_pre_withdrawal - withdrawal, 0)
      } else {
        A_t[i + 1] <- 0 # GMIB : Plus de fonds personnel après retraite
      }
      
      W_t[i + 1] <- withdrawal
    }
  }
  
  # Création du DataFrame
  df <- data.frame(
    Time = Time,
    Fund = A_t,
    Guarantee = G_t,
    Flow = W_t,
    Product = product_type
  )
  return(df)
}

# --- GÉNÉRATION DES DONNÉES ---
# Paramètres (Reprise des paramètres globales)
params_sim <- list(
  r_0 = 0.05,
  sigma_S = 0.2,
  phi = 0.02,
  A_0 = 100000,
  S_0 = 1,
  entry_Age = 55,
  retirement_Age = 65
)

# On utilise le même seed pour que la performance marché soit identique
seed_choice <- 2025

df_gmwb <- simulate_single_trajectory("GMWB", seed_choice, params_sim)
df_glwb <- simulate_single_trajectory("GLWB", seed_choice, params_sim)
df_gmib <- simulate_single_trajectory("GMIB", seed_choice, params_sim)

# Fusion pour le graphique comparatif
df_all <- rbind(df_gmwb, df_glwb, df_gmib)


# =============================================================================
# GRAPHIQUES INDIVIDUELS
# =============================================================================

plot_trajectory <- function(df, title_prod)
{
  # Pour le graph, on ne garde que Fund et Guarantee
  df_long <- df %>%
    select(Time, Fund, Guarantee) %>%
    pivot_longer(
      cols = c("Fund", "Guarantee"),
      names_to = "Variable",
      values_to = "Value"
    )
  
  p <- ggplot(df_long,
              aes(
                x = Time,
                y = Value,
                color = Variable,
                linetype = Variable
              )) +
    geom_line(linewidth = 1) +
    # Zone Accumulation
    annotate(
      "rect",
      xmin = 0,
      xmax = 10,
      ymin = -Inf,
      ymax = Inf,
      fill = "green",
      alpha = 0.1
    ) +
    geom_vline(xintercept = 10, linetype = "dashed") +
    annotate(
      "text",
      x = 5,
      y = max(df$Guarantee) * 1.1,
      label = "Accumulation",
      color = "darkgreen"
    ) +
    annotate(
      "text",
      x = 15,
      y = max(df$Guarantee) * 1.1,
      label = "Décaissement",
      color = "darkred"
    ) +
    labs(
      title = paste("Trajectoire Type :", title_prod),
      # subtitle = "Évolution du Compte (A_t) vs Base Garantie (G_t)",
      y = "Valeur ($)",
      x = "Année (0 = 55 ans)"
    ) +
    scale_color_manual(values = c("Fund" = "#1f77b4", "Guarantee" = "#d62728")) +
    scale_y_continuous(labels = scales::dollar) +
    theme_minimal()
  
  return(p)
}

p1 <- plot_trajectory(df_gmwb, "GMWB (Retraits finis)")
p2 <- plot_trajectory(df_glwb, "GLWB (Retraits à vie)")
p3 <- plot_trajectory(df_gmib, "GMIB (Annuitisation)")

# Affichage et Sauvegarde
print(p1)
print(p2)
print(p3)


# =============================================================================
# GRAPHIQUE COMPARATIF : ÉVOLUTION DES FLUX (A_t et W_t)
# =============================================================================
# Ce graphique compare comment le fonds se vide (A_t) et les revenus générés (W_t)

p_compare_fund <- ggplot(df_all, aes(x = Time, y = Fund, color = Product)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 10, linetype = "dashed") +
  annotate(
    "text",
    x = 5,
    y = 120000,
    label = "Accumulation\n(Identique)",
    color = "black",
    size = 3
  ) +
  annotate(
    "text",
    x = 20,
    y = 120000,
    label = "Décaissement\n(Divergent)",
    color = "black",
    size = 3
  ) +
  labs(
    title = "Comparaison de l'Épuisement du Fonds",
    # subtitle = "La même performance marché entraîne des soldes très différents",
    y = "Valeur du Compte ($)",
    x = "Nombre d'années",
    color = "Produit"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar)

p_compare_flow <- ggplot(df_all %>% filter(Time > 10),
                         aes(x = Time, y = Flow * 12, fill = Product)) + # Flow annuel
  geom_area(position = "identity", alpha = 0.4) + # Area pour voir les volumes
  geom_line(aes(color = Product), linewidth = 1) +
  labs(
    title = "Comparaison des Revenus Garantis (Annuel)",
    # subtitle = "GMIB offre souvent le revenu le plus élevé (via rente), GLWB le plus long",
    y = "Revenu Annuel ($)",
    x = "Nombre d'années",
    color = "Produit"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar)

print(p_compare_fund)
print(p_compare_flow)


# =============================================================================
# ANALYSE DE SENSIBILITÉ CIBLÉE : TAUX GARANTI (g) & TAUX DE RETRAIT
# =============================================================================

# 1. PARAMÉTRAGE DES VALEURS DE BASE
# -----------------------------------------------------------------------------
# On sauvegarde les valeurs initiales pour pouvoir les restaurer après l'analyse
base_g <- 0.06940535  #  valeur de base (1/a65_t0)
base_w <- 0.03        #  taux de retrait de base

# 2. MISE À JOUR DU MOTEUR DE SENSIBILITÉ
# -----------------------------------------------------------------------------
# On enrichit la fonction de mise à jour pour gérer 'g' et 'withdrawal_Rate'
update_parameters_extended <- function(param_name, value)
{
  # Configuration par défaut
  cfg <- list(
    r_0 = 0.05,
    sigma_S = 0.2,
    entry_Age = 55,
    retirement_Age = 65,
    max_Age = 115,
    withdrawal_Years = 30,
    phi = 0.02,
    # Paramètres cibles
    withdrawal_Rate = base_w,
    guaranteed_Rate = base_g
  )
  
  # Mise à jour spécifique
  if (param_name == "Taux Garanti (g)") {
    cfg$guaranteed_Rate <- value
  } else if (param_name == "Taux de Retrait") {
    cfg$withdrawal_Rate <- value
  }
  
  return(cfg)
}


# Fonction GMIM adaptée à l'analyse de sensibilité par rapport à g (forced_g)

simulate_GMIB_modular_se <- function(A_0,
                                  S_0,
                                  r_0,
                                  entry_Age,
                                  retirement_Age,
                                  a,
                                  b,
                                  sigma,
                                  sigma_S,
                                  nb_Sim,
                                  phi,
                                  q_x_d,
                                  q_x_w,
                                  nb_Time_Steps_R_per_year = 12,
                                  max_Age = 115,
                                  seed = 2025,
                                  theoretical_Bond_Price,
                                  forced_g = NULL)
{
  set.seed(seed)

  T_r <- retirement_Age - entry_Age
  g <- forced_g

  # Simulation
  sim <- simulation_Black_Scholes_01(
    S_0 = S_0,
    r_0 = r_0,
    t = T_r,
    nb_Time_Steps = T_r,
    nb_Time_Steps_R = nb_Time_Steps_R_per_year * T_r,
    a_R = a,
    b_R = b,
    sigma_R = sigma,
    sigma_S = sigma_S,
    nb_Sim = nb_Sim
  )

  St <- sim$St
  
  if (is.null(sim$list_Res_R)) {
    stop("simulation_Black_Scholes n'a pas renvoyé list_Res_R.")
  }
  
  r_path_annual <- sim$list_Res_R$mat_rt
  integral_rt_annual <- sim$list_Res_R$mat_Integral_rt
  
  #  FACTEURS D'ACTUALISATION
  discount_Factors <- exp(-integral_rt_annual)
  
  # Vérifier les NaN/Inf
  if (any(is.na(discount_Factors) |
          is.infinite(discount_Factors))) {
    warning("NaN ou Inf détectés dans discount_Factors. Remplacement par 0.")
    discount_Factors[is.na(discount_Factors) |
                       is.infinite(discount_Factors)] <- 0
  }
  
  # === 3) COMPTES ET GT (ACCUMULATION) ===
  At <- matrix(A_0, nrow = nb_Sim, ncol = T_r + 1)
  Gt <- matrix(A_0, nrow = nb_Sim, ncol = T_r + 1)
  
  for (year in 1:T_r) {
    # Éviter les divisions par zéro
    ratio <- St[, year + 1] / pmax(St[, year], 1e-10)
    ratio[is.na(ratio) | is.infinite(ratio)] <- 1
    
    At[, year + 1] <- At[, year] * exp(-phi) * ratio
    Gt[, year + 1] <- pmax(Gt[, year], At[, year + 1])
  }
  
  flows <- compute_flows(At, Gt, q_x_d, q_x_w)
  cash_Flows <- flows$cash_Flows
  
  # CALCUL DE g = 1/a65_t0
  
  if (is.na(a65_t0) || a65_t0 <= 0) {
    warning(sprintf(
      "a65_t0 invalide: %.6f. Utilisation de la valeur par défault.",
      a65_t0
    ))
    a65_t0 <- 10  # Valeur par défaut raisonnable
  }
  
  g_calc <- g
  
  r_Tr_vec <- r_path_annual[, T_r + 1]
  
  # Limiter les valeurs extrêmes
  r_Tr_vec <- pmin(pmax(r_Tr_vec, 0.001), 0.15)
  
  gmib <- compute_GMIB_payoff(
    At = At,
    Gt = Gt,
    r_Tr_vec = r_Tr_vec,
    q_x_d = q_x_d,
    Obligation_fun = theoretical_Bond_Price,
    a = a,
    b = b,
    sigma = sigma,
    guaranteed_Income_Rate = g_calc,
    retirement_Age = retirement_Age,
    max_Age = max_Age
  )
  
  annuity_Values_vec <- gmib$annuity_Values_vec
  annual_Payment <- gmib$annual_Payment
  final_Payment <- gmib$final_Payment
  
  # AJOUT DU FLUX FINAL
  cash_Flows[, T_r + 1] <- cash_Flows[, T_r + 1] + flows$survival_Probs[T_r +
                                                                          1] * final_Payment
  
  # Éviter les NaN dans cash_Flows
  cash_Flows[is.na(cash_Flows) | is.infinite(cash_Flows)] <- 0
  
  #  ACTUALISATION ET VALEURS
  # S'assurer que les dimensions correspondent
  min_cols <- min(ncol(discount_Factors), ncol(cash_Flows))
  discount_Factors <- discount_Factors[, 1:min_cols]
  cash_Flows <- cash_Flows[, 1:min_cols]
  
  discounted_CF <- discount_Factors * cash_Flows
  
  # Éviter les NaN dans le produit
  discounted_CF[is.na(discounted_CF) |
                  is.infinite(discounted_CF)] <- 0
  
  contract_Values <- rowSums(discounted_CF, na.rm = TRUE)
  
  # Vérification finale
  if (any(is.na(contract_Values) | is.infinite(contract_Values))) {
    warning("Certaines contract_Values sont encore NaN/Inf. Remplacement par 0.")
    contract_Values[is.na(contract_Values) |
                      is.infinite(contract_Values)] <- 0
  }
  
  #  MÉTRIQUES
  conversion_Rate <- mean(annuity_Values_vec > At[, T_r + 1], na.rm = TRUE)
  
  moneyness <- At / pmax(Gt, 1e-10)
  moneyness[!is.finite(moneyness)] <- 1
  
  total_death_flows <- rowSums(flows$death_Flows, na.rm = TRUE)
  total_lapse_flows <- rowSums(flows$lapse_Flows, na.rm = TRUE)
  
  # RETOUR
  list(
    values = contract_Values,
    mean_Value = mean(contract_Values, na.rm = TRUE),
    cash_Flows = cash_Flows,
    flows = flows,
    death_Flows = flows$death_Flows,
    lapse_Flows = flows$lapse_Flows,
    discount_Factors = discount_Factors,
    At = At,
    Gt = Gt,
    Moneyness = moneyness,
    annual_Payment = mean(annual_Payment, na.rm = TRUE),
    annuity_Values_vec = annuity_Values_vec,
    account_Value = At[, T_r + 1],
    conversion_Rate = conversion_Rate,
    annuity_Factors_vec = annuity_Values_vec / pmax(Gt[, T_r + 1] * g_calc, 1e-10),
    g = g_calc,
    total_death_flows = total_death_flows,
    total_lapse_flows = total_lapse_flows
  )
}


# 3. FONCTION D'EXÉCUTION (Wrapper)
# -----------------------------------------------------------------------------
run_special_sensitivity <- function(param_name, values_range, nb_Sim = 10000) {
  cat(sprintf(
    "\n=== Lancement de l'analyse de sensibilité pour : %s ===\n",
    param_name
  ))
  results_list <- list()
  
  # Valeurs de base (pour les paramètres qu'on ne fait pas varier)
  base_g <- 0.06940535  # Valeur de base pour GMIB
  base_w <- 0.03        # Valeur de base pour GMWB/GLWB
  
  # --- BOUCLE SUR LES VALEURS DU TEST ---
  for (val in values_range)
  {
    # 1. Déterminer les paramètres actifs pour cette itération
    # Si on teste 'g', on prend 'val', sinon on garde la base.
    current_g <- if (param_name == "Taux Garanti (g)")
      val
    else
      base_g
    
    # Si on teste 'withdrawal', on prend 'val', sinon on garde la base.
    current_w <- if (param_name == "Taux de Retrait")
      val
    else
      base_w
    
    # On détermine si on doit FORCER g dans le GMIB (seulement si on le teste)
    g_force_arg <- if (param_name == "Taux Garanti (g)")
      val
    else
      NULL
    
    
    # 2. Création des tables actuarielles (Standard, pas de choc ici)
    # On utilise entry_Age global (55 ans généralement)
    tbl <- create_tables_harmonized(55, "GMWB", 0, 0)
    
    prod_res <- list()
    
    # --- SIMULATION GMWB ---
    # On passe explicitement 'withdrawal_Rate = current_w'
    tot_gmwb <- 30 + (65 - 55) # withdrawal_Years + accumulation
    prod_res$GMWB <- simulate_GMWB(
      A_0 = A_0,
      S_0 = S_0,
      r_0 = r_0,
      entry_Age = 55,
      retirement_Age = 65,
      withdrawal_Years = 30,
      a = a,
      b = b,
      sigma = sigma,
      sigma_S = sigma_S,
      nb_Sim = nb_Sim,
      phi = phi,
      q_x_d = tbl$q_x_d[1:tot_gmwb],
      q_x_w = tbl$q_x_w[1:tot_gmwb],
      withdrawal_Rate = current_w,
      # <--- ICI : Taux mis à jour
      seed = 2025
    )
    
    # --- SIMULATION GLWB ---
    # On passe explicitement 'withdrawal_Rate = current_w'
    tbl_glwb <- create_tables_harmonized(55, "GLWB", 0, 0)
    tot_glwb <- 115 - 55
    prod_res$GLWB <- simulate_GLWB(
      A_0 = A_0,
      S_0 = S_0,
      r_0 = r_0,
      entry_Age = 55,
      retirement_Age = 65,
      max_Age = 115,
      a = a,
      b = b,
      sigma = sigma,
      sigma_S = sigma_S,
      nb_Sim = nb_Sim,
      phi = phi,
      q_x_d = tbl_glwb$q_x_d[1:tot_glwb],
      q_x_w = tbl_glwb$q_x_w[1:tot_glwb],
      withdrawal_Rate = current_w,
      # <--- ICI : Taux mis à jour
      seed = 2025
    )
    
    # --- SIMULATION GMIB ---
    # On passe 'forced_g = g_force_arg' pour obliger la fonction à utiliser notre valeur
    tbl_gmib <- create_tables_harmonized(55, "GMIB", 0, 0)
    tot_gmib <- 65 - 55
    prod_res$GMIB <- simulate_GMIB_modular_se(
      A_0 = A_0,
      S_0 = S_0,
      r_0 = r_0,
      entry_Age = 55,
      retirement_Age = 65,
      a = a,
      b = b,
      sigma = sigma,
      sigma_S = sigma_S,
      nb_Sim = nb_Sim,
      phi = phi,
      q_x_d = tbl_gmib$q_x_d[1:tot_gmib],
      q_x_w = tbl_gmib$q_x_w[1:tot_gmib],
      nb_Time_Steps_R_per_year = 12,
      max_Age = 115,
      seed = 2025,
      theoretical_Bond_Price = theoretical_Bond_Price,
      forced_g = g_force_arg # <--- ICI : C'est la clé pour que ça marche !
    )
    
    # Stockage du résultat
    results_list[[as.character(val)]] <- list(param_value = val, results =
                                                prod_res)
  }
  
  return(compile_results_harmonized(results_list, param_name))
}

# 4. EXÉCUTION DES ANALYSES
# -----------------------------------------------------------------------------

# A. Sensibilité au Taux Garanti 'g' (GMIB)
# Base = 6.94%. On teste de 4% à 10%
vals_g <- seq(0.04, 0.10, by = 0.01)
df_g <- run_special_sensitivity("Taux Garanti (g)", vals_g, nb_sim)

# B. Sensibilité au Taux de Retrait (GMWB/GLWB)
# Base = 3%. On teste de 2% à 6%
vals_w <- seq(0.02, 0.06, by = 0.01)
df_w <- run_special_sensitivity("Taux de Retrait", vals_w, nb_sim)

# 5. VISUALISATION
# -----------------------------------------------------------------------------

# Graphique pour 'g' (Focus GMIB)
p_g <- ggplot(subset(df_g, Product == "GMIB"), aes(x = Value, y = Mean_Value)) +
  geom_line(color = "forestgreen", size = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = base_g,
             linetype = "dashed",
             color = "gray") +
  theme_minimal() +
  labs(
    # title = "Sensibilité du GMIB au taux de conversion garanti (g)",
    # subtitle = paste("Base =", round(base_g * 100, 2), "%"),
    x = "Taux garanti (g)",
    y = "Valeur Risque-Neutre ($)"
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::dollar)

print(p_g)

# Graphique pour Taux de Retrait (Focus GMWB/GLWB)
p_w <- ggplot(subset(df_w, Product %in% c("GMWB", "GLWB")),
              aes(x = Value, y = Mean_Value, color = Product)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = base_w,
             linetype = "dashed",
             color = "gray") +
  theme_minimal() +
  labs(
    # title = "Sensibilité au taux de retrait contractuel",
    # subtitle = paste("Base =", base_w * 100, "%"),
    x = "Taux de retrait (%)",
    y = "Valeur Risque-Neutre ($)",
    color = "Produit"
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::dollar)

print(p_w)

# Sauvegarde
saveRDS(df_g, "sensitivity_analysis_results/sensitivity_g_GMIB.rds")
saveRDS(df_w, "sensitivity_analysis_results/sensitivity_withdrawal_GMWB_GLWB.rds")


# =============================================================================
# EXPORT DES AUTRES GRAPHIQUES EN PDF (FORMAT CARRÉ 7x7)
# =============================================================================
cat("\n=== EXPORT DES GRAPHIQUES COMPARATIFS ===\n")

graphs_to_export <- list(
  "Comparaison_Fonds.pdf" = p_compare_fund,
  "Comparaison_Flux.pdf"  = p_compare_flow,
  "Trajectoire_GMWB.pdf"  = p1,
  "Trajectoire_GLWB.pdf"  = p2,
  "Trajectoire_GMIB.pdf"  = p3,
  "Sensibility_age_ouverture.pdf" = p_age,
  "Sensibility_GMIB_guarantee.pdf" = p_g,
  "Sensibility_GMWB_GLWB_withdraw.pdf" = p_w
)

for (g_name in names(graphs_to_export)) {
  g_plot <- graphs_to_export[[g_name]]
  
  if (!is.null(g_plot)) {
    ggsave(
      filename = file.path(pdf_dir, g_name),
      plot     = g_plot,
      width    = 7,
      height   = 7,
      device   = "pdf"
    )
    cat(sprintf("Sauvegardé : %s\n", g_name))
  }
}


# =============================================================================
# 5. ANALYSE DES SCÉNARIOS (ÉTENDUE : Uniques + Composites)
# =============================================================================
cat("\n=== ANALYSE : SCÉNARIOS UNIQUES ET COMPOSITES ===\n")

# Fonction wrapper pour exécuter un scénario spécifique
run_scenario_detail <- function(scenario_name, magnitude_desc, r_val, sigma_val, stress_mort, stress_lapse)
{

  # 1. Configuration des paramètres
  # Configuration locale
  cfg_r <- r_val
  cfg_sigma <- sigma_val
  
  # Création des tables avec les chocs additifs (Mortalité / Déchéance)
  # stress_mort = -0.01 signifie qu'on soustrait 1% au qx
  tbl_gmwb <- create_tables_harmonized(55, "GMWB", stress_mort, stress_lapse)
  tbl_glwb <- create_tables_harmonized(55, "GLWB", stress_mort, stress_lapse)
  tbl_gmib <- create_tables_harmonized(55, "GMIB", stress_mort, stress_lapse)
  
  # Durées standards
  yrs_ret <- 65 - 55
  tot_gmwb <- 30 + yrs_ret
  tot_glwb <- 115 - 55
  tot_gmib <- yrs_ret
  
  prod_res <- list()
  
  # --- SIMULATIONS ---
  # GMWB
  prod_res$GMWB <- simulate_GMWB(A_0, S_0, cfg_r, 55, 65, 30, a, b, sigma, cfg_sigma, nb_sim, phi, 
                                 tbl_gmwb$q_x_d[1:tot_gmwb], tbl_gmwb$q_x_w[1:tot_gmwb], 0.03, 2025)
  
  # GLWB
  prod_res$GLWB <- simulate_GLWB(A_0, S_0, cfg_r, 55, 65, 115, a, b, sigma, cfg_sigma, nb_sim, phi, 
                                 tbl_glwb$q_x_d[1:tot_glwb], tbl_glwb$q_x_w[1:tot_glwb], 0.03, 2025)
  
  # GMIB (Avec correction pour passer 'g' recalculé si nécessaire, ici on laisse le standard)
  # Pour le GMIB, le taux garanti dépend de r_0. Si r change, g devrait changer théoriquement.
  # Ici on suppose que g est fixé à l'émission (donc basé sur r=5% initial) ou recalculé.
  # Hypothèse conservatrice : g recalculé avec le nouveau r.
  prod_res$GMIB <- simulate_GMIB_modular(A_0, S_0, cfg_r, 55, 65, a, b, sigma, cfg_sigma, nb_sim, phi, 
                                         tbl_gmib$q_x_d[1:tot_gmib], tbl_gmib$q_x_w[1:tot_gmib], 
                                         12, 115, 2025, theoretical_Bond_Price)
  
  # Retour sous forme de ligne de Dataframe
  return(data.frame(
    Scenario = scenario_name,
    Choc = magnitude_desc,
    GMWB = prod_res$GMWB$mean_Value,
    GLWB = prod_res$GLWB$mean_Value,
    GMIB = prod_res$GMIB$mean_Value
  ))
}

# --- DÉFINITION ET EXÉCUTION DES 7 SCÉNARIOS ---

# Liste des résultats
res_list <- list()

# 1. Base
res_list[[1]] <- run_scenario_detail("Base", "-", 
                                     r_val=0.05, sigma_val=0.20, stress_mort=0, stress_lapse=0)

# --- CHOCS UNIQUES (Sensibilités Isolées) ---

# 2. Taux d'intérêt (Hausse) : Impact négatif attendu sur la valeur (Rho)
res_list[[2]] <- run_scenario_detail("Hausse Taux", "+2% (r=7%)", 
                                     r_val=0.07, sigma_val=0.20, stress_mort=0, stress_lapse=0)

# 3. Volatilité (Hausse) : Impact positif attendu sur la valeur (Vega)
res_list[[3]] <- run_scenario_detail("Hausse Volatilité", "+10% (sig=30%)", 
                                     r_val=0.05, sigma_val=0.30, stress_mort=0, stress_lapse=0)

# 4. Mortalité (Baisse/Longévité) : Les gens vivent plus longtemps
# Choc additif de -1% (ou -0.01) sur les qx
res_list[[4]] <- run_scenario_detail("Choc Longévité", "q_x - 1%", 
                                     r_val=0.05, sigma_val=0.20, stress_mort=-0.01, stress_lapse=0)

# 5. Déchéance (Baisse/Rétention) : Les gens gardent leur contrat
# Choc additif de -50% proportionnel ou -5% absolu. Ici -0.05 absolu comme précédemment.
res_list[[5]] <- run_scenario_detail("Choc Rétention", "q_w - 5%", 
                                     r_val=0.05, sigma_val=0.20, stress_mort=0, stress_lapse=-0.05)


# --- CHOCS COMPOSITES (Stress Tests) ---

# 6. Crise Financière (Taux bas + Volatilité haute)
res_list[[6]] <- run_scenario_detail("Crise Financière", "r=2%, sig=30%", 
                                     r_val=0.02, sigma_val=0.30, stress_mort=0, stress_lapse=0)

# 7. Rétention & Longévité (Comportemental)
res_list[[7]] <- run_scenario_detail("Rétention + Longévité", "q_w-5%, q_x-1%", 
                                     r_val=0.05, sigma_val=0.20, stress_mort=-0.01, stress_lapse=-0.05)

# Fusion du tableau final
df_scenarios_final <- do.call(rbind, res_list)
print(df_scenarios_final)
saveRDS(df_scenarios_final, file.path(results_dir, "scenarios_complets.rds"))


cat("=== EXPORT TERMINÉ ===\n")


cat(" FIN DES ANALYSES SELON LA PERSPECTIVE DE L'ASSURE !")



#=========================================
#                                        #
#   ANALYSE : PERSPECTIVE DE L'ASSUREUR  #
#                                        #
#=========================================

# ==============================================================================
# 1. CHARGEMENT DES LIBRAIRIES ET CONFIGURATION
# ==============================================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)

set.seed(2025) # Pour la reproductibilité

# ==============================================================================
# 2. PARAMÈTRES ET FONCTIONS DE BASE
# ==============================================================================

# --- Scénario de Base ---
base_params <- list(
  r0 = 0.05,        # Taux sans risque initial
  a = 0.2,          # Vasicek a (vitesse retour)
  b = 0.05,         # Vasicek b (moyenne long terme)
  sigma_r = 0.01,   # Volatilité des taux
  S0 = 100000,      # Valeur initiale du fonds
  sigma_s = 0.20,   # Volatilité du sous-jacent
  phi = 0.02,       # MER (Frais de gestion)
  age_init = 55,    # Age à l'émission
  age_retraite = 65,# Age de retraite
  age_max = 115,    # Age limite
  dt = 1,           # Pas de temps (annuel)
  n_sim = 10000     # Nombre de simulations (augmenter à 10 000 pour rapport final)
)

# --- Générateur de Mortalité (Approximation Gompertz) ---
get_qx_vector <- function(age_start, age_end, shock = 0) {
  ages <- age_start:age_end
  # Paramètres approximatifs pour CPM2014
  B <- 0.0003
  C <- 1.1
  qx_raw <- B * C^ages
  # Application du choc additif (ex: +0.01)
  qx_shocked <- pmin(pmax(qx_raw + shock, 0), 1)
  return(qx_shocked)
}

# --- Générateur de Scénarios Économiques (Vasicek + Black-Scholes) ---
generate_scenarios <- function(p) {
  T_total <- p$age_max - p$age_init
  n_steps <- T_total / p$dt
  
  r_mat <- matrix(0, nrow = p$n_sim, ncol = n_steps + 1)
  A_mat <- matrix(0, nrow = p$n_sim, ncol = n_steps + 1) # Fonds de l'assuré
  
  r_mat[, 1] <- p$r0
  A_mat[, 1] <- p$S0
  
  for (t in 1:n_steps) {
    z_r <- rnorm(p$n_sim)
    z_s <- rnorm(p$n_sim)
    
    # Vasicek (Taux)
    dr <- p$a * (p$b - r_mat[, t]) * p$dt + p$sigma_r * sqrt(p$dt) * z_r
    r_mat[, t + 1] <- r_mat[, t] + dr
    
    # Black-Scholes (Fonds avec frais phi)
    # Drift = r_t - phi (sous Q)
    ret <- (r_mat[, t] - p$phi - 0.5 * p$sigma_s^2) * p$dt + p$sigma_s * sqrt(p$dt) * z_s
    A_mat[, t + 1] <- A_mat[, t] * exp(ret)
  }
  
  # Facteurs d'actualisation cumulés
  disc_factors <- t(apply(exp(-r_mat * p$dt), 1, cumprod))
  
  return(list(r = r_mat, A = A_mat, df = disc_factors))
}

# ==============================================================================
# 3. FONCTIONS DE VALORISATION (VUE ASSUREUR)
# ==============================================================================

# Calcul : VP(Coûts Garantis) - VP(Frais Perçus)

# --- GMWB (Retraits Garantis pendant M années) ---
val_gmwb_insurer <- function(p, scenarios, w_rate, qx, qw) {
  T_accum <- p$age_retraite - p$age_init
  T_withdraw <- 30 # Durée garantie (ex: 30 ans)
  
  # Base de garantie (Simple step-up à la retraite pour simplifier)
  G_Tr <- apply(scenarios$A[, 1:(T_accum+1)], 1, max) 
  W_annuel <- G_Tr * w_rate
  
  pv_net_liabs <- numeric(p$n_sim)
  
  for (i in 1:p$n_sim) {
    fund <- scenarios$A[i, ]
    disc <- scenarios$df[i, ]
    
    # Flux
    vp_fees <- 0
    vp_claims <- 0
    is_active <- TRUE
    
    for (t in 1:(T_accum + T_withdraw)) {
      age_curr <- p$age_init + t - 1
      if (!is_active) break
      
      # 1. Gestion Sorties (Décès / Lapse)
      prob_die <- qx[age_curr - p$age_init + 1]
      prob_lapse <- qw 
      
      # Tirage aléatoire sortie (simplifié pour Monte Carlo)
      # Pour plus de précision, on pondère les flux par les probas (méthode actuarielle)
      # Ici, méthode actuarielle par espérance des flux
      prob_survive_persist <- (1 - prob_die) * (1 - prob_lapse)
      
      # 2. Frais (Inflow)
      if (fund[t] > 0) {
        fee <- fund[t] * (1 - exp(-p$phi))
        vp_fees <- vp_fees + fee * disc[t+1] * prob_survive_persist # Pondéré
      }
      
      # 3. Garanties (Outflow) - Uniquement après retraite
      if (t > T_accum) {
        if (fund[t] < W_annuel[i]) {
          # Fonds insuffisant : Assureur paie le reste
          claim <- max(0, W_annuel[i] - fund[t])
          # Le fonds tombe à 0
          fund[t+1] <- 0 
          vp_claims <- vp_claims + claim * disc[t+1] * prob_survive_persist
        } else {
          # Retrait normal
          fund[t+1] <- fund[t] - W_annuel[i]
        }
      } else {
        # Accumulation
        fund[t+1] <- fund[t+1] # Pas de retrait
      }
      
      # Mise à jour proba survie globale (pour méthode espérance)
      # Note: Dans une simulation "pure", on ferait un runif(). 
      # Ici on actualise les flux par la probabilité cumulée de rester en vie/contrat.
    }
    pv_net_liabs[i] <- vp_claims - vp_fees
  }
  return(mean(pv_net_liabs))
}

# --- GLWB (Retraits Garantis à Vie) ---
val_glwb_insurer <- function(p, scenarios, w_rate, qx, qw) {
  T_accum <- p$age_retraite - p$age_init
  G_Tr <- apply(scenarios$A[, 1:(T_accum+1)], 1, max)
  W_annuel <- G_Tr * w_rate
  
  pv_net_liabs <- numeric(p$n_sim)
  
  for (i in 1:p$n_sim) {
    fund <- scenarios$A[i, ]
    disc <- scenarios$df[i, ]
    vp_fees <- 0; vp_claims <- 0
    prob_alive <- 1 # Probabilité cumulée de rester dans le contrat
    
    for (t in 1:(p$age_max - p$age_init)) {
      if (prob_alive < 0.001) break
      
      idx_qx <- t
      # Proba de sortir cette année
      p_die <- qx[idx_qx]
      p_lapse <- qw 
      
      # 1. Frais
      if (fund[t] > 0) {
        fee <- fund[t] * (1 - exp(-p$phi))
        vp_fees <- vp_fees + fee * disc[t+1] * prob_alive
      }
      
      # 2. Garantie (après retraite)
      if (t > T_accum) {
        amt_needed <- W_annuel[i]
        if (fund[t] < amt_needed) {
          claim <- max(0, amt_needed - fund[t])
          fund[t+1] <- 0
          vp_claims <- vp_claims + claim * disc[t+1] * prob_alive
        } else {
          fund[t+1] <- fund[t] - amt_needed
        }
      }
      
      # Mise à jour probabilité de survie du contrat
      prob_alive <- prob_alive * (1 - p_die) * (1 - p_lapse)
    }
    pv_net_liabs[i] <- vp_claims - vp_fees
  }
  return(mean(pv_net_liabs))
}

# --- GMIB (Option d'échange à la retraite) ---
val_gmib_insurer <- function(p, scenarios, w_rate, qx, qw) {
  T_accum <- p$age_retraite - p$age_init
  
  # Base garantie (Roll-up 4%)
  G_Tr <- p$S0 * (1.04)^T_accum 
  
  pv_net_liabs <- numeric(p$n_sim)
  
  for (i in 1:p$n_sim) {
    # 1. VP des Frais (Accumulation uniquement)
    # On approxime que le client paie jusqu'à la retraite sauf décès/lapse
    # Calcul simplifié vectoriel
    fund_path <- scenarios$A[i, 1:T_accum]
    disc_path <- scenarios$df[i, 2:(T_accum+1)]
    
    # Proba survie cumulée approximée pour l'exemple
    # (Idéalement boucle pas à pas comme GLWB)
    probs <- cumprod(rep((1 - 0.005)*(1 - qw), T_accum)) # Approx mort 0.5%
    
    fees <- fund_path * (1 - exp(-p$phi))
    vp_fees <- sum(fees * disc_path * probs)
    
    # 2. Option à la retraite
    # a_Tr (rente viagère à 65 ans) ~ approx 15
    # Taux de conversion g ~ 1/15
    annuity_val_factor <- 14.5 # Valeur d'une rente à 65 ans
    g <- 1/15
    
    val_garantie <- G_Tr * g * annuity_val_factor
    fund_at_ret <- scenarios$A[i, T_accum+1]
    
    # L'option est exercée si Valeur Rente Garantie > Fonds
    claim <- max(0, val_garantie - fund_at_ret)
    
    # Actualisation du coût (si survie jusqu'à retraite)
    prob_survival_ret <- probs[length(probs)]
    vp_claims <- claim * scenarios$df[i, T_accum+1] * prob_survival_ret
    
    pv_net_liabs[i] <- vp_claims - vp_fees
  }
  return(mean(pv_net_liabs))
}

# ==============================================================================
# 4. EXÉCUTION DES ANALYSES DE SENSIBILITÉ
# ==============================================================================

# Fonction générique pour lancer les tests
run_analysis <- function() {
  
  results_df <- data.frame()
  
  # A. Sensibilité MER (Frais)
  shocks_mer <- seq(0.005, 0.04, 0.005)
  print("Calcul sensibilité MER...")
  for (val in shocks_mer) {
    p <- base_params; p$phi <- val
    scen <- generate_scenarios(p)
    qx <- get_qx_vector(p$age_init, p$age_max, 0)
    
    res_gmwb <- val_gmwb_insurer(p, scen, 0.05, qx, 0.078)
    res_glwb <- val_glwb_insurer(p, scen, 0.05, qx, 0.034)
    res_gmib <- val_gmib_insurer(p, scen, NA, qx, 0.039)
    
    results_df <- rbind(results_df, data.frame(Factor="MER", Value=val, GMWB=res_gmwb, GLWB=res_glwb, GMIB=res_gmib))
  }
  
  # B. Sensibilité Volatilité
  shocks_vol <- seq(0.10, 0.30, 0.05)
  print("Calcul sensibilité Volatilité...")
  for (val in shocks_vol) {
    p <- base_params; p$sigma_s <- val
    scen <- generate_scenarios(p)
    qx <- get_qx_vector(p$age_init, p$age_max, 0)
    
    res_gmwb <- val_gmwb_insurer(p, scen, 0.05, qx, 0.078)
    res_glwb <- val_glwb_insurer(p, scen, 0.05, qx, 0.034)
    res_gmib <- val_gmib_insurer(p, scen, NA, qx, 0.039)
    
    results_df <- rbind(results_df, data.frame(Factor="Volatilité", Value=val, GMWB=res_gmwb, GLWB=res_glwb, GMIB=res_gmib))
  }
  
  # C. Sensibilité Taux de Retrait (Uniquement GMWB/GLWB)
  shocks_wd <- seq(0.03, 0.07, 0.01)
  print("Calcul sensibilité Taux Retrait...")
  for (val in shocks_wd) {
    p <- base_params
    scen <- generate_scenarios(p)
    qx <- get_qx_vector(p$age_init, p$age_max, 0)
    
    res_gmwb <- val_gmwb_insurer(p, scen, val, qx, 0.078)
    res_glwb <- val_glwb_insurer(p, scen, val, qx, 0.034)
    # GMIB insensible au taux de retrait (produit d'accumulation) -> Fixe
    res_gmib <- val_gmib_insurer(p, scen, NA, qx, 0.039)
    
    results_df <- rbind(results_df, data.frame(Factor="Taux_Retrait", Value=val, GMWB=res_gmwb, GLWB=res_glwb, GMIB=NA))
  }
  
  # D. Sensibilité Mortalité (Choc additif)
  shocks_mort <- seq(-0.01, 0.02, 0.005)
  print("Calcul sensibilité Mortalité...")
  for (val in shocks_mort) {
    p <- base_params
    scen <- generate_scenarios(p)
    qx <- get_qx_vector(p$age_init, p$age_max, val)
    
    res_gmwb <- val_gmwb_insurer(p, scen, 0.05, qx, 0.078)
    res_glwb <- val_glwb_insurer(p, scen, 0.05, qx, 0.034)
    res_gmib <- val_gmib_insurer(p, scen, NA, qx, 0.039)
    
    results_df <- rbind(results_df, data.frame(Factor="Mortalité (Choc)", Value=val, GMWB=res_gmwb, GLWB=res_glwb, GMIB=res_gmib))
  }
  
  # E. Sensibilité Déchéance (Choc additif sur taux base)
  shocks_lapse <- seq(-0.02, 0.05, 0.01)
  print("Calcul sensibilité Déchéance...")
  for (val in shocks_lapse) {
    p <- base_params
    scen <- generate_scenarios(p)
    qx <- get_qx_vector(p$age_init, p$age_max, 0)
    
    # Taux de base + Choc
    w_gmwb <- max(0, 0.078 + val)
    w_glwb <- max(0, 0.034 + val)
    w_gmib <- max(0, 0.039 + val)
    
    res_gmwb <- val_gmwb_insurer(p, scen, 0.05, qx, w_gmwb)
    res_glwb <- val_glwb_insurer(p, scen, 0.05, qx, w_glwb)
    res_gmib <- val_gmib_insurer(p, scen, NA, qx, w_gmib)
    
    results_df <- rbind(results_df, data.frame(Factor="Déchéance (Choc)", Value=val, GMWB=res_gmwb, GLWB=res_glwb, GMIB=res_gmib))
  }
  
  return(results_df)
}

# Lancement des calculs
data_final <- run_analysis()

# ==============================================================================
# 5. AFFICHAGE DES RÉSULTATS (TABLEAU + GRAPHIQUES)
# ==============================================================================

# --- Tableau Résumé (Format large) ---
# On sélectionne quelques points clés pour ne pas surcharger
summary_table <- data_final %>%
  group_by(Factor) %>%
  filter(Value %in% c(min(Value), median(Value), max(Value))) %>%
  mutate(across(c(GMWB, GLWB, GMIB), ~ round(., 0)))

print("--- TABLEAU RÉSUMÉ : VALEUR PASSIF ASSUREUR ($) ---")
kable(summary_table, caption = "Sensibilité de la Valeur Nette du Passif Assureur")

# --- Graphiques (Facettes) ---
# Transformation format long pour ggplot
data_long <- data_final %>%
  pivot_longer(cols = c(GMWB, GLWB, GMIB), names_to = "Produit", values_to = "Passif_Net") %>%
  filter(!is.na(Passif_Net))

# Création du plot
p <- ggplot(data_long, aes(x = Value, y = Passif_Net, color = Produit)) +
  geom_line(size = 1.2) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ Factor, scales = "free_x") +
  labs(
    title = "Analyse de sensibilité - Assureur",
    subtitle = "Valeur Nette du Passif = VP(Garanties) - VP(Frais)",
    y = "Valeur Nette ($)",
    x = "Valeur du Paramètre / Choc"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)
