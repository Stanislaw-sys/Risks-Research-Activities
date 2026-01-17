# =============================================================================
# NOTEBOOK COMPLET AVEC REPRODUCTIBILITÉ - VERSION CORRIGÉE
# =============================================================================

rm(list = ls())
seed <- 2025
set.seed(seed)

# Installer et charger les packages nécessaires
required_packages <- c("ggplot2", "dplyr", "tidyr", "PerformanceAnalytics", "moments", "scales")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(ggplot2)
library(dplyr)
library(tidyr)
library(PerformanceAnalytics)
library(moments)
library(scales)

# Fonctions Vasicek 
mu_s_t <- function(r_s, s, t, a, b) {
  coef <- exp(-a * (t - s))
  term1 <- r_s * coef
  term2 <- b * (1 - coef)
  return(term1 + term2)
}

beta_s_t <- function(sigma, s, t, a) {
  variance <- sigma ^ 2 / (2 * a) * (1 - exp(-2 * a * (t - s)))
  return(pmax(variance, 0))  # Protection contre valeurs négatives
}

# Prix ZCB
theoretical_Bond_Price <- function(r_s, s, t, a, b, sigma) {
  coef <- (1 - exp(-a * (t - s))) / a
  term1 <- (r_s - b + sigma ^ 2 / (2 * a ^ 2)) * coef
  term2 <- (b - sigma ^ 2 / (2 * a ^ 2)) * (t - s)
  term3 <- sigma ^ 2 / (4 * a) * coef ^ 2
  return(exp(-(term1 + term2 + term3)))
}

# =============================================================================
# FONCTION simulation_r_t CORRIGÉE (Problème de précision flottante)
# =============================================================================

simulation_r_t <- function(r_s, s, t, nb_Time_Steps, a, b, sigma, nb_Sim) {
  t_i <- seq(from = s, to = t, length.out = nb_Time_Steps + 1)
  mat_rt <- matrix(0, nrow = nb_Sim, ncol = nb_Time_Steps + 1)
  mat_Integral_rt <- matrix(0, nrow = nb_Sim, ncol = nb_Time_Steps + 1)
  mat_rt[, 1] <- r_s
  
  for(i in 1 : nb_Time_Steps) {
    vec_Mean <- mu_s_t(mat_rt[, i], s = t_i[i], t = t_i[i + 1], a, b)
    vec_Var <- beta_s_t(sigma, s = t_i[i], t = t_i[i + 1], a)
    # Protection contre variance négative due à erreurs numériques
    vec_Var <- pmax(vec_Var, 0)
    mat_rt[, i + 1] <- vec_Mean + sqrt(vec_Var) * rnorm(nb_Sim)
    mat_Integral_rt[, i + 1] <- mat_Integral_rt[, i] + 
      (mat_rt[, i + 1] + mat_rt[, i]) * (t_i[i + 1] - t_i[i]) * 0.5
  }

  # Utilisation d'une tolérance pour trouver les indices entiers
  indices <- which(abs(t_i - round(t_i)) < 1e-8)
  # S'assurer qu'on ne dépasse pas t
  indices <- indices[t_i[indices] <= t + 1e-8]
  
  list_Res <- list(mat_rt = mat_rt[, indices], mat_Integral_rt = mat_Integral_rt[, indices])
  
  return(list_Res)
}


# S_t
simulation_Black_Scholes_01 <- function(S_0, r_0, t, nb_Time_Steps, nb_Time_Steps_R,
                                     a_R, b_R, sigma_R, sigma_S, nb_Sim)
{ 
  list_Res_R <- simulation_r_t(r_s = r_0, s = 0, t = t, nb_Time_Steps = nb_Time_Steps_R,
                               a = a_R, b = b_R, sigma = sigma_R, nb_Sim = nb_Sim)
  
  const_Sigma_S_Square <- 0.5 * sigma_S ^ 2
  mat_Integral_rt <- list_Res_R$mat_Integral_rt
  mat_Integral_rt_Diff <- t(apply(X = mat_Integral_rt, MARGIN = 1, FUN = diff))
  St <- matrix(0, nrow = nb_Sim, ncol = nb_Time_Steps + 1)
  St[, 1] <- S_0
  
  for(i in 1 : nb_Time_Steps) {
    ln_Return_S <- mat_Integral_rt_Diff[, i] - const_Sigma_S_Square + rnorm(nb_Sim) * sigma_S
    St[, i + 1] <- exp(ln_Return_S) * St[, i]
  }
  
  list_Res_S <- list(St = St, list_Res_R = list_Res_R, mat_Integral_rt_Diff = mat_Integral_rt_Diff)
  
  return(list_Res_S)
}


# =============================================================================
# 2. WRAPPERS AVEC SEED 
# =============================================================================

# Wrapper pour utiliser simulation_Black_Scholes avec seed et paramètres mensuels
simulation_Black_Scholes <- function(S_0, r_0, t_years, nb_Sim,
                                               a, b, sigma,
                                               sigma_S, seed = 2025) {
  
  set.seed(seed)
  
  # Utilisation directe de la fonction 
  # nb_Time_Steps_R = t_years * 12 (simulation mensuelle des taux)
  # nb_Time_Steps = t_years (simulation annuelle de S)
  sim <- simulation_Black_Scholes_01(
    S_0 = S_0,
    r_0 = r_0,
    t = t_years,
    nb_Time_Steps = t_years,              # Simulation annuelle de S
    nb_Time_Steps_R = t_years * 12,       # Simulation mensuelle des taux (12 pas par an)
    a_R = a,                      # Paramètres mensuels
    b_R = b,
    sigma_R = sigma,
    sigma_S = sigma_S,                    # Volatilité annuelle
    nb_Sim = nb_Sim
  )
  
  # La fonction retourne déjà tout en base annuelle
  # sim$St : prix du fonds aux points annuels (t_years + 1 colonnes)
  # sim$list_Res_R$mat_rt : taux aux points annuels
  # sim$list_Res_R$mat_Integral_rt : intégrale des taux aux points annuels
  
  return(list(
    St = sim$St,
    mat_rt = sim$list_Res_R$mat_rt,
    mat_integral_rt = sim$list_Res_R$mat_Integral_rt,
    discount_factors = exp(-sim$list_Res_R$mat_Integral_rt)
  ))
}


# ---------------
# 3.1 GMWB
# ---------------
simulate_GMWB <- function(A_0, S_0, r_0, entry_Age, retirement_Age, withdrawal_Years,
                          a, b, sigma, sigma_S, nb_Sim,
                          phi, q_x_d, q_x_w, withdrawal_Rate, seed = 2025)
{

  set.seed(seed)
  T_r <- retirement_Age - entry_Age
  M <- withdrawal_Years
  total_Years <- T_r + M
  
  # Simulation
  time_factor <- 12  # Pour le pas mensuel
  #sim <- simulation_Black_Scholes(S_0 = S_0, r_0 = r_0, t = total_Years,
                                 # nb_Time_Steps = time_factor*total_Years, 
                                 # nb_Time_Steps_R = time_factor*total_Years,
                                 # a_R = a, b_R = b, sigma_R = sigma,
                                 # sigma_S = sigma_S, nb_Sim = nb_Sim, seed = seed)
  
  sim <- simulation_Black_Scholes(S_0, r_0, total_Years, nb_Sim,
                                  a, b, sigma, sigma_S, seed = 2025)

  # Toutes les valeurs sont déjà annuelles
  St <- sim$St
  discount_Factors <- sim$discount_factors

  # Vérification des dimensions
  if(ncol(St) != (total_Years + 1)) {
    stop(sprintf("St a %d colonnes, besoin de %d.", ncol(St), total_Years + 1))
  }

  # Initialisation des matrices
  At <- matrix(A_0, nrow = nb_Sim, ncol = total_Years + 1)
  Gt <- matrix(A_0, nrow = nb_Sim, ncol = total_Years + 1)
  cash_Flows <- matrix(0, nrow = nb_Sim, ncol = total_Years + 1)
  death_Flows <- matrix(0, nrow = nb_Sim, ncol = total_Years + 1)  # NOUVEAU
  lapse_Flows <- matrix(0, nrow = nb_Sim, ncol = total_Years + 1)  # NOUVEAU
  survival_Flows <- matrix(0, nrow = nb_Sim, ncol = total_Years + 1)  # NOUVEAU

  # Probabilités de survie
  p_d <- 1 - q_x_d[1:total_Years]
  p_w <- 1 - q_x_w[1:total_Years]
  survival_Probs <- c(1, cumprod(p_d * p_w))
  
  # Phase d'accumulation - AJOUT DES FLUX DE DÉCHÉANCE
  for(year in 1:T_r) {
    At[, year+1] <- At[, year] * exp(-phi) * St[, year+1]/St[, year]
    Gt[, year+1] <- pmax(Gt[, year], At[, year+1])
    
    # Flux de décès (existant)
    death_flow <- survival_Probs[year] * q_x_d[year] * At[, year+1]
    death_Flows[, year+1] <- death_flow  # NOUVEAU
    
    # Flux de déchéance (AJOUT)
    lapse_flow <- survival_Probs[year] * (1 - q_x_d[year]) * q_x_w[year] * At[, year+1]
    lapse_Flows[, year+1] <- lapse_flow  # NOUVEAU
    
    # Total = décès + déchéance
    cash_Flows[, year+1] <- death_flow + lapse_flow
  }
  
  # Calcul du retrait annuel garanti
  annual_Withdrawal <- Gt[, T_r+1] * withdrawal_Rate
  
  # Phase de décumulation - AJOUT DES FLUX DE DÉCHÉANCE
  for(year in (T_r+1):total_Years) {
    account_Before <- At[, year] * exp(-phi) * St[, year+1]/St[, year]
    At[, year+1] <- pmax(account_Before - annual_Withdrawal, 0)
    
    # Flux de décès (existant)
    death_flow <- survival_Probs[year] * q_x_d[year] * At[, year+1]
    death_Flows[, year+1] <- death_flow  # NOUVEAU
    
    # Flux de déchéance (AJOUT)
    lapse_flow <- survival_Probs[year] * (1 - q_x_d[year]) * q_x_w[year] * At[, year+1]
    lapse_Flows[, year+1] <- lapse_flow  # NOUVEAU
    
    # Flux de survie (retrait annuel) (existant)
    survival_flow <- survival_Probs[year+1] * annual_Withdrawal
    survival_Flows[, year+1] <- survival_flow  # NOUVEAU
    
    # Total = décès + déchéance + survie
    cash_Flows[, year+1] <- death_flow + lapse_flow + survival_flow
  }
  
  # Paiement final
  final_payment <- survival_Probs[total_Years+1] * At[, total_Years+1]
  cash_Flows[, total_Years+1] <- cash_Flows[, total_Years+1] + final_payment
  survival_Flows[, total_Years+1] <- survival_Flows[, total_Years+1] + final_payment  # NOUVEAU
  
  # Actualisation des flux
  discounted_CF <- discount_Factors * cash_Flows
  contract_Values <- rowSums(discounted_CF)
  
  # Calcul du moneyness
  moneyness <- At / Gt
  moneyness[!is.finite(moneyness)] <- 1
  
  # Statistiques des flux
  total_death_flows <- rowSums(death_Flows)
  total_lapse_flows <- rowSums(lapse_Flows)
  total_survival_flows <- rowSums(survival_Flows)
  
  list(
    values = contract_Values,
    mean_Value = mean(contract_Values),
    cash_Flows = cash_Flows,
    death_Flows = death_Flows,      # NOUVEAU
    lapse_Flows = lapse_Flows,      # NOUVEAU
    survival_Flows = survival_Flows, # NOUVEAU
    discount_Factors = discount_Factors,
    At = At,
    Gt = Gt,
    Moneyness = moneyness,
    annual_Withdrawal = mean(annual_Withdrawal),
    total_Years = total_Years,
    total_death_flows = total_death_flows,      # NOUVEAU
    total_lapse_flows = total_lapse_flows,      # NOUVEAU
    total_survival_flows = total_survival_flows # NOUVEAU
  )
}


# ----------------------------------------------------
# 2.2 GLWB - Version avec flux de déchéance
# ----------------------------------------------------
simulate_GLWB <- function(A_0, S_0, r_0, entry_Age, retirement_Age, max_Age,
                          a, b, sigma, sigma_S, nb_Sim,
                          phi, q_x_d, q_x_w, withdrawal_Rate, seed = 2025)
{

  set.seed(seed)
  T_r <- retirement_Age - entry_Age
  total_Years <- max_Age - entry_Age

  # Simulation S_t
  sim <- simulation_Black_Scholes(S_0, r_0, total_Years, nb_Sim,
                                  a, b, sigma, sigma_S, seed = 2025)

  # Toutes les valeurs sont déjà annuelles
  St <- sim$St
  discount_Factors <- sim$discount_factors

  # Vérification des dimensions
  if(ncol(St) != (total_Years + 1)) {
    stop(sprintf("St a %d colonnes, besoin de %d.", ncol(St), total_Years + 1))
  }

  # Initialisation des matrices
  At <- matrix(A_0, nrow = nb_Sim, ncol = total_Years + 1)
  Gt <- matrix(A_0, nrow = nb_Sim, ncol = total_Years + 1)
  cash_Flows <- matrix(0, nrow = nb_Sim, ncol = total_Years + 1)
  death_Flows <- matrix(0, nrow = nb_Sim, ncol = total_Years + 1)  # NOUVEAU
  lapse_Flows <- matrix(0, nrow = nb_Sim, ncol = total_Years + 1)  # NOUVEAU
  survival_Flows <- matrix(0, nrow = nb_Sim, ncol = total_Years + 1)  # NOUVEAU

  # Probabilités de survie
  p_d <- 1 - q_x_d[1:total_Years]
  p_w <- 1 - q_x_w[1:total_Years]
  survival_Probs <- c(1, cumprod(p_d * p_w))

  # Phase d'accumulation - AJOUT DES FLUX DE DÉCHÉANCE
  for(year in 1:T_r) {
    At[, year+1] <- At[, year] * exp(-phi) * St[, year+1]/St[, year]
    Gt[, year+1] <- pmax(Gt[, year], At[, year+1])
    
    # Flux de décès (existant)
    death_flow <- survival_Probs[year] * q_x_d[year] * At[, year+1]
    death_Flows[, year+1] <- death_flow  # NOUVEAU
    
    # Flux de déchéance (AJOUT)
    lapse_flow <- survival_Probs[year] * (1 - q_x_d[year]) * q_x_w[year] * At[, year+1]
    lapse_Flows[, year+1] <- lapse_flow  # NOUVEAU
    
    # Total = décès + déchéance
    cash_Flows[, year+1] <- death_flow + lapse_flow
  }
  
  # Calcul du retrait annuel garanti
  annual_Withdrawal <- Gt[, T_r+1] * withdrawal_Rate
  
  # Phase de décumulation à vie - AJOUT DES FLUX DE DÉCHÉANCE
  for(year in (T_r+1):total_Years) {
    account_Before <- At[, year] * exp(-phi) * St[, year+1]/St[, year]
    At[, year+1] <- pmax(account_Before - annual_Withdrawal, 0)
    
    # Flux de décès (existant)
    death_flow <- survival_Probs[year] * q_x_d[year] * At[, year+1]
    death_Flows[, year+1] <- death_flow  # NOUVEAU
    
    # Flux de déchéance (AJOUT)
    lapse_flow <- survival_Probs[year] * (1 - q_x_d[year]) * q_x_w[year] * At[, year+1]
    lapse_Flows[, year+1] <- lapse_flow  # NOUVEAU
    
    # Flux de survie (retrait annuel) (existant)
    survival_flow <- survival_Probs[year+1] * annual_Withdrawal
    survival_Flows[, year+1] <- survival_flow  # NOUVEAU
    
    # Total = décès + déchéance + survie
    cash_Flows[, year+1] <- death_flow + lapse_flow + survival_flow
  }
  
  # Paiement final
  final_payment <- survival_Probs[total_Years+1] * At[, total_Years+1]
  cash_Flows[, total_Years+1] <- cash_Flows[, total_Years+1] + final_payment
  survival_Flows[, total_Years+1] <- survival_Flows[, total_Years+1] + final_payment  # NOUVEAU
  
  # Actualisation des flux
  discounted_CF <- discount_Factors * cash_Flows
  contract_Values <- rowSums(discounted_CF)
  
  # Calcul du moneyness
  moneyness <- At / Gt
  moneyness[!is.finite(moneyness)] <- 1
  
  # Statistiques des flux
  total_death_flows <- rowSums(death_Flows)
  total_lapse_flows <- rowSums(lapse_Flows)
  total_survival_flows <- rowSums(survival_Flows)
  
  list(
    values = contract_Values,
    mean_Value = mean(contract_Values),
    cash_Flows = cash_Flows,
    death_Flows = death_Flows,      # NOUVEAU
    lapse_Flows = lapse_Flows,      # NOUVEAU
    survival_Flows = survival_Flows, # NOUVEAU
    discount_Factors = discount_Factors,
    At = At,
    Gt = Gt,
    Moneyness = moneyness,
    annual_Withdrawal = mean(annual_Withdrawal),
    total_Years = total_Years,
    total_death_flows = total_death_flows,      # NOUVEAU
    total_lapse_flows = total_lapse_flows,      # NOUVEAU
    total_survival_flows = total_survival_flows # NOUVEAU
  )
}


#===========================================================
# 2.4 Approche 2 : Procédurale
#------------------------------------------------------------

#--------------------------------
# Calcul de la rente actuarielle
#--------------------------------   

# La rente de x ans actualisée au temps s. 
# a_x(s) = sum_{u = 0}^{115 - x - s} P(s, s + u) {}_u p_{x + s}

a_x_s <- function(x, s, Qx, r_s, Obligation_fun, a, b, sigma, max_Age = 115)
{
  age_at_s <- x + s
  Tmax <- max_Age - age_at_s
  if(Tmax < 0) return(0)
  
  # Retrouver l'indice de départ
  if(!is.null(names(Qx))) {
    start_idx <- which(names(Qx) == paste("Age", age_at_s))
    # Fallback si nom non trouvé
    if(length(start_idx) == 0) start_idx <- age_at_s - 55 + 1 
  } else {
    # Supposition standard si pas de noms
    start_idx <- age_at_s - 55 + 1 
  }

  # Protection contre le dépassement de la table
  available_len <- length(Qx) - start_idx + 1
  if (available_len < 0) return(0)
  
  # On ajuste Tmax si la table finit avant 115 ans
  Tmax_calc <- min(Tmax, available_len - 1)
  
  # Extraction sécurisée
  qx_segment <- Qx[start_idx:(start_idx + Tmax_calc)]
  
  # Si des NA existent encore (ex: table incomplète), on les remplace par 1 (décès certain)
  qx_segment[is.na(qx_segment)] <- 1
  
  tpx <- cumprod(c(1, 1 - qx_segment))
  
  # Ajustement vecteur obligation pour correspondre à la longueur de tpx
  u_range <- 0:(length(tpx)-1)
  
  P_vec <- sapply(u_range, function(u) {
    Obligation_fun(r_s = r_s, s = s, t = s + u, a = a, b = b, sigma = sigma)
  })
  
  return(sum(P_vec * tpx))
}

#--------------------------------
# FLUX DÉCÈS / DÉCHÉANCE
#--------------------------------

compute_flows <- function(At, Gt, q_x_d, q_x_w)
{
  T_r <- ncol(At) - 1
  nb_Sim <- nrow(At)
  
  p_d <- 1 - q_x_d[1:T_r]
  p_w <- 1 - q_x_w[1:T_r]
  survival_Probs <- c(1, cumprod(p_d * p_w))
  
  death_Flows <- matrix(0, nrow = nb_Sim, ncol = T_r+1)
  lapse_Flows <- matrix(0, nrow = nb_Sim, ncol = T_r+1)
  cash_Flows <- matrix(0, nrow = nb_Sim, ncol = T_r+1)
  
  for(year in 1:T_r) {
    death_flow <- survival_Probs[year] * q_x_d[year] * At[, year+1]
    lapse_flow <- survival_Probs[year] * (1 - q_x_d[year]) * q_x_w[year] * At[, year+1]
    
    # Éviter les NaN
    death_flow[is.na(death_flow)] <- 0
    lapse_flow[is.na(lapse_flow)] <- 0
    
    death_Flows[, year+1] <- death_flow
    lapse_Flows[, year+1] <- lapse_flow
    cash_Flows[, year+1] <- death_flow + lapse_flow
  }
  
  list(
    cash_Flows = cash_Flows, 
    death_Flows = death_Flows, 
    lapse_Flows = lapse_Flows,
    survival_Probs = survival_Probs
  )
}

#---------------------------
# CALCUL DU PAYOFF GMIB
#---------------------------

compute_GMIB_payoff <- function(At, Gt, r_Tr_vec, q_x_d, Obligation_fun, 
                                a, b, sigma, guaranteed_Income_Rate, 
                                retirement_Age, max_Age)
{
  
  nb_Sim <- nrow(At)
  T_r <- ncol(At) - 1
  
  if(is.na(guaranteed_Income_Rate) || guaranteed_Income_Rate <= 0) {
    stop("guaranteed_Income_Rate absent ou invalide.")
  }
  
  annual_Payment <- Gt[, T_r+1] * guaranteed_Income_Rate
  annuity_Values_vec <- numeric(nb_Sim)
  
  for(i in 1:nb_Sim) {
    r_Tr <- r_Tr_vec[i]
    
    # Éviter les taux négatifs ou extrêmes
    r_Tr <- max(min(r_Tr, 0.15), 0.001)
    
    ann_factor <- a_x_s(
      x = retirement_Age, 
      s = 0, 
      Qx = q_x_d, 
      r_s = r_Tr, 
      Obligation_fun = Obligation_fun,
      a = a, 
      b = b, 
      sigma = sigma, 
      max_Age = max_Age
    )
    
    # Si ann_factor est invalide, utiliser une valeur par défaut
    if(is.na(ann_factor) || ann_factor <= 0) {
      ann_factor <- 10
    }
    
    annuity_Values_vec[i] <- annual_Payment[i] * ann_factor
  }
  
  final_Payment <- pmax(At[, T_r+1], annuity_Values_vec)
  
  list(
    final_Payment = final_Payment, 
    annuity_Values_vec = annuity_Values_vec, 
    annual_Payment = annual_Payment, 
    g = guaranteed_Income_Rate
  )
}


# =============================================================================
# FONCTION simulate_GMIB_modular CORRIGÉE
# =============================================================================

simulate_GMIB_modular <- function(A_0, S_0, r_0, entry_Age, retirement_Age,
                                            a, b, sigma, sigma_S, nb_Sim,
                                            phi, q_x_d, q_x_w,
                                            nb_Time_Steps_R_per_year = 12,
                                            max_Age = 115,
                                            seed = 2025,
                                            theoretical_Bond_Price)
{
  set.seed(seed)

  T_r <- retirement_Age - entry_Age

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

  if(is.null(sim$list_Res_R)) {
    stop("simulation_Black_Scholes n'a pas renvoyé list_Res_R.")
  }

  r_path_annual <- sim$list_Res_R$mat_rt
  integral_rt_annual <- sim$list_Res_R$mat_Integral_rt

  #  FACTEURS D'ACTUALISATION
  discount_Factors <- exp(-integral_rt_annual)

  # Vérifier les NaN/Inf
  if(any(is.na(discount_Factors) | is.infinite(discount_Factors))) {
    warning("NaN ou Inf détectés dans discount_Factors. Remplacement par 0.")
    discount_Factors[is.na(discount_Factors) | is.infinite(discount_Factors)] <- 0
  }

  # === 3) COMPTES ET GT (ACCUMULATION) ===
  At <- matrix(A_0, nrow = nb_Sim, ncol = T_r + 1)
  Gt <- matrix(A_0, nrow = nb_Sim, ncol = T_r + 1)

  for(year in 1:T_r) {
    # Éviter les divisions par zéro
    ratio <- St[, year+1] / pmax(St[, year], 1e-10)
    ratio[is.na(ratio) | is.infinite(ratio)] <- 1

    At[, year+1] <- At[, year] * exp(-phi) * ratio
    Gt[, year+1] <- pmax(Gt[, year], At[, year+1])
  }

  flows <- compute_flows(At, Gt, q_x_d, q_x_w)
  cash_Flows <- flows$cash_Flows
  
  # CALCUL DE g = 1/a65_t0
  a65_t0 <- a_x_s(
    x = retirement_Age, 
    s = 0, 
    Qx = q_x_d, 
    r_s = r_0,
    Obligation_fun = theoretical_Bond_Price, 
    a = a, 
    b = b, 
    sigma = sigma, 
    max_Age = max_Age
  )

  if(is.na(a65_t0) || a65_t0 <= 0) {
    warning(sprintf("a65_t0 invalide: %.6f. Utilisation de la valeur par défault.", a65_t0))
    a65_t0 <- 10  # Valeur par défaut raisonnable
  }

  g_calc <- 1 / a65_t0

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
  cash_Flows[, T_r+1] <- cash_Flows[, T_r+1] + flows$survival_Probs[T_r+1] * final_Payment

  # Éviter les NaN dans cash_Flows
  cash_Flows[is.na(cash_Flows) | is.infinite(cash_Flows)] <- 0

  #  ACTUALISATION ET VALEURS
  # S'assurer que les dimensions correspondent
  min_cols <- min(ncol(discount_Factors), ncol(cash_Flows))
  discount_Factors <- discount_Factors[, 1:min_cols]
  cash_Flows <- cash_Flows[, 1:min_cols]

  discounted_CF <- discount_Factors * cash_Flows

  # Éviter les NaN dans le produit
  discounted_CF[is.na(discounted_CF) | is.infinite(discounted_CF)] <- 0

  contract_Values <- rowSums(discounted_CF, na.rm = TRUE)

  # Vérification finale
  if(any(is.na(contract_Values) | is.infinite(contract_Values))) {
    warning("Certaines contract_Values sont encore NaN/Inf. Remplacement par 0.")
    contract_Values[is.na(contract_Values) | is.infinite(contract_Values)] <- 0
  }

  #  MÉTRIQUES
  conversion_Rate <- mean(annuity_Values_vec > At[, T_r+1], na.rm = TRUE)

  moneyness <- At / pmax(Gt, 1e-10)
  moneyness[!is.finite(moneyness)] <- 1

  total_death_flows <- rowSums(flows$death_Flows, na.rm = TRUE)
  total_lapse_flows <- rowSums(flows$lapse_Flows, na.rm = TRUE)

  # RETOUR
  list(
    values = contract_Values,
    mean_Value = mean(contract_Values, na.rm = TRUE),
    cash_Flows = cash_Flows,
    death_Flows = flows$death_Flows,
    lapse_Flows = flows$lapse_Flows,
    discount_Factors = discount_Factors,
    At = At,
    Gt = Gt,
    Moneyness = moneyness,
    annual_Payment = mean(annual_Payment, na.rm = TRUE),
    annuity_Values_vec = annuity_Values_vec,
    account_Value = At[, T_r+1],
    conversion_Rate = conversion_Rate,
    annuity_Factors_vec = annuity_Values_vec / pmax(Gt[, T_r+1] * g_calc, 1e-10),
    g = g_calc,
    total_death_flows = total_death_flows,
    total_lapse_flows = total_lapse_flows
  )
}


# =============================================================================
# 4. MÉTRIQUES DE RISQUE
# =============================================================================

calculate_risk_metrics <- function(contract_Values, confidence_Level = 0.95) {
  metrics <- list()
  
  metrics$VaR <- quantile(contract_Values, 1 - confidence_Level)
  metrics$CVaR <- mean(contract_Values[contract_Values <= metrics$VaR])
  
  metrics$mean <- mean(contract_Values)
  metrics$sd <- sd(contract_Values)
  metrics$min <- min(contract_Values)
  metrics$max <- max(contract_Values)
  metrics$median <- median(contract_Values)
  
  risk_free_rate <- 0.02
  metrics$sharpe_ratio <- (metrics$mean - risk_free_rate) / metrics$sd
  
  metrics$skewness <- skewness(contract_Values)
  metrics$kurtosis <- kurtosis(contract_Values)
  
  initial_investment <- mean(contract_Values) * 0.9
  metrics$loss_probability <- mean(contract_Values < initial_investment)
  
  returns <- diff(log(contract_Values))
  if(length(returns) > 1) {
    metrics$max_drawdown <- maxDrawdown(returns)
  } else {
    metrics$max_drawdown <- 0
  }
  
  return(metrics)
}

# =============================================================================
# 5. PARAMÈTRES ET EXÉCUTION
# =============================================================================

# Seed global pour reproductibilité
set.seed(2025)

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

cat("Paramètres Vasicek (mensuels utilisés directement):\n")
cat(sprintf("  a = %.6f (mensuel)\n", a))
cat(sprintf("  b = %.6f (mensuel)\n", b))
cat(sprintf("  σ = %.6f (mensuel)\n\n", sigma))
cat(sprintf("Volatilité du fonds (sigma_S) = %.3f (annuelle)\n\n", sigma_S))

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
params <- list(a = a, b = b, sigma = sigma, max_Age = 115)
a65_t0 <- a_x_s(
  x = retirement_Age,
  s = 0,
  Qx = mortality_rate,
  r_s = r_0,                       # taux court initial
  Obligation_fun = theoretical_Bond_Price,
  a = a, b = b, sigma = sigma, max_Age = 115
  #params = params
)

(guaranteed_Income_Rate <- 1/a65_t0) # = 0.06940535

phi <- 0.02
nb_Sim <- 10000  # Réduit pour tester, remettez à 10000 pour les résultats finaux

# Fonction pour créer les tables
create_mortality_tables <- function(entry_Age = 55, mortalityTable = mortality_rate, lapse_rate) {
  Age_id <- paste("Age", entry_Age)
  start_idx <- which(names(mortalityTable) == Age_id)
  if(length(start_idx) == 0) {
    start_idx <- 1
    warning(sprintf("Âge %d non trouvé dans la table, utilisation de l'indice 1", entry_Age))
  }
  
  q_x_d <- mortalityTable[start_idx:length(mortalityTable)]
  q_x_w <- rep(lapse_rate, length(q_x_d))
  
  list(q_x_d = q_x_d, q_x_w = q_x_w)
}

cat("=== EXÉCUTION DES SIMULATIONS ===\n")

# GMWB
cat("1. Simulation GMWB...")
gmwb_tables <- create_mortality_tables(entry_Age = 55, mortalityTable = mortality_rate,
                                       lapse_rates$GMWB)

gmwb_result <- simulate_GMWB(A_0 = A_0, S_0 = S_0, r_0 = r_0, entry_Age = entry_Age, 
                             retirement_Age = retirement_Age, withdrawal_Years = withdrawal_Years, a = a, b = b, 
                             sigma = sigma, sigma_S = sigma_S, nb_Sim = nb_Sim, phi = phi, 
                             q_x_d = gmwb_tables$q_x_d, q_x_w = gmwb_tables$q_x_w, withdrawal_Rate = withdrawal_Rate, seed = 2025)
cat(" Terminé\n")

# GLWB
cat("2. Simulation GLWB...")
glwb_tables <- create_mortality_tables(entry_Age = 55, mortalityTable = mortality_rate, 
                                       lapse_rates$GLWB)

glwb_result <- simulate_GLWB(A_0, S_0, r_0, entry_Age, retirement_Age, max_Age,
                             a, b, sigma, sigma_S, nb_Sim,
                             phi, glwb_tables$q_x_d, glwb_tables$q_x_w, withdrawal_Rate, seed = 2025)
cat(" Terminé\n")

# GMIB
cat("3. Simulation GMIB...") 
gmib_tables <- create_mortality_tables(entry_Age = 55, mortalityTable = mortality_rate,
                                       lapse_rates$GMIB)
gmib_result <- simulate_GMIB_modular(A_0, S_0, r_0, entry_Age, retirement_Age,
                                  a, b, sigma, sigma_S, nb_Sim = 2,
                                  phi, gmib_tables$q_x_d, gmib_tables$q_x_w,
                                  nb_Time_Steps_R_per_year = 12,
                                  max_Age = 115,
                                  seed = 2025,
                                  theoretical_Bond_Price)
#Test
gmib_result$values


# =============================================================================
# 6. ANALYSE DES RÉSULTATS
# =============================================================================

cat("\n=== RÉSULTATS DES SIMULATIONS ===\n")

# Valeurs moyennes
results_summary <- data.frame(
  Produit = c("GMWB", "GLWB", "GMIB"),
  Valeur_Moyenne = c(gmwb_result$mean_Value, glwb_result$mean_Value, gmib_result$mean_Value),
  Prime_Initiale = A_0,
  Ratio_Valeur_Prime = c(gmwb_result$mean_Value/A_0, 
                         glwb_result$mean_Value/A_0, 
                         gmib_result$mean_Value/A_0)
)

print(results_summary)

# Calcul des métriques de risque
cat("\n=== MÉTRIQUES DE RISQUE (95% Confidence) ===\n")

risk_metrics <- list(
  GMWB = calculate_risk_metrics(gmwb_result$values),
  GLWB = calculate_risk_metrics(glwb_result$values),
  GMIB = calculate_risk_metrics(gmib_result$values)
)

# Affichage des métriques
for(product in names(risk_metrics)) {
  cat(sprintf("\n%s:\n", product))
  m <- risk_metrics[[product]]
  cat(sprintf("  Valeur moyenne: $%.2f\n", m$mean))
  cat(sprintf("  VaR 95%%: $%.2f\n", m$VaR))
  cat(sprintf("  CVaR 95%%: $%.2f\n", m$CVaR))
  cat(sprintf("  Volatilité: $%.2f\n", m$sd))
  cat(sprintf("  Ratio de Sharpe: %.4f\n", m$sharpe_ratio))
  cat(sprintf("  Probabilité de perte: %.2f%%\n", m$loss_probability * 100))
}

# Métriques spécifiques GMIB
cat(sprintf("\nGMIB - Métriques spécifiques:\n"))
cat(sprintf("  Taux de conversion: %.2f%%\n", gmib_result$conversion_Rate * 100))
cat(sprintf("  Valeur moyenne compte: $%.2f\n", gmib_result$account_Value))
cat(sprintf("  Valeur moyenne rente: $%.2f\n", gmib_result$annuity_Value))
cat(sprintf("  Facteur d'annuité: %.4f\n", gmib_result$annuity_Factor))

# =============================================================================
# 7. VISUALISATIONS 
# =============================================================================

cat("\n=== GÉNÉRATION DES VISUALISATIONS ===\n")

# 7.1 Distribution des valeurs
plot_distributions <- function() {
  df <- data.frame(
    GMWB = gmwb_result$values,
    GLWB = glwb_result$values,
    GMIB = gmib_result$values
  ) %>%
    pivot_longer(everything(), names_to = "Produit", values_to = "Valeur")
  
  ggplot(df, aes(x = Valeur, fill = Produit)) +
    geom_density(alpha = 0.5) +
    labs(title = "Distribution des valeurs des contrats",
         # subtitle = paste("Seed = 2025 | Simulations =", format(nb_Sim, big.mark = ",")),
         x = "Valeur ($)", y = "Densité") +
    theme_minimal() +
    scale_x_continuous(labels = dollar) +
    scale_fill_manual(values = c("GMWB" = "blue", "GLWB" = "red", "GMIB" = "green"))
}

# 7.2 Comparaison des métriques
plot_metrics_comparison <- function() {
  metrics_df <- data.frame(
    Produit = rep(c("GMWB", "GLWB", "GMIB"), each = 2),
    Métrique = rep(c("VaR 95%", "CVaR 95%"), 3),
    Valeur = c(risk_metrics$GMWB$VaR, risk_metrics$GMWB$CVaR,
               risk_metrics$GLWB$VaR, risk_metrics$GLWB$CVaR,
               risk_metrics$GMIB$VaR, risk_metrics$GMIB$CVaR)
  )
  
  ggplot(metrics_df, aes(x = Produit, y = Valeur, fill = Métrique)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Métriques de risque par produit",
         # subtitle = "Niveau de confiance: 95%",
         x = "Produit", y = "Valeur ($)") +
    theme_minimal() +
    scale_y_continuous(labels = dollar) +
    scale_fill_brewer(palette = "Set1")
}

# 7.3 Évolution des comptes (moyenne)
plot_account_evolution <- function() {
  years_gmwb <- 0:(ncol(gmwb_result$At)-1)
  years_glwb <- 0:(ncol(glwb_result$At)-1)
  years_gmib <- 0:(ncol(gmib_result$At)-1)
  
  max_years <- max(length(years_gmwb), length(years_glwb), length(years_gmib))
  
  df <- data.frame(
    Année = 0:(max_years-1),
    GMWB = c(colMeans(gmwb_result$At), rep(NA, max_years - length(years_gmwb))),
    GLWB = c(colMeans(glwb_result$At), rep(NA, max_years - length(years_glwb))),
    GMIB = c(colMeans(gmib_result$At), rep(NA, max_years - length(years_gmib)))
  ) %>%
    pivot_longer(-Année, names_to = "Produit", values_to = "Valeur_Compte")
  
  ggplot(df, aes(x = Année, y = Valeur_Compte, color = Produit)) +
    geom_line(size = 1.2) +
    geom_vline(xintercept = retirement_Age - entry_Age, linetype = "dashed", 
               color = "gray", size = 1) +
    annotate("text", x = retirement_Age - entry_Age, y = max(df$Valeur_Compte, na.rm = TRUE)*0.9,
             label = "Âge de retraite (65 ans)", angle = 90, vjust = -0.5) +
    labs(title = "Évolution moyenne des comptes",
         # subtitle = "Valeur moyenne sur toutes les simulations",
         x = "Nombre d'années depuis la souscription", y = "Valeur du compte ($)") +
    theme_minimal() +
    scale_y_continuous(labels = dollar) +
    scale_color_manual(values = c("GMWB" = "blue", "GLWB" = "red", "GMIB" = "green"))
}

# 7.4 Boxplot des valeurs finales
plot_value_boxplot <- function() {
  df <- data.frame(
    GMWB = gmwb_result$values,
    GLWB = glwb_result$values,
    GMIB = gmib_result$values
  ) %>%
    pivot_longer(everything(), names_to = "Produit", values_to = "Valeur")
  
  ggplot(df, aes(x = Produit, y = Valeur, fill = Produit)) +
    geom_boxplot(alpha = 0.7) +
    geom_hline(yintercept = A_0, linetype = "dashed", color = "red", size = 1) +
    annotate("text", x = 2.5, y = A_0 * 1.02, 
             label = "Prime initiale", color = "red") +
    labs(title = "Distribution des valeurs finales",
         # subtitle = paste("Comparaison des 3 produits | Seed = 2025"),
         x = "Produit", y = "Valeur du Contrat ($)") +
    theme_minimal() +
    scale_y_continuous(labels = dollar) +
    scale_fill_manual(values = c("GMWB" = "lightblue", "GLWB" = "lightcoral", "GMIB" = "lightgreen"))
}

# Génération des graphiques
tryCatch({
  plot_distributions()
  cat("Graphique 1: Distribution des valeurs\n")
}, error = function(e) cat(" Erreur graphique 1:", e$message, "\n"))

tryCatch({
  plot_metrics_comparison()
  cat("Graphique 2: Comparaison des métriques\n")
}, error = function(e) cat(" Erreur graphique 2:", e$message, "\n"))

tryCatch({
  plot_account_evolution()
  cat("Graphique 3: Évolution des comptes\n")
}, error = function(e) cat(" Erreur graphique 3:", e$message, "\n"))

tryCatch({
  plot_value_boxplot()
  cat("Graphique 4: Boxplot des valeurs finales\n")
}, error = function(e) cat(" Erreur graphique 4:", e$message, "\n"))

# =============================================================================
# 8. VISUALISATION MONEYNESS ET TRAJECTOIRES
# =============================================================================

plot_trajectories_At_Gt <- function(gmwb_res, glwb_res, gmib_res, entry_age, ret_age) {
  
  extract_mean_paths <- function(res, name) {
    if(is.null(res$At) || is.null(res$Gt)) {
      warning(paste("Données manquantes pour", name))
      return(NULL)
    }
    
    n_cols <- min(ncol(res$At), ncol(res$Gt))
    years <- 0:(n_cols-1)
    
    data.frame(
      Time = years,
      Age = entry_age + years,
      At_Mean = colMeans(res$At[, 1:n_cols, drop = FALSE]),
      Gt_Mean = colMeans(res$Gt[, 1:n_cols, drop = FALSE]),
      Product = name
    )
  }
  
  df_gmwb <- extract_mean_paths(gmwb_res, "GMWB")
  df_glwb <- extract_mean_paths(glwb_res, "GLWB")
  df_gmib <- extract_mean_paths(gmib_res, "GMIB")
  
  combined_df <- bind_rows(df_gmwb, df_glwb, df_gmib)
  
  plot_df <- combined_df %>%
    pivot_longer(
      cols = c(At_Mean, Gt_Mean),
      names_to = "Variable",
      values_to = "Value"
    ) %>%
    mutate(
      Variable = factor(Variable, 
                        levels = c("At_Mean", "Gt_Mean"),
                        labels = c("Valeur de Compte (At)", "Base Garantie (Gt)"))
    )
  
  p <- ggplot(plot_df, aes(x = Age, y = Value, color = Product)) +
    geom_line(aes(linetype = Variable), size = 1) +
    geom_vline(xintercept = ret_age, color = "black", linetype = "dotted", size = 0.8) +
    annotate("text", x = ret_age, y = max(plot_df$Value) * 0.1, 
             label = "Âge de retraite", angle = 90, vjust = -1, size = 3) +
    labs(
      title = "Évolution moyenne des comptes et garanties",
      # subtitle = "Comparaison des trois produits avec effet de cliquet (step-up)",
      x = "Âge de l'assuré",
      y = "Valeur ($)",
      color = "Produit",
      linetype = "Type de valeur"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      plot.title = element_text(face = "bold"),
      axis.text = element_text(size = 10)
    ) +
    scale_y_continuous(
      labels = scales::dollar_format(accuracy = 1),
      limits = c(0, max(plot_df$Value) * 1.1)
    ) +
    scale_color_manual(
      values = c("GMWB" = "#1f77b4", "GLWB" = "#d62728", "GMIB" = "#2ca02c")
    ) +
    scale_linetype_manual(
      values = c("Valeur de Compte (At)" = "solid", "Base Garantie (Gt)" = "dashed")
    )
  
  return(p)
}

plot_moneyness_distribution <- function(gmwb_res, glwb_res, gmib_res) {
  
  calculate_moneyness <- function(res, product_name, ret_year) {
    if(is.null(res$At) || is.null(res$Gt)) {
      return(data.frame(Moneyness = numeric(0), Product = character(0)))
    }
    
    if(ret_year > ncol(res$At)) {
      ret_year <- ncol(res$At)
    }
    
    moneyness <- res$Gt[, ret_year] / res$At[, ret_year]
    moneyness <- moneyness[is.finite(moneyness) & moneyness < 10]
    
    data.frame(
      Moneyness = moneyness,
      Product = product_name
    )
  }
  
  ret_year_gmwb <- (retirement_Age - entry_Age + 1)
  ret_year_glwb <- (retirement_Age - entry_Age + 1)
  ret_year_gmib <- (retirement_Age - entry_Age + 1)
  
  df_money <- bind_rows(
    calculate_moneyness(gmwb_res, "GMWB", ret_year_gmwb),
    calculate_moneyness(glwb_res, "GLWB", ret_year_glwb),
    calculate_moneyness(gmib_res, "GMIB", ret_year_gmib)
  )
  
  stats_df <- df_money %>%
    group_by(Product) %>%
    summarise(
      Mean = mean(Moneyness, na.rm = TRUE),
      Median = median(Moneyness, na.rm = TRUE),
      SD = sd(Moneyness, na.rm = TRUE),
      .groups = 'drop'
    )
  
  cat("Statistiques du Moneyness à la retraite:\n")
  print(stats_df)
  
  p <- ggplot(df_money, aes(x = Product, y = Moneyness, fill = Product)) +
    geom_violin(alpha = 0.6, scale = "width", trim = TRUE) +
    geom_boxplot(width = 0.1, alpha = 0.9, outlier.shape = NA) +
    geom_point(data = stats_df, aes(y = Mean), 
               color = "black", shape = 18, size = 4, show.legend = FALSE) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed", size = 1) +
    geom_text(x = 0.5, y = 1.05, label = "Seuil ITM (In The Money)", 
              color = "red", hjust = 0, size = 3.5) +
    labs(
      title = "Distribution du moneyness à la retraite",
      x = "Produit",
      y = "Moneyness Ratio (Gt / At)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold"),
      axis.text = element_text(size = 10)
    ) +
    scale_fill_manual(
      values = c("GMWB" = "#1f77b4", "GLWB" = "#d62728", "GMIB" = "#2ca02c")
    ) +
    coord_cartesian(ylim = c(0.5, 2.5)) +
    annotate("text", x = 1:3, y = 0.45, 
             label = sprintf("Moyenne = %.2f", stats_df$Mean),
             size = 3, color = "darkblue")
  
  return(p)
}

# =============================================================================
# 9. EXÉCUTION DES VISUALISATIONS AVANCÉES
# =============================================================================

cat("\n=== GÉNÉRATION DES VISUALISATIONS AVANCÉES ===\n")

if(exists("gmwb_result") && exists("glwb_result") && exists("gmib_result")) {
  
  tryCatch({
    cat("1. Génération du graphique des trajectoires...\n")
    p1 <- plot_trajectories_At_Gt(gmwb_result, glwb_result, gmib_result, 
                                  entry_Age, retirement_Age)
    print(p1)
    cat("Graphique des trajectoires généré\n")
  }, error = function(e) cat(" Erreur:", e$message, "\n"))
  
  tryCatch({
    cat("2. Génération du graphique de distribution du moneyness...\n")
    p2 <- plot_moneyness_distribution(gmwb_result, glwb_result, gmib_result)
    print(p2)
    cat("Graphique moneyness généré\n")
  }, error = function(e) cat(" Erreur:", e$message, "\n"))
  
  cat("\nToutes les visualisations ont été générées avec succès!\n")
  
} else {
  cat("\nERREUR : Les résultats de simulation n'existent pas.\n")
  cat("Variables requises : gmwb_result, glwb_result, gmib_result\n")
}

# =============================================================================
# 10. SAUVEGARDE DES RÉSULTATS
# =============================================================================

cat("\n=== SAUVEGARDE DES RÉSULTATS ===\n")

results_dir <- "simulation_results"
if(!dir.exists(results_dir)) {
  dir.create(results_dir)
  cat("Création du dossier", results_dir, "\n")
}

# Sauvegarde des données brutes
saveRDS(list(
  parameters = list(
    seed = 2025,
    A_0 = A_0, r_0 = r_0, sigma_S = sigma_S,
    vasicek_monthly = c(a = a, b = b, sigma = sigma),
    demographic = c(entry_Age = entry_Age, retirement_Age = retirement_Age, 
                    max_Age = max_Age, withdrawal_Years = withdrawal_Years),
    fees = phi,
    lapse_rates = lapse_rates,
    withdrawal_rate = withdrawal_Rate,
    guaranteed_income_rate = guaranteed_Income_Rate,
    nb_Sim = nb_Sim
  ),
  results = list(
    GMWB = gmwb_result,
    GLWB = glwb_result,
    GMIB = gmib_result
  ),
  risk_metrics = risk_metrics
), file = file.path(results_dir, "complete_simulation.rds"))

# Sauvegarde des résultats sous forme de tableau
summary_table <- data.frame(
  Produit = c("GMWB", "GLWB", "GMIB"),
  Valeur_Moyenne = c(gmwb_result$mean_Value, glwb_result$mean_Value, gmib_result$mean_Value),
  Écart_Type = c(sd(gmwb_result$values), sd(glwb_result$values), sd(gmib_result$values)),
  VaR_95 = c(risk_metrics$GMWB$VaR, risk_metrics$GLWB$VaR, risk_metrics$GMIB$VaR),
  CVaR_95 = c(risk_metrics$GMWB$CVaR, risk_metrics$GLWB$CVaR, risk_metrics$GMIB$CVaR),
  Ratio_Sharpe = c(risk_metrics$GMWB$sharpe_ratio, risk_metrics$GLWB$sharpe_ratio, risk_metrics$GMIB$sharpe_ratio),
  Prob_Perte = c(risk_metrics$GMWB$loss_probability * 100, 
                 risk_metrics$GLWB$loss_probability * 100, 
                 risk_metrics$GMIB$loss_probability * 100)
)

write.csv(summary_table, file.path(results_dir, "summary_results.csv"), row.names = FALSE)

# Sauvegarde des valeurs individuelles
write.csv(data.frame(
  Simulation = 1:nb_Sim,
  GMWB = gmwb_result$values,
  GLWB = glwb_result$values,
  GMIB = gmib_result$values
), file.path(results_dir, "individual_values.csv"), row.names = FALSE)

cat("Fichiers sauvegardés dans le dossier '", results_dir, "/':\n", sep = "")
cat("  - complete_simulation.rds (données complètes)\n")
cat("  - summary_results.csv (tableau récapitulatif)\n")
cat("  - individual_values.csv (valeurs individuelles)\n")

# =============================================================================
# 11. RAPPORT SYNTHÉTIQUE 
# =============================================================================

cat("\n=== RAPPORT SYNTHÉTIQUE ===\n")
cat(sprintf("Date d'exécution: %s\n", Sys.Date()))
cat(sprintf("Seed utilisé: %d\n", 2025))
cat(sprintf("Nombre de simulations: %s\n", format(nb_Sim, big.mark = ",")))
cat(sprintf("Prime initiale: $%s\n", format(A_0, big.mark = ",")))
cat("\nClassement par valeur moyenne:\n")

ranking <- results_summary[order(-results_summary$Valeur_Moyenne), ]
for(i in 1:nrow(ranking)) {
  cat(sprintf("%d. %s: $%s (%.2f%% de la prime)\n", 
              i, ranking$Produit[i], format(round(ranking$Valeur_Moyenne[i]), big.mark = ","),
              ranking$Ratio_Valeur_Prime[i] * 100))
}
cat("\n=== NOTEBOOK TERMINÉ AVEC SUCCÈS ===\n")

