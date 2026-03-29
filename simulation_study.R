# Load required libraries
library(survival)
library(dplyr)
library(tidyr)
library(purrr)
library(future)
library(furrr)

# --- 1. Theoretical Statistical Functions ---
greenwood_var_closed <- function(p_t, p_c, n) {
  if (all(p_t == 0)) return(rep(0, length(p_t)))
  L_T <- -log(1 - p_t)
  L_C <- -log(1 - p_c)
  var_expected <- ((1 - p_t)^2 / n) * (L_T / (L_T + L_C)) * (1 / ((1 - p_t) * (1 - p_c)) - 1)
  if(length(p_c) == 1) p_c <- rep(p_c, length(p_t))
  var_expected <- ifelse(p_c == 0, (p_t * (1 - p_t)) / n, var_expected)
  return(var_expected)
}

variance_iptw <- function(var, inflation_factor) { var * inflation_factor }

# --- 2. Data Generating Process ---
generate_data <- function(n, alpha_1, beta_X, lambda_C, lambda_0, beta_A, t_max = 1) {
  X <- rnorm(n, 0, 1)
  
  # Treatment Assignment
  ps <- 1 / (1 + exp(-(0 + alpha_1 * X)))
  A <- rbinom(n, 1, ps)
  
  # Survival Times 
  hazard_T <- lambda_0 * exp(beta_A * A + beta_X * X)
  T_time <- rexp(n, hazard_T)
  
  # Censoring Times
  C_time <- rexp(n, lambda_C)
  
  time <- pmin(T_time, C_time, t_max)
  status <- as.integer(T_time <= pmin(C_time, t_max))
  iptw <- A / ps + (1 - A) / (1 - ps)
  
  data.frame(A, X, ps, iptw, time, status, T_time, C_time)
}

# --- 3. Scenario Evaluator Function ---
evaluate_scenario <- function(n_sample, alpha_1, beta_X, lambda_C, lambda_0, beta_A, n_sim = 500, t_max = 1) {
  
  # 1. Generate Ground Truth (N = 500,000)
  df_true <- generate_data(500000, alpha_1, beta_X, lambda_C, lambda_0, beta_A, t_max)
  
  # --- A. Measure True Confounding Bias for RD and RR ---
  # Crude risks (Unadjusted observed data)
  crude_p1 <- mean(df_true$T_time[df_true$A == 0] <= t_max)
  crude_p2 <- mean(df_true$T_time[df_true$A == 1] <= t_max)
  crude_RD <- crude_p2 - crude_p1
  crude_RR <- crude_p2 / crude_p1
  
  # Causal Marginal risks (What IPTW targets) via exact potential outcomes
  hazard_T0 <- lambda_0 * exp(beta_A * 0 + beta_X * df_true$X)
  hazard_T1 <- lambda_0 * exp(beta_A * 1 + beta_X * df_true$X)
  
  causal_p1 <- mean(1 - exp(-hazard_T0 * t_max))
  causal_p2 <- mean(1 - exp(-hazard_T1 * t_max))
  causal_RD <- causal_p2 - causal_p1
  causal_RR <- causal_p2 / causal_p1
  
  abs_bias_RD <- abs(crude_RD - causal_RD)
  abs_bias_RR <- abs(crude_RR - causal_RR)
  
  # --- B. Theoretical Variance Setup ---
  c1_true <- mean(df_true$C_time[df_true$A == 0] <= t_max)
  c2_true <- mean(df_true$C_time[df_true$A == 1] <= t_max)
  
  w0 <- df_true$iptw[df_true$A == 0]
  w1 <- df_true$iptw[df_true$A == 1]
  v_inf_1 <- length(w0) * sum(w0^2) / (sum(w0))^2
  v_inf_2 <- length(w1) * sum(w1^2) / (sum(w1))^2
  
  n1_sim <- n_sample / 2
  n2_sim <- n_sample / 2
  
  cens_v1 <- greenwood_var_closed(causal_p1, c1_true, n1_sim)
  cens_v2 <- greenwood_var_closed(causal_p2, c2_true, n2_sim)
  
  theo_var_p1 <- variance_iptw(cens_v1, v_inf_1)
  theo_var_p2 <- variance_iptw(cens_v2, v_inf_2)
  
  theo_var_RD <- theo_var_p1 + theo_var_p2
  theo_var_logRR <- (theo_var_p1 / causal_p1^2) + (theo_var_p2 / causal_p2^2)
  
  # 2. Run Monte Carlo Iterations
  sim_results <- future_map_dfr(1:n_sim, function(i) {
    df <- generate_data(n_sample, alpha_1, beta_X, lambda_C, lambda_0, beta_A, t_max)
    
    km_weighted <- survfit(Surv(time, status) ~ A, weights = iptw, data = df)
    sum_weighted <- summary(km_weighted, times = t_max, extend = TRUE)
    
    iptw_p1 <- 1 - sum_weighted$surv[1]
    iptw_p2 <- 1 - sum_weighted$surv[2]
    
    data.frame(
      RD = iptw_p2 - iptw_p1,
      log_RR = log(iptw_p2) - log(iptw_p1)
    )
  }, .options = furrr_options(seed = TRUE))
  
  # 3. Compare Empirical vs Theoretical
  sim_results <- sim_results %>% filter(is.finite(log_RR)) # Safely handle rare p=0 events in N=500
  emp_var_RD <- var(sim_results$RD, na.rm = TRUE)
  emp_var_logRR <- var(sim_results$log_RR, na.rm = TRUE)
  
  return(data.frame(
    True_p1 = causal_p1,
    True_p2 = causal_p2,
    Abs_Bias_RD = abs_bias_RD,
    Abs_Bias_RR = abs_bias_RR,
    Theo_Var_RD = theo_var_RD,
    Emp_Var_RD = emp_var_RD,
    Ratio_RD = emp_var_RD / theo_var_RD,
    Theo_Var_logRR = theo_var_logRR,
    Emp_Var_logRR = emp_var_logRR,
    Ratio_logRR = emp_var_logRR / theo_var_logRR
  ))
}

# --- 4. Define Grid and Execute ---
run_grid_simulation <- function(n_sim = 500) {
  
  grid <- expand.grid(
    N = c(500, 2000, 10000),
    BaseRisk = c("Low (~5%)", "High (~50%)"),
    Censoring = c("None", "Modest", "High"),
    Assoc_AX = c("None", "Modest", "Strong"),
    Assoc_YX = c("None", "Modest", "Strong"),
    stringsAsFactors = FALSE
  )
  
  grid <- grid %>%
    mutate(
      lambda_C = case_when(Censoring == "None" ~ 0.001, Censoring == "Modest" ~ 0.1, Censoring == "High" ~ 0.4),
      alpha_1 = case_when(Assoc_AX == "None" ~ 0, Assoc_AX == "Modest" ~ 0.5, Assoc_AX == "Strong" ~ 1.5),
      beta_X = case_when(Assoc_YX == "None" ~ 0, Assoc_YX == "Modest" ~ 0.5, Assoc_YX == "Strong" ~ 1.5),
      
      # Target an RD of roughly +0.10 across baselines
      lambda_0 = case_when(BaseRisk == "Low (~5%)" ~ 0.051, BaseRisk == "High (~50%)" ~ 0.693),
      beta_A = case_when(BaseRisk == "Low (~5%)" ~ 1.153, BaseRisk == "High (~50%)" ~ 0.279)
    )
  
  cat(sprintf("Running grid of %d scenarios (%d simulations each)...\n", nrow(grid), n_sim))
  
  plan(multisession, workers = max(1, availableCores() - 1))
  
  results <- map_dfr(1:nrow(grid), function(i) {
    row <- grid[i, ]
    cat(sprintf(" Scenario %d/%d: N=%d, Risk=%s, Cens=%s, A~X=%s, Y~X=%s\n", 
                i, nrow(grid), row$N, row$BaseRisk, row$Censoring, row$Assoc_AX, row$Assoc_YX))
    
    eval_metrics <- evaluate_scenario(row$N, row$alpha_1, row$beta_X, row$lambda_C, row$lambda_0, row$beta_A, n_sim)
    bind_cols(row[, 1:5], eval_metrics)
  })
  
  plan(sequential)
  
  # Format final output for crisp readability
  results <- results %>%
    mutate(
      across(c(True_p1, True_p2, Abs_Bias_RD, Abs_Bias_RR), ~sprintf("%.3f", .)),
      across(c(Ratio_RD, Ratio_logRR), ~round(., 3))
    ) %>%
    arrange(BaseRisk, Assoc_AX, Assoc_YX, N, Censoring)
  
  return(results)
}

# Execute
set.seed(101)
final_grid_results <- run_grid_simulation(n_sim = 500)

# Preview highlighting the Bias vs Ratio tradeoff
print("=== Sample Output: Low Baseline (~5%), N=10,000 ===")
final_grid_results %>% 
  filter(N == 10000, BaseRisk == "Low (~5%)") %>%
  select(Assoc_AX, Assoc_YX, Abs_Bias_RD, Abs_Bias_RR, Ratio_RD, Ratio_logRR) %>%
  tail(5) %>% print()