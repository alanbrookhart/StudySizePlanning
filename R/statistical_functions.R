# ==============================================================================
# Script: statistical_functions.R
# Author: Alan Brookhart (alan.brookhart@duke.edu)
# Date: April 2026
#
# Description: 
#   The core statistical engine for the study size planning 
#   application. This script contains all closed-form mathematical derivations 
#   required to estimate precision and statistical power for observational 
#   cohort studies subject to right-censoring and confounding adjustment.
#
# Key Methodologies:
#   - Baseline Variance: Standard binomial variance estimators.
#   - Censoring Adjustment: Closed-form asymptotic variance of the Kaplan-Meier 
#     risk estimator under exponential event and independent censoring times 
#     (Greenwood's formula).
#   - Confounding Adjustment: Approximate variance inflation for Inverse 
#     Probability of Treatment Weighting (IPTW) using Kish's design effect.
#   - Effect Measures: Confidence interval bounds and power calculations for 
#     both the Risk Difference (additive) and Risk Ratio (multiplicative/log scale).
#
# Optimization Solvers:
#   Includes univariate root-finding functions (`solve_sample_size_rd` and `_rr`) 
#   to dynamically back-calculate the required sample size to achieve a target 
#   statistical power given user-defined parameters.
#
# Usage:
#   This file is sourced dynamically by the Shiny server (`app.R`) for real-time 
#   UI calculations and by the validation suite (`simulation_study.R`) to 
#   generate theoretical benchmarks for the Monte Carlo simulations. All functions 
#   are thoroughly unit-tested.
# ==============================================================================


#' Calculate Greenwood's Variance with Closed-Form Solution
#'
#' Computes the exact asymptotic variance of the Kaplan-Meier risk estimator
#' under exponential event and independent exponential censoring times.
#'
#' @param p_t Numeric vector of event risks at time t.
#' @param p_c Numeric or vector of censoring risks at time t.
#' @param n Integer sample size.
#' @return Numeric vector of variance estimates.
#' @export
greenwood_var_closed <- function(p_t, p_c, n) {
  # Handle zero risk to avoid division by zero
  if (all(p_t == 0)) return(rep(0, length(p_t)))
  
  L_T <- -log(1 - p_t)
  L_C <- -log(1 - p_c)
  
  var_expected <- ((1 - p_t)^2 / n) * (L_T / (L_T + L_C)) * (1 / ((1 - p_t) * (1 - p_c)) - 1)
  
  # Expand p_c to match p_t length to prevent ifelse() collapsing the array
  if(length(p_c) == 1) p_c <- rep(p_c, length(p_t))
  
  # Fall back to binomial variance if censoring is exactly 0
  var_expected <- ifelse(p_c == 0, (p_t * (1 - p_t)) / n, var_expected)
  
  return(var_expected)
}

#' Calculate Baseline (Binomial) Variance
#'
#' Computes the exact variance of a proportion under simple random sampling.
#'
#' @param p Numeric vector of proportions.
#' @param n Integer sample size.
#' @return Numeric vector of variance estimates.
#' @export
variance_baseline <- function(p, n) {
  (p * (1 - p)) / n
}

#' Calculate Risk Difference Metrics
#'
#' Computes the standard error, confidence interval width, and statistical power 
#' for a risk difference on the additive scale.
#'
#' @param p1 Risk in group 1.
#' @param p2 Risk in group 2.
#' @param var1 Variance estimate for group 1.
#' @param var2 Variance estimate for group 2.
#' @param alpha Significance level (default is 0.05).
#' @return A list containing the Risk Difference (RD), Standard Error (SE), 
#' CI Width, and Statistical Power against the null.
#' @export
metrics_risk_difference <- function(p1, p2, var1, var2, alpha = 0.05) {
  Z <- qnorm(1 - alpha / 2)
  
  RD <- p2 - p1
  se_rd <- sqrt(var1 + var2)
  width_rd <- 2 * Z * se_rd
  
  # Power calculation against H0: RD = 0
  power_rd <- pnorm(abs(RD)/se_rd - Z) + pnorm(-abs(RD)/se_rd - Z)
  
  list(
    RD = RD,
    SE = se_rd,
    Width = width_rd,
    Power = power_rd
  )
}

#' Calculate Risk Ratio Metrics
#'
#' Computes the standard error, confidence interval width, and statistical power 
#' for a risk ratio using the Delta method on the log scale.
#'
#' @param p1 Risk in group 1.
#' @param p2 Risk in group 2.
#' @param var1 Variance estimate for group 1.
#' @param var2 Variance estimate for group 2.
#' @param alpha Significance level (default is 0.05).
#' @return A list containing the Risk Ratio (RR), log RR, log Standard Error (SE_log), 
#' CI Width on the original scale, and Statistical Power against the null.
#' @export
metrics_risk_ratio <- function(p1, p2, var1, var2, alpha = 0.05) {
  Z <- qnorm(1 - alpha / 2)
  
  RR <- p2 / p1
  log_RR <- log(RR)
  
  # Delta method: Var(log(p)) = Var(p) / p^2
  se_log_rr <- sqrt(var1 / (p1^2) + var2 / (p2^2))
  
  # Width on original scale
  width_rr <- RR * (exp(Z * se_log_rr) - exp(-Z * se_log_rr))
  
  # Power against H0: log(RR) = 0
  power_rr <- pnorm(abs(log_RR)/se_log_rr - Z) + pnorm(-abs(log_RR)/se_log_rr - Z)
  
  list(
    RR = RR,
    log_RR = log_RR,
    SE_log = se_log_rr,
    Width = width_rr,
    Power = power_rr
  )
}

#' Apply IPTW Variance Inflation
#'
#' Applies a variance inflation factor to account for inverse probability weighting.
#' Used primarily for study size planning approximations.
#'
#' @param var Numeric base variance.
#' @param inflation_factor Numeric variance inflation factor (>= 1).
#' @return Numeric inflated variance.
#' @export
variance_iptw <- function(var, inflation_factor) {
  var * inflation_factor
}

#' Calculate Variance Inflation from Weights (Kish's Approximation)
#'
#' Computes the variance inflation (design effect) for Hajek-type estimators
#' with IPT weights based on Kish's effective sample size approximation.
#'
#' @param weights Numeric vector of IPT weights for a treatment group.
#' @return Numeric variance inflation factor.
#' @export
calculate_variance_inflation <- function(weights) {
  n <- length(weights)
  sum_w <- sum(weights)
  sum_w_sq <- sum(weights^2)

  # Kish's variance inflation formula
  (n * sum_w_sq) / (sum_w^2)
}

#' Solve for Sample Size to Achieve Target Power (Risk Difference)
#'
#' Numerically solves for the required sample size in group 1 to achieve
#' a target statistical power for detecting a risk difference, accounting
#' for censoring and IPTW variance inflation.
#'
#' @param target_power Numeric target power (e.g., 0.80 for 80% power).
#' @param p1 Risk in group 1.
#' @param p2 Risk in group 2.
#' @param allocation_ratio Numeric ratio n2/n1 (e.g., 1 for equal allocation).
#' @param cens_1 Censoring risk in group 1.
#' @param cens_2 Censoring risk in group 2.
#' @param v1 IPTW variance inflation factor for group 1.
#' @param v2 IPTW variance inflation factor for group 2.
#' @param alpha Significance level (default 0.05).
#' @param n_max Maximum sample size to search (default 1e6).
#' @return Required sample size for group 1, or NA if no solution found.
#' @export
solve_sample_size_rd <- function(target_power, p1, p2, allocation_ratio = 1,
                                  cens_1 = 0, cens_2 = 0, v1 = 1, v2 = 1,
                                  alpha = 0.05, n_max = 1e6) {

  # Objective function: difference between achieved power and target power
  power_diff <- function(n1) {
    n2 <- n1 * allocation_ratio

    # Calculate variances for each group
    var_1 <- greenwood_var_closed(p1, cens_1, n1)
    var_2 <- greenwood_var_closed(p2, cens_2, n2)

    # Apply IPTW inflation
    var_1 <- variance_iptw(var_1, v1)
    var_2 <- variance_iptw(var_2, v2)

    # Calculate power
    metrics <- metrics_risk_difference(p1, p2, var_1, var_2, alpha)

    # Return difference from target
    metrics$Power - target_power
  }

  # Check if solution exists in reasonable range
  power_at_max <- power_diff(n_max)
  if (power_at_max < 0) {
    warning("Target power not achievable with n1 <= ", n_max)
    return(NA)
  }

  # Check if very small sample size is sufficient
  power_at_min <- power_diff(10)
  if (power_at_min > 0) {
    return(10)  # Minimum practical sample size
  }

  # Use numerical root finding
  result <- tryCatch({
    uniroot(power_diff, interval = c(10, n_max), tol = 0.1)
  }, error = function(e) {
    warning("Failed to find sample size: ", e$message)
    return(list(root = NA))
  })

  return(ceiling(result$root))
}

#' Solve for Sample Size to Achieve Target Power (Risk Ratio)
#'
#' Numerically solves for the required sample size in group 1 to achieve
#' a target statistical power for detecting a risk ratio, accounting
#' for censoring and IPTW variance inflation.
#'
#' @param target_power Numeric target power (e.g., 0.80 for 80% power).
#' @param p1 Risk in group 1.
#' @param p2 Risk in group 2.
#' @param allocation_ratio Numeric ratio n2/n1 (e.g., 1 for equal allocation).
#' @param cens_1 Censoring risk in group 1.
#' @param cens_2 Censoring risk in group 2.
#' @param v1 IPTW variance inflation factor for group 1.
#' @param v2 IPTW variance inflation factor for group 2.
#' @param alpha Significance level (default 0.05).
#' @param n_max Maximum sample size to search (default 1e6).
#' @return Required sample size for group 1, or NA if no solution found.
#' @export
solve_sample_size_rr <- function(target_power, p1, p2, allocation_ratio = 1,
                                  cens_1 = 0, cens_2 = 0, v1 = 1, v2 = 1,
                                  alpha = 0.05, n_max = 1e6) {

  # Objective function: difference between achieved power and target power
  power_diff <- function(n1) {
    n2 <- n1 * allocation_ratio

    # Calculate variances for each group
    var_1 <- greenwood_var_closed(p1, cens_1, n1)
    var_2 <- greenwood_var_closed(p2, cens_2, n2)

    # Apply IPTW inflation
    var_1 <- variance_iptw(var_1, v1)
    var_2 <- variance_iptw(var_2, v2)

    # Calculate power
    metrics <- metrics_risk_ratio(p1, p2, var_1, var_2, alpha)

    # Return difference from target
    metrics$Power - target_power
  }

  # Check if solution exists in reasonable range
  power_at_max <- power_diff(n_max)
  if (power_at_max < 0) {
    warning("Target power not achievable with n1 <= ", n_max)
    return(NA)
  }

  # Check if very small sample size is sufficient
  power_at_min <- power_diff(10)
  if (power_at_min > 0) {
    return(10)  # Minimum practical sample size
  }

  # Use numerical root finding
  result <- tryCatch({
    uniroot(power_diff, interval = c(10, n_max), tol = 0.1)
  }, error = function(e) {
    warning("Failed to find sample size: ", e$message)
    return(list(root = NA))
  })

  return(ceiling(result$root))
}