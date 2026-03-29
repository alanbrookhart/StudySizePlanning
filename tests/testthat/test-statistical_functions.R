# Unit tests for statistical functions

# Source the functions
source("../../R/statistical_functions.R")

# --- Tests for greenwood_var_closed ---
test_that("greenwood_var_closed returns zero for zero risk", {
  result <- greenwood_var_closed(p_t = 0, p_c = 0.2, n = 100)
  expect_equal(result, 0)
})

test_that("greenwood_var_closed falls back to binomial when censoring is zero", {
  p_t <- 0.1
  n <- 100
  result <- greenwood_var_closed(p_t = p_t, p_c = 0, n = n)
  expected <- (p_t * (1 - p_t)) / n
  expect_equal(result, expected)
})

test_that("greenwood_var_closed handles vector inputs correctly", {
  p_t <- c(0.1, 0.2, 0.3)
  p_c <- 0.2
  n <- 100
  result <- greenwood_var_closed(p_t = p_t, p_c = p_c, n = n)
  expect_length(result, 3)
  expect_true(all(result > 0))
})

test_that("greenwood_var_closed produces larger variance than binomial with censoring", {
  p_t <- 0.1
  p_c <- 0.3
  n <- 100
  var_censored <- greenwood_var_closed(p_t = p_t, p_c = p_c, n = n)
  var_binomial <- (p_t * (1 - p_t)) / n
  expect_gt(var_censored, var_binomial)
})

test_that("greenwood_var_closed variance increases with higher censoring", {
  p_t <- 0.1
  n <- 100
  var_low_cens <- greenwood_var_closed(p_t = p_t, p_c = 0.1, n = n)
  var_high_cens <- greenwood_var_closed(p_t = p_t, p_c = 0.5, n = n)
  expect_gt(var_high_cens, var_low_cens)
})

# --- Tests for variance_baseline ---
test_that("variance_baseline computes binomial variance correctly", {
  p <- 0.5
  n <- 100
  result <- variance_baseline(p, n)
  expected <- (0.5 * 0.5) / 100
  expect_equal(result, expected)
})

test_that("variance_baseline handles edge cases", {
  expect_equal(variance_baseline(0, 100), 0)
  expect_equal(variance_baseline(1, 100), 0)
})

test_that("variance_baseline works with vectors", {
  p <- c(0.1, 0.2, 0.3)
  n <- 100
  result <- variance_baseline(p, n)
  expect_length(result, 3)
  expect_true(all(result >= 0))
})

# --- Tests for metrics_risk_difference ---
test_that("metrics_risk_difference computes RD correctly", {
  p1 <- 0.1
  p2 <- 0.2
  var1 <- 0.0009
  var2 <- 0.0016
  result <- metrics_risk_difference(p1, p2, var1, var2, alpha = 0.05)

  expect_equal(result$RD, 0.1)
  expect_true(result$SE > 0)
  expect_true(result$Width > 0)
  expect_true(result$Power >= 0 && result$Power <= 1)
})

test_that("metrics_risk_difference power is low when RD is zero", {
  p1 <- 0.1
  p2 <- 0.1
  var1 <- 0.0009
  var2 <- 0.0009
  result <- metrics_risk_difference(p1, p2, var1, var2, alpha = 0.05)

  expect_equal(result$RD, 0)
  expect_true(result$Power < 0.1)  # Should be close to alpha
})

test_that("metrics_risk_difference power increases with larger sample size (smaller variance)", {
  p1 <- 0.1
  p2 <- 0.2
  # Large sample (small variance)
  result_large <- metrics_risk_difference(p1, p2, var1 = 0.00009, var2 = 0.00016, alpha = 0.05)
  # Small sample (large variance)
  result_small <- metrics_risk_difference(p1, p2, var1 = 0.009, var2 = 0.016, alpha = 0.05)

  expect_gt(result_large$Power, result_small$Power)
})

test_that("metrics_risk_difference handles vector inputs", {
  p1 <- 0.1
  p2 <- c(0.15, 0.20, 0.25)
  var1 <- 0.0009
  var2 <- c(0.001275, 0.0016, 0.001875)
  result <- metrics_risk_difference(p1, p2, var1, var2, alpha = 0.05)

  expect_length(result$RD, 3)
  expect_length(result$Power, 3)
})

# --- Tests for metrics_risk_ratio ---
test_that("metrics_risk_ratio computes RR correctly", {
  p1 <- 0.1
  p2 <- 0.2
  var1 <- 0.0009
  var2 <- 0.0016
  result <- metrics_risk_ratio(p1, p2, var1, var2, alpha = 0.05)

  expect_equal(result$RR, 2)
  expect_equal(result$log_RR, log(2))
  expect_true(result$SE_log > 0)
  expect_true(result$Width > 0)
  expect_true(result$Power >= 0 && result$Power <= 1)
})

test_that("metrics_risk_ratio power is low when RR is 1", {
  p1 <- 0.1
  p2 <- 0.1
  var1 <- 0.0009
  var2 <- 0.0009
  result <- metrics_risk_ratio(p1, p2, var1, var2, alpha = 0.05)

  expect_equal(result$RR, 1)
  expect_equal(result$log_RR, 0)
  expect_true(result$Power < 0.1)  # Should be close to alpha
})

test_that("metrics_risk_ratio width increases with variance", {
  p1 <- 0.1
  p2 <- 0.2
  # Small variance
  result_small_var <- metrics_risk_ratio(p1, p2, var1 = 0.00009, var2 = 0.00016, alpha = 0.05)
  # Large variance
  result_large_var <- metrics_risk_ratio(p1, p2, var1 = 0.009, var2 = 0.016, alpha = 0.05)

  expect_gt(result_large_var$Width, result_small_var$Width)
})

test_that("metrics_risk_ratio handles vector inputs", {
  p1 <- 0.1
  p2 <- c(0.15, 0.20, 0.25)
  var1 <- 0.0009
  var2 <- c(0.001275, 0.0016, 0.001875)
  result <- metrics_risk_ratio(p1, p2, var1, var2, alpha = 0.05)

  expect_length(result$RR, 3)
  expect_length(result$Power, 3)
})

# --- Tests for variance_iptw ---
test_that("variance_iptw applies inflation correctly", {
  var <- 0.001
  inflation <- 1.5
  result <- variance_iptw(var, inflation)
  expect_equal(result, 0.0015)
})

test_that("variance_iptw with inflation = 1 returns original variance", {
  var <- 0.001
  result <- variance_iptw(var, 1)
  expect_equal(result, var)
})

test_that("variance_iptw works with vectors", {
  var <- c(0.001, 0.002, 0.003)
  inflation <- 1.5
  result <- variance_iptw(var, inflation)
  expect_equal(result, c(0.0015, 0.003, 0.0045))
})

# --- Tests for calculate_variance_inflation ---
test_that("calculate_variance_inflation returns 1 for equal weights", {
  weights <- rep(1, 100)
  result <- calculate_variance_inflation(weights)
  expect_equal(result, 1)
})

test_that("calculate_variance_inflation returns > 1 for unequal weights", {
  weights <- c(rep(0.5, 50), rep(2, 50))
  result <- calculate_variance_inflation(weights)
  expect_gt(result, 1)
})

test_that("calculate_variance_inflation increases with weight variability", {
  weights_low_var <- c(rep(0.9, 50), rep(1.1, 50))
  weights_high_var <- c(rep(0.5, 50), rep(2, 50))

  result_low <- calculate_variance_inflation(weights_low_var)
  result_high <- calculate_variance_inflation(weights_high_var)

  expect_gt(result_high, result_low)
})

test_that("calculate_variance_inflation matches Kish's formula", {
  weights <- c(1, 2, 3, 4, 5)
  n <- length(weights)
  sum_w <- sum(weights)
  sum_w_sq <- sum(weights^2)
  expected <- (n * sum_w_sq) / (sum_w^2)

  result <- calculate_variance_inflation(weights)
  expect_equal(result, expected)
})
