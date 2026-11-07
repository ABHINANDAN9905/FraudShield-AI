# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Script: app/R/ml_engine.R
# ==============================================================================
# Description:
#   Advanced Machine Learning Benchmarking, Financial Cost Simulation,
#   Threshold Optimization, Statistical Confidence, Calibration,
#   Population Stability Monitoring, and SQLite Experiment Registry.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(DBI)
  library(RSQLite)
})

# In-memory engine cache
.ml_engine_cache <- new.env(parent = emptyenv())

# ------------------------------------------------------------------------------
# 1. ADVANCED BENCHMARKING CALCULATIONS
# ------------------------------------------------------------------------------

#' Compute comprehensive multi-dimensional classification metrics
#' @param dt Scored data.table containing 'class' and 'fraud_probability'
#' @param threshold Numeric decision threshold (default 0.50)
#' @return List of classification, probability, and operational metrics
compute_advanced_metrics <- function(dt, threshold = 0.50) {
  if (is.null(dt) || nrow(dt) == 0) return(NULL)
  
  y_true <- as.integer(dt$class)
  p_pred <- as.numeric(dt$fraud_probability)
  y_pred <- as.integer(p_pred >= threshold)
  
  n <- length(y_true)
  tp <- sum(y_true == 1 & y_pred == 1)
  fp <- sum(y_true == 0 & y_pred == 1)
  fn <- sum(y_true == 1 & y_pred == 0)
  tn <- sum(y_true == 0 & y_pred == 0)
  
  # Basic classification rates
  precision <- if (tp + fp > 0) tp / (tp + fp) else 0.0
  recall    <- if (tp + fn > 0) tp / (tp + fn) else 0.0
  spec      <- if (tn + fp > 0) tn / (tn + fp) else 0.0
  bal_acc   <- (recall + spec) / 2
  
  # F-measures
  f1  <- if (precision + recall > 0) (2 * precision * recall) / (precision + recall) else 0.0
  f2  <- if (4 * precision + recall > 0) (5 * precision * recall) / (4 * precision + recall) else 0.0
  f05 <- if (0.25 * precision + recall > 0) (1.25 * precision * recall) / (0.25 * precision + recall) else 0.0
  
  # Advanced correlation & agreement
  denom_mcc <- sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) * as.numeric(tn + fp) * as.numeric(tn + fn))
  mcc <- if (denom_mcc > 0) (as.numeric(tp) * as.numeric(tn) - as.numeric(fp) * as.numeric(fn)) / denom_mcc else 0.0
  
  p_obs <- (tp + tn) / n
  p_exp <- ((as.numeric(tp + fp) * as.numeric(tp + fn)) + (as.numeric(fn + tn) * as.numeric(fp + tn))) / (as.numeric(n)^2)
  kappa <- if (1 - p_exp > 0) (p_obs - p_exp) / (1 - p_exp) else 0.0
  
  gmean <- sqrt(recall * spec)
  fpr   <- if (fp + tn > 0) fp / (fp + tn) else 0.0
  fnr   <- if (fn + tp > 0) fn / (fn + tp) else 0.0
  npv   <- if (tn + fn > 0) tn / (tn + fn) else 0.0
  
  # Probability calibration metrics
  brier <- mean((p_pred - y_true)^2)
  eps <- 1e-15
  p_clipped <- pmin(pmax(p_pred, eps), 1 - eps)
  logloss <- -mean(y_true * log(p_clipped) + (1 - y_true) * log(1 - p_clipped))
  
  # Expected Calibration Error (10 bins)
  bin_breaks <- seq(0, 1, by = 0.1)
  bins <- cut(p_pred, breaks = bin_breaks, include.lowest = TRUE)
  calib_agg <- data.table(p = p_pred, y = y_true, bin = bins)[, .(
    count = .N,
    conf = mean(p),
    acc = mean(y)
  ), by = bin]
  ece <- sum((calib_agg$count / n) * abs(calib_agg$acc - calib_agg$conf))
  
  list(
    n = n, tp = tp, fp = fp, fn = fn, tn = tn,
    threshold = threshold,
    precision = precision, recall = recall, specificity = spec,
    balanced_accuracy = bal_acc,
    f1_score = f1, f2_score = f2, f05_score = f05,
    mcc = mcc, kappa = kappa, gmean = gmean,
    fpr = fpr, fnr = fnr, npv = npv,
    brier_score = brier, log_loss = logloss, ece = ece
  )
}

# ------------------------------------------------------------------------------
# 2. FINANCIAL COST SIMULATION & OPTIMIZATION
# ------------------------------------------------------------------------------

#' Simulate financial fraud exposure, investigation cost, and net benefit
#' @param dt Scored test data.table
#' @param threshold Numeric threshold
#' @param cost_fp Numeric investigation cost per false alarm ($ USD)
#' @param cost_fn_pct Numeric financial loss multiplier on missed frauds (default 1.0)
#' @param recovery_rate Post-fraud recovery rate (default 0.10)
#' @param capacity Daily investigator capacity
simulate_financial_risk <- function(dt, threshold = 0.50, cost_fp = 15.0, cost_fn_pct = 1.0, recovery_rate = 0.10, capacity = 100) {
  if (is.null(dt) || nrow(dt) == 0) return(NULL)
  
  y_true <- dt$class
  p_pred <- dt$fraud_probability
  amt    <- dt$amount
  is_alt <- as.integer(p_pred >= threshold)
  
  total_exposure   <- sum(amt[y_true == 1])
  baseline_loss    <- total_exposure * cost_fn_pct * (1 - recovery_rate)
  
  # Missed fraud loss (False Negatives)
  missed_amt       <- sum(amt[y_true == 1 & is_alt == 0])
  missed_loss      <- missed_amt * cost_fn_pct * (1 - recovery_rate)
  
  # Prevented fraud volume (True Positives)
  caught_amt       <- sum(amt[y_true == 1 & is_alt == 1])
  prevented_loss   <- caught_amt * (1 - recovery_rate)
  
  # False Positive investigation overhead
  fp_count         <- sum(y_true == 0 & is_alt == 1)
  tp_count         <- sum(y_true == 1 & is_alt == 1)
  fn_count         <- sum(y_true == 1 & is_alt == 0)
  total_alerts     <- fp_count + tp_count
  fp_invest_cost   <- fp_count * cost_fp
  
  # Total Decision Cost
  total_cost       <- missed_loss + fp_invest_cost
  net_benefit      <- baseline_loss - total_cost
  savings_pct      <- if (baseline_loss > 0) (net_benefit / baseline_loss) * 100 else 0.0
  cost_per_detected <- if (tp_count > 0) fp_invest_cost / tp_count else 0.0
  cost_per_1k_tx   <- (total_cost / nrow(dt)) * 1000
  
  list(
    threshold          = threshold,
    total_exposure     = total_exposure,
    baseline_loss      = baseline_loss,
    missed_amount      = missed_amt,
    missed_loss        = missed_loss,
    prevented_amount   = caught_amt,
    prevented_loss     = prevented_loss,
    false_positives    = fp_count,
    true_positives     = tp_count,
    false_negatives    = fn_count,
    total_alerts       = total_alerts,
    fp_invest_cost     = fp_invest_cost,
    total_cost         = total_cost,
    net_benefit        = net_benefit,
    savings_pct        = savings_pct,
    cost_per_detected  = cost_per_detected,
    cost_per_1k_tx     = cost_per_1k_tx,
    capacity_exceeded  = (total_alerts > capacity)
  )
}

#' Generate cost curve across thresholds to locate global cost-minimizing threshold
#' @param dt Scored data.table
#' @param cost_fp Numeric investigation cost ($)
#' @param cost_fn_pct Numeric loss multiplier
#' @param recovery_rate Numeric recovery rate
#' @return data.frame of threshold evaluations
generate_cost_threshold_curve <- function(dt, cost_fp = 15.0, cost_fn_pct = 1.0, recovery_rate = 0.10) {
  thresholds <- seq(0.05, 0.95, by = 0.05)
  res_list <- lapply(thresholds, function(th) {
    sim <- simulate_financial_risk(dt, threshold = th, cost_fp = cost_fp, cost_fn_pct = cost_fn_pct, recovery_rate = recovery_rate)
    data.frame(
      Threshold        = th,
      Total_Cost       = sim$total_cost,
      Missed_Loss      = sim$missed_loss,
      FP_Invest_Cost   = sim$fp_invest_cost,
      Net_Benefit      = sim$net_benefit,
      True_Positives   = sim$true_positives,
      False_Positives  = sim$false_positives,
      Total_Alerts     = sim$total_alerts,
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, res_list)
  opt_idx <- which.min(df$Total_Cost)
  df$Is_Optimal <- FALSE
  df$Is_Optimal[opt_idx] <- TRUE
  return(df)
}

# ------------------------------------------------------------------------------
# 3. STATISTICAL CONFIDENCE INTERVALS (Reproducible Bootstrap)
# ------------------------------------------------------------------------------

#' Compute Bootstrap Confidence Intervals for model performance metrics
#' @param dt Scored data.table
#' @param threshold Decision threshold
#' @param n_boot Number of resamples (default 100 for responsive UI)
#' @param seed Random seed for reproducibility
compute_bootstrap_metrics <- function(dt, threshold = 0.50, n_boot = 100, seed = 42) {
  cache_key <- paste0("boot_ci_", threshold, "_", n_boot, "_", seed)
  if (exists(cache_key, envir = .ml_engine_cache)) {
    return(get(cache_key, envir = .ml_engine_cache))
  }
  
  y_true <- as.integer(dt$class)
  p_pred <- as.numeric(dt$fraud_probability)
  
  fraud_idx <- which(y_true == 1)
  legit_idx <- which(y_true == 0)
  n_fraud <- length(fraud_idx)
  
  set.seed(seed)
  boot_rec  <- numeric(n_boot)
  boot_prec <- numeric(n_boot)
  boot_f1   <- numeric(n_boot)
  boot_mcc  <- numeric(n_boot)
  
  for (b in 1:n_boot) {
    s_f <- sample(fraud_idx, n_fraud, replace = TRUE)
    # Stratified subsample of legitimate class for high performance
    s_l <- sample(legit_idx, min(length(legit_idx), 3000), replace = TRUE)
    idx <- c(s_f, s_l)
    
    yb <- y_true[idx]
    pb <- p_pred[idx]
    predb <- as.integer(pb >= threshold)
    
    tp <- sum(yb == 1 & predb == 1)
    fp <- sum(yb == 0 & predb == 1)
    fn <- sum(yb == 1 & predb == 0)
    tn <- sum(yb == 0 & predb == 0)
    
    r <- if (tp + fn > 0) tp / (tp + fn) else 0.0
    p <- if (tp + fp > 0) tp / (tp + fp) else 0.0
    f <- if (r + p > 0) (2 * p * r) / (p + r) else 0.0
    denom <- sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) * as.numeric(tn + fp) * as.numeric(tn + fn))
    m <- if (denom > 0) (as.numeric(tp)*tn - as.numeric(fp)*fn) / denom else 0.0
    
    boot_rec[b]  <- r
    boot_prec[b] <- p
    boot_f1[b]   <- f
    boot_mcc[b]  <- m
  }
  
  res <- data.frame(
    Metric   = c("Recall (Sensitivity)", "Precision (PPV)", "F1-Score", "Matthews Corr (MCC)"),
    Estimate = c(mean(boot_rec), mean(boot_prec), mean(boot_f1), mean(boot_mcc)),
    CI_Lower = c(quantile(boot_rec, 0.025), quantile(boot_prec, 0.025), quantile(boot_f1, 0.025), quantile(boot_mcc, 0.025)),
    CI_Upper = c(quantile(boot_rec, 0.975), quantile(boot_prec, 0.975), quantile(boot_f1, 0.975), quantile(boot_mcc, 0.975)),
    Variance = c(var(boot_rec), var(boot_prec), var(boot_f1), var(boot_mcc)),
    stringsAsFactors = FALSE
  )
  
  assign(cache_key, res, envir = .ml_engine_cache)
  return(res)
}

# ------------------------------------------------------------------------------
# 4. PROBABILITY CALIBRATION CURVE (Reliability Diagram)
# ------------------------------------------------------------------------------

#' Compute empirical calibration reliability curve across deciles
#' @param dt Scored data.table
#' @param n_bins Number of probability bins (default 10)
compute_calibration_curve <- function(dt, n_bins = 10) {
  p_pred <- dt$fraud_probability
  y_true <- dt$class
  
  breaks <- seq(0, 1, length.out = n_bins + 1)
  bins <- cut(p_pred, breaks = breaks, include.lowest = TRUE)
  
  agg <- data.table(p = p_pred, y = y_true, bin = bins)[, .(
    Bin_Count       = .N,
    Mean_Predicted  = mean(p),
    Observed_Rate   = mean(y)
  ), by = bin][order(bin)]
  
  agg[, Lower_Bound := breaks[1:(length(breaks)-1)]]
  agg[, Upper_Bound := breaks[2:length(breaks)]]
  agg[, Bin_Label := sprintf("%.1f - %.1f", Lower_Bound, Upper_Bound)]
  
  return(as.data.frame(agg))
}

# ------------------------------------------------------------------------------
# 5. POPULATION STABILITY & DRIFT DIAGNOSTIC
# ------------------------------------------------------------------------------

#' Calculate Population Stability Index (PSI) between baseline and comparison timeframes
#' @param dt Scored data.table
#' @param split_hour Elapsed hour dividing reference and diagnostic windows (default 24h)
compute_population_drift <- function(dt, split_hour = 24) {
  ref_scores  <- dt[elapsed_hours <= split_hour, fraud_probability]
  comp_scores <- dt[elapsed_hours > split_hour, fraud_probability]
  
  bins <- 10
  breaks <- seq(0, 1, length.out = bins + 1)
  ref_h  <- hist(ref_scores, breaks = breaks, plot = FALSE)$counts
  comp_h <- hist(comp_scores, breaks = breaks, plot = FALSE)$counts
  
  # Smooth zero bins with Laplace smoothing
  ref_pct  <- (ref_h + 1) / (length(ref_scores) + bins)
  comp_pct <- (comp_h + 1) / (length(comp_scores) + bins)
  
  psi_components <- (comp_pct - ref_pct) * log(comp_pct / ref_pct)
  psi_total <- sum(psi_components)
  
  status <- if (psi_total < 0.10) {
    "Stable (No Significant Shift)"
  } else if (psi_total < 0.25) {
    "Moderate Warning (Distribution Shift Observed)"
  } else {
    "Critical Drift (Significant Population Shift)"
  }
  
  status_color <- if (psi_total < 0.10) "#16A34A" else if (psi_total < 0.25) "#D97706" else "#DC2626"
  
  list(
    psi = psi_total,
    status = status,
    status_color = status_color,
    ref_window = sprintf("Day 1 (Hours 0.0 - %.1fh): %s transactions", split_hour, format(length(ref_scores), big.mark = ",")),
    comp_window = sprintf("Day 2 (Hours %.1f - 48.0h): %s transactions", split_hour, format(length(comp_scores), big.mark = ",")),
    bins_df = data.frame(
      Bin = sprintf("%.1f - %.1f", breaks[1:bins], breaks[2:(bins+1)]),
      Reference_Pct = ref_pct * 100,
      Comparison_Pct = comp_pct * 100,
      PSI_Component = psi_components
    )
  )
}

# ------------------------------------------------------------------------------
# 6. SQLITE EXPERIMENT REGISTRY REPOSITORY
# ------------------------------------------------------------------------------

#' Connect to SQLite database and ensure experiment registry table exists
get_sqlite_connection <- function() {
  root <- get_project_root()
  db_path <- file.path(root, "data", "investigations.sqlite")
  con <- dbConnect(SQLite(), db_path)
  
  dbExecute(con, "
  CREATE TABLE IF NOT EXISTS ml_experiments (
    experiment_id TEXT PRIMARY KEY,
    model_name TEXT NOT NULL,
    model_version TEXT NOT NULL,
    paradigm TEXT NOT NULL,
    hyperparameters TEXT,
    random_seed INTEGER,
    training_date TEXT,
    evaluation_date TEXT,
    train_rows INTEGER,
    test_rows INTEGER,
    pr_auc REAL,
    roc_auc REAL,
    recall REAL,
    precision REAL,
    f1_score REAL,
    optimal_threshold REAL,
    experiment_notes TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )")
  
  # Ensure baseline experiments exist
  count <- dbGetQuery(con, "SELECT COUNT(*) as n FROM ml_experiments")$n
  if (count == 0) {
    seed_runs <- data.frame(
      experiment_id = c("EXP-2026-001", "EXP-2026-002", "EXP-2026-003", "EXP-2026-004"),
      model_name = c("Random Forest", "XGBoost", "Logistic Regression", "Isolation Forest"),
      model_version = c("v1.2.0-champion", "v1.1.0-challenger", "v1.0.0-baseline", "v0.9.0-unsupervised"),
      paradigm = c("Supervised Ensemble", "Gradient Boosted Trees", "Penalized GLM", "Unsupervised Anomaly"),
      hyperparameters = c(
        "trees=80, mtry=6, min_node_size=5, splitrule=gini",
        "nrounds=100, max_depth=6, eta=0.1, scale_pos_weight=570",
        "family=binomial, link=logit, max_iter=100",
        "ntrees=100, sample_size=256, ndim=1"
      ),
      random_seed = c(42, 42, 42, 42),
      training_date = c("2026-11-05 00:59:37", "2026-11-05 00:59:41", "2026-11-05 00:58:28", "2026-11-05 00:59:42"),
      evaluation_date = c("2026-11-05 01:00:15", "2026-11-05 01:00:15", "2026-11-05 01:00:15", "2026-11-05 01:00:15"),
      train_rows = c(227845, 227845, 227845, 227845),
      test_rows = c(56962, 56962, 56962, 56962),
      pr_auc = c(0.8782, 0.8523, 0.7625, 0.2715),
      roc_auc = c(0.9664, 0.9860, 0.9707, 0.9549),
      recall = c(0.8065, 0.8387, 0.6129, 0.3978),
      precision = c(0.9615, 0.7500, 0.8382, 0.2868),
      f1_score = c(0.8772, 0.7919, 0.7081, 0.3333),
      optimal_threshold = c(0.50, 0.50, 0.50, 0.58),
      experiment_notes = c(
        "Champion architecture: Highest PR-AUC (0.8782) and Precision (96.15%). Only 3 false alarms in test set.",
        "Challenger: Highest ROC-AUC (0.9860) and Recall (83.87%), but 26 false positives.",
        "Baseline GLM: Strong explainability, moderate sensitivity (61.29% recall).",
        "Unsupervised benchmark: Anomaly scoring without labels; evaluated at score percentile 0.58."
      ),
      stringsAsFactors = FALSE
    )
    dbWriteTable(con, "ml_experiments", seed_runs, append = TRUE)
  }
  
  return(con)
}

#' Fetch all experiment runs from SQLite registry
get_experiment_registry <- function() {
  con <- tryCatch(get_sqlite_connection(), error = function(e) NULL)
  if (is.null(con)) return(data.frame())
  df <- dbGetQuery(con, "SELECT * FROM ml_experiments ORDER BY created_at DESC")
  dbDisconnect(con)
  return(df)
}

#' Log a new experiment record into the SQLite registry
log_experiment_run <- function(exp_id, model_name, version, paradigm, params, seed, pr_auc, roc_auc, rec, prec, f1, thresh, notes) {
  con <- tryCatch(get_sqlite_connection(), error = function(e) NULL)
  if (is.null(con)) return(FALSE)
  
  stmt <- "
  INSERT OR REPLACE INTO ml_experiments (
    experiment_id, model_name, model_version, paradigm, hyperparameters,
    random_seed, training_date, evaluation_date, train_rows, test_rows,
    pr_auc, roc_auc, recall, precision, f1_score, optimal_threshold, experiment_notes
  ) VALUES (?, ?, ?, ?, ?, ?, datetime('now'), datetime('now'), 227845, 56962, ?, ?, ?, ?, ?, ?, ?)
  "
  res <- dbExecute(con, stmt, params = list(
    exp_id, model_name, version, paradigm, params, seed,
    pr_auc, roc_auc, rec, prec, f1, thresh, notes
  ))
  dbDisconnect(con)
  return(res > 0)
}

