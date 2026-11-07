# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Script: app/R/dashboard_data.R
# ==============================================================================
# Description:
#   Centralized data, model, and metadata loader for the Shiny Dashboard.
#   Implements robust project root detection, in-memory caching, and safe I/O.
# ==============================================================================

# Ensure project library is prioritized
local({
  curr <- getwd()
  cand1 <- file.path(curr, ".Rlibrary")
  cand2 <- file.path(dirname(curr), ".Rlibrary")
  proj_lib <- if (dir.exists(cand1)) cand1 else if (dir.exists(cand2)) cand2 else NULL
  if (!is.null(proj_lib) && !(proj_lib %in% .libPaths())) {
    .libPaths(c(proj_lib, .libPaths()))
  }
})

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ranger)
  library(xgboost)
  library(isotree)
})

# In-memory session cache
.dash_cache <- new.env(parent = emptyenv())

#' Locate the FraudShield project root directory reliably
#' @return Normalized absolute path string.
get_project_root <- function() {
  if (exists("proj_root", envir = .dash_cache)) {
    return(get("proj_root", envir = .dash_cache))
  }
  curr <- getwd()
  if (file.exists(file.path(curr, "FraudShield.Rproj")) || dir.exists(file.path(curr, "models"))) {
    root <- normalizePath(curr, winslash = "/")
  } else if (file.exists(file.path(dirname(curr), "FraudShield.Rproj")) || dir.exists(file.path(dirname(curr), "models"))) {
    root <- normalizePath(dirname(curr), winslash = "/")
  } else {
    root <- normalizePath(curr, winslash = "/")
  }
  assign("proj_root", root, envir = .dash_cache)
  return(root)
}

#' Load Holdout Test Dataset (56,962 rows)
#' @return data.table of test set.
get_test_data <- function() {
  if (exists("test_data", envir = .dash_cache)) {
    return(get("test_data", envir = .dash_cache))
  }
  root <- get_project_root()
  rds_path <- file.path(root, "data", "processed", "test_data.rds")
  if (!file.exists(rds_path)) {
    stop(sprintf("Test dataset not found at %s", rds_path))
  }
  dt <- as.data.table(readRDS(rds_path))
  data.table::setindex(dt, NULL)
  assign("test_data", dt, envir = .dash_cache)
  return(dt)
}

#' Retrieve Holdout Test Dataset augmented with model probabilities, predictions & risk tiers
#' @param model_type "random_forest", "xgboost", "logistic_regression"
#' @param threshold Numeric operating threshold (default 0.50)
#' @return data.table with pre-scored columns
get_scored_test_data <- function(model_type = "random_forest", threshold = 0.50) {
  cache_key <- paste0("scored_test_", tolower(model_type))
  
  if (exists(cache_key, envir = .dash_cache)) {
    dt <- copy(get(cache_key, envir = .dash_cache))
    dt[, model_prediction := ifelse(fraud_probability >= threshold, "Flagged Alert", "Cleared")]
    dt[, is_alert := as.integer(fraud_probability >= threshold)]
    dt[, risk_category := factor(fcase(
      fraud_probability < 0.20, "Low Risk",
      fraud_probability < threshold, "Medium Risk",
      fraud_probability < 0.80, "High Risk",
      default = "Critical Risk"
    ), levels = c("Low Risk", "Medium Risk", "High Risk", "Critical Risk"))]
    return(dt)
  }
  
  test_dt <- copy(get_test_data())
  m_type  <- tolower(model_type)
  pred_cols <- c(paste0("v", 1:28), "amount_log", "elapsed_hours")
  
  # Compute probabilities via cached model
  prob <- numeric(nrow(test_dt))
  if (m_type == "random_forest") {
    rf_model <- get_cached_model("random_forest")
    prob <- predict(rf_model, data = as.data.frame(test_dt[, ..pred_cols]))$predictions[, "Fraud"]
  } else if (m_type == "xgboost") {
    xgb_model <- get_cached_model("xgboost")
    X_mat <- as.matrix(test_dt[, ..pred_cols])
    dtest <- xgboost::xgb.DMatrix(data = X_mat)
    prob <- predict(xgb_model, dtest)
  } else if (m_type == "logistic_regression") {
    lr_model <- get_cached_model("logistic_regression")
    prob <- suppressWarnings(predict(lr_model, newdata = as.data.frame(test_dt[, ..pred_cols]), type = "response"))
  }
  
  prob <- pmin(pmax(as.numeric(prob), 0), 1)
  
  # Augment test_dt with presentation columns
  test_dt[, transaction_id    := sprintf("TX-%05d", .I)]
  test_dt[, fraud_probability := round(prob, 4)]
  test_dt[, model_prediction  := ifelse(fraud_probability >= threshold, "Flagged Alert", "Cleared")]
  test_dt[, is_alert          := as.integer(fraud_probability >= threshold)]
  test_dt[, risk_category     := factor(fcase(
    fraud_probability < 0.20, "Low Risk",
    fraud_probability < threshold, "Medium Risk",
    fraud_probability < 0.80, "High Risk",
    default = "Critical Risk"
  ), levels = c("Low Risk", "Medium Risk", "High Risk", "Critical Risk"))]
  test_dt[, actual_status     := factor(ifelse(class == 1, "Confirmed Fraud", "Legitimate"), levels = c("Legitimate", "Confirmed Fraud"))]
  test_dt[, elapsed_seconds   := as.numeric(time)]
  test_dt[, scoring_model     := m_type]
  
  data.table::setindex(test_dt, NULL)
  assign(cache_key, test_dt, envir = .dash_cache)
  return(test_dt)
}

#' Load Cleaned Dataset Sample for Rapid Exploration & Visualizations
#' @param n_sample Integer. Subsample size for ultra-responsive UI (defaults to 30,000).
#' @return data.table
get_exploration_dataset <- function(n_sample = 30000) {
  cache_key <- paste0("explore_data_", n_sample)
  if (exists(cache_key, envir = .dash_cache)) {
    return(get(cache_key, envir = .dash_cache))
  }
  root <- get_project_root()
  csv_path <- file.path(root, "data", "processed", "creditcard_cleaned.csv")
  if (!file.exists(csv_path)) {
    stop("Cleaned dataset not found at data/processed/creditcard_cleaned.csv")
  }
  
  # Read with data.table
  dt <- data.table::fread(csv_path, nrows = if (is.null(n_sample)) -1 else n_sample * 2)
  if (!is.null(n_sample) && nrow(dt) > n_sample) {
    # Stratified quick subsample preserving all frauds in read window
    set.seed(42)
    fraud_dt <- dt[class == 1]
    legit_dt <- dt[class == 0]
    n_legit  <- min(nrow(legit_dt), n_sample - nrow(fraud_dt))
    dt <- rbind(fraud_dt, legit_dt[sample(.N, n_legit)])
  }
  data.table::setindex(dt, NULL)
  assign(cache_key, dt, envir = .dash_cache)
  return(dt)
}

#' Load Model Comparison Evaluation Metrics
#' @return data.frame
get_model_comparison_table <- function() {
  if (exists("model_comp_tbl", envir = .dash_cache)) {
    return(get("model_comp_tbl", envir = .dash_cache))
  }
  root <- get_project_root()
  csv_path <- file.path(root, "outputs", "tables", "model_comparison.csv")
  if (!file.exists(csv_path)) {
    stop("Model comparison table not found.")
  }
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  assign("model_comp_tbl", df, envir = .dash_cache)
  return(df)
}

#' Load Threshold Performance Grid Table
#' @return data.frame
get_threshold_analysis_table <- function() {
  if (exists("thresh_tbl", envir = .dash_cache)) {
    return(get("thresh_tbl", envir = .dash_cache))
  }
  root <- get_project_root()
  csv_path <- file.path(root, "outputs", "tables", "threshold_analysis.csv")
  if (!file.exists(csv_path)) {
    stop("Threshold analysis table not found.")
  }
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  assign("thresh_tbl", df, envir = .dash_cache)
  return(df)
}

#' Load Confusion Matrices Table
#' @return data.frame
get_confusion_matrices_table <- function() {
  if (exists("conf_mat_tbl", envir = .dash_cache)) {
    return(get("conf_mat_tbl", envir = .dash_cache))
  }
  root <- get_project_root()
  csv_path <- file.path(root, "outputs", "tables", "confusion_matrices.csv")
  if (!file.exists(csv_path)) {
    stop("Confusion matrices table not found.")
  }
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  assign("conf_mat_tbl", df, envir = .dash_cache)
  return(df)
}

#' Load Class Distribution Table
#' @return data.frame
get_class_distribution_table <- function() {
  if (exists("class_dist_tbl", envir = .dash_cache)) {
    return(get("class_dist_tbl", envir = .dash_cache))
  }
  root <- get_project_root()
  csv_path <- file.path(root, "outputs", "tables", "class_distribution_summary.csv")
  
  if (file.exists(csv_path)) {
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
  } else {
    # Compute from cleaned dataset
    clean_csv <- file.path(root, "data", "processed", "creditcard_cleaned.csv")
    if (file.exists(clean_csv)) {
      dt <- data.table::fread(clean_csv, select = c("class", "amount"))
      df <- dt[, .(
        Records = .N,
        Percentage = round((.N / 284807) * 100, 4),
        Mean_Amount = round(mean(amount), 2),
        Total_Amount = round(sum(amount), 2)
      ), by = class][order(class)]
      df$Status <- ifelse(df$class == 1, "Fraudulent", "Legitimate")
      setnames(df, "class", "Class")
    } else {
      df <- data.frame(
        Class = c(0, 1),
        Status = c("Legitimate", "Fraudulent"),
        Records = c(284315, 492),
        Percentage = c(99.8273, 0.1727),
        Mean_Amount = c(88.29, 122.21),
        Total_Amount = c(25102462.00, 60127.97),
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Ensure consistent status labels
  df$Status <- ifelse(df$Class == 1, "Fraudulent", "Legitimate")
  assign("class_dist_tbl", df, envir = .dash_cache)
  return(df)
}

#' Load Hourly Fraud Distribution Table
#' @return data.frame
get_hourly_risk_table <- function() {
  if (exists("hourly_tbl", envir = .dash_cache)) {
    return(get("hourly_tbl", envir = .dash_cache))
  }
  root <- get_project_root()
  csv_path <- file.path(root, "outputs", "tables", "hourly_fraud_risk_table.csv")
  
  if (file.exists(csv_path)) {
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
  } else {
    # Compute from cleaned dataset
    clean_csv <- file.path(root, "data", "processed", "creditcard_cleaned.csv")
    if (file.exists(clean_csv)) {
      dt <- data.table::fread(clean_csv, select = c("time", "class", "amount"))
      dt[, elapsed_hour := as.integer(floor(time / 3600))]
      df <- dt[, .(
        Total_Transactions = .N,
        Fraud_Count = sum(class == 1),
        Legit_Count = sum(class == 0),
        Fraud_Rate_Pct = round((sum(class == 1) / .N) * 100, 4),
        Mean_Amount = round(mean(amount), 2),
        Total_Amount = round(sum(amount), 2)
      ), by = elapsed_hour][order(elapsed_hour)]
      setnames(df, "elapsed_hour", "Elapsed_Hour")
    } else {
      df <- data.frame()
    }
  }
  
  # Ensure standard aliases for seamless visualization binding
  if (nrow(df) > 0) {
    if ("Elapsed_Hour" %in% names(df)) {
      df$Hour <- df$Elapsed_Hour
    } else if ("Hour" %in% names(df)) {
      df$Elapsed_Hour <- df$Hour
    }
    if (!"Fraud_Percentage" %in% names(df) && "Fraud_Rate_Pct" %in% names(df)) {
      df$Fraud_Percentage <- df$Fraud_Rate_Pct
    } else if (!"Fraud_Rate_Pct" %in% names(df) && "Fraud_Percentage" %in% names(df)) {
      df$Fraud_Rate_Pct <- df$Fraud_Percentage
    }
  }
  
  assign("hourly_tbl", df, envir = .dash_cache)
  return(df)
}

#' Load Executive Portfolio KPIs from Verified Dataset Metrics
#' @return list of KPI metrics
get_portfolio_kpis <- function() {
  if (exists("portfolio_kpis", envir = .dash_cache)) {
    return(get("portfolio_kpis", envir = .dash_cache))
  }
  
  class_df <- tryCatch(get_class_distribution_table(), error = function(e) NULL)
  
  if (!is.null(class_df) && nrow(class_df) >= 2) {
    tot_tx    <- sum(class_df$Records)
    fraud_rec <- class_df[class_df$Class == 1, ]
    legit_rec <- class_df[class_df$Class == 0, ]
    
    fraud_tx   <- fraud_rec$Records
    legit_tx   <- legit_rec$Records
    fraud_prev <- (fraud_tx / tot_tx) * 100
    
    fraud_tot_amt <- if ("Total_Amount" %in% names(fraud_rec) && !is.na(fraud_rec$Total_Amount)) fraud_rec$Total_Amount else 60127.97
    avg_ticket    <- 88.35
    fraud_avg     <- fraud_rec$Mean_Amount
    legit_avg     <- legit_rec$Mean_Amount
  } else {
    tot_tx        <- 284807
    fraud_tx      <- 492
    legit_tx      <- 284315
    fraud_prev    <- (492 / 284807) * 100
    fraud_tot_amt <- 60127.97
    avg_ticket    <- 88.35
    fraud_avg     <- 122.21
    legit_avg     <- 88.29
  }
  
  res <- list(
    total_transactions     = tot_tx,
    fraud_transactions     = fraud_tx,
    legitimate_volume      = legit_tx,
    fraud_prevalence_pct   = fraud_prev,
    total_fraud_loss       = fraud_tot_amt,
    avg_ticket_size        = avg_ticket,
    fraud_avg_ticket       = fraud_avg,
    legit_avg_ticket       = legit_avg,
    imbalance_ratio_str    = "578 : 1"
  )
  
  assign("portfolio_kpis", res, envir = .dash_cache)
  return(res)
}

#' Load Feature Statistics Table
#' @return data.frame
get_feature_statistics_table <- function() {
  if (exists("feat_stats_tbl", envir = .dash_cache)) {
    return(get("feat_stats_tbl", envir = .dash_cache))
  }
  root <- get_project_root()
  csv_path <- file.path(root, "outputs", "tables", "feature_statistics.csv")
  if (file.exists(csv_path)) {
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
  } else {
    df <- data.frame()
  }
  assign("feat_stats_tbl", df, envir = .dash_cache)
  return(df)
}

#' Retrieve Serialized Model Binary
#' @param model_type "random_forest", "xgboost", "logistic_regression", "isolation_forest"
#' @return R model object
get_cached_model <- function(model_type = "random_forest") {
  clean_type <- gsub("[ _-]+", "_", tolower(trimws(model_type)))
  m_key <- paste0("model_", clean_type)
  if (exists(m_key, envir = .dash_cache)) {
    return(get(m_key, envir = .dash_cache))
  }
  root <- get_project_root()
  m_files <- list(
    random_forest       = file.path(root, "models", "random_forest.rds"),
    xgboost             = file.path(root, "models", "xgboost_model.rds"),
    logistic_regression = file.path(root, "models", "logistic_regression.rds"),
    isolation_forest    = file.path(root, "models", "isolation_forest.rds")
  )
  fpath <- m_files[[clean_type]]
  if (is.null(fpath) || !file.exists(fpath)) {
    stop(sprintf("Model file for '%s' not found.", model_type))
  }
  m_obj <- readRDS(fpath)
  assign(m_key, m_obj, envir = .dash_cache)
  return(m_obj)
}

#' Extract Feature Importance from Random Forest / Models
#' @return data.frame of feature names and importance scores
get_feature_importance_df <- function() {
  if (exists("feat_imp_df", envir = .dash_cache)) {
    return(get("feat_imp_df", envir = .dash_cache))
  }
  rf_model <- tryCatch(get_cached_model("random_forest"), error = function(e) NULL)
  if (!is.null(rf_model) && !is.null(rf_model$variable.importance)) {
    imp <- rf_model$variable.importance
    df <- data.frame(
      Feature = names(imp),
      Importance = as.numeric(imp),
      stringsAsFactors = FALSE
    )
    df <- df[order(-df$Importance), ]
    df$RelativeImportance <- round((df$Importance / max(df$Importance)) * 100, 2)
  } else {
    # Fallback to feature correlation rankings
    root <- get_project_root()
    corr_path <- file.path(root, "outputs", "tables", "feature_correlation.csv")
    if (file.exists(corr_path)) {
      corr_df <- read.csv(corr_path, stringsAsFactors = FALSE)
      df <- data.frame(
        Feature = corr_df$Feature,
        Importance = abs(corr_df$Correlation_with_Class),
        RelativeImportance = round(abs(corr_df$Correlation_with_Class) * 100 / max(abs(corr_df$Correlation_with_Class)), 2),
        stringsAsFactors = FALSE
      )
    } else {
      df <- data.frame(Feature = character(0), Importance = numeric(0), RelativeImportance = numeric(0))
    }
  }
  assign("feat_imp_df", df, envir = .dash_cache)
  return(df)
}

