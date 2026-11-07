# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Script: app/R/prediction_engine.R
# ==============================================================================
# Description:
#   Enterprise-Grade Transaction Risk Assessment and Decision Intelligence Engine.
#   Provides API-ready, production-grade functions for single and batch prediction,
#   schema validation, normalized 0-100 risk scoring, multi-model consensus,
#   weighted PR-AUC ensembling, financial risk simulation, uncertainty diagnostics,
#   and persistent RSQLite audit registry.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(DBI)
  library(RSQLite)
  library(jsonlite)
  library(digest)
})

# In-memory prediction cache
.pred_engine_cache <- new.env(parent = emptyenv())

# Canonical 30-feature vector expected by all trained supervised models
CANONICAL_PREDICTORS <- c(paste0("v", 1:28), "amount_log", "elapsed_hours")

# ------------------------------------------------------------------------------
# 1. SCHEMA VALIDATION & INPUT PREPROCESSING
# ------------------------------------------------------------------------------

#' Validate incoming transaction data for model ingestion
#' @param input_data data.frame, data.table, or named list
#' @return list(valid = logical, errors = character(), warnings = character(), clean_data = data.table)
validate_transaction_input <- function(input_data) {
  errors <- character()
  warnings <- character()
  
  if (is.null(input_data)) {
    return(list(valid = FALSE, errors = "Input data is null or empty.", warnings = warnings, clean_data = NULL))
  }
  
  if (is.list(input_data) && !is.data.frame(input_data)) {
    input_data <- as.data.frame(t(unlist(input_data)), stringsAsFactors = FALSE)
  }
  
  dt <- as.data.table(copy(input_data))
  if (nrow(dt) == 0) {
    return(list(valid = FALSE, errors = "Input dataset contains 0 records.", warnings = warnings, clean_data = NULL))
  }
  
  # Normalize column names to lowercase for robust matching
  names(dt) <- tolower(trimws(names(dt)))
  
  # Check for monetary amount column
  if (!"amount" %in% names(dt)) {
    errors <- c(errors, "Missing required column: 'amount' (Transaction monetary value).")
  } else {
    amt_non_num <- sum(!is.finite(as.numeric(dt$amount)))
    if (amt_non_num > 0) {
      errors <- c(errors, sprintf("Column 'amount' contains %d non-finite or non-numeric values.", amt_non_num))
    }
  }
  
  # Check for PCA features v1 through v28
  pca_cols <- paste0("v", 1:28)
  missing_pca <- setdiff(pca_cols, names(dt))
  if (length(missing_pca) > 0) {
    errors <- c(errors, sprintf("Missing required PCA features: %s.", paste(missing_pca, collapse = ", ")))
  } else {
    # Check for NaN, Inf
    for (col in pca_cols) {
      non_num <- sum(!is.finite(as.numeric(dt[[col]])))
      if (non_num > 0) {
        errors <- c(errors, sprintf("Column '%s' contains %d non-finite values (Inf, -Inf, or NaN).", col, non_num))
        break
      }
    }
  }
  
  # Preprocess canonical features if valid
  clean_dt <- NULL
  if (length(errors) == 0) {
    clean_dt <- copy(dt)
    
    # Ensure amount is numeric and non-negative
    clean_dt[, amount := pmax(as.numeric(amount), 0.0)]
    
    # Derive amount_log if not present
    if (!"amount_log" %in% names(clean_dt)) {
      clean_dt[, amount_log := round(log1p(amount), 4)]
    } else {
      clean_dt[, amount_log := as.numeric(amount_log)]
    }
    
    # Derive elapsed_hours if not present
    if (!"elapsed_hours" %in% names(clean_dt)) {
      if ("time" %in% names(clean_dt)) {
        clean_dt[, elapsed_hours := round(as.numeric(time) / 3600, 4)]
      } else {
        clean_dt[, elapsed_hours := 0.0]
        warnings <- c(warnings, "Neither 'elapsed_hours' nor 'time' provided; defaulted elapsed_hours to 0.0.")
      }
    } else {
      clean_dt[, elapsed_hours := as.numeric(elapsed_hours)]
    }
    
    # Ensure numeric types across all canonical features
    for (f in CANONICAL_PREDICTORS) {
      clean_dt[[f]] <- as.numeric(clean_dt[[f]])
    }
  }
  
  list(
    valid = (length(errors) == 0),
    errors = errors,
    warnings = warnings,
    clean_data = clean_dt
  )
}

#' Preprocess transaction data for model input
#' @param input_data Raw data.frame or data.table
#' @return Canonical data.table ready for inference
preprocess_transaction <- function(input_data) {
  val_res <- validate_transaction_input(input_data)
  if (!val_res$valid) {
    stop(paste("Validation failure:", paste(val_res$errors, collapse = "; ")))
  }
  return(val_res$clean_data)
}

# ------------------------------------------------------------------------------
# 2. MODEL PREDICTION FUNCTIONS
# ------------------------------------------------------------------------------

#' Predict fraud probability or anomaly score
#' @param clean_data Preprocessed data.table containing CANONICAL_PREDICTORS
#' @param model_type "random_forest", "xgboost", "logistic_regression", "isolation_forest"
#' @return numeric vector of probabilities (or anomaly scores for isolation forest)
predict_fraud_probability <- function(clean_data, model_type = "random_forest") {
  clean_type <- gsub("[ _-]+", "_", tolower(trimws(model_type)))
  if (clean_type %in% c("best", "champion")) clean_type <- "random_forest"
  
  model_obj <- get_cached_model(clean_type)
  df_input <- as.data.frame(clean_data[, ..CANONICAL_PREDICTORS])
  
  n_rows <- nrow(df_input)
  preds <- numeric(n_rows)
  
  if (clean_type == "random_forest") {
    rf_res <- predict(model_obj, data = df_input)$predictions
    preds <- as.numeric(rf_res[, "Fraud"])
  } else if (clean_type == "xgboost") {
    mat_input <- as.matrix(df_input)
    preds <- as.numeric(predict(model_obj, mat_input))
  } else if (clean_type == "logistic_regression") {
    preds <- as.numeric(suppressWarnings(predict(model_obj, newdata = df_input, type = "response")))
  } else if (clean_type == "isolation_forest") {
    preds <- as.numeric(isotree::predict.isolation_forest(model_obj, df_input))
  } else {
    stop(sprintf("Unsupported model architecture '%s'.", model_type))
  }
  
  if (clean_type != "isolation_forest") {
    preds <- pmin(pmax(preds, 0.0), 1.0)
  }
  return(preds)
}

# ------------------------------------------------------------------------------
# 3. NORMALIZED RISK SCORING & POLICY ENGINE
# ------------------------------------------------------------------------------

#' Map raw model prediction to a standardized 0-100 risk score
#' @param prob Numeric raw probability or anomaly score
#' @param model_type Model type string
#' @return Numeric risk score bounded [0.0, 100.0]
calculate_risk_score <- function(prob, model_type = "random_forest") {
  clean_type <- gsub("[ _-]+", "_", tolower(trimws(model_type)))
  
  if (clean_type == "isolation_forest") {
    # Isolation forest anomaly score baseline is ~0.35; critical anomaly is >= 0.65
    raw_score <- (prob - 0.35) / (0.65 - 0.35) * 100
    risk_score <- pmin(pmax(raw_score, 0.0), 100.0)
  } else {
    # Supervised probability maps directly to 0-100 scale
    risk_score <- pmin(pmax(prob * 100.0, 0.0), 100.0)
  }
  round(risk_score, 1)
}

#' Classify transaction risk into 5 policy bands
#' @param risk_score Numeric risk score (0-100)
#' @param bands List of cutoff boundaries
#' @return Factor of risk category
classify_risk_category <- function(risk_score, bands = list(very_low = 20, low = 40, medium = 60, high = 80)) {
  vl <- bands$very_low
  l  <- bands$low
  m  <- bands$medium
  h  <- bands$high
  
  cats <- fcase(
    risk_score < vl, "Very Low Risk",
    risk_score >= vl & risk_score < l, "Low Risk",
    risk_score >= l  & risk_score < m, "Medium Risk",
    risk_score >= m  & risk_score < h, "High Risk",
    risk_score >= h, "Critical Risk",
    default = "Unknown Risk"
  )
  
  factor(cats, levels = c("Very Low Risk", "Low Risk", "Medium Risk", "High Risk", "Critical Risk"))
}

#' Apply configurable decision policy to determine recommended action
#' @param risk_score Numeric score (0-100)
#' @param prob Supervised probability (0-1)
#' @param threshold Operating decision threshold (default 0.50)
#' @param policy_config Optional custom rules
#' @return list(decision, reason, action_code, badge_color, policy_version, disclaimer)
apply_decision_policy <- function(risk_score, prob, threshold = 0.50, policy_config = NULL) {
  crit_thresh <- if (!is.null(policy_config$critical_threshold)) policy_config$critical_threshold else 0.80
  rev_thresh  <- if (!is.null(policy_config$review_threshold)) policy_config$review_threshold else threshold
  mon_thresh  <- if (!is.null(policy_config$monitor_threshold)) policy_config$monitor_threshold else 0.30
  
  if (prob >= crit_thresh || risk_score >= 80.0) {
    decision    <- "Decline / Block Recommendation"
    reason      <- sprintf("Critical fraud risk: Estimated probability (%.1f%%) exceeds critical cutoff (%.1f%%). High risk of unrecoverable loss.", prob * 100, crit_thresh * 100)
    action_code <- "DEC"
    badge_color <- "#DC2626"
    badge_bg    <- "#FEE2E2"
  } else if (prob >= rev_thresh || risk_score >= 60.0) {
    decision    <- "Manual Review (Escalate to Fraud Desk)"
    reason      <- sprintf("High fraud risk: Probability (%.1f%%) exceeds operational threshold (%.1f%%). Feature signals warrant forensic verification.", prob * 100, rev_thresh * 100)
    action_code <- "REV"
    badge_color <- "#EA580C"
    badge_bg    <- "#FFEDD5"
  } else if (prob >= mon_thresh || risk_score >= 40.0) {
    decision    <- "Monitor (Step-Up Authentication / 3DS OTP)"
    reason      <- sprintf("Medium risk: Probability (%.1f%%) elevated above baseline. Recommend secondary challenge before authorization.", prob * 100)
    action_code <- "MON"
    badge_color <- "#D97706"
    badge_bg    <- "#FEF3C7"
  } else {
    decision    <- "Approve / Low Risk (Auto-Clear)"
    reason      <- sprintf("Nominal behavior: Fraud probability (%.2f%%) well below alert threshold. Transaction aligns with legitimate profile.", prob * 100)
    action_code <- "APP"
    badge_color <- "#16A34A"
    badge_bg    <- "#DCFCE7"
  }
  
  list(
    decision        = decision,
    reason          = reason,
    action_code     = action_code,
    badge_color     = badge_color,
    badge_bg        = badge_bg,
    policy_version  = "POL-2026.4-ENTERPRISE",
    disclaimer      = "Analytical recommendation for risk triage. Not an automated clearinghouse settlement block."
  )
}

# ------------------------------------------------------------------------------
# 4. MULTI-MODEL PREDICTION & VALIDATED ENSEMBLE
# ------------------------------------------------------------------------------

#' Evaluate transaction across all available models with PR-AUC weighted ensemble
#' @param clean_data Preprocessed single row data.table
#' @param threshold Decision threshold (default 0.50)
#' @return list of predictions, consensus, disagreement, and ensemble details
compare_model_predictions <- function(clean_data, threshold = 0.50) {
  p_rf  <- predict_fraud_probability(clean_data, "random_forest")[1]
  p_xgb <- predict_fraud_probability(clean_data, "xgboost")[1]
  p_lr  <- predict_fraud_probability(clean_data, "logistic_regression")[1]
  s_iso <- predict_fraud_probability(clean_data, "isolation_forest")[1]
  
  # Validation PR-AUC scores from holdout evaluation (outputs/tables/model_comparison.csv)
  pr_rf  <- 0.8782
  pr_xgb <- 0.8523
  pr_lr  <- 0.7625
  total_weight <- pr_rf + pr_xgb + pr_lr
  
  w_rf  <- pr_rf / total_weight
  w_xgb <- pr_xgb / total_weight
  w_lr  <- pr_lr / total_weight
  
  # Weighted PR-AUC Ensemble probability
  p_ens <- (w_rf * p_rf) + (w_xgb * p_xgb) + (w_lr * p_lr)
  p_ens <- pmin(pmax(p_ens, 0.0), 1.0)
  
  dec_rf  <- ifelse(p_rf >= threshold, "Alert", "Cleared")
  dec_xgb <- ifelse(p_xgb >= threshold, "Alert", "Cleared")
  dec_lr  <- ifelse(p_lr >= threshold, "Alert", "Cleared")
  dec_ens <- ifelse(p_ens >= threshold, "Alert", "Cleared")
  
  # Consensus evaluation
  all_agree <- (dec_rf == dec_xgb && dec_xgb == dec_lr)
  two_agree <- (dec_rf == dec_xgb || dec_rf == dec_lr || dec_xgb == dec_lr)
  
  consensus_label <- if (all_agree) {
    sprintf("Unanimous Consensus (%s)", dec_rf)
  } else if (two_agree) {
    "Majority Agreement (2 of 3 Models Agree)"
  } else {
    "Split Decision (High Model Divergence)"
  }
  
  # Maximum absolute spread across supervised models
  disagreement_spread <- max(c(p_rf, p_xgb, p_lr)) - min(c(p_rf, p_xgb, p_lr))
  
  models_df <- data.frame(
    Model = c("Random Forest (Champion)", "XGBoost (High-Throughput)", "Logistic Regression (GLM)", "Isolation Forest (Unsupervised)", "PR-AUC Weighted Ensemble"),
    Architecture = c("Tree Ensemble (80 trees)", "Gradient Boosted Trees", "Penalized Logistic GLM", "Isolation Depth Trees", "PR-AUC Weighted Soft-Voting"),
    Score_Type = c("Calibrated Fraud Probability", "Fraud Probability (Scaled)", "Logit Fraud Probability", "Raw Anomaly Score (0.35-0.85)", "Ensemble Fraud Probability"),
    Raw_Output = c(p_rf, p_xgb, p_lr, s_iso, p_ens),
    Formatted_Score = c(sprintf("%.2f%%", p_rf * 100), sprintf("%.2f%%", p_xgb * 100), sprintf("%.2f%%", p_lr * 100), sprintf("%.4f", s_iso), sprintf("%.2f%%", p_ens * 100)),
    Risk_Score = c(calculate_risk_score(p_rf, "rf"), calculate_risk_score(p_xgb, "xgb"), calculate_risk_score(p_lr, "lr"), calculate_risk_score(s_iso, "iso"), calculate_risk_score(p_ens, "rf")),
    Decision = c(dec_rf, dec_xgb, dec_lr, ifelse(s_iso >= 0.58, "Anomalous", "Normal"), dec_ens),
    Weight = c(sprintf("%.1f%%", w_rf * 100), sprintf("%.1f%%", w_xgb * 100), sprintf("%.1f%%", w_lr * 100), "N/A (Unsupervised)", "100.0%"),
    Calibration_Status = c("Tree Frequency Calibrated", "Positive-Weighted Shift", "Inherent Logit Calibrated", "Uncalibrated Anomaly Metric", "Composite Weighted Calibration"),
    stringsAsFactors = FALSE
  )
  
  list(
    models_table = models_df,
    ensemble_prob = p_ens,
    ensemble_decision = dec_ens,
    consensus_label = consensus_label,
    all_agree = all_agree,
    disagreement_spread = disagreement_spread,
    weights = list(rf = w_rf, xgb = w_xgb, lr = w_lr)
  )
}

# ------------------------------------------------------------------------------
# 5. UNCERTAINTY & CONFIDENCE ANALYSIS
# ------------------------------------------------------------------------------

#' Compute prediction uncertainty, decision boundary distance, and confidence rating
#' @param prob Fraud probability
#' @param threshold Operating decision threshold
#' @param disagreement_spread Model spread across architectures
#' @param noise_variance Perturbation variance
#' @return list of metrics and qualitative uncertainty tier
compute_prediction_uncertainty <- function(prob, threshold = 0.50, disagreement_spread = 0.0, noise_variance = 0.0) {
  # Distance from the active operating decision boundary
  boundary_dist <- abs(prob - threshold)
  
  # Composite uncertainty index (0 to 100, where 100 is maximum ambiguity)
  # High uncertainty when:
  # 1. Very close to threshold boundary (boundary_dist -> 0)
  # 2. Large divergence between models
  # 3. High sensitivity to small perturbations
  dist_penalty  <- pmax(0, (0.25 - boundary_dist) / 0.25) * 60 # Up to 60 pts
  spread_penalty <- pmin(disagreement_spread * 100, 30)        # Up to 30 pts
  noise_penalty  <- pmin(noise_variance * 500, 10)             # Up to 10 pts
  
  uncertainty_index <- round(pmin(pmax(dist_penalty + spread_penalty + noise_penalty, 0.0), 100.0), 1)
  
  if (uncertainty_index < 25.0) {
    uncertainty_tier <- "Low Uncertainty"
    conf_rating      <- "High Confidence (Decisive Prediction)"
    conf_color       <- "#16A34A"
    conf_recommend   <- "Automated policy execution safe."
  } else if (uncertainty_index < 55.0) {
    uncertainty_tier <- "Moderate Uncertainty"
    conf_rating      <- "Moderate Confidence"
    conf_color       <- "#D97706"
    conf_recommend   <- "Routine secondary monitoring advised."
  } else {
    uncertainty_tier <- "High Uncertainty"
    conf_rating      <- "Low Confidence (Borderline / Conflicted)"
    conf_color       <- "#DC2626"
    conf_recommend   <- "Mandatory manual investigation recommended due to proximity to decision threshold or model divergence."
  }
  
  list(
    boundary_distance   = round(boundary_dist, 4),
    uncertainty_index   = uncertainty_index,
    uncertainty_tier    = uncertainty_tier,
    confidence_rating   = conf_rating,
    confidence_color    = conf_color,
    recommendation      = conf_recommend
  )
}

# ------------------------------------------------------------------------------
# 6. FINANCIAL EXPOSURE & COST SIMULATOR
# ------------------------------------------------------------------------------

#' Simulate financial fraud exposure, investigation cost, and net benefit
#' @param amount Transaction dollar amount
#' @param prob Fraud probability
#' @param threshold Decision cutoff
#' @param params List of operational assumptions
#' @return list of financial projections
calculate_financial_exposure <- function(amount, prob, threshold = 0.50,
                                         params = list(
                                           loss_rate = 1.0,
                                           recovery_rate = 0.15,
                                           investigation_cost = 25.0,
                                           fn_multiplier = 1.5
                                         )) {
  loss_rt <- if (!is.null(params$loss_rate)) params$loss_rate else 1.0
  rec_rt  <- if (!is.null(params$recovery_rate)) params$recovery_rate else 0.15
  inv_cst <- if (!is.null(params$investigation_cost)) params$investigation_cost else 25.0
  fn_mult <- if (!is.null(params$fn_multiplier)) params$fn_multiplier else 1.5
  
  amt <- pmax(as.numeric(amount), 0.0)
  p   <- pmin(pmax(as.numeric(prob), 0.0), 1.0)
  is_flagged <- (p >= threshold)
  
  # Potential exposure if fraudulent
  potential_exposure <- amt * loss_rt
  # Net exposure factoring recovery
  net_fraud_loss <- potential_exposure * (1.0 - rec_rt)
  # Expected loss without intervention
  expected_unmitigated_loss <- p * net_fraud_loss
  
  if (is_flagged) {
    # If flagged, alert prevents loss but incurs investigation overhead
    investigation_incurred <- inv_cst
    prevented_loss <- net_fraud_loss
    missed_loss <- 0.0
    net_benefit <- (p * prevented_loss) - investigation_incurred
  } else {
    # If cleared, no investigation cost, but risk of false-negative missed fraud
    investigation_incurred <- 0.0
    prevented_loss <- 0.0
    missed_loss <- net_fraud_loss * fn_mult
    net_benefit <- - (p * missed_loss)
  }
  
  # Generate cost-benefit curve across thresholds
  thresh_seq <- seq(0.05, 0.95, by = 0.05)
  curve_df <- data.frame(
    Threshold = thresh_seq,
    Expected_Cost = sapply(thresh_seq, function(th) {
      if (p >= th) inv_cst else (p * net_fraud_loss * fn_mult)
    }),
    Expected_Benefit = sapply(thresh_seq, function(th) {
      if (p >= th) (p * net_fraud_loss) - inv_cst else 0.0
    }),
    stringsAsFactors = FALSE
  )
  
  list(
    amount                     = amt,
    potential_exposure         = potential_exposure,
    net_fraud_loss             = net_fraud_loss,
    expected_unmitigated_loss  = expected_unmitigated_loss,
    investigation_cost         = investigation_incurred,
    prevented_exposure         = if (is_flagged) prevented_loss else 0.0,
    missed_fraud_exposure      = if (!is_flagged) (p * missed_loss) else 0.0,
    net_expected_benefit       = net_benefit,
    cost_benefit_curve         = curve_df,
    is_alert                   = is_flagged
  )
}

# ------------------------------------------------------------------------------
# 7. BATCH TRANSACTION SCORING ENGINE
# ------------------------------------------------------------------------------

#' Process batch CSV transaction scoring with schema validation and progress tracking
#' @param batch_df Raw uploaded data.frame or data.table
#' @param model_type Scoring model architecture
#' @param threshold Operating decision threshold
#' @param policy_config Decision policy configuration
#' @param progress_callback Optional callback function(p, msg)
#' @return list(summary = list, scored_data = data.table, invalid_data = data.table)
score_batch_transactions <- function(batch_df, model_type = "random_forest", threshold = 0.50,
                                     policy_config = NULL, progress_callback = NULL) {
  start_time <- Sys.time()
  
  if (is.null(batch_df) || nrow(batch_df) == 0) {
    stop("Uploaded batch dataset contains zero rows.")
  }
  
  raw_dt <- as.data.table(copy(batch_df))
  n_total <- nrow(raw_dt)
  
  if (!is.null(progress_callback)) progress_callback(0.1, "Validating schema...")
  
  # Normalize names
  orig_names <- names(raw_dt)
  names(raw_dt) <- tolower(trimws(names(raw_dt)))
  
  # Assign internal transaction ID if missing
  if (!"transaction_id" %in% names(raw_dt)) {
    raw_dt[, transaction_id := sprintf("TX-BATCH-%05d", .I)]
  }
  
  # Check for amount
  if (!"amount" %in% names(raw_dt)) {
    stop("Batch CSV is missing required column: 'amount'.")
  }
  
  # Identify valid vs invalid rows
  pca_cols <- paste0("v", 1:28)
  missing_pca <- setdiff(pca_cols, names(raw_dt))
  if (length(missing_pca) > 0) {
    stop(sprintf("Batch CSV is missing required PCA feature columns: %s.", paste(missing_pca, collapse = ", ")))
  }
  
  # Row-by-row finite validation
  valid_mask <- rep(TRUE, n_total)
  error_reasons <- rep("", n_total)
  
  # Amount validation
  invalid_amt <- !is.finite(as.numeric(raw_dt$amount)) | (as.numeric(raw_dt$amount) < 0)
  valid_mask[invalid_amt] <- FALSE
  error_reasons[invalid_amt] <- "Invalid or negative transaction amount"
  
  # Check PCA finite
  for (col in pca_cols) {
    col_non_num <- !is.finite(as.numeric(raw_dt[[col]]))
    valid_mask[col_non_num] <- FALSE
    error_reasons[col_non_num] <- paste0(error_reasons[col_non_num], sprintf("; Non-finite value in %s", col))
  }
  
  invalid_dt <- raw_dt[!valid_mask, ]
  if (nrow(invalid_dt) > 0) {
    invalid_dt[, validation_error := trimws(gsub("^;\\s*", "", error_reasons[!valid_mask]))]
  }
  
  valid_dt <- raw_dt[valid_mask, ]
  n_valid <- nrow(valid_dt)
  
  if (n_valid == 0) {
    return(list(
      summary = list(
        total_uploaded = n_total,
        valid_count = 0,
        invalid_count = n_total,
        high_risk_count = 0,
        critical_risk_count = 0,
        review_count = 0,
        elapsed_seconds = round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)
      ),
      scored_data = data.table(),
      invalid_data = invalid_dt
    ))
  }
  
  if (!is.null(progress_callback)) progress_callback(0.3, "Preprocessing canonical features...")
  
  # Preprocess valid rows
  valid_dt[, amount := as.numeric(amount)]
  if (!"amount_log" %in% names(valid_dt)) {
    valid_dt[, amount_log := round(log1p(amount), 4)]
  } else {
    valid_dt[, amount_log := as.numeric(amount_log)]
  }
  
  if (!"elapsed_hours" %in% names(valid_dt)) {
    if ("time" %in% names(valid_dt)) {
      valid_dt[, elapsed_hours := round(as.numeric(time) / 3600, 4)]
    } else {
      valid_dt[, elapsed_hours := 0.0]
    }
  } else {
    valid_dt[, elapsed_hours := as.numeric(elapsed_hours)]
  }
  
  for (col in pca_cols) {
    valid_dt[[col]] <- as.numeric(valid_dt[[col]])
  }
  
  if (!is.null(progress_callback)) progress_callback(0.5, "Executing model predictions...")
  
  # Vectorized model prediction
  probs <- predict_fraud_probability(valid_dt, model_type)
  
  # Anomaly score via Isolation Forest
  iso_obj <- tryCatch(get_cached_model("isolation_forest"), error = function(e) NULL)
  anom_scores <- if (!is.null(iso_obj)) {
    as.numeric(isotree::predict.isolation_forest(iso_obj, as.data.frame(valid_dt[, ..CANONICAL_PREDICTORS])))
  } else {
    rep(0.5, n_valid)
  }
  
  if (!is.null(progress_callback)) progress_callback(0.8, "Applying decision policies & risk bands...")
  
  # Assign risk metrics
  risk_scores <- vapply(probs, function(p) calculate_risk_score(p, model_type), numeric(1))
  risk_cats   <- classify_risk_category(risk_scores)
  
  # Decision recommendations
  policies <- lapply(seq_len(n_valid), function(i) {
    apply_decision_policy(risk_scores[i], probs[i], threshold, policy_config)
  })
  
  decisions    <- vapply(policies, function(x) x$decision, character(1))
  action_codes <- vapply(policies, function(x) x$action_code, character(1))
  reasons      <- vapply(policies, function(x) x$reason, character(1))
  
  # Assemble scored data.table
  valid_dt[, fraud_probability      := round(probs, 4)]
  valid_dt[, isolation_anomaly_score := round(anom_scores, 4)]
  valid_dt[, risk_score             := risk_scores]
  valid_dt[, risk_category          := risk_cats]
  valid_dt[, predicted_class        := as.integer(probs >= threshold)]
  valid_dt[, decision_threshold     := threshold]
  valid_dt[, decision_recommendation:= decisions]
  valid_dt[, action_code            := action_codes]
  valid_dt[, decision_reason        := reasons]
  valid_dt[, scoring_model          := model_type]
  valid_dt[, scored_at              := format(Sys.time(), "%Y-%m-%d %H:%M:%S")]
  
  if (!is.null(progress_callback)) progress_callback(1.0, "Batch scoring complete.")
  
  elapsed_sec <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)
  
  list(
    summary = list(
      total_uploaded      = n_total,
      valid_count         = n_valid,
      invalid_count       = nrow(invalid_dt),
      high_risk_count     = sum(risk_cats == "High Risk"),
      critical_risk_count = sum(risk_cats == "Critical Risk"),
      review_count        = sum(action_codes == "REV"),
      decline_count       = sum(action_codes == "DEC"),
      elapsed_seconds     = elapsed_sec
    ),
    scored_data  = valid_dt,
    invalid_data = invalid_dt
  )
}

# ------------------------------------------------------------------------------
# 8. PERSISTENT RSQLite PREDICTION REGISTRY
# ------------------------------------------------------------------------------

#' Ensure prediction audit table exists in SQLite database
#' @return connection object
init_prediction_audit_table <- function() {
  db_path <- get_db_path()
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  create_table_sql <- "
    CREATE TABLE IF NOT EXISTS prediction_audit (
      prediction_id TEXT PRIMARY KEY,
      transaction_id TEXT,
      timestamp TEXT,
      model_name TEXT,
      model_version TEXT,
      input_source TEXT,
      amount REAL,
      raw_probability REAL,
      anomaly_score REAL,
      risk_score REAL,
      risk_category TEXT,
      predicted_class INTEGER,
      decision_threshold REAL,
      decision_recommendation TEXT,
      action_code TEXT,
      calibration_status TEXT,
      policy_version TEXT,
      features_json TEXT
    );
  "
  DBI::dbExecute(conn, create_table_sql)
  
  # Check if table has rows; if not, seed with top representative cases
  cnt <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM prediction_audit")$n
  if (cnt == 0) {
    seed_initial_prediction_audits(conn)
  }
  return(TRUE)
}

#' Seed initial audit records from verified test partition
#' @param conn SQLite connection
seed_initial_prediction_audits <- function(conn) {
  test_dt <- tryCatch(get_test_data(), error = function(e) NULL)
  if (is.null(test_dt)) return(NULL)
  
  set.seed(42)
  # 15 fraud cases + 10 legitimate cases
  fraud_cases <- test_dt[class == 1][1:min(15, sum(class == 1))]
  legit_cases <- test_dt[class == 0][1:10]
  sample_dt <- rbind(fraud_cases, legit_cases)
  
  rf_model  <- tryCatch(get_cached_model("random_forest"), error = function(e) NULL)
  iso_model <- tryCatch(get_cached_model("isolation_forest"), error = function(e) NULL)
  
  pred_cols <- CANONICAL_PREDICTORS
  X_clean <- as.data.frame(sample_dt[, ..pred_cols])
  
  probs <- if (!is.null(rf_model)) {
    predict(rf_model, data = X_clean)$predictions[, "Fraud"]
  } else {
    rep(0.5, nrow(sample_dt))
  }
  
  anoms <- if (!is.null(iso_model)) {
    as.numeric(isotree::predict.isolation_forest(iso_model, X_clean))
  } else {
    rep(0.5, nrow(sample_dt))
  }
  
  for (i in seq_len(nrow(sample_dt))) {
    p <- round(probs[i], 4)
    a <- round(anoms[i], 4)
    r_score <- calculate_risk_score(p, "random_forest")
    r_cat   <- as.character(classify_risk_category(r_score))
    pol     <- apply_decision_policy(r_score, p, threshold = 0.50)
    tx_id   <- sprintf("TX-%05d", sample_dt$time[i] %% 50000 + 1000)
    pred_id <- sprintf("PRED-AUDIT-%04d", i + 100)
    ts      <- format(Sys.time() - (i * 7200), "%Y-%m-%d %H:%M:%S")
    
    # Store key signals snapshot in JSON
    feat_list <- list(
      v14 = round(sample_dt$v14[i], 3),
      v17 = round(sample_dt$v17[i], 3),
      v12 = round(sample_dt$v12[i], 3),
      v10 = round(sample_dt$v10[i], 3),
      v11 = round(sample_dt$v11[i], 3),
      v4  = round(sample_dt$v4[i], 3),
      amount = round(sample_dt$amount[i], 2),
      elapsed_hours = round(sample_dt$elapsed_hours[i], 2)
    )
    feat_json <- jsonlite::toJSON(feat_list, auto_unbox = TRUE)
    
    insert_sql <- "
      INSERT OR IGNORE INTO prediction_audit (
        prediction_id, transaction_id, timestamp, model_name, model_version,
        input_source, amount, raw_probability, anomaly_score, risk_score,
        risk_category, predicted_class, decision_threshold, decision_recommendation,
        action_code, calibration_status, policy_version, features_json
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
    "
    DBI::dbExecute(conn, insert_sql, params = list(
      pred_id, tx_id, ts, "Random Forest (Champion)", "v1.2.0",
      "Historical Test Dataset", round(sample_dt$amount[i], 2),
      p, a, r_score, r_cat, as.integer(p >= 0.50), 0.50,
      pol$decision, pol$action_code, "Tree Frequency Calibrated",
      pol$policy_version, feat_json
    ))
  }
}

#' Save prediction event into SQLite audit registry
#' @param record Named list of audit attributes
#' @return boolean
save_prediction_audit <- function(record) {
  db_path <- get_db_path()
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  pred_id <- if (!is.null(record$prediction_id)) record$prediction_id else sprintf("PRED-%s-%s", format(Sys.time(), "%Y%m%d%H%M%S"), substr(digest::digest(runif(1)), 1, 4))
  tx_id   <- if (!is.null(record$transaction_id)) record$transaction_id else sprintf("TX-LIVE-%s", substr(digest::digest(runif(1)), 1, 6))
  ts      <- if (!is.null(record$timestamp)) record$timestamp else format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  insert_sql <- "
    INSERT OR REPLACE INTO prediction_audit (
      prediction_id, transaction_id, timestamp, model_name, model_version,
      input_source, amount, raw_probability, anomaly_score, risk_score,
      risk_category, predicted_class, decision_threshold, decision_recommendation,
      action_code, calibration_status, policy_version, features_json
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
  "
  DBI::dbExecute(conn, insert_sql, params = list(
    pred_id,
    tx_id,
    ts,
    as.character(record$model_name),
    as.character(if (!is.null(record$model_version)) record$model_version else "v1.2.0"),
    as.character(if (!is.null(record$input_source)) record$input_source else "Single Transaction Form"),
    as.numeric(record$amount),
    as.numeric(record$raw_probability),
    as.numeric(if (!is.null(record$anomaly_score)) record$anomaly_score else NA),
    as.numeric(record$risk_score),
    as.character(record$risk_category),
    as.integer(record$predicted_class),
    as.numeric(record$decision_threshold),
    as.character(record$decision_recommendation),
    as.character(if (!is.null(record$action_code)) record$action_code else "REV"),
    as.character(if (!is.null(record$calibration_status)) record$calibration_status else "Calibrated"),
    as.character(if (!is.null(record$policy_version)) record$policy_version else "POL-2026.4-ENTERPRISE"),
    as.character(if (!is.null(record$features_json)) record$features_json else "{}")
  ))
  return(TRUE)
}

#' Retrieve prediction audit trail with optional filtering
#' @param limit Maximum rows to return (default 200)
#' @param filter_model Optional model filter
#' @param filter_category Optional category filter
#' @param search_query Text search on transaction_id
#' @return data.frame
get_prediction_audit_history <- function(limit = 200, filter_model = NULL, filter_category = NULL, search_query = NULL) {
  db_path <- get_db_path()
  if (!file.exists(db_path)) init_prediction_audit_table()
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  query <- "SELECT * FROM prediction_audit WHERE 1=1"
  params <- list()
  
  if (!is.null(filter_model) && filter_model != "All") {
    query <- paste(query, "AND model_name LIKE ?")
    params <- c(params, paste0("%", filter_model, "%"))
  }
  
  if (!is.null(filter_category) && filter_category != "All") {
    query <- paste(query, "AND risk_category = ?")
    params <- c(params, filter_category)
  }
  
  if (!is.null(search_query) && trimws(search_query) != "") {
    query <- paste(query, "AND (transaction_id LIKE ? OR prediction_id LIKE ?)")
    s_term <- paste0("%", trimws(search_query), "%")
    params <- c(params, s_term, s_term)
  }
  
  query <- paste(query, "ORDER BY timestamp DESC LIMIT ?")
  params <- c(params, as.integer(limit))
  
  df <- if (length(params) > 0) {
    DBI::dbGetQuery(conn, query, params = params)
  } else {
    DBI::dbGetQuery(conn, query)
  }
  return(df)
}

