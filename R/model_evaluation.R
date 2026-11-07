# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Module: Machine Learning Model Evaluation Engine
# Script: R/model_evaluation.R
# ==============================================================================
# Description:
#   Implements production-grade, financial-risk evaluation metrics focusing on
#   imbalanced metrics (PR-AUC, ROC-AUC, Recall, Precision, F1-Score, Cost Matrix).
#   Provides threshold optimization routines and confusion matrix analytics.
# ==============================================================================

# Ensure project library is available
local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib) && !(proj_lib %in% .libPaths())) {
    .libPaths(c(proj_lib, .libPaths()))
  }
})

suppressPackageStartupMessages({
  library(data.table)
  library(pROC)
  library(PRROC)
})

# ------------------------------------------------------------------------------
# 1. Core Classification Metrics Calculator
# ------------------------------------------------------------------------------

#' Calculate comprehensive classification metrics at a specific operating threshold
#'
#' @param y_true Binary numeric vector (0 = Legitimate, 1 = Fraud).
#' @param y_prob Numeric vector of predicted fraud probabilities in [0, 1].
#' @param threshold Numeric operating threshold (default: 0.50).
#' @param model_name Character label for model identification.
#' @return Named list of classification metrics.
calculate_classification_metrics <- function(y_true, y_prob, threshold = 0.50, model_name = "Model") {
  # Input sanitation
  y_true <- as.integer(as.character(y_true))
  y_prob <- as.numeric(y_prob)
  y_prob <- pmin(pmax(y_prob, 0), 1)

  y_pred <- as.integer(y_prob >= threshold)

  # Confusion matrix counts
  tp <- sum(y_true == 1 & y_pred == 1)
  fp <- sum(y_true == 0 & y_pred == 1)
  tn <- sum(y_true == 0 & y_pred == 0)
  fn <- sum(y_true == 1 & y_pred == 0)

  total_pos <- sum(y_true == 1)
  total_neg <- sum(y_true == 0)
  total_obs <- length(y_true)

  # Standard metrics
  accuracy    <- (tp + tn) / total_obs
  precision   <- if ((tp + fp) > 0) tp / (tp + fp) else 0.0
  recall      <- if (total_pos > 0) tp / total_pos else 0.0
  specificity <- if (total_neg > 0) tn / total_neg else 0.0
  fpr         <- if (total_neg > 0) fp / total_neg else 0.0
  fnr         <- if (total_pos > 0) fn / total_pos else 0.0
  f1_score    <- if ((precision + recall) > 0) 2 * (precision * recall) / (precision + recall) else 0.0

  # ROC-AUC via pROC
  roc_obj <- tryCatch({
    pROC::roc(y_true, y_prob, quiet = TRUE, levels = c(0, 1), direction = "<")
  }, error = function(e) NULL)
  roc_auc <- if (!is.null(roc_obj)) as.numeric(pROC::auc(roc_obj)) else NA_real_

  # PR-AUC via PRROC (Area under Precision-Recall Curve)
  pr_obj <- tryCatch({
    pos_scores <- y_prob[y_true == 1]
    neg_scores <- y_prob[y_true == 0]
    PRROC::pr.curve(scores.class0 = pos_scores, scores.class1 = neg_scores, curve = FALSE)
  }, error = function(e) NULL)
  pr_auc <- if (!is.null(pr_obj)) pr_obj$auc.integral else NA_real_

  # Average Precision (AP) approximation if integral is missing
  if (is.na(pr_auc) && !is.null(pr_obj)) {
    pr_auc <- pr_obj$auc.davis.goadrich
  }

  return(list(
    model_name   = model_name,
    threshold    = threshold,
    tp           = tp,
    fp           = fp,
    tn           = tn,
    fn           = fn,
    accuracy     = accuracy,
    precision    = precision,
    recall       = recall,
    specificity  = specificity,
    f1_score     = f1_score,
    fpr          = fpr,
    fnr          = fnr,
    roc_auc      = roc_auc,
    pr_auc       = pr_auc,
    total_fraud  = total_pos,
    total_legit  = total_neg
  ))
}

# ------------------------------------------------------------------------------
# 2. Operating Threshold Grid Analysis
# ------------------------------------------------------------------------------

#' Evaluate model metrics across a spectrum of decision thresholds
#'
#' @param y_true Binary numeric vector of actual labels.
#' @param y_prob Numeric vector of predicted fraud probabilities.
#' @param thresholds Numeric vector of thresholds to test.
#' @param model_name Character label.
#' @return data.frame comparing metrics across thresholds.
evaluate_threshold_grid <- function(y_true, y_prob,
                                    thresholds = seq(0.10, 0.90, by = 0.10),
                                    model_name = "Model") {
  res_list <- lapply(thresholds, function(th) {
    m <- calculate_classification_metrics(y_true, y_prob, threshold = th, model_name = model_name)
    data.frame(
      Model        = model_name,
      Threshold    = th,
      TP           = m$tp,
      FP           = m$fp,
      TN           = m$tn,
      FN           = m$fn,
      Precision    = round(m$precision, 4),
      Recall       = round(m$recall, 4),
      F1_Score     = round(m$f1_score, 4),
      Specificity  = round(m$specificity, 4),
      FPR          = round(m$fpr, 6),
      FNR          = round(m$fnr, 4),
      PR_AUC       = round(m$pr_auc, 4),
      ROC_AUC      = round(m$roc_auc, 4),
      stringsAsFactors = FALSE
    )
  })

  df <- do.call(rbind, res_list)
  return(df)
}

# ------------------------------------------------------------------------------
# 3. Financial Cost-Benefit Metric Calculation
# ------------------------------------------------------------------------------

#' Compute expected financial loss / cost of fraud classification
#'
#' @param y_true Binary numeric vector.
#' @param y_pred Binary numeric predictions.
#' @param amounts Numeric vector of transaction dollar amounts.
#' @param cost_investigation Numeric cost per false alarm investigation (default: $15.00).
#' @return list of financial metrics.
calculate_financial_cost <- function(y_true, y_pred, amounts, cost_investigation = 15.00) {
  # FN: Missed fraud results in loss of entire transaction amount
  fraud_loss_missed <- sum(amounts[y_true == 1 & y_pred == 0])

  # TP: Intercepted fraud saves the amount (recovered)
  fraud_saved       <- sum(amounts[y_true == 1 & y_pred == 1])

  # FP: Legitimate transaction falsely blocked requires manual compliance triage
  fp_count          <- sum(y_true == 0 & y_pred == 1)
  investigation_cost<- fp_count * cost_investigation

  total_operational_loss <- fraud_loss_missed + investigation_cost

  return(list(
    missed_fraud_loss  = fraud_loss_missed,
    intercepted_amount = fraud_saved,
    false_positives    = fp_count,
    investigation_cost = investigation_cost,
    net_exposure_loss  = total_operational_loss
  ))
}
