# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Script: R/prediction.R
# ==============================================================================
# Description:
#   Root prediction pipeline interface. Exposes production prediction routines,
#   schema validation, risk tiering, batch processing, and audit persistence.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ranger)
  library(xgboost)
  library(isotree)
  library(DBI)
  library(RSQLite)
})

# Path resolution for app engine
local({
  root <- getwd()
  engine_path <- file.path(root, "app", "R", "prediction_engine.R")
  if (file.exists(engine_path)) {
    source(engine_path, local = FALSE)
  }
})

#' High-level wrapper function to predict fraud risk for incoming transactions
#' @param new_data data.frame or data.table of incoming transactions
#' @param model_type "random_forest", "xgboost", "logistic_regression", "isolation_forest"
#' @param threshold Decision threshold (default 0.50)
#' @return data.table with predictions and policy classifications
predict_fraud_risk <- function(new_data, model_type = "random_forest", threshold = 0.50) {
  val_res <- validate_transaction_input(new_data)
  if (!val_res$valid) {
    stop(paste("Validation failure:", paste(val_res$errors, collapse = "; ")))
  }

  clean_dt <- val_res$clean_data
  probs <- predict_fraud_probability(clean_dt, model_type)

  # Isolation forest anomaly score
  iso_obj <- tryCatch(get_cached_model("isolation_forest"), error = function(e) NULL)
  anoms <- if (!is.null(iso_obj)) {
    as.numeric(isotree::predict.isolation_forest(iso_obj, as.data.frame(clean_dt[, ..CANONICAL_PREDICTORS])))
  } else {
    rep(0.5, nrow(clean_dt))
  }

  risk_scores <- vapply(probs, function(p) calculate_risk_score(p, model_type), numeric(1))
  risk_cats <- classify_risk_category(risk_scores)

  policies <- lapply(seq_along(probs), function(i) {
    apply_decision_policy(risk_scores[i], probs[i], threshold)
  })

  res_dt <- copy(clean_dt)
  res_dt[, fraud_probability := round(probs, 4)]
  res_dt[, anomaly_score := round(anoms, 4)]
  res_dt[, risk_score := risk_scores]
  res_dt[, risk_category := risk_cats]
  res_dt[, decision_threshold := threshold]
  res_dt[, is_fraud_alert := as.integer(probs >= threshold)]
  res_dt[, recommended_action := vapply(policies, function(x) x$decision, character(1))]
  res_dt[, scoring_model := model_type]

  return(res_dt)
}
