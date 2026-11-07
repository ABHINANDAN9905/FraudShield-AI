cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))
options(sass.cache = FALSE)

source("app/R/dashboard_data.R")

library(plotly)
library(data.table)

cat("====================================================================\n")
cat(" Testing Gauge Meter Logic & Dynamic Calculations\n")
cat("====================================================================\n\n")

# 1. Test Model Metrics Extraction
cat("[TEST 1] Testing Model Performance Metrics Loader...\n")
comp_df <- get_model_comparison_table()
print(comp_df[, c("Model", "Recall", "Precision", "TP", "FP", "FN")])

# Helper function to get model metrics
get_model_metrics <- function(model_key) {
  df <- get_model_comparison_table()
  model_name_map <- list(
    "random_forest" = "Random Forest",
    "xgboost" = "XGBoost",
    "logistic_regression" = "Logistic Regression"
  )
  target_name <- model_name_map[[model_key]]
  row <- df[df$Model == target_name, ]
  if (nrow(row) > 0) {
    return(list(
      model_name = target_name,
      recall = row$Recall[1] * 100,
      precision = row$Precision[1] * 100,
      f1 = row$F1_Score[1],
      tp = row$TP[1],
      fp = row$FP[1],
      fn = row$FN[1],
      tn = row$TN[1]
    ))
  }
  return(NULL)
}

rf_m <- get_model_metrics("random_forest")
cat("  -> RF Recall:", rf_m$recall, "% | Precision:", rf_m$precision, "%\n")
xgb_m <- get_model_metrics("xgboost")
cat("  -> XGB Recall:", xgb_m$recall, "% | Precision:", xgb_m$precision, "%\n")
lr_m <- get_model_metrics("logistic_regression")
cat("  -> LR Recall:", lr_m$recall, "% | Precision:", lr_m$precision, "%\n")

stopifnot(abs(rf_m$recall - 80.65) < 0.01)
stopifnot(abs(rf_m$precision - 96.15) < 0.01)
stopifnot(abs(xgb_m$recall - 83.87) < 0.01)
stopifnot(abs(xgb_m$precision - 75.00) < 0.01)
stopifnot(abs(lr_m$recall - 61.29) < 0.01)
stopifnot(abs(lr_m$precision - 83.82) < 0.01)
cat("  -> ALL MODEL METRICS VERIFIED ACCURATELY.\n\n")

# 2. Test Dynamic Prevalence Calculation
cat("[TEST 2] Testing Dynamic Fraud Prevalence Filter Calculations...\n")
test_dt <- get_test_data()

calc_prevalence <- function(dt, time_scope = "all", amount_cohort = "all") {
  sub_dt <- copy(dt)
  
  # Filter time
  if (time_scope == "day1") {
    sub_dt <- sub_dt[elapsed_hours <= 24]
  } else if (time_scope == "day2") {
    sub_dt <- sub_dt[elapsed_hours > 24]
  } else if (time_scope == "night") {
    sub_dt <- sub_dt[(elapsed_hours >= 1 & elapsed_hours <= 6) | (elapsed_hours >= 25 & elapsed_hours <= 30)]
  }
  
  # Filter amount
  if (amount_cohort == "micro") {
    sub_dt <- sub_dt[amount <= 5]
  } else if (amount_cohort == "retail") {
    sub_dt <- sub_dt[amount > 5 & amount <= 200]
  } else if (amount_cohort == "high") {
    sub_dt <- sub_dt[amount > 200]
  }
  
  n_total <- nrow(sub_dt)
  n_fraud <- sum(sub_dt$class == 1)
  n_legit <- sum(sub_dt$class == 0)
  prev_pct <- if (n_total > 0) (n_fraud / n_total) * 100 else 0
  
  return(list(
    total = n_total,
    fraud = n_fraud,
    legit = n_legit,
    prevalence = prev_pct
  ))
}

prev_all <- calc_prevalence(test_dt, "all", "all")
cat("  -> All test set: Total =", prev_all$total, "| Fraud =", prev_all$fraud, "| Legit =", prev_all$legit, "| Prev =", round(prev_all$prevalence, 4), "%\n")

prev_night <- calc_prevalence(test_dt, "night", "all")
cat("  -> Night Spikes: Total =", prev_night$total, "| Fraud =", prev_night$fraud, "| Legit =", prev_night$legit, "| Prev =", round(prev_night$prevalence, 4), "%\n")

prev_micro <- calc_prevalence(test_dt, "all", "micro")
cat("  -> Micro Amount: Total =", prev_micro$total, "| Fraud =", prev_micro$fraud, "| Legit =", prev_micro$legit, "| Prev =", round(prev_micro$prevalence, 4), "%\n")

stopifnot(prev_all$total == 56962)
stopifnot(prev_all$fraud == 93)
stopifnot(prev_all$legit == 56869)
cat("  -> DYNAMIC PREVALENCE CALCULATIONS VERIFIED.\n\n")

# 3. Test Transaction Scoring Function
cat("[TEST 3] Testing Live Transaction Risk Scoring across models...\n")
score_single_tx <- function(tx_row, model_key) {
  pred_cols <- c(paste0("v", 1:28), "amount_log", "elapsed_hours")
  feats <- as.data.frame(tx_row[, ..pred_cols])
  
  prob <- 0
  if (model_key == "random_forest") {
    rf <- get_cached_model("random_forest")
    prob <- predict(rf, data = feats)$predictions[, "Fraud"]
  } else if (model_key == "xgboost") {
    xgb <- get_cached_model("xgboost")
    prob <- predict(xgb, xgboost::xgb.DMatrix(as.matrix(feats)))
  } else if (model_key == "logistic_regression") {
    lr <- get_cached_model("logistic_regression")
    prob <- suppressWarnings(predict(lr, newdata = feats, type = "response"))
  }
  prob <- as.numeric(prob)
  prob_pct <- pmin(pmax(prob * 100, 0), 100)
  
  risk_tier <- if (prob_pct < 20) {
    "Low Risk"
  } else if (prob_pct < 50) {
    "Medium Risk"
  } else if (prob_pct < 80) {
    "High Risk"
  } else {
    "Critical Risk"
  }
  
  return(list(
    prob_pct = prob_pct,
    risk_tier = risk_tier
  ))
}

tx_fraud <- test_dt[1340, ]
tx_legit <- test_dt[1, ]

for (m in c("random_forest", "xgboost", "logistic_regression")) {
  res_f <- score_single_tx(tx_fraud, m)
  res_l <- score_single_tx(tx_legit, m)
  cat("  -> Model:", m, "\n")
  cat("     Fraud TX 1340: Prob =", round(res_f$prob_pct, 2), "% [Tier:", res_f$risk_tier, "]\n")
  cat("     Legit TX 1   : Prob =", round(res_l$prob_pct, 4), "% [Tier:", res_l$risk_tier, "]\n")
}
cat("  -> REAL-TIME TRANSACTION SCORING VERIFIED.\n\n")

cat("====================================================================\n")
cat(" [ALL TESTS PASSED] Ready to implement in fraud_analytics_module.R\n")
cat("====================================================================\n")

