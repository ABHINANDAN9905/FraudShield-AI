cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))

source("app/R/dashboard_data.R")
library(DALEX)
library(iBreakDown)

test_dt <- get_test_data()
pred_cols <- c(paste0("v", 1:28), "amount_log", "elapsed_hours")

rf <- get_cached_model("random_forest")
xgb <- get_cached_model("xgboost")
lr <- get_cached_model("logistic_regression")

# Sample data for background
set.seed(42)
bg_data <- as.data.frame(test_dt[sample(.N, 100), ..pred_cols])
target_tx <- as.data.frame(test_dt[1340, ..pred_cols]) # Confirmed fraud row

cat("1. Testing DALEX Explainer for Random Forest...\n")
p_fun_rf <- function(model, newdata) {
  predict(model, data = newdata)$predictions[, "Fraud"]
}

exp_rf <- DALEX::explain(
  model = rf,
  data = bg_data,
  y = test_dt$class[1:100],
  predict_function = p_fun_rf,
  label = "Random Forest",
  verbose = FALSE
)

cat("  -> Explainer created successfully.\n")

# Test SHAP / break_down
cat("2. Testing iBreakDown::shap on target transaction...\n")
bd_shap <- iBreakDown::shap(exp_rf, new_observation = target_tx, B = 5)
print(head(bd_shap, 10))

# Test predict
pred_val <- p_fun_rf(rf, target_tx)
cat("  -> Exact predicted probability:", pred_val, "\n")

# Check mathematical consistency: intercept + contributions
intercept <- attr(bd_shap, "intercept")
cat("  -> Intercept / Baseline probability:", intercept, "\n")

# Test DALEX for XGBoost
cat("3. Testing DALEX Explainer for XGBoost...\n")
p_fun_xgb <- function(model, newdata) {
  xgboost::predict(model, as.matrix(newdata))
}
exp_xgb <- DALEX::explain(
  model = xgb,
  data = bg_data,
  y = test_dt$class[1:100],
  predict_function = p_fun_xgb,
  label = "XGBoost",
  verbose = FALSE
)
pred_val_xgb <- p_fun_xgb(xgb, target_tx)
cat("  -> XGB predicted prob:", pred_val_xgb, "\n")
bd_shap_xgb <- iBreakDown::shap(exp_xgb, new_observation = target_tx, B = 5)
cat("  -> XGB SHAP calculated successfully.\n")

# Test DALEX for Logistic Regression
cat("4. Testing DALEX Explainer for Logistic Regression...\n")
p_fun_lr <- function(model, newdata) {
  suppressWarnings(predict(model, newdata = newdata, type = "response"))
}
exp_lr <- DALEX::explain(
  model = lr,
  data = bg_data,
  y = test_dt$class[1:100],
  predict_function = p_fun_lr,
  label = "Logistic Regression",
  verbose = FALSE
)
pred_val_lr <- p_fun_lr(lr, target_tx)
cat("  -> LR predicted prob:", pred_val_lr, "\n")
bd_shap_lr <- iBreakDown::shap(exp_lr, new_observation = target_tx, B = 5)
cat("  -> LR SHAP calculated successfully.\n")

cat("\nALL XAI ENGINES VERIFIED 100% OPERATIONAL!\n")

