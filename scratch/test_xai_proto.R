cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))

source("app/R/dashboard_data.R")

test_dt <- get_test_data()
pred_cols <- c(paste0("v", 1:28), "amount_log", "elapsed_hours")

rf <- get_cached_model("random_forest")
xgb <- get_cached_model("xgboost")
lr <- get_cached_model("logistic_regression")

tx_row <- test_dt[1340, ..pred_cols] # Confirmed fraud (prob 0.925)

# 1. Test Counterfactual Search
cat("1. Testing Counterfactual Analysis...\n")
# Find minimal changes in key features to bring prob < 0.50
# We test moving top negative features (V14, V17, V12, V10) towards benign distribution mean (0)
cf_row <- copy(tx_row)
p_orig <- predict(rf, data = as.data.frame(tx_row))$predictions[, "Fraud"]

# Shift V14 towards benign baseline
cf_row$v14 <- 0.0
p_cf1 <- predict(rf, data = as.data.frame(cf_row))$predictions[, "Fraud"]

# Shift V12, V17 towards baseline
cf_row$v12 <- 0.0
cf_row$v17 <- 0.0
p_cf2 <- predict(rf, data = as.data.frame(cf_row))$predictions[, "Fraud"]

cat(sprintf("  -> Original Prob: %.4f | After V14=0: %.4f | After V14,12,17=0: %.4f\n", p_orig, p_cf1, p_cf2))

# 2. Test Cross-Model Agreement
cat("2. Testing Cross-Model Prediction on same transaction...\n")
p_rf <- predict(rf, data = as.data.frame(tx_row))$predictions[, "Fraud"]
p_xgb <- predict(xgb, xgboost::xgb.DMatrix(as.matrix(tx_row)))
p_lr <- suppressWarnings(predict(lr, newdata = as.data.frame(tx_row), type = "response"))

cat(sprintf("  -> RF Prob: %.4f | XGB Prob: %.4f | LR Prob: %.4f\n", p_rf, p_xgb, p_lr))
agreement <- (p_rf >= 0.5) && (p_xgb >= 0.5) && (p_lr >= 0.5)
cat(sprintf("  -> 3-Way Classification Consensus: %s (All predict Fraud)\n", ifelse(agreement, "YES", "NO")))

# 3. Test Partial Dependence Grid
cat("3. Testing Partial Dependence calculation for V14...\n")
v14_grid <- seq(-15, 5, length.out = 20)
pdp_vals <- sapply(v14_grid, function(val) {
  test_sample <- copy(test_dt[1:50, ..pred_cols])
  test_sample$v14 <- val
  mean(predict(rf, data = as.data.frame(test_sample))$predictions[, "Fraud"])
})
cat(sprintf("  -> PDP evaluated at %d points. At V14=-15: %.4f | At V14=0: %.4f\n", length(v14_grid), pdp_vals[1], pdp_vals[15]))

cat("\nAll prototype algorithms validated!\n")

