cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))
options(sass.cache = FALSE)

source("app/R/dashboard_data.R")

test_dt <- get_test_data()
cat("Test data loaded:", nrow(test_dt), "rows\n")

# Find some interesting transactions
fraud_indices <- which(test_dt$class == 1)
legit_indices <- which(test_dt$class == 0)

cat("First 5 fraud row indices:", head(fraud_indices, 5), "\n")
cat("First 5 legit row indices:", head(legit_indices, 5), "\n")

# Test RF, XGB, and LR on a fraud and a legit transaction
pred_cols <- c(paste0("v", 1:28), "amount_log", "elapsed_hours")

rf <- get_cached_model("random_forest")
xgb <- get_cached_model("xgboost")
lr <- get_cached_model("logistic_regression")

# Test on fraud sample 1
sample_tx <- test_dt[fraud_indices[1], ..pred_cols]
cat("\nTesting on Fraud TX (row", fraud_indices[1], ", Amount:", test_dt$amount[fraud_indices[1]], "):\n")
rf_p <- predict(rf, data = as.data.frame(sample_tx))$predictions[, "Fraud"]
xgb_p <- predict(xgb, xgboost::xgb.DMatrix(as.matrix(sample_tx)))
lr_p <- suppressWarnings(predict(lr, newdata = as.data.frame(sample_tx), type = "response"))

cat("  RF Prob  :", round(rf_p * 100, 2), "%\n")
cat("  XGB Prob :", round(xgb_p * 100, 2), "%\n")
cat("  LR Prob  :", round(lr_p * 100, 2), "%\n")

# Test on legit sample 1
sample_legit <- test_dt[legit_indices[1], ..pred_cols]
cat("\nTesting on Legit TX (row", legit_indices[1], ", Amount:", test_dt$amount[legit_indices[1]], "):\n")
rf_p2 <- predict(rf, data = as.data.frame(sample_legit))$predictions[, "Fraud"]
xgb_p2 <- predict(xgb, xgboost::xgb.DMatrix(as.matrix(sample_legit)))
lr_p2 <- suppressWarnings(predict(lr, newdata = as.data.frame(sample_legit), type = "response"))

cat("  RF Prob  :", round(rf_p2 * 100, 4), "%\n")
cat("  XGB Prob :", round(xgb_p2 * 100, 4), "%\n")
cat("  LR Prob  :", round(lr_p2 * 100, 4), "%\n")

