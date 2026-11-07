cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))

source("app/R/dashboard_data.R")

test_dt <- get_test_data()
pred_cols <- c(paste0("v", 1:28), "amount_log", "elapsed_hours")

cat("Testing model inference on test subset (1000 rows):\n")
sub_dt <- test_dt[1:1000, ..pred_cols]

# 1. Random Forest
t0 <- Sys.time()
rf <- get_cached_model("random_forest")
p_rf <- predict(rf, data = as.data.frame(sub_dt))$predictions[, "Fraud"]
t1 <- Sys.time()
cat("  -> RF latency (1000 rows):", round(as.numeric(difftime(t1, t0, units="secs")), 3), "s | Sample prob:", round(head(p_rf, 3), 4), "\n")

# 2. XGBoost
t0 <- Sys.time()
xgb <- get_cached_model("xgboost")
p_xgb <- predict(xgb, xgboost::xgb.DMatrix(as.matrix(sub_dt)))
t1 <- Sys.time()
cat("  -> XGB latency (1000 rows):", round(as.numeric(difftime(t1, t0, units="secs")), 3), "s | Sample prob:", round(head(p_xgb, 3), 4), "\n")

# 3. Logistic Regression
t0 <- Sys.time()
lr <- get_cached_model("logistic_regression")
p_lr <- suppressWarnings(predict(lr, newdata = as.data.frame(sub_dt), type = "response"))
t1 <- Sys.time()
cat("  -> LR latency (1000 rows):", round(as.numeric(difftime(t1, t0, units="secs")), 3), "s | Sample prob:", round(head(p_lr, 3), 4), "\n")

# 4. Isolation Forest
t0 <- Sys.time()
iso <- get_cached_model("isolation_forest")
# For isotree, predict gives anomaly scores
p_iso <- isotree::predict.isolation_forest(iso, as.data.frame(sub_dt))
t1 <- Sys.time()
cat("  -> IsoForest latency (1000 rows):", round(as.numeric(difftime(t1, t0, units="secs")), 3), "s | Sample score:", round(head(p_iso, 3), 4), "\n")

