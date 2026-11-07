local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib)) .libPaths(c(proj_lib, .libPaths()))
})
source("app/global.R")
test_dt <- get_test_data()

cat("Testing XGBoost prediction on 56,962 rows...\n")
t0 <- Sys.time()
xgb_model <- get_cached_model("xgboost")
pred_cols <- c(paste0("v", 1:28), "amount_log", "elapsed_hours")
X_mat <- as.matrix(test_dt[, ..pred_cols])
dtest <- xgboost::xgb.DMatrix(data = X_mat)
xgb_preds <- predict(xgb_model, dtest)
t1 <- Sys.time()
cat("XGB Prediction took: ", as.numeric(t1 - t0, units = "secs"), "seconds.\n")

cat("Testing Random Forest prediction on 56,962 rows...\n")
t0 <- Sys.time()
rf_model <- get_cached_model("random_forest")
rf_preds <- predict(rf_model, data = as.data.frame(test_dt[, ..pred_cols]))$predictions[, "Fraud"]
t1 <- Sys.time()
cat("RF Prediction took: ", as.numeric(t1 - t0, units = "secs"), "seconds.\n")

