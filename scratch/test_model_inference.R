cat("--- Testing Model Inference and Outputs ---\n")
source("app/global.R")

test_dt <- get_test_data()
sample_row <- test_dt[1340, ] # Confirmed fraud row
legit_row  <- test_dt[1, ]    # Confirmed legit row

pred_cols <- c(paste0("v", 1:28), "amount_log", "elapsed_hours")

rf  <- get_cached_model("random_forest")
xgb <- get_cached_model("xgboost")
lr  <- get_cached_model("logistic_regression")
iso <- get_cached_model("isolation_forest")

cat("RF prediction on Fraud:\n")
print(predict(rf, data = as.data.frame(sample_row[, ..pred_cols]))$predictions[, "Fraud"])

cat("XGB prediction on Fraud:\n")
print(predict(xgb, as.matrix(sample_row[, ..pred_cols])))

cat("LR prediction on Fraud:\n")
print(suppressWarnings(predict(lr, newdata = as.data.frame(sample_row[, ..pred_cols]), type = "response")))

cat("ISO prediction on Fraud:\n")
print(isotree::predict.isolation_forest(iso, as.data.frame(sample_row[, ..pred_cols])))

cat("\nRF prediction on Legit:\n")
print(predict(rf, data = as.data.frame(legit_row[, ..pred_cols]))$predictions[, "Fraud"])

cat("XGB prediction on Legit:\n")
print(predict(xgb, as.matrix(legit_row[, ..pred_cols])))

cat("LR prediction on Legit:\n")
print(suppressWarnings(predict(lr, newdata = as.data.frame(legit_row[, ..pred_cols]), type = "response")))

cat("ISO prediction on Legit:\n")
print(isotree::predict.isolation_forest(iso, as.data.frame(legit_row[, ..pred_cols])))

