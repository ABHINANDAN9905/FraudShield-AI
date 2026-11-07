cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))
options(sass.cache = FALSE)

source("app/global.R")

cat("====================================================================\n")
cat(" Testing Shiny Server-Side Logic for Fraud Analytics\n")
cat("====================================================================\n\n")

test_dt <- get_test_data()

# 1. Test reactive scoring across all curated choices
tx_choices <- c("TX-01340", "TX-01347", "TX-01363", "TX-01380", "TX-01652", "TX-00001", "TX-00002", "TX-00042", "TX-00100")
models <- c("random_forest", "xgboost", "logistic_regression")

for (m in models) {
  cat(sprintf("\n--- Testing Model: %s ---\n", m))
  for (tx in tx_choices) {
    tx_idx <- as.integer(gsub("[^0-9]", "", tx))
    row_dt <- test_dt[tx_idx, ]
    pred_cols <- c(paste0("v", 1:28), "amount_log", "elapsed_hours")
    feats <- as.data.frame(row_dt[, ..pred_cols])
    
    if (m == "random_forest") {
      rf_mod <- get_cached_model("random_forest")
      prob <- predict(rf_mod, data = feats)$predictions[, "Fraud"]
    } else if (m == "xgboost") {
      xgb_mod <- get_cached_model("xgboost")
      prob <- predict(xgb_mod, xgboost::xgb.DMatrix(as.matrix(feats)))
    } else if (m == "logistic_regression") {
      lr_mod <- get_cached_model("logistic_regression")
      prob <- suppressWarnings(predict(lr_mod, newdata = feats, type = "response"))
    }
    prob_pct <- pmin(pmax(as.numeric(prob) * 100, 0), 100)
    risk_tier <- if (prob_pct < 20) "Low" else if (prob_pct < 50) "Medium" else if (prob_pct < 80) "High" else "Critical"
    
    cat(sprintf("  %s | Class: %d | Amount: $%7.2f | Prob: %6.2f%% | Tier: %-8s\n",
                tx, row_dt$class[1], row_dt$amount[1], prob_pct, risk_tier))
  }
}

cat("\n====================================================================\n")
cat(" [SERVER SIMULATION PASSED] All calculations execute cleanly\n")
cat("====================================================================\n")

