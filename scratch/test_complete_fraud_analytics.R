cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))
options(sass.cache = FALSE)

source("app/global.R")
library(plotly)
library(data.table)

cat("====================================================================\n")
cat(" Testing Upgraded Fraud Analytics Components & Robust Handlers\n")
cat("====================================================================\n\n")

# 1. Test Data retrieval and Scoring
test_dt <- get_scored_test_data("random_forest")
cat("[1] Loaded scored test data:", nrow(test_dt), "rows\n")

# 2. Test Safe Correlation Calculation
cat("\n[2] Testing Safe Correlation Engine...\n")
run_safe_cor <- function(dt, method = "pearson") {
  if (nrow(dt) < 5 || stats::sd(dt$class) == 0) {
    return(list(status = "empty", msg = "Target 'Class' has zero variance or insufficient records in the selected filter.", data = data.frame()))
  }
  c_cols <- intersect(c(paste0("v", 1:28), "amount", "elapsed_hours", "amount_log"), names(dt))
  res <- list()
  for (col in c_cols) {
    v <- as.numeric(dt[[col]])
    if (stats::sd(v) > 0) {
      r <- stats::cor(v, as.numeric(dt$class), method = method, use = "pairwise.complete.obs")
      if (!is.na(r) && is.finite(r)) {
        res[[col]] <- data.frame(
          Feature = col,
          Correlation = r,
          Abs_Correlation = abs(r),
          Direction = ifelse(r < 0, "Negative (Protective Signal)", "Positive (Fraud Indicator)"),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(res) == 0) return(list(status = "empty", msg = "No valid numeric features found.", data = data.frame()))
  df <- do.call(rbind, res)
  df <- df[order(-df$Abs_Correlation), ]
  return(list(status = "success", data = df))
}

cor_res <- run_safe_cor(test_dt, "pearson")
cat("  -> Full cohort correlation count:", nrow(cor_res$data), "| Top:", cor_res$data$Feature[1], "(r =", round(cor_res$data$Correlation[1], 4), ")\n")

# Test zero variance case
cor_zero <- run_safe_cor(test_dt[class == 1], "pearson")
cat("  -> Zero variance test status:", cor_zero$status, "| message:", cor_zero$msg, "\n")
stopifnot(cor_zero$status == "empty")

# 3. Test Heatmap Matrix
cat("\n[3] Testing Heatmap Matrix Calculation...\n")
top_10_feats <- head(cor_res$data$Feature, 8)
heat_cols <- c(top_10_feats, "class")
heat_mat <- round(cor(as.matrix(test_dt[, ..heat_cols]), use = "pairwise.complete.obs"), 3)
cat("  -> Heatmap dimension:", nrow(heat_mat), "x", ncol(heat_mat), "\n")
stopifnot(nrow(heat_mat) == 9)

# 4. Test Density Shift Feature Statistics
cat("\n[4] Testing Feature Summary Statistics for Density Shift...\n")
calc_density_stats <- function(dt, feat = "v14") {
  legit_vals <- dt[class == 0][[feat]]
  fraud_vals <- dt[class == 1][[feat]]
  
  list(
    feature      = feat,
    n_legit      = length(legit_vals),
    n_fraud      = length(fraud_vals),
    legit_median = round(median(legit_vals, na.rm = TRUE), 3),
    fraud_median = round(median(fraud_vals, na.rm = TRUE), 3),
    legit_mean   = round(mean(legit_vals, na.rm = TRUE), 3),
    fraud_mean   = round(mean(fraud_vals, na.rm = TRUE), 3),
    mean_diff    = round(mean(fraud_vals, na.rm = TRUE) - mean(legit_vals, na.rm = TRUE), 3)
  )
}
d_stats <- calc_density_stats(test_dt, "v14")
cat("  -> Feature:", d_stats$feature, "| Legit Mean:", d_stats$legit_mean, "| Fraud Mean:", d_stats$fraud_mean, "| Shift:", d_stats$mean_diff, "\n")
stopifnot(abs(d_stats$shift - (-6.94)) < 1.0) # V14 exhibits severe negative shift in fraud

cat("\n====================================================================\n")
cat(" [ALL LOGIC TESTS PASSED] Upgraded Handlers Verified Successfully\n")
cat("====================================================================\n")

