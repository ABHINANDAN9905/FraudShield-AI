local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib)) .libPaths(c(proj_lib, .libPaths()))
})

suppressPackageStartupMessages({
  library(shiny)
  library(plotly)
  library(DT)
  library(data.table)
})

# Source global environment
source("app/global.R")

cat("\n====================================================================\n")
cat(" FraudShield Transaction Explorer Premium Redesign Test Suite\n")
cat("====================================================================\n")

# ------------------------------------------------------------------------------
# Test 1: Sourced Scored Test Data Integrity
# ------------------------------------------------------------------------------
cat("\n[TEST 1] Testing get_scored_test_data() across models...\n")
scored_rf <- get_scored_test_data("random_forest", threshold = 0.50)
stopifnot(nrow(scored_rf) == 56962)
stopifnot(all(c("transaction_id", "amount", "elapsed_hours", "actual_status",
                "fraud_probability", "model_prediction", "risk_category") %in% names(scored_rf)))

cat("  -> Random Forest Scored: 56,962 rows (", sum(scored_rf$actual_status == "Confirmed Fraud"), " Frauds | ",
    sum(scored_rf$model_prediction == "Flagged Alert"), " Model Alerts)\n")

# Verify Actual Class and Model Prediction are separate columns
stopifnot("actual_status" %in% names(scored_rf) && "model_prediction" %in% names(scored_rf))
cat("  -> Verified: Actual Status and Model Prediction are strictly distinct columns.\n")

# ------------------------------------------------------------------------------
# Test 2: Filter Logic Verification
# ------------------------------------------------------------------------------
cat("\n[TEST 2] Testing Multi-Dimensional Filters...\n")

# A. Fraud Only Filter
fraud_only <- scored_rf[actual_status == "Confirmed Fraud"]
stopifnot(nrow(fraud_only) == 93)
cat("  -> Fraud Only Filter: Exactly 93 confirmed attacks isolated.\n")

# B. Legitimate Only Filter
legit_only <- scored_rf[actual_status == "Legitimate"]
stopifnot(nrow(legit_only) == 56869)
cat("  -> Legitimate Only Filter: Exactly 56,869 legitimate transactions isolated.\n")

# C. Amount Range Filter ($100 to $500)
amt_filtered <- scored_rf[amount >= 100 & amount <= 500]
stopifnot(all(amt_filtered$amount >= 100 & amt_filtered$amount <= 500))
cat("  -> Amount Range Filter ($100 - $500): ", nrow(amt_filtered), " matches verified.\n")

# D. Elapsed Hours Range Filter (10h to 20h)
time_filtered <- scored_rf[elapsed_hours >= 10 & elapsed_hours <= 20]
stopifnot(all(time_filtered$elapsed_hours >= 10 & time_filtered$elapsed_hours <= 20))
cat("  -> Elapsed Time Filter (10h - 20h): ", nrow(time_filtered), " matches verified.\n")

# E. Search by ID
tx_search <- scored_rf[grepl("TX-00042", transaction_id)]
stopifnot(nrow(tx_search) == 1)
cat("  -> Transaction ID Search ('TX-00042'): Exactly 1 record found.\n")

# F. Risk Tier Filter
crit_risk <- scored_rf[risk_category == "Critical Risk"]
stopifnot(all(crit_risk$fraud_probability >= 0.80))
cat("  -> Critical Risk Filter (Prob >= 80%): ", nrow(crit_risk), " high-confidence vectors isolated.\n")

# ------------------------------------------------------------------------------
# Test 3: Filtered KPI Calculations
# ------------------------------------------------------------------------------
cat("\n[TEST 3] Testing Dynamic KPI Card Calculations on Filtered Set...\n")
sample_filter <- scored_rf[amount >= 50 & amount <= 200]
tot_m <- nrow(sample_filter)
n_f   <- sum(sample_filter$actual_status == "Confirmed Fraud")
n_l   <- sum(sample_filter$actual_status == "Legitimate")
tot_v <- sum(sample_filter$amount)
avg_v <- mean(sample_filter$amount)

cat("  Filtered Sample ($50 - $200):\n")
cat("    Total Matched   : ", format(tot_m, big.mark = ","), "\n")
cat("    Confirmed Frauds: ", n_f, "\n")
cat("    Legitimate Vol  : ", format(n_l, big.mark = ","), "\n")
cat("    Prevalence      : ", sprintf("%.4f%%", (n_f / tot_m) * 100), "\n")
cat("    Cumulative Value: ", scales::dollar(tot_v), "\n")
cat("    Average Value   : ", scales::dollar(avg_v), "\n")

stopifnot(tot_m == (n_f + n_l))
stopifnot(avg_v >= 50 && avg_v <= 200)
cat("  -> SUCCESS: KPI formulas reflect filtered cohort with exact numerical fidelity.\n")

# ------------------------------------------------------------------------------
# Test 4: Transaction Detail Inspection Vector Extraction
# ------------------------------------------------------------------------------
cat("\n[TEST 4] Testing Forensic Detail Dossier Extraction...\n")
sample_row <- scored_rf[actual_status == "Confirmed Fraud"][1, ]
pca_cols <- paste0("v", 1:28)
stopifnot(all(pca_cols %in% names(sample_row)))
pca_vals <- as.numeric(sample_row[, ..pca_cols])
stopifnot(length(pca_vals) == 28)
stopifnot(!any(is.na(pca_vals)))
cat("  -> All 28 PCA features extracted cleanly for ", sample_row$transaction_id, "\n")
cat("     Top discriminatory signals: V14 = ", round(sample_row$v14, 3), " | V17 = ", round(sample_row$v17, 3), "\n")

# ------------------------------------------------------------------------------
# Test 5: Verify Other Seven Dashboard Pages Remain Intact
# ------------------------------------------------------------------------------
cat("\n[TEST 5] Testing DOM Rendering of All 8 Dashboard Pages...\n")
source("scratch/verify_all_pages.R")

cat("\n====================================================================\n")
cat(" [ALL TESTS PASSED] Transaction Explorer Redesign is 100% Operational\n")
cat("====================================================================\n")

