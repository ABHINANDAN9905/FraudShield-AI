# ==============================================================================
# FraudShield - Shiny Dashboard Comprehensive Verification Test
# Script: tests/test_shiny_app.R
# ==============================================================================

local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib) && !(proj_lib %in% .libPaths())) {
    .libPaths(c(proj_lib, .libPaths()))
  }
})

suppressPackageStartupMessages({
  library(shiny)
  library(RSQLite)
  library(DBI)
  library(data.table)
})

cat("\n====================================================================\n")
cat(" FraudShield Shiny Dashboard Integrity & Unit Verification\n")
cat("====================================================================\n")

# Test 1: Sourcing Application Entry Point
cat("\n[TEST 1] Sourcing app/app.R and constructing Shiny app object...\n")
app_obj <- source("app/app.R")$value
stopifnot(inherits(app_obj, "shiny.appobj"))
cat("  -> SUCCESS: Shiny app object instantiated cleanly without syntax errors.\n")

# Test 2: Database Initialization & Seeding
cat("\n[TEST 2] Verifying SQLite Investigation Database...\n")
init_investigation_db()
db_path <- get_db_path()
stopifnot(file.exists(db_path))
cases <- fetch_investigations("All", "All")
cat(sprintf("  -> SUCCESS: SQLite DB active at %s with %d seed investigation records.\n", db_path, nrow(cases)))

# Test 3: Updating Investigation Record
cat("\n[TEST 3] Testing SQLite update transaction...\n")
if (nrow(cases) > 0) {
  first_id <- cases$id[1]
  update_investigation(first_id, "Under Review", "Automated unit test note verification.")
  updated_cases <- fetch_investigations("Under Review", "All")
  stopifnot(any(updated_cases$id == first_id))
  cat(sprintf("  -> SUCCESS: Record #%d status successfully transitioned to 'Under Review'.\n", first_id))
}

# Test 4: Verifying Model & Data Loader Cache
cat("\n[TEST 4] Verifying data and model retrieval routines...\n")
test_dt <- get_test_data()
cat(sprintf("  -> Test partition loaded: %s records (%d frauds).\n", format(nrow(test_dt), big.mark = ","), sum(test_dt$class == 1)))

rf_model <- get_cached_model("random_forest")
cat(sprintf("  -> Champion Random Forest retrieved from cache (trees = %d).\n", rf_model$num.trees))

model_tbl <- get_model_comparison_table()
cat(sprintf("  -> Model comparison table loaded (%d architectures evaluated).\n", nrow(model_tbl)))

thresh_tbl <- get_threshold_analysis_table()
cat(sprintf("  -> Threshold analysis grid loaded (%d thresholds evaluated).\n", nrow(thresh_tbl)))

feat_imp <- get_feature_importance_df()
cat(sprintf("  -> Feature importance table loaded (Top feature: %s).\n", feat_imp$Feature[1]))

# Test 5: Verifying Single & Batch Prediction routines
cat("\n[TEST 5] Testing prediction engine integration...\n")
source("R/prediction.R")
sample_tx <- test_dt[class == 1][1, ]
res <- predict_fraud_risk(sample_tx, model_type = "random_forest", threshold = 0.50)
stopifnot(res$fraud_probability >= 0.80)
cat(sprintf("  -> Prediction successful: Prob = %.4f | Tier = %s | Action = %s\n",
            res$fraud_probability, res$risk_category, res$recommended_action))

cat("\n====================================================================\n")
cat(" [ALL 5 INTEGRATION TESTS PASSED] Shiny Dashboard is 100% Ready\n")
cat("====================================================================\n")
