# ==============================================================================
# FraudShield - Test Suite for Advanced ML Model Intelligence Engine (Page 4)
# Script: scratch/test_model_performance_engine.R
# ==============================================================================

cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))
options(sass.cache = FALSE)

source("app/global.R")
library(shiny)
library(plotly)
library(DT)

cat("====================================================================\n")
cat(" FraudShield Advanced ML Model Intelligence Engine Test Suite\n")
cat("====================================================================\n\n")

# [TEST 1] DOM Verification of UI Components across all 10 tabs
cat("[TEST 1] Testing model_performance_ui() DOM generation...\n")
mp_ui <- model_performance_ui("mp_test")
ui_str <- as.character(mp_ui)
cat("  -> Rendered UI DOM character length:", nchar(ui_str), "\n")

expected_ids <- c(
  "active_model", "opt_threshold", "model_status_badge",
  "benchmark_metrics_table", "bench_pr_curves", "bench_roc_curves",
  "bootstrap_ci_plot", "bootstrap_ci_table",
  "threshold_confusion_matrix_ui", "threshold_tradeoff_plot", "operating_policies_table",
  "sim_cost_fp", "sim_cost_fn_pct", "sim_recovery_rate", "sim_capacity",
  "cost_kpi_cards_ui", "cost_curve_plotly",
  "calibration_plot", "calibration_deciles_table",
  "drift_status_ui", "drift_dist_plot", "drift_psi_table",
  "error_type_filter", "error_records_dt", "error_amount_plot", "error_time_plot",
  "explain_feat_imp_plot", "explain_local_waterfall",
  "governance_metadata_ui", "readiness_checklist_table",
  "download_experiments_csv", "experiments_dt"
)

for (id_name in expected_ids) {
  if (grepl(id_name, ui_str, fixed = TRUE)) {
    cat(sprintf("  -> ID '%s': FOUND [OK]\n", id_name))
  } else {
    stop(sprintf("ID '%s' NOT FOUND in UI DOM!", id_name))
  }
}

# [TEST 2] Testing SQLite Experiment Registry
cat("\n[TEST 2] Testing SQLite Experiment Registry...\n")
exp_df <- get_experiment_registry()
cat("  -> Total experiments in registry:", nrow(exp_df), "\n")
stopifnot(nrow(exp_df) >= 4)
stopifnot("EXP-2026-001" %in% exp_df$experiment_id)
cat("  -> Verified Champion Random Forest experiment record in database [OK]\n")

# [TEST 3] Running shiny::testServer on model_performance_server
cat("\n[TEST 3] Running shiny::testServer on model_performance_server...\n")
testServer(model_performance_server, {
  cat("  -> Server session initialized successfully.\n")
  
  # A. Test Random Forest (Champion) Initial Metrics
  session$setInputs(active_model = "random_forest", opt_threshold = 0.50)
  session$flushReact()
  
  m <- active_metrics()
  cat(sprintf("  -> Champion RF: Precision = %.2f%% | Recall = %.2f%% | F1 = %.4f | F2 = %.4f\n",
              m$precision * 100, m$recall * 100, m$f1_score, m$f2_score))
  cat(sprintf("     TP = %d | FP = %d | FN = %d | TN = %d\n", m$tp, m$fp, m$fn, m$tn))
  stopifnot(m$tp == 75)
  stopifnot(m$fp == 3)
  stopifnot(m$fn == 18)
  stopifnot(abs(m$recall - 0.8065) < 0.01)
  stopifnot(abs(m$precision - 0.9615) < 0.01)
  
  # B. Test Bootstrap Confidence Intervals
  cat("  -> Testing Bootstrap Resampling (B = 100, Seed 42)...\n")
  ci_df <- bootstrap_reactive()
  stopifnot(nrow(ci_df) == 4)
  rec_ci <- ci_df[ci_df$Metric == "Recall (Sensitivity)", ]
  cat(sprintf("     Recall Point: %.2f%% | 95%% CI: [%.2f%%, %.2f%%]\n",
              rec_ci$Estimate * 100, rec_ci$CI_Lower * 100, rec_ci$CI_Upper * 100))
  
  # C. Test Financial Cost Simulation
  cat("  -> Testing Financial Risk Simulation...\n")
  session$setInputs(sim_cost_fp = 15.0, sim_cost_fn_pct = 1.0, sim_recovery_rate = 0.10, sim_capacity = 100)
  session$flushReact()
  sim <- financial_sim_reactive()
  cat(sprintf("     Exposure: $%.2f | Missed Loss: $%.2f | FP Cost: $%.2f | Net Benefit: $%.2f (%.1f%% saved)\n",
              sim$total_exposure, sim$missed_loss, sim$fp_invest_cost, sim$net_benefit, sim$savings_pct))
  stopifnot(sim$total_exposure > 11000)
  stopifnot(sim$fp_invest_cost == 45) # 3 FP * $15
  
  # D. Test Calibration Curve Deciles
  cat("  -> Testing Probability Calibration Curve (10 bins)...\n")
  cal_df <- calibration_reactive()
  stopifnot(nrow(cal_df) == 10)
  cat("     Evaluated 10 probability deciles successfully [OK]\n")
  
  # E. Test Population Drift (PSI)
  cat("  -> Testing Temporal Drift Diagnostics...\n")
  d <- drift_reactive()
  cat(sprintf("     PSI: %.4f | Status: %s\n", d$psi, d$status))
  stopifnot(d$psi < 0.10) # Stable
  
  # F. Test Error Cohort Extraction
  cat("  -> Testing False Negatives Extraction...\n")
  session$setInputs(error_type_filter = "FN")
  session$flushReact()
  fn_dt <- error_data_reactive()
  cat(sprintf("     Extracted %d False Negatives (Missed Frauds)\n", nrow(fn_dt)))
  stopifnot(nrow(fn_dt) == 18)
  
  session$setInputs(error_type_filter = "FP")
  session$flushReact()
  fp_dt <- error_data_reactive()
  cat(sprintf("     Extracted %d False Positives (False Alarms)\n", nrow(fp_dt)))
  stopifnot(nrow(fp_dt) == 3)
  
  # G. Test Switching Model to XGBoost
  cat("  -> Switching to Challenger Model: XGBoost...\n")
  session$setInputs(active_model = "xgboost")
  session$flushReact()
  m_xgb <- active_metrics()
  cat(sprintf("     XGBoost: Recall = %.2f%% | Precision = %.2f%% | TP = %d | FP = %d\n",
              m_xgb$recall * 100, m_xgb$precision * 100, m_xgb$tp, m_xgb$fp))
  stopifnot(m_xgb$tp == 78)
  stopifnot(m_xgb$fp == 26)
  
  # H. Test Isolation Forest Anomaly Model
  cat("  -> Switching to Isolation Forest Anomaly Model...\n")
  session$setInputs(active_model = "isolation_forest", opt_threshold = 0.58)
  session$flushReact()
  m_iso <- active_metrics()
  cat(sprintf("     Isolation Forest: TP = %d | FP = %d | FN = %d | Anomaly Recall = %.2f%%\n",
              m_iso$tp, m_iso$fp, m_iso$fn, m_iso$recall * 100))
  stopifnot(m_iso$tp > 30)
  
  cat("  -> shiny::testServer executed with 100% success across all modules.\n")
})

# [TEST 4] Test Full App UI DOM Integration Across All 8 Pages
cat("\n[TEST 4] Verifying All 8 Pages in Main Application DOM...\n")
source("app/ui.R")
app_html <- as.character(ui)
cat("  -> Main Application DOM string length:", nchar(app_html), "chars\n")

pages <- c(
  "Executive Overview",
  "Transaction Explorer",
  "Fraud Analytics",
  "ML Model Performance",
  "Explainable AI",
  "Risk Scoring",
  "Investigation Center",
  "Reports"
)

for (pg in pages) {
  if (grepl(pg, app_html, fixed = TRUE)) {
    cat(sprintf("  -> Page '%-25s': FOUND [OK]\n", pg))
  } else {
    stop(sprintf("Page '%s' NOT FOUND in app UI DOM!", pg))
  }
}

cat("\n====================================================================\n")
cat(" [ALL TESTS PASSED] ML Model Performance Engine is 100% Operational\n")
cat("====================================================================\n")

