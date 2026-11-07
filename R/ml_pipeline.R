# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Module: Master Machine Learning Pipeline Orchestrator
# Script: R/ml_pipeline.R
# ==============================================================================
# Description:
#   End-to-end master orchestrator coordinating:
#     1. Dataset ingestion & canonical feature alignment
#     2. Stratified Cross-Validation & Imbalance Benchmarking
#     3. Hyperparameter tuning (RF & XGBoost)
#     4. Training 4 distinct model architectures
#     5. Holdout test set evaluation & model comparison
#     6. Threshold optimization & financial cost modeling
#     7. Model serialization & explainable AI reporting
#     8. Verification of real-time prediction engine
# ==============================================================================

# Ensure project library is available
local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib) && !(proj_lib %in% .libPaths())) {
    .libPaths(c(proj_lib, .libPaths()))
  }
})

suppressPackageStartupMessages({
  library(data.table)
  library(rsample)
  library(ranger)
  library(xgboost)
  library(isotree)
})

# Source all modular components
source(file.path("R", "model_evaluation.R"))
source(file.path("R", "hyperparameter_tuning.R"))
source(file.path("R", "model_training.R"))
source(file.path("R", "model_comparison.R"))
source(file.path("R", "prediction.R"))

#' Master ML Pipeline Execution Routine
#'
#' @param train_path Character path to train dataset (RDS or CSV).
#' @param test_path Character path to test dataset (RDS or CSV).
#' @param operating_threshold Numeric default decision threshold.
#' @param run_cv_benchmark Logical. Whether to run the 3-fold imbalance benchmark.
#' @param tune_hyperparameters Logical. Whether to re-run cross-validation hyperparameter search.
#' @return List containing all trained models, comparison results, and test predictions.
run_ml_pipeline <- function(train_path = file.path("data", "processed", "train_data.rds"),
                            test_path  = file.path("data", "processed", "test_data.rds"),
                            operating_threshold = 0.50,
                            run_cv_benchmark = TRUE,
                            tune_hyperparameters = TRUE) {
  cat("\n====================================================================\n")
  cat(" ==================================================================\n")
  cat("  FRAUDSHIELD - MACHINE LEARNING ENGINE MASTER ORCHESTRATOR\n")
  cat(" ==================================================================\n")
  cat("====================================================================\n\n")

  pipeline_t0 <- Sys.time()

  # ----------------------------------------------------------------------------
  # Step 1: Ingest Partitioned Datasets
  # ----------------------------------------------------------------------------
  cat(">>> STEP 1: Ingesting Stratified Train & Test Partitions\n")

  if (!file.exists(train_path) || !file.exists(test_path)) {
    cat("[INFO] Partitions not found as RDS. Checking feature dataset...\n")
    feat_path <- file.path("data", "processed", "creditcard_features.csv")
    if (!file.exists(feat_path)) {
      cat("[INFO] creditcard_features.csv not found. Running feature engineering...\n")
      source(file.path("R", "feature_engineering.R"))
      run_feature_engineering()
    }
  }

  train_dt <- as.data.table(readRDS(train_path))
  test_dt  <- as.data.table(readRDS(test_path))

  cat(sprintf("[OK] Loaded Train Data: %s rows (%d frauds, %.4f%%)\n",
              format(nrow(train_dt), big.mark = ","), sum(train_dt$class == 1), mean(train_dt$class == 1) * 100))
  cat(sprintf("[OK] Loaded Test Data : %s rows (%d frauds, %.4f%%)\n",
              format(nrow(test_dt), big.mark = ","), sum(test_dt$class == 1), mean(test_dt$class == 1) * 100))

  # Define canonical predictor features
  # Features: v1 to v28 + amount_log + elapsed_hours
  pred_cols <- c(paste0("v", 1:28), "amount_log", "elapsed_hours")
  cat(sprintf("[OK] Canonical Predictor Features (%d features): %s\n",
              length(pred_cols), paste(c(head(pred_cols, 4), "...", tail(pred_cols, 2)), collapse = ", ")))

  # ----------------------------------------------------------------------------
  # Step 2: Stratified Cross-Validation & Imbalance Strategy Benchmark
  # ----------------------------------------------------------------------------
  if (run_cv_benchmark) {
    cat("\n>>> STEP 2: Cross-Validation & Imbalance Strategy Benchmark (Inside Folds)\n")
    imbalance_cv_summary <- compare_imbalance_strategies_cv(train_dt, pred_cols = pred_cols, v = 3)
  } else {
    cat("\n>>> STEP 2: Cross-Validation Benchmark (Skipped - using saved results)\n")
  }

  # ----------------------------------------------------------------------------
  # Step 3: Hyperparameter Tuning via Cross-Validation
  # ----------------------------------------------------------------------------
  if (tune_hyperparameters) {
    cat("\n>>> STEP 3: Hyperparameter Optimization (PR-AUC Maximization)\n")
    xgb_tune <- tune_xgboost_cv(train_dt, pred_cols = pred_cols, v = 3)
    rf_tune  <- tune_random_forest_cv(train_dt, pred_cols = pred_cols, v = 3)
    best_xgb_params <- xgb_tune$best_params
    best_rf_params  <- rf_tune$best_params
  } else {
    cat("\n>>> STEP 3: Using Pre-Tuned Optimal Hyperparameters (PR-AUC Maximized)\n")
    neg_count <- sum(train_dt$class == 0)
    pos_count <- sum(train_dt$class == 1)
    best_xgb_params <- list(max_depth = 6, learning_rate = 0.10, nrounds = 100, scale_pos_weight = neg_count / pos_count)
    best_rf_params  <- list(mtry = 6, min_node_size = 5, num_trees = 80)
  }

  # ----------------------------------------------------------------------------
  # Step 4: Model Training on Full Training Partition (80%)
  # ----------------------------------------------------------------------------
  cat("\n>>> STEP 4: Fitting All 4 Models on 80% Training Data\n")
  models_list <- train_all_models(
    train_dt   = train_dt,
    pred_cols  = pred_cols,
    rf_params  = best_rf_params,
    xgb_params = best_xgb_params
  )

  # ----------------------------------------------------------------------------
  # Step 5: Holdout Test Set Evaluation & Comprehensive Comparison
  # ----------------------------------------------------------------------------
  cat("\n>>> STEP 5: Model Evaluation on Isolated Holdout Test Set\n")
  comp_results <- run_model_comparison_pipeline(
    models_list = models_list,
    test_dt     = test_dt,
    pred_cols   = pred_cols,
    operating_threshold = operating_threshold
  )

  # ----------------------------------------------------------------------------
  # Step 6: Prediction Engine Verification
  # ----------------------------------------------------------------------------
  cat("\n>>> STEP 6: Real-Time Prediction Engine Verification\n")

  # Clear any transient data.table secondary indexes to avoid index desync
  data.table::setindex(test_dt, NULL)

  # Test Sample 1: A genuine fraud record from the test set
  fraud_indices <- which(test_dt$class == 1)
  sample_fraud  <- test_dt[fraud_indices[1], ]
  pred_fraud    <- predict_fraud_risk(sample_fraud, model_type = "best", threshold = operating_threshold)

  cat(sprintf("\n[TEST 1 - CONFIRMED FRAUD RECORD]:\n"))
  cat(sprintf("  Actual Class        : Fraud (1)\n"))
  cat(sprintf("  Fraud Probability   : %.4f (%.2f%%)\n", pred_fraud$fraud_probability, pred_fraud$fraud_probability * 100))
  cat(sprintf("  Isolation Score     : %.4f\n", pred_fraud$anomaly_score))
  cat(sprintf("  Assigned Risk Tier  : %s\n", pred_fraud$risk_category))
  cat(sprintf("  Recommended Action  : %s\n", pred_fraud$recommended_action))

  # Test Sample 2: A normal legitimate transaction
  legit_indices <- which(test_dt$class == 0)
  sample_legit  <- test_dt[legit_indices[1], ]
  pred_legit    <- predict_fraud_risk(sample_legit, model_type = "best", threshold = operating_threshold)

  cat(sprintf("\n[TEST 2 - CONFIRMED LEGITIMATE RECORD]:\n"))
  cat(sprintf("  Actual Class        : Legitimate (0)\n"))
  cat(sprintf("  Fraud Probability   : %.4f (%.2f%%)\n", pred_legit$fraud_probability, pred_legit$fraud_probability * 100))
  cat(sprintf("  Isolation Score     : %.4f\n", pred_legit$anomaly_score))
  cat(sprintf("  Assigned Risk Tier  : %s\n", pred_legit$risk_category))
  cat(sprintf("  Recommended Action  : %s\n", pred_legit$recommended_action))

  # Test Sample 3: Input validation check (missing column handling)
  cat("\n[TEST 3 - INPUT ERROR HANDLING VERIFICATION]:\n")
  bad_input <- copy(sample_legit)
  bad_input[, v1 := NULL]
  err_status <- tryCatch({
    predict_fraud_risk(bad_input)
    "FAILED TO THROW ERROR"
  }, error = function(e) {
    sprintf("CORRECTLY CAUGHT ERROR: %s", e$message)
  })
  cat(sprintf("  Validation Result: %s\n", err_status))

  pipeline_t1 <- Sys.time()
  total_mins <- round(as.numeric(pipeline_t1 - pipeline_t0, units = "mins"), 2)

  cat("\n====================================================================\n")
  cat(sprintf(" [COMPLETE] FraudShield ML Engine Finished in %.2f Minutes\n", total_mins))
  cat(" Champion Supervised Model: ", comp_results$best_model, "\n")
  cat(" All Models Serialized in models/*.rds\n")
  cat(" Evaluation Tables Saved in outputs/tables/\n")
  cat(" Analytical Plots Saved in outputs/plots/\n")
  cat(" Comprehensive Report Saved in outputs/reports/ml_summary_report.txt\n")
  cat("====================================================================\n\n")

  return(invisible(list(
    models      = models_list,
    comparison  = comp_results,
    test_sample_fraud = pred_fraud,
    test_sample_legit = pred_legit,
    execution_time_minutes = total_mins
  )))
}

# Execute automatically when invoked directly via Rscript (sys.nframe() == 0L)
if (sys.nframe() == 0L) {
  ml_run_results <- run_ml_pipeline()
}

