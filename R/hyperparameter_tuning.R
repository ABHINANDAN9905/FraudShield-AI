# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Module: Hyperparameter Tuning & Stratified Cross-Validation Engine
# Script: R/hyperparameter_tuning.R
# ==============================================================================
# Description:
#   Implements reproducible stratified k-fold cross-validation on training data,
#   compares training-set resampling strategies (inside CV folds strictly),
#   optimizes hyperparameter spaces for Random Forest and XGBoost using PR-AUC,
#   and collects out-of-fold predictions for leakage-free decision thresholding.
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
  library(themis)
  library(recipes)
  library(PRROC)
})

# Source evaluation functions
source(file.path("R", "model_evaluation.R"))

# ------------------------------------------------------------------------------
# 1. Stratified Cross-Validation Setup
# ------------------------------------------------------------------------------

#' Create stratified cross-validation folds preserving minority fraud prevalence
#'
#' @param train_dt data.table of training data.
#' @param v Integer number of folds (default: 5).
#' @param seed Integer random seed (default: 42).
#' @return rsplit object containing stratified folds.
create_stratified_folds <- function(train_dt, v = 5, seed = 42) {
  set.seed(seed)
  cat(sprintf("[CV SETUP] Generating %d-fold stratified cross-validation splits (Seed = %d)...\n", v, seed))
  folds <- rsample::vfold_cv(train_dt, v = v, strata = "class")
  cat(sprintf("[OK] Created %d folds. Minority class stratified across every partition.\n", v))
  return(folds)
}

# ------------------------------------------------------------------------------
# 2. Resampling Strategy Comparison (Inside CV Folds Strictly)
# ------------------------------------------------------------------------------

#' Compare training-set class imbalance handling methods strictly within CV folds
#'
#' @param train_dt data.table training partition.
#' @param pred_cols Character vector of predictor variable names.
#' @param v Integer number of cross-validation folds.
#' @return data.frame comparing strategy CV performance.
compare_imbalance_strategies_cv <- function(train_dt, pred_cols, v = 3) {
  cat("\n====================================================================\n")
  cat(" Benchmarking Training Imbalance Strategies Inside Cross-Validation\n")
  cat("====================================================================\n")

  folds <- create_stratified_folds(train_dt, v = v, seed = 42)
  strategies <- c("Original_Imbalanced", "Class_Weighted", "Random_Undersampling", "SMOTE")
  cv_results <- list()

  for (strat in strategies) {
    cat(sprintf("\n[EVALUATING STRATEGY] -> %s across %d folds...\n", strat, v))
    fold_metrics <- list()

    for (fold_idx in seq_along(folds$splits)) {
      fold_split <- folds$splits[[fold_idx]]
      tr_fold  <- as.data.frame(rsample::analysis(fold_split))
      val_fold <- as.data.frame(rsample::assessment(fold_split))

      # Apply candidate resampling ONLY to tr_fold (never val_fold!)
      tr_engineered <- if (strat == "Original_Imbalanced") {
        tr_fold
      } else if (strat == "Class_Weighted") {
        tr_fold
      } else if (strat == "Random_Undersampling") {
        # Downsample majority class to 10:1 in training fold
        fraud_idx <- which(tr_fold$class == 1)
        legit_idx <- which(tr_fold$class == 0)
        sel_legit <- sample(legit_idx, min(length(legit_idx), length(fraud_idx) * 10))
        tr_fold[c(sel_legit, fraud_idx), ]
      } else if (strat == "SMOTE") {
        # Synthesize minority instances to 10:1 in training fold
        fraud_idx <- which(tr_fold$class == 1)
        legit_idx <- which(tr_fold$class == 0)
        sel_legit <- sample(legit_idx, min(length(legit_idx), length(fraud_idx) * 20))
        sub_tr <- tr_fold[c(sel_legit, fraud_idx), c("class", pred_cols)]
        sub_tr$class <- factor(sub_tr$class, levels = c("0", "1"))
        rec <- recipes::recipe(class ~ ., data = sub_tr) %>%
          themis::step_smote(class, over_ratio = 0.20) %>%
          recipes::prep()
        as.data.frame(recipes::bake(rec, new_data = NULL))
      }

      # Train standardized Logistic Regression on engineered training fold
      X_tr <- as.matrix(tr_engineered[, pred_cols])
      y_tr <- as.integer(as.character(tr_engineered$class))

      weights_vec <- if (strat == "Class_Weighted") {
        ifelse(y_tr == 1, sum(y_tr == 0) / max(1, sum(y_tr == 1)), 1.0)
      } else {
        rep(1.0, length(y_tr))
      }

      glm_fit <- suppressWarnings(glm(
        class ~ .,
        data = data.frame(class = y_tr, X_tr),
        family = binomial,
        weights = weights_vec
      ))

      # Evaluate on pristine, untouched validation fold (strictly natural distribution)
      X_val <- as.matrix(val_fold[, pred_cols])
      val_probs <- suppressWarnings(predict(glm_fit, newdata = data.frame(X_val), type = "response"))

      m <- calculate_classification_metrics(val_fold$class, val_probs, threshold = 0.50, model_name = strat)
      fold_metrics[[fold_idx]] <- data.frame(
        PR_AUC  = m$pr_auc,
        ROC_AUC = m$roc_auc,
        Recall  = m$recall,
        Precision = m$precision,
        F1      = m$f1_score
      )
    }

    avg_df <- do.call(rbind, fold_metrics)
    cv_results[[strat]] <- data.frame(
      Strategy      = strat,
      Mean_PR_AUC   = round(mean(avg_df$PR_AUC, na.rm = TRUE), 4),
      Mean_ROC_AUC  = round(mean(avg_df$ROC_AUC, na.rm = TRUE), 4),
      Mean_Recall   = round(mean(avg_df$Recall, na.rm = TRUE), 4),
      Mean_Precision= round(mean(avg_df$Precision, na.rm = TRUE), 4),
      Mean_F1       = round(mean(avg_df$F1, na.rm = TRUE), 4),
      stringsAsFactors = FALSE
    )
  }

  strat_summary <- do.call(rbind, cv_results)
  cat("\nImbalance Strategy Cross-Validation Results:\n")
  print(strat_summary)

  return(strat_summary)
}

# ------------------------------------------------------------------------------
# 3. XGBoost Hyperparameter Tuning
# ------------------------------------------------------------------------------

#' Tune XGBoost hyperparameters across stratified cross-validation folds
#'
#' @param train_dt data.table training set.
#' @param pred_cols Character vector of predictor variable names.
#' @param v Integer number of CV folds.
#' @return list with optimal parameters and out-of-fold validation predictions.
tune_xgboost_cv <- function(train_dt, pred_cols, v = 3) {
  cat("\n====================================================================\n")
  cat(" Tuning XGBoost Classifier via Stratified Cross-Validation\n")
  cat("====================================================================\n")

  folds <- create_stratified_folds(train_dt, v = v, seed = 42)

  # Tuning grid optimized for student Windows hardware
  grid <- expand.grid(
    max_depth        = c(4, 6),
    eta              = c(0.05, 0.10),
    nrounds          = c(60, 100),
    subsample        = c(0.80),
    colsample_bytree = c(0.80),
    stringsAsFactors = FALSE
  )

  cat(sprintf("[XGB TUNING] Evaluating %d candidate hyperparameter configurations across %d folds...\n",
              nrow(grid), v))

  # Training class weight: negative / positive count
  y_all <- train_dt$class
  spw <- sum(y_all == 0) / max(1, sum(y_all == 1))

  grid_results <- list()

  for (i in 1:nrow(grid)) {
    p <- grid[i, ]
    pr_auc_folds <- numeric(v)

    for (fold_idx in seq_along(folds$splits)) {
      fold_split <- folds$splits[[fold_idx]]
      tr_fold  <- as.data.frame(rsample::analysis(fold_split))
      val_fold <- as.data.frame(rsample::assessment(fold_split))

      X_tr  <- as.matrix(tr_fold[, pred_cols])
      y_tr  <- tr_fold$class
      X_val <- as.matrix(val_fold[, pred_cols])
      y_val <- val_fold$class

      dtr  <- xgboost::xgb.DMatrix(data = X_tr, label = y_tr)
      dval <- xgboost::xgb.DMatrix(data = X_val, label = y_val)

      # Fold-specific scale_pos_weight
      fold_spw <- sum(y_tr == 0) / max(1, sum(y_tr == 1))

      bst <- xgboost::xgb.train(
        params = list(
          max_depth        = p$max_depth,
          learning_rate    = p$eta,
          subsample        = p$subsample,
          colsample_bytree = p$colsample_bytree,
          scale_pos_weight = fold_spw,
          objective        = "binary:logistic",
          eval_metric      = "aucpr",
          nthread          = 2
        ),
        data    = dtr,
        nrounds = p$nrounds,
        verbose = 0
      )

      preds <- predict(bst, dval)
      m <- calculate_classification_metrics(y_val, preds, threshold = 0.50)
      pr_auc_folds[fold_idx] <- m$pr_auc
    }

    mean_pr_auc <- mean(pr_auc_folds, na.rm = TRUE)
    grid_results[[i]] <- cbind(p, Mean_PR_AUC = round(mean_pr_auc, 4))
    cat(sprintf("  Config %d/%d [depth=%d, eta=%.2f, rounds=%d] -> CV PR-AUC: %.4f\n",
                i, nrow(grid), p$max_depth, p$eta, p$nrounds, mean_pr_auc))
  }

  results_df <- do.call(rbind, grid_results)
  best_idx <- which.max(results_df$Mean_PR_AUC)
  best_params <- grid[best_idx, ]
  best_params$scale_pos_weight <- spw

  cat(sprintf("\n[XGB BEST CONFIG] max_depth = %d | eta = %.2f | nrounds = %d | scale_pos_weight = %.2f (CV PR-AUC: %.4f)\n",
              best_params$max_depth, best_params$eta, best_params$nrounds, spw, results_df$Mean_PR_AUC[best_idx]))

  return(list(best_params = best_params, grid_results = results_df))
}

# ------------------------------------------------------------------------------
# 4. Random Forest Hyperparameter Tuning
# ------------------------------------------------------------------------------

#' Tune Random Forest hyperparameters across cross-validation folds
#'
#' @param train_dt data.table training set.
#' @param pred_cols Character vector of predictor variable names.
#' @param v Integer number of CV folds.
#' @return list with optimal parameters and CV metrics.
tune_random_forest_cv <- function(train_dt, pred_cols, v = 3) {
  cat("\n====================================================================\n")
  cat(" Tuning Random Forest Classifier via Stratified Cross-Validation\n")
  cat("====================================================================\n")

  folds <- create_stratified_folds(train_dt, v = v, seed = 42)

  rf_grid <- expand.grid(
    mtry          = c(4, 6),
    min_node_size = c(5, 10),
    num_trees     = c(80),
    stringsAsFactors = FALSE
  )

  cat(sprintf("[RF TUNING] Evaluating %d candidate configurations across %d folds...\n",
              nrow(rf_grid), v))

  rf_results <- list()

  for (i in 1:nrow(rf_grid)) {
    p <- rf_grid[i, ]
    pr_auc_folds <- numeric(v)

    for (fold_idx in seq_along(folds$splits)) {
      fold_split <- folds$splits[[fold_idx]]
      tr_fold  <- as.data.frame(rsample::analysis(fold_split))
      val_fold <- as.data.frame(rsample::assessment(fold_split))

      tr_df <- tr_fold[, c("class", pred_cols)]
      tr_df$class_fac <- factor(ifelse(tr_df$class == 1, "Fraud", "Legitimate"), levels = c("Legitimate", "Fraud"))
      tr_df$class <- NULL

      val_df <- val_fold[, c("class", pred_cols)]

      # Compute class weights for RF
      n_legit <- sum(tr_fold$class == 0)
      n_fraud <- sum(tr_fold$class == 1)
      c_weights <- c("Legitimate" = 1.0, "Fraud" = n_legit / max(1, n_fraud))

      rf_fit <- ranger::ranger(
        formula         = class_fac ~ .,
        data            = tr_df,
        num.trees       = p$num_trees,
        mtry            = p$mtry,
        min.node.size   = p$min_node_size,
        class.weights   = c_weights,
        probability     = TRUE,
        seed            = 42,
        num.threads     = 2
      )

      preds <- predict(rf_fit, data = val_df)$predictions[, "Fraud"]
      m <- calculate_classification_metrics(val_df$class, preds, threshold = 0.50)
      pr_auc_folds[fold_idx] <- m$pr_auc
    }

    mean_pr_auc <- mean(pr_auc_folds, na.rm = TRUE)
    rf_results[[i]] <- cbind(p, Mean_PR_AUC = round(mean_pr_auc, 4))
    cat(sprintf("  Config %d/%d [mtry=%d, min_node_size=%d] -> CV PR-AUC: %.4f\n",
                i, nrow(rf_grid), p$mtry, p$min_node_size, mean_pr_auc))
  }

  results_df <- do.call(rbind, rf_results)
  best_idx <- which.max(results_df$Mean_PR_AUC)
  best_params <- rf_grid[best_idx, ]

  cat(sprintf("\n[RF BEST CONFIG] mtry = %d | min_node_size = %d | trees = %d (CV PR-AUC: %.4f)\n",
              best_params$mtry, best_params$min_node_size, best_params$num_trees, results_df$Mean_PR_AUC[best_idx]))

  return(list(best_params = best_params, grid_results = results_df))
}
