# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Module: Machine Learning Model Training Engine
# Script: R/model_training.R
# ==============================================================================
# Description:
#   Trains 4 diverse machine learning algorithms on the 80% training partition:
#     1. Logistic Regression (Baseline Supervised Classifier)
#     2. Random Forest (Ensemble Tree Classifier via ranger)
#     3. XGBoost (Gradient Boosted Decision Trees via xgboost)
#     4. Isolation Forest (Unsupervised Anomaly Detection via isotree)
#   Records exact training durations, handles class imbalance, serializes trained
#   models to models/*.rds, and exports model metadata.
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
  library(ranger)
  library(xgboost)
  library(isotree)
})

# Source helper modules
source(file.path("R", "model_evaluation.R"))
source(file.path("R", "hyperparameter_tuning.R"))

# ------------------------------------------------------------------------------
# 1. Train Model 1: Logistic Regression
# ------------------------------------------------------------------------------

#' Train Baseline Logistic Regression Classifier
#'
#' @param train_dt data.table training set.
#' @param pred_cols Character vector of predictor features.
#' @return list containing fitted model and execution metadata.
train_logistic_regression <- function(train_dt, pred_cols) {
  cat("\n====================================================================\n")
  cat(" [TRAINING MODEL 1/4] LOGISTIC REGRESSION (BASELINE)\n")
  cat("====================================================================\n")

  df_train <- train_dt[, c("class", pred_cols), with = FALSE]

  t0 <- Sys.time()
  fit <- glm(class ~ ., data = df_train, family = binomial(link = "logit"))
  t1 <- Sys.time()

  fit_duration <- as.numeric(t1 - t0, units = "secs")
  cat(sprintf("[OK] Logistic Regression trained in %.2f seconds.\n", fit_duration))

  dir.create("models", recursive = TRUE, showWarnings = FALSE)
  saveRDS(fit, file.path("models", "logistic_regression.rds"))
  cat("[OK] Serialized model saved to: models/logistic_regression.rds\n")

  return(list(
    model        = fit,
    model_type   = "Logistic Regression",
    pred_cols    = pred_cols,
    fit_duration = fit_duration
  ))
}

# ------------------------------------------------------------------------------
# 2. Train Model 2: Random Forest (ranger)
# ------------------------------------------------------------------------------

#' Train Cost-Weighted Random Forest Classifier via ranger
#'
#' @param train_dt data.table training set.
#' @param pred_cols Character vector of predictor features.
#' @param best_params Optional tuned parameter list.
#' @return list containing fitted model and execution metadata.
train_random_forest <- function(train_dt, pred_cols, best_params = NULL) {
  cat("\n====================================================================\n")
  cat(" [TRAINING MODEL 2/4] RANDOM FOREST (RANGER ENSEMBLE)\n")
  cat("====================================================================\n")

  # Format binary target as factor with positive class 'Fraud'
  df_rf <- copy(train_dt[, c("class", pred_cols), with = FALSE])
  df_rf[, class_fac := factor(ifelse(class == 1, "Fraud", "Legitimate"), levels = c("Legitimate", "Fraud"))]
  df_rf[, class := NULL]

  # Compute inverse class weighting
  n_legit <- sum(train_dt$class == 0)
  n_fraud <- sum(train_dt$class == 1)
  class_weights <- c("Legitimate" = 1.0, "Fraud" = n_legit / max(1, n_fraud))

  mtry_val <- if (!is.null(best_params$mtry)) best_params$mtry else 5
  min_node <- if (!is.null(best_params$min_node_size)) best_params$min_node_size else 10
  n_trees  <- if (!is.null(best_params$num_trees)) best_params$num_trees else 100

  cat(sprintf("[INFO] Fitting Random Forest: trees=%d, mtry=%d, min_node_size=%d\n",
              n_trees, mtry_val, min_node))
  cat(sprintf("[INFO] Inverse class weighting applied: Fraud weight = %.2f\n", class_weights["Fraud"]))

  t0 <- Sys.time()
  fit <- ranger::ranger(
    formula         = class_fac ~ .,
    data            = df_rf,
    num.trees       = n_trees,
    mtry            = mtry_val,
    min.node.size   = min_node,
    class.weights   = class_weights,
    importance      = "impurity",
    probability     = TRUE,
    seed            = 42,
    num.threads     = 2
  )
  t1 <- Sys.time()

  fit_duration <- as.numeric(t1 - t0, units = "secs")
  cat(sprintf("[OK] Random Forest trained in %.2f seconds.\n", fit_duration))

  saveRDS(fit, file.path("models", "random_forest.rds"))
  cat("[OK] Serialized model saved to: models/random_forest.rds\n")

  return(list(
    model        = fit,
    model_type   = "Random Forest",
    pred_cols    = pred_cols,
    fit_duration = fit_duration,
    parameters   = list(trees = n_trees, mtry = mtry_val, min_node_size = min_node)
  ))
}

# ------------------------------------------------------------------------------
# 3. Train Model 3: XGBoost (Gradient Boosted Decision Trees)
# ------------------------------------------------------------------------------

#' Train Cost-Weighted XGBoost Classifier
#'
#' @param train_dt data.table training set.
#' @param pred_cols Character vector of predictor features.
#' @param best_params Optional tuned parameter list.
#' @return list containing fitted model and execution metadata.
train_xgboost <- function(train_dt, pred_cols, best_params = NULL) {
  cat("\n====================================================================\n")
  cat(" [TRAINING MODEL 3/4] XGBOOST (GRADIENT BOOSTED TREES)\n")
  cat("====================================================================\n")

  X_mat <- as.matrix(train_dt[, ..pred_cols])
  y_vec <- train_dt$class

  # Calculate class weighting on training set strictly
  spw <- sum(y_vec == 0) / max(1, sum(y_vec == 1))
  cat(sprintf("[INFO] Training scale_pos_weight calculated: %.2f : 1\n", spw))

  dtrain <- xgboost::xgb.DMatrix(data = X_mat, label = y_vec)

  nrounds   <- if (!is.null(best_params$nrounds)) best_params$nrounds else 100
  max_depth <- if (!is.null(best_params$max_depth)) best_params$max_depth else 5
  eta_val   <- if (!is.null(best_params$eta)) best_params$eta else 0.10

  cat(sprintf("[INFO] Training XGBoost: nrounds=%d, max_depth=%d, eta=%.2f\n",
              nrounds, max_depth, eta_val))

  t0 <- Sys.time()
  fit <- xgboost::xgb.train(
    params = list(
      max_depth        = max_depth,
      learning_rate    = eta_val,
      subsample        = 0.80,
      colsample_bytree = 0.80,
      scale_pos_weight = spw,
      objective        = "binary:logistic",
      eval_metric      = "aucpr",
      nthread          = 2
    ),
    data    = dtrain,
    nrounds = nrounds,
    verbose = 0
  )
  t1 <- Sys.time()

  fit_duration <- as.numeric(t1 - t0, units = "secs")
  cat(sprintf("[OK] XGBoost trained in %.2f seconds.\n", fit_duration))

  saveRDS(fit, file.path("models", "xgboost_model.rds"))
  cat("[OK] Serialized model saved to: models/xgboost_model.rds\n")

  return(list(
    model        = fit,
    model_type   = "XGBoost",
    pred_cols    = pred_cols,
    fit_duration = fit_duration,
    parameters   = list(nrounds = nrounds, max_depth = max_depth, eta = eta_val, scale_pos_weight = spw)
  ))
}

# ------------------------------------------------------------------------------
# 4. Train Model 4: Isolation Forest (Unsupervised Anomaly Detection)
# ------------------------------------------------------------------------------

#' Train Unsupervised Isolation Forest via isotree
#'
#' @param train_dt data.table training set.
#' @param pred_cols Character vector of predictor features (strictly WITHOUT Class).
#' @return list containing fitted model and execution metadata.
train_isolation_forest <- function(train_dt, pred_cols) {
  cat("\n====================================================================\n")
  cat(" [TRAINING MODEL 4/4] ISOLATION FOREST (UNSUPERVISED ANOMALY DETECTION)\n")
  cat("====================================================================\n")

  # STRICT RULE: Completely omit Class label from unsupervised anomaly training
  X_unsupervised <- as.matrix(train_dt[, ..pred_cols])

  cat(sprintf("[INFO] Training Isolation Forest on %s records across %d features.\n",
              format(nrow(X_unsupervised), big.mark = ","), ncol(X_unsupervised)))
  cat("       Class labels are strictly excluded from the training process.\n")

  t0 <- Sys.time()
  fit <- isotree::isolation.forest(
    data        = X_unsupervised,
    ntrees      = 100,
    ndim        = 1,
    seed        = 42,
    nthreads    = 2
  )
  t1 <- Sys.time()

  fit_duration <- as.numeric(t1 - t0, units = "secs")
  cat(sprintf("[OK] Isolation Forest trained in %.2f seconds.\n", fit_duration))

  saveRDS(fit, file.path("models", "isolation_forest.rds"))
  cat("[OK] Serialized model saved to: models/isolation_forest.rds\n")

  return(list(
    model        = fit,
    model_type   = "Isolation Forest",
    pred_cols    = pred_cols,
    fit_duration = fit_duration,
    is_unsupervised = TRUE
  ))
}

# ------------------------------------------------------------------------------
# 5. Master Training Coordinator
# ------------------------------------------------------------------------------

#' Train all 4 FraudShield models and persist pipeline metadata
#'
#' @param train_dt data.table training set.
#' @param pred_cols Character vector of predictor features.
#' @param rf_params Optional tuned RF parameters.
#' @param xgb_params Optional tuned XGBoost parameters.
#' @return List of all trained model objects.
train_all_models <- function(train_dt, pred_cols, rf_params = NULL, xgb_params = NULL) {
  cat("\n====================================================================\n")
  cat(" FraudShield Machine Learning Engine - Model Training Suite\n")
  cat("====================================================================\n")

  mod_lr  <- train_logistic_regression(train_dt, pred_cols)
  mod_rf  <- train_random_forest(train_dt, pred_cols, best_params = rf_params)
  mod_xgb <- train_xgboost(train_dt, pred_cols, best_params = xgb_params)
  mod_iso <- train_isolation_forest(train_dt, pred_cols)

  metadata <- list(
    trained_timestamp = Sys.time(),
    r_version         = R.version.string,
    training_records  = nrow(train_dt),
    fraud_records     = sum(train_dt$class == 1),
    features          = pred_cols,
    models            = list(
      logistic_regression = list(duration = mod_lr$fit_duration),
      random_forest       = list(duration = mod_rf$fit_duration, params = mod_rf$parameters),
      xgboost             = list(duration = mod_xgb$fit_duration, params = mod_xgb$parameters),
      isolation_forest    = list(duration = mod_iso$fit_duration, unsupervised = TRUE)
    )
  )

  saveRDS(metadata, file.path("models", "pipeline_metadata.rds"))
  cat("\n[OK] Pipeline metadata saved to: models/pipeline_metadata.rds\n")

  return(list(
    logistic_regression = mod_lr,
    random_forest       = mod_rf,
    xgboost             = mod_xgb,
    isolation_forest    = mod_iso,
    metadata            = metadata
  ))
}
