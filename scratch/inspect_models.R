cat("--- Reading models/pipeline_metadata.rds ---\n")
if (file.exists("models/pipeline_metadata.rds")) {
  meta <- readRDS("models/pipeline_metadata.rds")
  str(meta)
}

cat("\n--- Inspecting models: classes and predictors ---\n")
rf <- readRDS("models/random_forest.rds")
cat("Random Forest class:", class(rf), "\n")
cat("RF independent.variable.names:\n")
print(rf$forest$independent.variable.names)

xgb <- readRDS("models/xgboost_model.rds")
cat("\nXGBoost class:", class(xgb), "\n")
cat("XGB feature names:\n")
print(xgb$feature_names)

lr <- readRDS("models/logistic_regression.rds")
cat("\nLogistic Regression class:", class(lr), "\n")
cat("LR coefficients names:\n")
print(names(coef(lr)))

iso <- readRDS("models/isolation_forest.rds")
cat("\nIsolation Forest class:", class(iso), "\n")

cat("\n--- Inspecting test_data.rds column names and types ---\n")
test_data <- readRDS("data/processed/test_data.rds")
print(names(test_data))
print(dim(test_data))

