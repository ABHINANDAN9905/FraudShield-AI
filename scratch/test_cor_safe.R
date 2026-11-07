cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))

source("app/R/dashboard_data.R")

test_dt <- get_scored_test_data("random_forest")
cat("Scored test data rows:", nrow(test_dt), "\n")
cat("Columns in scored test data:\n", paste(names(test_dt), collapse = ", "), "\n\n")

# Function to safely compute correlations with Class
compute_safe_correlations <- function(dt, method = "pearson") {
  # 1. Target validation
  if (!"class" %in% names(dt)) return(NULL)
  target <- as.numeric(dt$class)
  target_valid <- !is.na(target) & is.finite(target)
  
  if (sum(target_valid) < 5 || stats::sd(target[target_valid]) == 0) {
    # Zero variance or insufficient data
    return(list(
      status = "zero_variance",
      message = "Correlation cannot be computed because the target 'Class' has zero variance in the active filter selection (e.g. all transactions are the same class).",
      results = data.frame()
    ))
  }
  
  # 2. Extract valid numeric candidate features (exclude class, IDs, characters, factors)
  candidate_cols <- c(paste0("v", 1:28), "amount", "elapsed_hours", "amount_log")
  candidate_cols <- intersect(candidate_cols, names(dt))
  
  valid_feats <- character(0)
  cors <- numeric(0)
  
  for (col in candidate_cols) {
    vec <- as.numeric(dt[[col]])
    idx <- target_valid & !is.na(vec) & is.finite(vec)
    if (sum(idx) >= 5 && stats::sd(vec[idx]) > 0) {
      r_val <- tryCatch(
        stats::cor(vec[idx], target[idx], method = method),
        error = function(e) NA_real_
      )
      if (!is.na(r_val) && is.finite(r_val)) {
        valid_feats <- c(valid_feats, col)
        cors <- c(cors, r_val)
      }
    }
  }
  
  if (length(cors) == 0) {
    return(list(
      status = "no_features",
      message = "No numeric features with non-zero variance available.",
      results = data.frame()
    ))
  }
  
  res_df <- data.frame(
    Feature = valid_feats,
    Correlation = cors,
    Abs_Correlation = abs(cors),
    Direction = ifelse(cors < 0, "Negative (Protective Signal)", "Positive (Fraud Indicator)"),
    stringsAsFactors = FALSE
  )
  res_df <- res_df[order(-res_df$Abs_Correlation), ]
  
  return(list(
    status = "success",
    results = res_df
  ))
}

# Test 1: Full dataset
t1 <- compute_safe_correlations(test_dt, "pearson")
cat("Test 1 (Full dataset): status =", t1$status, "| Features evaluated =", nrow(t1$results), "\n")
print(head(t1$results, 6))

# Test 2: Filtered to only Fraud (zero variance in class)
t2 <- compute_safe_correlations(test_dt[class == 1], "pearson")
cat("\nTest 2 (Only Fraud): status =", t2$status, "| message =", t2$message, "\n")

# Test 3: Spearman method
t3 <- compute_safe_correlations(test_dt, "spearman")
cat("\nTest 3 (Spearman): status =", t3$status, "| Top feature =", t3$results$Feature[1], "| r =", round(t3$results$Correlation[1], 4), "\n")

