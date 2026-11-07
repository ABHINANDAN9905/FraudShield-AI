local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib)) .libPaths(c(proj_lib, .libPaths()))
})
source("app/global.R")

t0 <- Sys.time()
scored_rf <- get_scored_test_data(model_type = "random_forest", threshold = 0.50)
t1 <- Sys.time()
cat("First scored call took: ", as.numeric(t1 - t0, units = "secs"), "seconds.\n")
cat("Dimensions: ", nrow(scored_rf), "x", ncol(scored_rf), "\n")
cat("Columns:\n")
print(names(scored_rf)[(ncol(scored_rf) - 8):ncol(scored_rf)])

t0 <- Sys.time()
scored_rf_cached <- get_scored_test_data(model_type = "random_forest", threshold = 0.70)
t1 <- Sys.time()
cat("Cached call took: ", as.numeric(t1 - t0, units = "secs"), "seconds.\n")
cat("Summary of Risk Categories at threshold 0.70:\n")
print(table(scored_rf_cached$risk_category))
cat("Summary of Actual Status:\n")
print(table(scored_rf_cached$actual_status))

