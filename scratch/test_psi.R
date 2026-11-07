cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))

source("app/R/dashboard_data.R")

test_dt <- get_scored_test_data("random_forest")

day1_scores <- test_dt[elapsed_hours <= 24, fraud_probability]
day2_scores <- test_dt[elapsed_hours > 24, fraud_probability]

calc_psi <- function(ref, comp, bins = 10) {
  breaks <- seq(0, 1, length.out = bins + 1)
  ref_counts <- hist(ref, breaks = breaks, plot = FALSE)$counts
  comp_counts <- hist(comp, breaks = breaks, plot = FALSE)$counts
  
  ref_pct <- (ref_counts + 1) / (length(ref) + bins)
  comp_pct <- (comp_counts + 1) / (length(comp) + bins)
  
  psi_vec <- (comp_pct - ref_pct) * log(comp_pct / ref_pct)
  return(sum(psi_vec))
}

psi_val <- calc_psi(day1_scores, day2_scores)
cat(sprintf("PSI between Day 1 and Day 2 prediction scores: %.4f\n", psi_val))
status <- if (psi_val < 0.10) "Stable (No significant distribution drift)" else if (psi_val < 0.25) "Moderate Warning" else "Critical Drift"
cat(sprintf("Status: %s\n", status))

