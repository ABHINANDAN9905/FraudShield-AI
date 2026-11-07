cat("--- Checking available packages ---\n")
pkgs <- c("jsonlite", "digest", "DT", "plotly", "scales", "bslib", "shinyWidgets", "RSQLite", "DBI", "ranger", "xgboost", "isotree", "DALEX", "iBreakDown")
for (p in pkgs) {
  cat(sprintf("%-15s: %s\n", p, requireNamespace(p, quietly = TRUE)))
}

