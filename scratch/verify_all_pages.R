local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib) && !(proj_lib %in% .libPaths())) {
    .libPaths(c(proj_lib, .libPaths()))
  }
})
# Source data, helpers, modules
app_dir <- file.path(getwd(), "app")
source(file.path(app_dir, "R", "dashboard_data.R"))
source(file.path(app_dir, "R", "dashboard_helpers.R"))
source(file.path(app_dir, "modules", "overview_module.R"))
source(file.path(app_dir, "modules", "transaction_module.R"))
source(file.path(app_dir, "modules", "fraud_analytics_module.R"))
source(file.path(app_dir, "modules", "model_performance_module.R"))
source(file.path(app_dir, "modules", "explainability_module.R"))
source(file.path(app_dir, "modules", "prediction_module.R"))
source(file.path(app_dir, "modules", "investigation_module.R"))
source(file.path(app_dir, "modules", "reports_module.R"))
source(file.path(app_dir, "ui.R"))

ui_str <- as.character(htmltools::renderTags(ui)$html)
cat("UI Rendered String Length: ", nchar(ui_str), " chars.\n")

pages <- c(
  "FraudShield",
  "Executive Overview",
  "Transaction Explorer",
  "Fraud Analytics",
  "ML Model Performance",
  "Explainable AI",
  "Risk Scoring",
  "Investigation Center",
  "Reports &amp; Data Quality"
)

for (p in pages) {
  cat(sprintf("  %-30s : %s\n", p, ifelse(grepl(p, ui_str, fixed = TRUE), "PASSED [OK]", "FAILED [MISSING]")))
  stopifnot(grepl(p, ui_str, fixed = TRUE))
}

cat("\n>>> ALL 8 DASHBOARD PAGES VERIFIED IN RENDERED UI DOM <<<\n")
