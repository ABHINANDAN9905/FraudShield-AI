local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib) && !(proj_lib %in% .libPaths())) {
    .libPaths(c(proj_lib, .libPaths()))
  }
})

suppressPackageStartupMessages({
  library(shiny)
  library(htmltools)
})

app <- source("app/app.R")$value
ui_html <- as.character(renderTags(app$ui)$html)

cat("\n--- Verifying Rendered UI Elements ---\n")
check_strings <- c(
  "FraudShield",
  "Executive Overview",
  "Transaction Explorer",
  "Fraud Analytics",
  "ML Model Performance",
  "Explainable AI",
  "Risk Scoring & Prediction",
  "Investigation Center",
  "Reports & Data Quality"
)

for (s in check_strings) {
  found <- grepl(s, ui_html, fixed = TRUE)
  cat(sprintf("  [%s] %s\n", ifelse(found, "OK", "MISSING"), s))
  stopifnot(found)
}

cat("\nHTML Size: ", nchar(ui_html), " bytes.\n")
cat("ALL 8 PAGES AND BRANDING SUCCESSFULLY VERIFIED IN RENDERED HTML!\n")

