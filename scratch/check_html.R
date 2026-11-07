local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib) && !(proj_lib %in% .libPaths())) {
    .libPaths(c(proj_lib, .libPaths()))
  }
})

app <- source("app/app.R")$value
tags_out <- htmltools::renderTags(app$ui)
full_html <- paste(as.character(tags_out$html), as.character(tags_out$head), collapse = " ")

pages <- c(
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

for (p in pages) {
  cat(sprintf("%-30s : %s\n", p, ifelse(grepl(p, full_html, fixed = TRUE), "PRESENT", "NOT FOUND")))
}

