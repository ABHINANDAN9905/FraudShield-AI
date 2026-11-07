local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib) && !(proj_lib %in% .libPaths())) {
    .libPaths(c(proj_lib, .libPaths()))
  }
})
app <- source("app/app.R")$value
cat("class(app$ui): ", class(app$ui), "\n")
rendered <- as.character(app$ui)
cat("nchar rendered: ", nchar(rendered), "\n")
cat("sample rendered:\n", substr(rendered, 1, 500), "\n")
cat("grepl FraudShield in as.character(app$ui): ", grepl("FraudShield", rendered), "\n")

