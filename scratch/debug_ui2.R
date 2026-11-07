local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib) && !(proj_lib %in% .libPaths())) {
    .libPaths(c(proj_lib, .libPaths()))
  }
})
app <- source("app/app.R")$value
cat("names(app):\n")
print(names(app))
cat("app$ui:\n")
print(class(app$ui))
if (is.function(app$ui)) {
  cat("app$ui is a function! Evaluating app$ui()...\n")
  evaluated_ui <- as.character(app$ui(list()))
  cat("grepl FraudShield in evaluated_ui: ", grepl("FraudShield", evaluated_ui), "\n")
}

