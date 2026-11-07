local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib)) .libPaths(c(proj_lib, .libPaths()))
})
library(shiny)
res <- tryCatch({
  source("app/ui.R")
  "SUCCESS"
}, error = function(e) {
  e$message
})
cat("source('app/ui.R') result: \n", res, "\n")

