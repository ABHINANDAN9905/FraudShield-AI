local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib)) .libPaths(c(proj_lib, .libPaths()))
})
library(shiny)
res <- tryCatch({
  source("app/server.R")
  "SUCCESS"
}, error = function(e) {
  e$message
})
cat("source('app/server.R') result: \n", res, "\n")

