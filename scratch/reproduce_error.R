local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib)) .libPaths(c(proj_lib, .libPaths()))
})
library(shiny)
res <- tryCatch({
  app <- shinyAppDir("app")
  "SUCCESS"
}, error = function(e) {
  e$message
})
cat("shinyAppDir('app') result: ", res, "\n")

