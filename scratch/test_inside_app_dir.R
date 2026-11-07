local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib)) .libPaths(c(proj_lib, .libPaths()))
})
library(shiny)

# Change working directory into app/
orig_dir <- getwd()
setwd("app")
cat("Testing while getwd() is: ", getwd(), "\n")

res_global <- tryCatch({
  source("global.R")
  "GLOBAL_OK"
}, error = function(e) e$message)
cat("source('global.R'): ", res_global, "\n")

res_ui <- tryCatch({
  source("ui.R")
  "UI_OK"
}, error = function(e) e$message)
cat("source('ui.R'): ", res_ui, "\n")

res_server <- tryCatch({
  source("server.R")
  "SERVER_OK"
}, error = function(e) e$message)
cat("source('server.R'): ", res_server, "\n")

res_app <- tryCatch({
  app <- source("app.R")$value
  if (inherits(app, "shiny.appobj")) "APPOBJ_OK" else "FAIL"
}, error = function(e) e$message)
cat("source('app.R'): ", res_app, "\n")

setwd(orig_dir)
cat("Reset working directory back to: ", getwd(), "\n")

