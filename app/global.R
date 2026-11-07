# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Script: app/global.R
# ==============================================================================
# Description:
#   Global environment definition for FraudShield Shiny Application.
#   Automatically sourced by Shiny prior to evaluating ui.R and server.R.
#   Loads all core packages, initializes database connection, sources helpers,
#   data loaders, and all 8 dashboard modules into the global session scope.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Environment & Library Path Resolution
# ------------------------------------------------------------------------------
local({
  curr <- getwd()
  cand1 <- file.path(curr, ".Rlibrary")
  cand2 <- file.path(dirname(curr), ".Rlibrary")
  proj_lib <- if (dir.exists(cand1)) cand1 else if (dir.exists(cand2)) cand2 else NULL
  if (!is.null(proj_lib) && !(proj_lib %in% .libPaths())) {
    .libPaths(c(proj_lib, .libPaths()))
  }
  
  # Configure isolated project cache to avoid permission errors
  root_dir <- if (file.exists("FraudShield.Rproj")) curr else dirname(curr)
  cache_dir <- file.path(root_dir, ".Rcache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(R_USER_CACHE_DIR = normalizePath(cache_dir, winslash = "/"))
  options(sass.cache = FALSE)
})

# ------------------------------------------------------------------------------
# 2. Package Imports
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(DT)
  library(shinyWidgets)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(RSQLite)
  library(DBI)
  library(ranger)
  library(xgboost)
  library(isotree)
  library(htmltools)
})

# ------------------------------------------------------------------------------
# 3. Path Resolution for Application Directory
# ------------------------------------------------------------------------------
get_app_dir <- function() {
  curr <- getwd()
  if (dir.exists(file.path(curr, "modules"))) {
    return(normalizePath(curr, winslash = "/"))
  } else if (dir.exists(file.path(curr, "app", "modules"))) {
    return(normalizePath(file.path(curr, "app"), winslash = "/"))
  } else if (dir.exists(file.path(dirname(curr), "modules"))) {
    return(normalizePath(dirname(curr), winslash = "/"))
  }
  return(normalizePath(curr, winslash = "/"))
}

app_dir <- get_app_dir()

# ------------------------------------------------------------------------------
# 4. Source Data Access Layer, ML Engine, XAI Engine, Prediction Engine & UI Helpers
# ------------------------------------------------------------------------------
source(file.path(app_dir, "R", "dashboard_data.R"), local = FALSE)
source(file.path(app_dir, "R", "ml_engine.R"), local = FALSE)
source(file.path(app_dir, "R", "xai_engine.R"), local = FALSE)
source(file.path(app_dir, "R", "prediction_engine.R"), local = FALSE)
source(file.path(app_dir, "R", "dashboard_helpers.R"), local = FALSE)

# ------------------------------------------------------------------------------
# 5. Source All 8 Dashboard Modules (UI & Server Functions)
# ------------------------------------------------------------------------------
source(file.path(app_dir, "modules", "overview_module.R"), local = FALSE)
source(file.path(app_dir, "modules", "transaction_module.R"), local = FALSE)
source(file.path(app_dir, "modules", "fraud_analytics_module.R"), local = FALSE)
source(file.path(app_dir, "modules", "model_performance_module.R"), local = FALSE)
source(file.path(app_dir, "modules", "explainability_module.R"), local = FALSE)
source(file.path(app_dir, "modules", "prediction_module.R"), local = FALSE)
source(file.path(app_dir, "modules", "investigation_module.R"), local = FALSE)
source(file.path(app_dir, "modules", "reports_module.R"), local = FALSE)

# ------------------------------------------------------------------------------
# 6. Global Persistence Initialization
# ------------------------------------------------------------------------------
tryCatch({
  init_investigation_db()
  init_prediction_audit_table()
}, error = function(e) {
  message("[FraudShield DB] SQLite pre-flight check notice: ", e$message)
})

