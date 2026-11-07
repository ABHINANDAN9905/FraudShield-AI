# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Script: app/server.R
# ==============================================================================
# Description:
#   Main Server entry coordinating all 8 specialized dashboard modules.
# ==============================================================================

# Source global environment if not already loaded in current session
if (!exists("overview_server", mode = "function") || !exists("fetch_investigations", mode = "function")) {
  app_dir <- if (file.exists("global.R")) getwd() else if (file.exists("app/global.R")) file.path(getwd(), "app") else dirname(getwd())
  glob_file <- file.path(app_dir, "global.R")
  if (file.exists(glob_file)) {
    source(glob_file, local = FALSE)
  }
}

server <- function(input, output, session) {
  
  # Initialize SQLite database on session start if needed
  tryCatch({
    init_investigation_db()
  }, error = function(e) {
    message("Notice: SQLite initialization handled gracefully: ", e$message)
  })
  
  # Module 1: Executive Overview
  overview_server("overview")
  
  # Module 2: Transaction Explorer
  transaction_server("transaction")
  
  # Module 3: Fraud Analytics
  fraud_analytics_server("fraud_analytics")
  
  # Module 4: ML Model Performance
  model_performance_server("model_perf")
  
  # Module 5: Explainable AI
  explainability_server("explainability")
  
  # Module 6: Risk Scoring & Prediction
  prediction_server("prediction")
  
  # Module 7: Investigation Center
  investigation_server("investigation")
  
  # Module 8: Reports & Data Quality
  reports_server("reports")
}

