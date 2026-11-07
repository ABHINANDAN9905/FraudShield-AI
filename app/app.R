# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Script: app/app.R
# ==============================================================================
# Description:
#   Primary application entry point. Enables launching via:
#     shiny::runApp("app")
#   or directly running inside RStudio Desktop.
# ==============================================================================

# Determine app directory structure
app_dir <- if (file.exists("global.R")) getwd() else if (file.exists("app/global.R")) file.path(getwd(), "app") else dirname(getwd())

# Source Global environment (Packages, Helpers, Data Loaders, 8 Modules)
source(file.path(app_dir, "global.R"), local = FALSE)

# Source User Interface & Reactive Server
source(file.path(app_dir, "ui.R"), local = FALSE)
source(file.path(app_dir, "server.R"), local = FALSE)

# Return Shiny App instance
shinyApp(ui = ui, server = server)
