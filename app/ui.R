# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Script: app/ui.R
# ==============================================================================
# Description:
#   Main UI Definition for FraudShield using bslib enterprise theme,
#   branded dark navy navigation navbar, and modularized view containers.
# ==============================================================================

# Source global environment if not already loaded in current session
if (!exists("overview_ui", mode = "function") || !exists("create_kpi_card", mode = "function")) {
  app_dir <- if (file.exists("global.R")) getwd() else if (file.exists("app/global.R")) file.path(getwd(), "app") else dirname(getwd())
  glob_file <- file.path(app_dir, "global.R")
  if (file.exists(glob_file)) {
    source(glob_file, local = FALSE)
  }
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(DT)
  library(shinyWidgets)
})

# Custom bslib fintech theme
fraudshield_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#0066FF",
  secondary = "#64748B",
  success = "#16A34A",
  danger = "#DC2626",
  warning = "#D97706",
  info = "#0284C7",
  font_scale = 0.95
)

ui <- bslib::page_navbar(
  theme = fraudshield_theme,
  title = tags$div(
    style = "display: flex; align-items: center; gap: 10px; font-weight: 800; letter-spacing: -0.02em;",
    tags$span(style = "color: #38BDF8; font-size: 1.35rem;", icon("shield-alt")),
    tags$span(style = "color: #FFFFFF; font-size: 1.15rem;", "FraudShield"),
    tags$span(
      class = "badge",
      style = "background-color: rgba(56, 189, 248, 0.2); color: #38BDF8; font-size: 0.65rem; border: 1px solid rgba(56, 189, 248, 0.4); text-transform: uppercase;",
      "Risk Intelligence v1.0"
    )
  ),
  navbar_options = bslib::navbar_options(bg = "#0B192C"),
  id = "main_nav",
  fluid = TRUE,
  
  # Inject enterprise fintech styling overrides
  header = tags$head(
    tags$style(HTML("
      /* Body & Navbar Styling */
      body {
        background-color: #F8FAFC !important;
        color: #0F172A;
      }
      .navbar {
        border-bottom: 2px solid #1E3E62;
        padding-top: 10px;
        padding-bottom: 10px;
      }
      .navbar-nav .nav-link {
        font-weight: 600;
        font-size: 0.88rem;
        color: #94A3B8 !important;
        padding: 8px 14px !important;
        border-radius: 6px;
        transition: all 0.2s ease;
      }
      .navbar-nav .nav-link:hover {
        color: #FFFFFF !important;
        background-color: rgba(255, 255, 255, 0.05);
      }
      .navbar-nav .nav-link.active {
        color: #FFFFFF !important;
        background-color: #0066FF !important;
      }
      /* Card Overrides */
      .card {
        border: 1px solid #E2E8F0 !important;
        border-radius: 8px !important;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.04) !important;
        background-color: #FFFFFF !important;
      }
      .card-header {
        background-color: #FFFFFF !important;
        border-bottom: 1px solid #E2E8F0 !important;
        padding: 14px 18px !important;
        font-size: 0.95rem;
      }
      /* DataTable Polish */
      table.dataTable thead th {
        background-color: #F8FAFC !important;
        color: #475569 !important;
        font-weight: 700 !important;
        font-size: 0.80rem !important;
        text-transform: uppercase !important;
        letter-spacing: 0.04em !important;
        border-bottom: 2px solid #E2E8F0 !important;
      }
      table.dataTable tbody td {
        font-size: 0.85rem !important;
        vertical-align: middle !important;
      }
    "))
  ),
  
  # Page 1: Executive Overview
  bslib::nav_panel(
    title = tags$span(icon("tachometer-alt"), " Executive Overview"),
    value = "overview",
    overview_ui("overview")
  ),
  
  # Page 2: Transaction Explorer
  bslib::nav_panel(
    title = tags$span(icon("table"), " Transaction Explorer"),
    value = "transactions",
    transaction_ui("transaction")
  ),
  
  # Page 3: Fraud Analytics
  bslib::nav_panel(
    title = tags$span(icon("chart-line"), " Fraud Analytics"),
    value = "analytics",
    fraud_analytics_ui("fraud_analytics")
  ),
  
  # Page 4: ML Model Performance
  bslib::nav_panel(
    title = tags$span(icon("award"), " ML Model Performance"),
    value = "model_perf",
    model_performance_ui("model_perf")
  ),
  
  # Page 5: Explainable AI
  bslib::nav_panel(
    title = tags$span(icon("lightbulb"), " Explainable AI"),
    value = "explainability",
    explainability_ui("explainability")
  ),
  
  # Page 6: Risk Scoring & Prediction
  bslib::nav_panel(
    title = tags$span(icon("shield-alt"), " Risk Scoring & Prediction"),
    value = "prediction",
    prediction_ui("prediction")
  ),
  
  # Page 7: Investigation Center
  bslib::nav_panel(
    title = tags$span(icon("folder-open"), " Investigation Center"),
    value = "investigation",
    investigation_ui("investigation")
  ),
  
  # Page 8: Reports & Data Quality
  bslib::nav_panel(
    title = tags$span(icon("file-alt"), " Reports & Data Quality"),
    value = "reports",
    reports_ui("reports")
  ),
  
  # Footer disclaimer
  footer = tags$div(
    class = "text-center text-muted py-3 mt-4",
    style = "border-top: 1px solid #E2E8F0; font-size: 0.80rem;",
    "FraudShield™ Financial Fraud Detection & Risk Intelligence Platform • Pure R Shiny Implementation • Research & Simulation Environment"
  )
)
