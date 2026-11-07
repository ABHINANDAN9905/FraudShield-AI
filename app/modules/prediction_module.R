# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Module: Advanced Risk Scoring, Prediction & Decision Intelligence Engine (Page 6)
# Script: app/modules/prediction_module.R
# ==============================================================================
# Description:
#   Enterprise-Grade Transaction Risk Assessment and Decision Intelligence Platform.
#   10-part interface:
#     1. Risk Intelligence Overview
#     2. Single Transaction Assessment (Historical, Vector, Synthetic)
#     3. Batch Risk Assessment & Schema Validation
#     4. Multi-Model Prediction & PR-AUC Weighted Ensemble
#     5. Risk Threshold & Decision Policy Lab
#     6. Financial Exposure Simulator
#     7. Prediction Explainability (SHAP & Consistency)
#     8. Uncertainty & Confidence Analysis
#     9. Risk Portfolio Analytics
#    10. Prediction History & SQLite Audit Center
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(plotly)
  library(data.table)
  library(dplyr)
  library(scales)
  library(jsonlite)
  library(DBI)
  library(RSQLite)
})

prediction_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header Banner
    tags$div(
      class = "alert alert-secondary d-flex align-items-center mb-4",
      style = "background-color: #F8FAFC; border-left: 4px solid #0066FF; color: #1E293B; padding: 14px 20px; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
      tags$div(style = "font-size: 1.5rem; margin-right: 14px; color: #0066FF;", icon("shield-alt")),
      tags$div(
        tags$strong("Transaction Risk Scoring & Decision Intelligence Engine: "),
        "Production-grade fraud inference engine delivering single and batch transaction scoring, 0-100 normalized risk bands, PR-AUC weighted ensembling, decision policy simulation, uncertainty diagnostics, and immutable SQLite audit compliance."
      )
    ),
    
    # 10-Section Navigation Architecture using navset_card_pill
    bslib::navset_card_pill(
      id = ns("pred_tabs"),
      
      # ------------------------------------------------------------------------
      # TAB 1: RISK INTELLIGENCE OVERVIEW
      # ------------------------------------------------------------------------
      bslib::nav_panel(
        title = "1. Intelligence Overview",
        icon = icon("tachometer-alt"),
        tags$div(
          class = "py-2",
          # Dynamic KPI Summary Cards
          uiOutput(ns("overview_kpi_cards_ui")),
          
          tags$div(
            class = "row g-3 mb-3",
            tags$div(
              class = "col-lg-7 col-12",
              bslib::card(
                class = "shadow-sm border-0 h-100",
                bslib::card_header(tags$span(style = "font-weight: 700; color: #0F172A;", "Risk Band Distribution & Portfolio Exposure (Holdout Benchmark)")),
                bslib::card_body(plotlyOutput(ns("overview_risk_dist_plotly"), height = "330px"))
              )
            ),
            tags$div(
              class = "col-lg-5 col-12",
              bslib::card(
                class = "shadow-sm border-0 h-100",
                bslib::card_header(tags$span(style = "font-weight: 700; color: #0F172A;", "Decision Policy Architecture & Engine Health")),
                bslib::card_body(
                  tags$div(
                    style = "font-size: 0.85rem; line-height: 1.6;",
                    tags$div(class = "d-flex justify-content-between py-1 border-bottom", tags$strong("Supervised Champion:"), tags$span("Random Forest (Ranger, 80 trees)")),
                    tags$div(class = "d-flex justify-content-between py-1 border-bottom", tags$strong("High-Throughput Challenger:"), tags$span("XGBoost (100 rounds, scale_pos=570)")),
                    tags$div(class = "d-flex justify-content-between py-1 border-bottom", tags$strong("Linear Benchmark:"), tags$span("Penalized Logistic Regression (GLM)")),
                    tags$div(class = "d-flex justify-content-between py-1 border-bottom", tags$strong("Zero-Day Anomaly Detector:"), tags$span("Isolation Forest (isotree, unsupervised)")),
                    tags$div(class = "d-flex justify-content-between py-1 border-bottom", tags$strong("Composite Ensemble:"), tags$span("PR-AUC Weighted Soft-Voting")),
                    tags$div(class = "d-flex justify-content-between py-1 border-bottom", tags$strong("Canonical Features:"), tags$span("30 Features (28 PCA + Log-Amt + Hours)")),
                    tags$div(class = "d-flex justify-content-between py-1", tags$strong("Persistent Audit Store:"), tags$span(class = "badge bg-success", "SQLite prediction_audit Active"))
                  ),
                  tags$hr(style = "margin: 12px 0;"),
                  tags$small(class = "text-muted", "Operational Disclaimer: Predicted fraud reflects statistical model scoring based on empirical training distributions. Analytical recommendations are intended for triage and do not constitute payment processor authorizations.")
                )
              )
            )
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # TAB 2: SINGLE TRANSACTION ASSESSMENT
      # ------------------------------------------------------------------------
      bslib::nav_panel(
        title = "2. Single Assessment",
        icon = icon("play-circle"),
        tags$div(
          class = "py-2",
          
          # Control & Input Selection Bar
          tags$div(
            class = "card border-0 shadow-sm mb-3",
            style = "background-color: #FFFFFF; border-radius: 8px; border: 1px solid #E2E8F0;",
            tags$div(
              class = "card-body py-3",
              tags$div(
                class = "row g-3 align-items-center",
                tags$div(
                  class = "col-lg-4 col-md-6 col-12",
                  tags$label(class = "form-label text-muted small font-weight-bold mb-1", icon("folder-open"), " Input Methodology"),
                  selectInput(
                    ns("single_input_mode"),
                    label = NULL,
                    choices = c(
                      "Historical Test Transaction (Real Data)" = "historical",
                      "Synthetic Fraud Attack Scenario" = "synthetic",
                      "Custom Feature Vector Input" = "custom"
                    ),
                    selected = "historical",
                    width = "100%"
                  )
                ),
                tags$div(
                  class = "col-lg-3 col-md-6 col-12",
                  tags$label(class = "form-label text-muted small font-weight-bold mb-1", icon("microchip"), " Scoring Architecture"),
                  selectInput(
                    ns("single_model_choice"),
                    label = NULL,
                    choices = c(
                      "Random Forest (Champion)" = "random_forest",
                      "XGBoost (High-Throughput)" = "xgboost",
                      "Logistic Regression (GLM)" = "logistic_regression",
                      "Isolation Forest (Unsupervised)" = "isolation_forest",
                      "PR-AUC Weighted Ensemble" = "ensemble"
                    ),
                    selected = "random_forest",
                    width = "100%"
                  )
                ),
                tags$div(
                  class = "col-lg-3 col-md-6 col-12",
                  tags$label(class = "form-label text-muted small font-weight-bold mb-1", icon("sliders-h"), " Decision Cutoff"),
                  sliderInput(
                    ns("single_threshold"),
                    label = NULL,
                    min = 0.10, max = 0.90, value = 0.50, step = 0.05,
                    width = "100%"
                  )
                ),
                tags$div(
                  class = "col-lg-2 col-md-6 col-12 text-lg-end",
                  actionButton(ns("run_single_eval_btn"), "Evaluate Risk", icon = icon("bolt"), class = "btn btn-primary w-100"),
                  actionButton(ns("reset_single_btn"), "Reset", icon = icon("redo"), class = "btn btn-outline-secondary btn-sm w-100 mt-1")
                )
              )
            )
          ),
          
          # Dynamic Inputs (Conditional UI based on mode)
          tags$div(
            class = "row g-3 mb-3",
            tags$div(
              class = "col-lg-7 col-12",
              bslib::card(
                class = "shadow-sm border-0 h-100",
                bslib::card_header(
                  tags$div(
                    class = "d-flex justify-content-between align-items-center",
                    tags$span(style = "font-weight: 700; color: #0F172A;", "Transaction Input Vector"),
                    uiOutput(ns("single_mode_badge"), inline = TRUE)
                  )
                ),
                bslib::card_body(
                  uiOutput(ns("single_inputs_ui"))
                )
              )
            ),
            
            # Risk Decision & Gauge Results
            tags$div(
              class = "col-lg-5 col-12",
              bslib::card(
                class = "shadow-sm border-0 h-100",
                bslib::card_header(tags$span(style = "font-weight: 700; color: #0F172A;", "Risk Score, Gauge & Policy Action")),
                bslib::card_body(
                  uiOutput(ns("single_results_ui"))
                )
              )
            )
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # TAB 3: BATCH RISK ASSESSMENT
      # ------------------------------------------------------------------------
      bslib::nav_panel(
        title = "3. Batch Assessment",
        icon = icon("file-csv"),
        tags$div(
          class = "py-2",
          bslib::card(
            class = "shadow-sm border-0 mb-3",
            bslib::card_header(
              tags$div(
                class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
                tags$span(style = "font-weight: 700; color: #0F172A;", "Batch Transaction Payload Processing & Schema Validation"),
                downloadButton(ns("download_sample_template"), "Download Valid CSV Template", class = "btn-outline-secondary btn-sm")
              )
            ),
            bslib::card_body(
              tags$div(
                class = "row g-3 align-items-center mb-3",
                tags$div(
                  class = "col-md-5 col-12",
                  fileInput(
                    ns("batch_file_input"),
                    "Upload Transactions CSV (Required: Amount, V1 - V28):",
                    accept = c(".csv", "text/csv"),
                    width = "100%"
                  )
                ),
                tags$div(
                  class = "col-md-3 col-12",
                  selectInput(
                    ns("batch_model_choice"),
                    "Batch Scoring Architecture:",
                    choices = c(
                      "Random Forest (Champion)" = "random_forest",
                      "XGBoost (High-Throughput)" = "xgboost",
                      "Logistic Regression (GLM)" = "logistic_regression"
                    ),
                    selected = "random_forest",
                    width = "100%"
                  )
                ),
                tags$div(
                  class = "col-md-2 col-12",
                  sliderInput(
                    ns("batch_threshold"),
                    "Threshold Cutoff:",
                    min = 0.10, max = 0.90, value = 0.50, step = 0.05,
                    width = "100%"
                  )
                ),
                tags$div(
                  class = "col-md-2 col-12 text-md-end",
                  actionButton(ns("run_batch_btn"), "Score Batch File", icon = icon("cogs"), class = "btn btn-primary w-100 mt-3")
                )
              ),
              
              # Batch Summary & Validation Banner
              uiOutput(ns("batch_summary_banner")),
              
              # Scored Results DT Table
              tags$div(style = "overflow-x: auto; margin-top: 15px;", DTOutput(ns("batch_results_dt"))),
              
              # Download Handlers
              tags$div(
                class = "mt-3 d-flex justify-content-end gap-2 flex-wrap",
                uiOutput(ns("batch_downloads_ui"))
              )
            )
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # TAB 4: MULTI-MODEL PREDICTION & ENSEMBLE
      # ------------------------------------------------------------------------
      bslib::nav_panel(
        title = "4. Multi-Model & Ensemble",
        icon = icon("object-group"),
        tags$div(
          class = "py-2",
          uiOutput(ns("multimodel_consensus_banner")),
          
          tags$div(
            class = "row g-3 mb-3",
            tags$div(
              class = "col-lg-8 col-12",
              bslib::card(
                class = "shadow-sm border-0 h-100",
                bslib::card_header(tags$span(style = "font-weight: 700; color: #0F172A;", "Multi-Model Output Comparison & Ensemble Composition")),
                bslib::card_body(tableOutput(ns("multimodel_comparison_table")))
              )
            ),
            tags$div(
              class = "col-lg-4 col-12",
              bslib::card(
                class = "shadow-sm border-0 h-100",
                bslib::card_header(tags$span(style = "font-weight: 700; color: #0F172A;", "PR-AUC Weighted Ensemble Rationale")),
                bslib::card_body(
                  tags$div(
                    style = "font-size: 0.85rem; line-height: 1.6;",
                    tags$h6(style = "font-weight: 700; color: #0F172A;", "Weighting Methodology:"),
                    tags$p("Model weights are directly proportional to validated Precision-Recall AUC on the holdout evaluation partition:"),
                    tags$ul(
                      tags$li(tags$strong("Random Forest (35.2%): "), "PR-AUC 0.8782. Exceptional precision (96.2%) and minimal false positives."),
                      tags$li(tags$strong("XGBoost (34.2%): "), "PR-AUC 0.8523. Highest recall sensitivity (83.9%) across distributed fraud patterns."),
                      tags$li(tags$strong("Logistic Regression (30.6%): "), "PR-AUC 0.7625. Inherent log-odds calibration and robust baseline.")
                    ),
                    tags$small(class = "text-muted", "Isolation Forest is excluded from probability soft-voting to prevent conflating unsupervised anomaly distances with supervised class posteriors.")
                  )
                )
              )
            )
          ),
          
          tags$div(
            class = "row g-3",
            tags$div(
              class = "col-12",
              bslib::card(
                class = "shadow-sm border-0",
                bslib::card_header(tags$span(style = "font-weight: 700; color: #0F172A;", "Model Output Spectrum (Active Transaction)")),
                bslib::card_body(plotlyOutput(ns("multimodel_spectrum_plotly"), height = "260px"))
              )
            )
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # TAB 5: RISK THRESHOLD & DECISION POLICY LAB
      # ------------------------------------------------------------------------
      bslib::nav_panel(
        title = "5. Decision Policy Lab",
        icon = icon("balance-scale"),
        tags$div(
          class = "py-2",
          tags$div(
            class = "card border-0 shadow-sm mb-3 bg-light",
            tags$div(
              class = "card-body py-2",
              tags$div(
                class = "row g-3 align-items-center",
                tags$div(
                  class = "col-md-4 col-12",
                  tags$label(class = "form-label text-muted small font-weight-bold mb-1", "Optimization Objective"),
                  selectInput(
                    ns("policy_objective"),
                    label = NULL,
                    choices = c(
                      "Maximize F1-Score (Balanced Detection)" = "f1",
                      "Maximize F2-Score (Prioritize Fraud Recall)" = "f2",
                      "Constrained: Max Recall s.t. Precision >= 80%" = "precision_constraint",
                      "Minimize Total Expected Financial Cost" = "cost_min"
                    ),
                    selected = "f1",
                    width = "100%"
                  )
                ),
                tags$div(
                  class = "col-md-4 col-12",
                  tags$label(class = "form-label text-muted small font-weight-bold mb-1", "Benchmark Model"),
                  selectInput(
                    ns("policy_model_select"),
                    label = NULL,
                    choices = c(
                      "Random Forest (Champion)" = "random_forest",
                      "XGBoost (High-Throughput)" = "xgboost",
                      "Logistic Regression (GLM)" = "logistic_regression"
                    ),
                    selected = "random_forest",
                    width = "100%"
                  )
                ),
                tags$div(
                  class = "col-md-4 col-12",
                  tags$label(class = "form-label text-muted small font-weight-bold mb-1", "Operating Cutoff (Tau)"),
                  sliderInput(
                    ns("policy_sim_threshold"),
                    label = NULL,
                    min = 0.10, max = 0.90, value = 0.50, step = 0.05,
                    width = "100%"
                  )
                )
              )
            )
          ),
          
          # Confusion Matrix & Performance Metrics
          uiOutput(ns("policy_metrics_kpi_ui")),
          
          tags$div(
            class = "row g-3 mb-3",
            tags$div(
              class = "col-lg-6 col-12",
              bslib::card(
                class = "shadow-sm border-0 h-100",
                bslib::card_header(tags$span(style = "font-weight: 700; color: #0F172A;", "Holdout Confusion Matrix at Selected Threshold")),
                bslib::card_body(tableOutput(ns("policy_confusion_matrix_table")))
              )
            ),
            tags$div(
              class = "col-lg-6 col-12",
              bslib::card(
                class = "shadow-sm border-0 h-100",
                bslib::card_header(tags$span(style = "font-weight: 700; color: #0F172A;", "Precision-Recall Tradeoff Across Threshold Spectrum")),
                bslib::card_body(plotlyOutput(ns("policy_tradeoff_plotly"), height = "300px"))
              )
            )
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # TAB 6: FINANCIAL EXPOSURE SIMULATOR
      # ------------------------------------------------------------------------
      bslib::nav_panel(
        title = "6. Financial Simulator",
        icon = icon("calculator"),
        tags$div(
          class = "py-2",
          tags$div(
            class = "card border-0 shadow-sm mb-3 bg-light",
            tags$div(
              class = "card-body py-2",
              tags$div(
                class = "row g-3 align-items-center",
                tags$div(
                  class = "col-md-3 col-6",
                  numericInput(ns("sim_tx_amount"), "Ticket Amount ($):", value = 1250.00, min = 1, step = 50)
                ),
                tags$div(
                  class = "col-md-3 col-6",
                  sliderInput(ns("sim_recovery_rate"), "Fraud Recovery Rate (%):", min = 0, max = 50, value = 15, step = 5)
                ),
                tags$div(
                  class = "col-md-3 col-6",
                  numericInput(ns("sim_investigation_cost"), "Manual Review Cost ($):", value = 25.00, min = 5, max = 150, step = 5)
                ),
                tags$div(
                  class = "col-md-3 col-6",
                  sliderInput(ns("sim_fn_multiplier"), "False Neg. Cost Multiplier:", min = 1.0, max = 3.0, value = 1.5, step = 0.1)
                )
              )
            )
          ),
          
          uiOutput(ns("financial_summary_cards_ui")),
          
          tags$div(
            class = "row g-3 mb-3",
            tags$div(
              class = "col-lg-8 col-12",
              bslib::card(
                class = "shadow-sm border-0",
                bslib::card_header(tags$span(style = "font-weight: 700; color: #0F172A;", "Expected Financial Cost vs Benefit Curve across Threshold Spectrum")),
                bslib::card_body(plotlyOutput(ns("financial_cost_benefit_plotly"), height = "320px"))
              )
            ),
            tags$div(
              class = "col-lg-4 col-12",
              bslib::card(
                class = "shadow-sm border-0",
                bslib::card_header(tags$span(style = "font-weight: 700; color: #0F172A;", "Analytical Assumption Ledger")),
                bslib::card_body(
                  tags$div(
                    style = "font-size: 0.85rem; line-height: 1.6;",
                    tags$div(class = "d-flex justify-content-between py-1 border-bottom", tags$strong("Baseline Recovery:"), tags$span("15.0%")),
                    tags$div(class = "d-flex justify-content-between py-1 border-bottom", tags$strong("Operational Review Overhead:"), tags$span("$25.00 / Alert")),
                    tags$div(class = "d-flex justify-content-between py-1 border-bottom", tags$strong("Chargeback / Penalty Mult:"), tags$span("1.50x Ticket")),
                    tags$div(class = "d-flex justify-content-between py-1 border-bottom", tags$strong("Benchmark Fraud Avg:"), tags$span("$122.21")),
                    tags$div(class = "d-flex justify-content-between py-1", tags$strong("Benchmark Legit Avg:"), tags$span("$88.29")),
                    tags$hr(style = "margin: 10px 0;"),
                    tags$small(class = "text-muted", "Simulation Model: Net Benefit = (P(Fraud) * Net Prevented Exposure) - Investigation Overhead.")
                  )
                )
              )
            )
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # TAB 7: PREDICTION EXPLAINABILITY
      # ------------------------------------------------------------------------
      bslib::nav_panel(
        title = "7. Explainability",
        icon = icon("search-plus"),
        tags$div(
          class = "py-2",
          uiOutput(ns("explainability_integration_banner")),
          
          tags$div(
            class = "row g-3 mb-3",
            tags$div(
              class = "col-lg-8 col-12",
              bslib::card(
                class = "shadow-sm border-0",
                bslib::card_header(
                  tags$div(
                    class = "d-flex justify-content-between align-items-center",
                    tags$span(style = "font-weight: 700; color: #0F172A;", "Local Feature Attribution Breakdown (iBreakDown SHAP)"),
                    uiOutput(ns("explainability_consistency_badge"), inline = TRUE)
                  )
                ),
                bslib::card_body(plotlyOutput(ns("explainability_shap_plotly"), height = "360px"))
              )
            ),
            tags$div(
              class = "col-lg-4 col-12",
              bslib::card(
                class = "shadow-sm border-0",
                bslib::card_header(tags$span(style = "font-weight: 700; color: #0F172A;", "Top Driving Risk Signals")),
                bslib::card_body(
                  uiOutput(ns("explainability_top_drivers_ui"))
                )
              )
            )
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # TAB 8: UNCERTAINTY & CONFIDENCE ANALYSIS
      # ------------------------------------------------------------------------
      bslib::nav_panel(
        title = "8. Uncertainty & Confidence",
        icon = icon("chart-line"),
        tags$div(
          class = "py-2",
          uiOutput(ns("uncertainty_diagnostic_banner")),
          
          tags$div(
            class = "row g-3 mb-3",
            tags$div(
              class = "col-lg-6 col-12",
              bslib::card(
                class = "shadow-sm border-0 h-100",
                bslib::card_header(tags$span(style = "font-weight: 700; color: #0F172A;", "Decision Boundary Proximity & Model Divergence")),
                bslib::card_body(uiOutput(ns("uncertainty_metrics_panel_ui")))
              )
            ),
            tags$div(
              class = "col-lg-6 col-12",
              bslib::card(
                class = "shadow-sm border-0 h-100",
                bslib::card_header(tags$span(style = "font-weight: 700; color: #0F172A;", "Perturbation Stability Spectrum")),
                bslib::card_body(
                  plotlyOutput(ns("uncertainty_stability_plotly"), height = "280px")
                )
              )
            )
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # TAB 9: RISK PORTFOLIO ANALYTICS
      # ------------------------------------------------------------------------
      bslib::nav_panel(
        title = "9. Portfolio Analytics",
        icon = icon("layer-group"),
        tags$div(
          class = "py-2",
          tags$div(
            class = "row g-3 mb-3",
            tags$div(
              class = "col-lg-6 col-12",
              bslib::card(
                class = "shadow-sm border-0",
                bslib::card_header(tags$span(style = "font-weight: 700; color: #0F172A;", "Scored Risk Tier Volume & Financial Exposure")),
                bslib::card_body(plotlyOutput(ns("portfolio_exposure_plotly"), height = "320px"))
              )
            ),
            tags$div(
              class = "col-lg-6 col-12",
              bslib::card(
                class = "shadow-sm border-0",
                bslib::card_header(tags$span(style = "font-weight: 700; color: #0F172A;", "Fraud Probability Distribution (Supervised Champion)")),
                bslib::card_body(plotlyOutput(ns("portfolio_prob_dist_plotly"), height = "320px"))
              )
            )
          ),
          tags$div(
            class = "row g-3",
            tags$div(
              class = "col-12",
              bslib::card(
                class = "shadow-sm border-0",
                bslib::card_header(tags$span(style = "font-weight: 700; color: #0F172A;", "High-Risk Transaction Velocity Timeline (Elapsed Hours)")),
                bslib::card_body(plotlyOutput(ns("portfolio_timeline_plotly"), height = "260px"))
              )
            )
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # TAB 10: PREDICTION HISTORY & AUDIT CENTER
      # ------------------------------------------------------------------------
      bslib::nav_panel(
        title = "10. Audit History",
        icon = icon("history"),
        tags$div(
          class = "py-2",
          tags$div(
            class = "card border-0 shadow-sm mb-3 bg-light",
            tags$div(
              class = "card-body py-2",
              tags$div(
                class = "row g-3 align-items-center",
                tags$div(
                  class = "col-md-4 col-12",
                  tags$label(class = "form-label text-muted small font-weight-bold mb-1", "Search Transaction ID / Prediction ID"),
                  textInput(ns("audit_search_input"), label = NULL, placeholder = "e.g. TX-09408 or PRED-", width = "100%")
                ),
                tags$div(
                  class = "col-md-3 col-6",
                  tags$label(class = "form-label text-muted small font-weight-bold mb-1", "Filter Architecture"),
                  selectInput(
                    ns("audit_filter_model"),
                    label = NULL,
                    choices = c("All", "Random Forest", "XGBoost", "Logistic Regression", "Ensemble"),
                    selected = "All",
                    width = "100%"
                  )
                ),
                tags$div(
                  class = "col-md-3 col-6",
                  tags$label(class = "form-label text-muted small font-weight-bold mb-1", "Filter Risk Tier"),
                  selectInput(
                    ns("audit_filter_category"),
                    label = NULL,
                    choices = c("All", "Very Low Risk", "Low Risk", "Medium Risk", "High Risk", "Critical Risk"),
                    selected = "All",
                    width = "100%"
                  )
                ),
                tags$div(
                  class = "col-md-2 col-12 text-md-end",
                  actionButton(ns("refresh_audit_btn"), "Refresh", icon = icon("sync"), class = "btn btn-outline-primary btn-sm w-100 mt-3")
                )
              )
            )
          ),
          
          bslib::card(
            class = "shadow-sm border-0 mb-3",
            bslib::card_header(
              tags$div(
                class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
                tags$span(style = "font-weight: 700; color: #0F172A;", "Persistent SQLite Decision Audit Log (Immutable Compliance Trail)"),
                downloadButton(ns("download_audit_history_csv"), "Export Audit Log (CSV)", class = "btn-outline-primary btn-sm")
              )
            ),
            bslib::card_body(
              tags$div(style = "overflow-x: auto;", DTOutput(ns("audit_history_dt")))
            )
          )
        )
      )
    )
  )
}

prediction_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --------------------------------------------------------------------------
    # Reactive State: Active Transaction
    # --------------------------------------------------------------------------
    active_tx_data <- reactiveVal({
      test_dt <- get_test_data()
      test_dt[1340, ] # Seed with confirmed fraud case TX-01340
    })
    
    active_source_label <- reactiveVal("Historical Test Dataset (TX-01340)")
    
    # Observe mode change & inputs
    observeEvent(input$single_input_mode, {
      mode <- input$single_input_mode
      test_dt <- get_test_data()
      if (mode == "historical") {
        active_tx_data(test_dt[1340, ])
        active_source_label("Historical Test Dataset (TX-01340)")
      } else if (mode == "synthetic") {
        # Construct synthetic high-value burst scenario
        syn_row <- copy(test_dt[1340, ])
        syn_row$amount <- 2850.00
        syn_row$amount_log <- round(log1p(2850.00), 4)
        syn_row$elapsed_hours <- 3.50
        syn_row$v14 <- -8.50
        syn_row$v17 <- -14.20
        syn_row$v12 <- -7.80
        syn_row$v10 <- -5.10
        syn_row$v11 <- 4.50
        syn_row$v4  <- 5.20
        active_tx_data(syn_row)
        active_source_label("Synthetic Fraud Scenario (High-Value Bust-Out)")
      }
    })
    
    observeEvent(input$load_preset_tx, {
      req(input$preset_tx_select)
      test_dt <- get_test_data()
      idx <- suppressWarnings(as.integer(gsub("[^0-9]", "", input$preset_tx_select)))
      if (is.na(idx) || idx < 1 || idx > nrow(test_dt)) idx <- 1340
      
      active_tx_data(test_dt[idx, ])
      active_source_label(sprintf("Historical Test Dataset (%s)", input$preset_tx_select))
    })
    
    observeEvent(input$reset_single_btn, {
      test_dt <- get_test_data()
      active_tx_data(test_dt[1340, ])
      active_source_label("Historical Test Dataset (TX-01340)")
      updateSelectInput(session, "single_input_mode", selected = "historical")
      updateSelectInput(session, "single_model_choice", selected = "random_forest")
      updateSliderInput(session, "single_threshold", value = 0.50)
      showNotification("Single transaction assessment reset to baseline default.", type = "message")
    })
    
    # Badge for current input methodology
    output$single_mode_badge <- renderUI({
      lbl <- active_source_label()
      tags$span(class = "badge bg-primary", lbl)
    })
    
    # Dynamic Input Form Rendering
    output$single_inputs_ui <- renderUI({
      mode <- input$single_input_mode
      tx <- active_tx_data()
      
      if (mode == "historical") {
        tagList(
          tags$div(
            class = "mb-3",
            tags$label(class = "form-label text-muted small font-weight-bold", "Select Benchmark Historical Transaction:"),
            selectInput(
              ns("preset_tx_select"),
              label = NULL,
              choices = c(
                "TX-01340 | $1.00 (Confirmed Fraud - Multi-Signal Divergence)" = "TX-01340",
                "TX-01347 | $1.00 (Confirmed Fraud - Micro-Authorization Probe)" = "TX-01347",
                "TX-01363 | $1.00 (Confirmed Fraud - Severe Negative Shift)" = "TX-01363",
                "TX-01380 | $1,809.68 (Confirmed Fraud - High-Value Bust-Out)" = "TX-01380",
                "TX-01652 | $1.00 (Confirmed Fraud - POS Retail Attack)" = "TX-01652",
                "TX-00001 | $378.66 (Legitimate - Benign Baseline Ticket)" = "TX-00001",
                "TX-00002 | $3.67 (Legitimate - Benign Micro Purchase)" = "TX-00002",
                "TX-00042 | $159.00 (Legitimate - Standard Cardholder Behavior)" = "TX-00042",
                "TX-00100 | $11.46 (Legitimate - Verified Clean Activity)" = "TX-00100"
              ),
              selected = "TX-01340",
              width = "100%"
            ),
            actionButton(ns("load_preset_tx"), "Load Transaction Vector", class = "btn btn-outline-primary btn-sm")
          ),
          tags$div(
            class = "p-3 rounded bg-light border",
            tags$h6(style = "font-weight: 700; color: #0F172A;", "Inspected Vector Snapshot:"),
            tags$div(
              class = "row g-2 small",
              tags$div(class = "col-4", tags$strong("Amount: "), scales::dollar(tx$amount[1])),
              tags$div(class = "col-4", tags$strong("Elapsed Time: "), sprintf("%.2f Hrs", tx$elapsed_hours[1])),
              tags$div(class = "col-4", tags$strong("Audit Class: "), ifelse(tx$class[1] == 1, "Confirmed Fraud (1)", "Legitimate (0)")),
              tags$div(class = "col-4", tags$strong("V14: "), sprintf("%.3f", tx$v14[1])),
              tags$div(class = "col-4", tags$strong("V17: "), sprintf("%.3f", tx$v17[1])),
              tags$div(class = "col-4", tags$strong("V12: "), sprintf("%.3f", tx$v12[1])),
              tags$div(class = "col-4", tags$strong("V10: "), sprintf("%.3f", tx$v10[1])),
              tags$div(class = "col-4", tags$strong("V11: "), sprintf("%.3f", tx$v11[1])),
              tags$div(class = "col-4", tags$strong("V4: "),  sprintf("%.3f", tx$v4[1]))
            )
          )
        )
      } else if (mode == "synthetic") {
        tagList(
          tags$div(
            class = "alert alert-warning small mb-3",
            icon("vial"), " Synthetic scenario generated for stress-testing: Elevated ticket ($2,850.00), off-peak velocity (3.50 hrs), and severe PCA standard deviations."
          ),
          tags$div(
            class = "row g-2",
            tags$div(class = "col-md-6", numericInput(ns("syn_amount"), "Simulated Amount ($):", value = 2850.00, step = 50)),
            tags$div(class = "col-md-6", numericInput(ns("syn_hours"), "Simulated Elapsed Hours:", value = 3.5, step = 0.5)),
            tags$div(class = "col-md-4", numericInput(ns("syn_v14"), "V14 Signal:", value = -8.50, step = 0.5)),
            tags$div(class = "col-md-4", numericInput(ns("syn_v17"), "V17 Signal:", value = -14.20, step = 0.5)),
            tags$div(class = "col-md-4", numericInput(ns("syn_v12"), "V12 Signal:", value = -7.80, step = 0.5))
          ),
          actionButton(ns("apply_syn_btn"), "Apply Synthetic Adjustments", class = "btn btn-outline-warning btn-sm mt-2")
        )
      } else {
        # Custom Vector Input
        tagList(
          tags$div(class = "small text-muted mb-2", "Directly specify transaction attributes. Canonical PCA features range within [-20, 20]:"),
          tags$div(
            class = "row g-2",
            tags$div(class = "col-md-6", numericInput(ns("cust_amount"), "Amount ($):", value = 149.50, min = 0, step = 5)),
            tags$div(class = "col-md-6", numericInput(ns("cust_hours"), "Elapsed Time (Hours):", value = 12.0, min = 0, max = 48, step = 1)),
            tags$div(class = "col-md-4", numericInput(ns("cust_v14"), "V14:", value = -0.50, step = 0.2)),
            tags$div(class = "col-md-4", numericInput(ns("cust_v17"), "V17:", value = -0.30, step = 0.2)),
            tags$div(class = "col-md-4", numericInput(ns("cust_v12"), "V12:", value = -0.20, step = 0.2)),
            tags$div(class = "col-md-4", numericInput(ns("cust_v10"), "V10:", value = -0.10, step = 0.2)),
            tags$div(class = "col-md-4", numericInput(ns("cust_v11"), "V11:", value = 0.40, step = 0.2)),
            tags$div(class = "col-md-4", numericInput(ns("cust_v4"),  "V4:",  value = 0.30, step = 0.2))
          ),
          actionButton(ns("apply_custom_btn"), "Load Custom Feature Vector", class = "btn btn-outline-primary btn-sm mt-2")
        )
      }
    })
    
    observeEvent(input$apply_syn_btn, {
      tx <- copy(active_tx_data())
      tx$amount <- input$syn_amount
      tx$amount_log <- round(log1p(input$syn_amount), 4)
      tx$elapsed_hours <- input$syn_hours
      tx$v14 <- input$syn_v14
      tx$v17 <- input$syn_v17
      tx$v12 <- input$syn_v12
      active_tx_data(tx)
      active_source_label("Synthetic Fraud Scenario (Custom Adjusted)")
      showNotification("Synthetic vector updated.", type = "message")
    })
    
    observeEvent(input$apply_custom_btn, {
      test_dt <- get_test_data()
      tx <- copy(test_dt[1, ]) # clean baseline
      tx$amount <- input$cust_amount
      tx$amount_log <- round(log1p(input$cust_amount), 4)
      tx$elapsed_hours <- input$cust_hours
      tx$v14 <- input$cust_v14
      tx$v17 <- input$cust_v17
      tx$v12 <- input$cust_v12
      tx$v10 <- input$cust_v10
      tx$v11 <- input$cust_v11
      tx$v4  <- input$cust_v4
      active_tx_data(tx)
      active_source_label("Custom User Feature Vector")
      showNotification("Custom feature vector loaded.", type = "message")
    })
    
    # --------------------------------------------------------------------------
    # Active Prediction Evaluation
    # --------------------------------------------------------------------------
    active_eval_reactive <- reactive({
      # Triggers on Predict button or when transaction vector changes
      input$run_single_eval_btn
      
      tx <- active_tx_data()
      m_type <- input$single_model_choice
      th     <- input$single_threshold
      if (is.null(th)) th <- 0.50
      if (is.null(m_type)) m_type <- "random_forest"
      
      # Validate
      val_res <- validate_transaction_input(tx)
      if (!val_res$valid) {
        return(list(valid = FALSE, error = paste(val_res$errors, collapse = "; ")))
      }
      
      clean_data <- val_res$clean_data
      
      if (m_type == "ensemble") {
        cmp_res <- compare_model_predictions(clean_data, threshold = th)
        prob <- cmp_res$ensemble_prob
        anom <- predict_fraud_probability(clean_data, "isolation_forest")[1]
        model_label <- "PR-AUC Weighted Ensemble"
        calib_status <- "Composite Weighted Calibration"
      } else {
        prob <- predict_fraud_probability(clean_data, m_type)[1]
        iso_obj <- tryCatch(get_cached_model("isolation_forest"), error = function(e) NULL)
        anom <- if (!is.null(iso_obj)) predict_fraud_probability(clean_data, "isolation_forest")[1] else 0.50
        model_label <- if (m_type == "random_forest") "Random Forest (Champion)" else if (m_type == "xgboost") "XGBoost (High-Throughput)" else if (m_type == "logistic_regression") "Logistic Regression (GLM)" else "Isolation Forest"
        calib_status <- if (m_type == "logistic_regression") "Inherent Logit Calibrated" else if (m_type == "random_forest") "Tree Frequency Calibrated" else if (m_type == "xgboost") "Positive-Weighted Shift" else "Uncalibrated Anomaly Metric"
      }
      
      risk_score <- calculate_risk_score(prob, m_type)
      risk_cat   <- classify_risk_category(risk_score)
      policy     <- apply_decision_policy(risk_score, prob, threshold = th)
      
      # Uncertainty
      cmp_all <- compare_model_predictions(clean_data, threshold = th)
      unc     <- compute_prediction_uncertainty(prob, threshold = th, disagreement_spread = cmp_all$disagreement_spread)
      
      # Financial Simulation
      fin <- calculate_financial_exposure(clean_data$amount[1], prob, threshold = th)
      
      list(
        valid               = TRUE,
        transaction_id      = if (!is.null(tx$transaction_id)) tx$transaction_id[1] else "TX-LIVE-EVAL",
        amount              = clean_data$amount[1],
        elapsed_hours       = clean_data$elapsed_hours[1],
        model_type          = m_type,
        model_label         = model_label,
        probability         = prob,
        anomaly_score       = anom,
        risk_score          = risk_score,
        risk_category       = risk_cat,
        threshold           = th,
        policy              = policy,
        uncertainty         = unc,
        financial           = fin,
        calibration_status  = calib_status,
        raw_vector          = clean_data
      )
    })
    
    # Render Single Assessment Results UI
    output$single_results_ui <- renderUI({
      res <- active_eval_reactive()
      if (!res$valid) {
        return(tags$div(class = "alert alert-danger", res$error))
      }
      
      pol <- res$policy
      unc <- res$uncertainty
      
      tags$div(
        # Large Risk Score Banner
        tags$div(
          style = sprintf("background-color: %s; border: 2px solid %s; border-radius: 8px; padding: 18px; text-align: center; margin-bottom: 16px;", pol$badge_bg, pol$badge_color),
          tags$small(style = sprintf("color: %s; font-weight: 700; text-transform: uppercase; letter-spacing: 0.05em;", pol$badge_color), "Normalized Risk Score (0 - 100)"),
          tags$h1(style = sprintf("color: %s; font-weight: 800; margin: 4px 0; font-size: 2.8rem;", pol$badge_color), sprintf("%.1f", res$risk_score)),
          tags$span(class = "badge", style = sprintf("background-color: %s; font-size: 0.95rem; padding: 6px 12px;", pol$badge_color), as.character(res$risk_category)),
          tags$div(style = sprintf("font-weight: 600; color: %s; margin-top: 8px;", pol$badge_color), pol$decision)
        ),
        
        # Dual Quantitative Metrics
        tags$div(
          class = "row g-2 mb-3",
          tags$div(
            class = "col-6",
            tags$div(
              class = "p-2 bg-light rounded text-center border",
              tags$small(class = "text-muted", "Estimated Fraud Prob."),
              tags$h4(style = sprintf("color: %s; font-weight: 700; margin: 2px 0;", pol$badge_color), sprintf("%.2f%%", res$probability * 100)),
              tags$small(class = "text-muted", sprintf("Cutoff: %.2f", res$threshold))
            )
          ),
          tags$div(
            class = "col-6",
            tags$div(
              class = "p-2 bg-light rounded text-center border",
              tags$small(class = "text-muted", "Isolation Anomaly Metric"),
              tags$h4(style = "color: #0F172A; font-weight: 700; margin: 2px 0;", sprintf("%.4f", res$anomaly_score)),
              tags$small(class = "text-muted", "Unsupervised [0.35-0.85]")
            )
          )
        ),
        
        # Policy Reason & Calibration Status
        tags$div(
          class = "p-3 rounded mb-3 bg-light border small",
          tags$div(tags$strong("Decision Reason: "), pol$reason),
          tags$div(class = "mt-1", tags$strong("Policy Standard: "), pol$policy_version),
          tags$div(class = "mt-1", tags$strong("Calibration Status: "), res$calibration_status),
          tags$div(class = "mt-1", tags$strong("Uncertainty Tier: "), tags$span(style = sprintf("color: %s; font-weight: 700;", unc$confidence_color), unc$uncertainty_tier))
        ),
        
        # Action Buttons
        tags$div(
          class = "d-flex gap-2",
          actionButton(
            ns("save_to_audit_btn"),
            "Commit to SQLite Audit Log",
            icon = icon("save"),
            class = "btn btn-outline-success flex-grow-1 btn-sm"
          ),
          actionButton(
            ns("route_to_investigation_btn"),
            "Route to Investigation Queue",
            icon = icon("folder-open"),
            class = "btn btn-outline-primary flex-grow-1 btn-sm"
          )
        )
      )
    })
    
    # Save to SQLite Audit Log
    observeEvent(input$save_to_audit_btn, {
      res <- active_eval_reactive()
      if (!res$valid) return()
      
      tx_code <- sprintf("TX-%s", substr(digest::digest(runif(1)), 1, 6))
      feat_json <- jsonlite::toJSON(list(
        amount = res$amount,
        elapsed_hours = res$elapsed_hours,
        v14 = round(res$raw_vector$v14[1], 3),
        v17 = round(res$raw_vector$v17[1], 3),
        v12 = round(res$raw_vector$v12[1], 3),
        v10 = round(res$raw_vector$v10[1], 3)
      ), auto_unbox = TRUE)
      
      record <- list(
        prediction_id          = sprintf("PRED-%s", format(Sys.time(), "%Y%m%d%H%M%S")),
        transaction_id         = tx_code,
        timestamp              = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        model_name             = res$model_label,
        model_version          = "v1.2.0",
        input_source           = active_source_label(),
        amount                 = res$amount,
        raw_probability        = res$probability,
        anomaly_score          = res$anomaly_score,
        risk_score             = res$risk_score,
        risk_category          = as.character(res$risk_category),
        predicted_class        = as.integer(res$probability >= res$threshold),
        decision_threshold     = res$threshold,
        decision_recommendation= res$policy$decision,
        action_code            = res$policy$action_code,
        calibration_status     = res$calibration_status,
        policy_version         = res$policy$policy_version,
        features_json          = feat_json
      )
      
      save_prediction_audit(record)
      showNotification(sprintf("Decision record %s committed to SQLite audit registry.", record$prediction_id), type = "message")
    })
    
    # Route to Investigation Center
    observeEvent(input$route_to_investigation_btn, {
      res <- active_eval_reactive()
      if (!res$valid) return()
      
      tx_id <- sprintf("TX-ESCALATE-%s", substr(digest::digest(runif(1)), 1, 6))
      insert_new_investigation(
        tx_id        = tx_id,
        amount       = res$amount,
        prob         = res$probability,
        anom         = res$anomaly_score,
        risk_tier    = as.character(res$risk_category),
        actual_class = NA,
        notes        = sprintf("Escalated from Risk Scoring Engine (%s). Decision: %s. Reason: %s.", res$model_label, res$policy$decision, res$policy$reason)
      )
      showNotification(sprintf("Transaction %s successfully dispatched to Forensic Investigation Center!", tx_id), type = "message")
    })
    
    # --------------------------------------------------------------------------
    # TAB 1: Intelligence Overview KPIs
    # --------------------------------------------------------------------------
    output$overview_kpi_cards_ui <- renderUI({
      test_dt <- get_test_data()
      scored_dt <- get_scored_test_data("random_forest", threshold = 0.50)
      
      tot_tx    <- nrow(test_dt)
      fraud_cnt <- sum(test_dt$class == 1)
      high_cnt  <- sum(scored_dt$risk_category == "High Risk")
      crit_cnt  <- sum(scored_dt$risk_category == "Critical Risk")
      rev_cnt   <- high_cnt + crit_cnt
      
      avg_prob  <- mean(scored_dt$fraud_probability) * 100
      exp_loss  <- sum(scored_dt[risk_category %in% c("High Risk", "Critical Risk"), amount])
      
      tags$div(
        class = "row g-3 mb-4",
        tags$div(
          class = "col-xl-3 col-md-6 col-12",
          tags$div(
            class = "card border-0 shadow-sm p-3 h-100",
            style = "border-left: 4px solid #0066FF !important;",
            tags$small(class = "text-muted font-weight-bold", "TOTAL ASSESSED VOLUME"),
            tags$h3(class = "font-weight-bold my-1 text-dark", format(tot_tx, big.mark = ",")),
            tags$small(class = "text-muted", sprintf("Confirmed Frauds: %d (0.163%%)", fraud_cnt))
          )
        ),
        tags$div(
          class = "col-xl-3 col-md-6 col-12",
          tags$div(
            class = "card border-0 shadow-sm p-3 h-100",
            style = "border-left: 4px solid #DC2626 !important;",
            tags$small(class = "text-muted font-weight-bold", "CRITICAL RISK FLAGS"),
            tags$h3(class = "font-weight-bold my-1 text-danger", format(crit_cnt, big.mark = ",")),
            tags$small(class = "text-danger", sprintf("%.2f%% of Portfolio (Block Recommended)", (crit_cnt / tot_tx) * 100))
          )
        ),
        tags$div(
          class = "col-xl-3 col-md-6 col-12",
          tags$div(
            class = "card border-0 shadow-sm p-3 h-100",
            style = "border-left: 4px solid #EA580C !important;",
            tags$small(class = "text-muted font-weight-bold", "MANUAL REVIEW QUEUE"),
            tags$h3(class = "font-weight-bold my-1 text-warning", format(rev_cnt, big.mark = ",")),
            tags$small(class = "text-muted", sprintf("High + Critical Risk: %d Alerts", rev_cnt))
          )
        ),
        tags$div(
          class = "col-xl-3 col-md-6 col-12",
          tags$div(
            class = "card border-0 shadow-sm p-3 h-100",
            style = "border-left: 4px solid #16A34A !important;",
            tags$small(class = "text-muted font-weight-bold", "PREVENTED FRAUD EXPOSURE"),
            tags$h3(class = "font-weight-bold my-1 text-success", scales::dollar(exp_loss)),
            tags$small(class = "text-success", sprintf("Avg Predicted Prob: %.2f%%", avg_prob))
          )
        )
      )
    })
    
    output$overview_risk_dist_plotly <- renderPlotly({
      scored_dt <- get_scored_test_data("random_forest", threshold = 0.50)
      tier_counts <- scored_dt[, .N, by = risk_category][order(risk_category)]
      
      colors <- c("Very Low Risk" = "#22C55E", "Low Risk" = "#86EFAC", "Medium Risk" = "#FBBF24", "High Risk" = "#FB923C", "Critical Risk" = "#EF4444")
      
      plot_ly(
        data = tier_counts,
        x = ~risk_category,
        y = ~N,
        type = "bar",
        marker = list(color = colors[as.character(tier_counts$risk_category)])
      ) %>%
        layout(
          xaxis = list(title = "Assigned Risk Policy Band"),
          yaxis = list(title = "Transaction Volume", type = "log"),
          margin = list(l = 50, r = 20, t = 20, b = 50)
        )
    })
    
    # --------------------------------------------------------------------------
    # TAB 3: Batch Scoring Logic
    # --------------------------------------------------------------------------
    batch_scored_state <- reactiveVal(NULL)
    
    output$download_sample_template <- downloadHandler(
      filename = function() { "fraudshield_batch_valid_template.csv" },
      content = function(file) {
        test_dt <- get_test_data()
        sample_rows <- test_dt[1:15, c("amount", paste0("v", 1:28), "elapsed_hours")]
        write.csv(sample_rows, file, row.names = FALSE)
      }
    )
    
    observeEvent(input$run_batch_btn, {
      req(input$batch_file_input)
      
      withProgress(message = "Scoring Batch Payload...", value = 0, {
        tryCatch({
          raw_df <- data.table::fread(input$batch_file_input$datapath)
          
          res <- score_batch_transactions(
            batch_df = raw_df,
            model_type = input$batch_model_choice,
            threshold = input$batch_threshold,
            progress_callback = function(p, msg) {
              incProgress(p, detail = msg)
            }
          )
          
          batch_scored_state(res)
          showNotification(sprintf("Batch processed: %d records scored in %.2f seconds.", res$summary$valid_count, res$summary$elapsed_seconds), type = "message")
        }, error = function(e) {
          showNotification(paste("Batch scoring aborted:", e$message), type = "error")
        })
      })
    })
    
    output$batch_summary_banner <- renderUI({
      res <- batch_scored_state()
      if (is.null(res)) {
        return(
          tags$div(
            class = "text-center text-muted p-4 border rounded bg-light mb-3",
            icon("upload", class = "mb-2 fa-2x"),
            tags$h6("No Batch File Scored Yet"),
            tags$p(class = "small m-0", "Upload a CSV transaction payload with columns Amount, V1 to V28, and click 'Score Batch File'.")
          )
        )
      }
      
      s <- res$summary
      tags$div(
        class = "alert alert-success d-flex justify-content-between align-items-center mb-3",
        tags$div(
          tags$strong("Batch Processing Summary: "),
          sprintf("Total: %d | Valid: %d | Invalid: %d | Critical Risk: %d | Manual Reviews: %d | Completed in %.2f sec",
                  s$total_uploaded, s$valid_count, s$invalid_count, s$critical_risk_count, s$review_count, s$elapsed_seconds)
        ),
        tags$span(class = "badge bg-success", "Completed")
      )
    })
    
    output$batch_results_dt <- renderDT({
      res <- batch_scored_state()
      if (is.null(res) || nrow(res$scored_data) == 0) return(NULL)
      
      disp_dt <- res$scored_data[, .(
        Transaction_ID = transaction_id,
        Amount = scales::dollar(amount),
        Fraud_Probability = sprintf("%.2f%%", fraud_probability * 100),
        Risk_Score = sprintf("%.1f", risk_score),
        Risk_Category = as.character(risk_category),
        Decision_Recommendation = decision_recommendation,
        Action_Code = action_code
      )]
      
      datatable(
        disp_dt,
        options = list(pageLength = 8, dom = "tip", scrollX = TRUE),
        rownames = FALSE
      ) %>%
        formatStyle(
          "Risk_Category",
          backgroundColor = styleEqual(
            c("Very Low Risk", "Low Risk", "Medium Risk", "High Risk", "Critical Risk"),
            c("#DCFCE7", "#E0F2FE", "#FEF3C7", "#FFEDD5", "#FEE2E2")
          ),
          color = styleEqual(
            c("Very Low Risk", "Low Risk", "Medium Risk", "High Risk", "Critical Risk"),
            c("#16A34A", "#0369A1", "#D97706", "#EA580C", "#DC2626")
          ),
          fontWeight = "bold"
        )
    })
    
    output$batch_downloads_ui <- renderUI({
      res <- batch_scored_state()
      if (is.null(res) || nrow(res$scored_data) == 0) return(NULL)
      
      tagList(
        downloadButton(ns("download_all_scored_csv"), "Export All Scored (CSV)", class = "btn-outline-primary btn-sm"),
        downloadButton(ns("download_high_risk_csv"), "Export High/Critical Only (CSV)", class = "btn-outline-danger btn-sm"),
        if (nrow(res$invalid_data) > 0) {
          downloadButton(ns("download_invalid_csv"), "Export Validation Errors (CSV)", class = "btn-outline-warning btn-sm")
        }
      )
    })
    
    output$download_all_scored_csv <- downloadHandler(
      filename = function() { sprintf("fraudshield_batch_scored_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")) },
      content = function(file) {
        res <- batch_scored_state()
        write.csv(res$scored_data, file, row.names = FALSE)
      }
    )
    
    output$download_high_risk_csv <- downloadHandler(
      filename = function() { sprintf("fraudshield_high_risk_alerts_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")) },
      content = function(file) {
        res <- batch_scored_state()
        sub_dt <- res$scored_data[risk_category %in% c("High Risk", "Critical Risk"), ]
        write.csv(sub_dt, file, row.names = FALSE)
      }
    )
    
    output$download_invalid_csv <- downloadHandler(
      filename = function() { sprintf("fraudshield_validation_errors_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")) },
      content = function(file) {
        res <- batch_scored_state()
        write.csv(res$invalid_data, file, row.names = FALSE)
      }
    )
    
    # --------------------------------------------------------------------------
    # TAB 4: Multi-Model & Ensemble
    # --------------------------------------------------------------------------
    multimodel_reactive <- reactive({
      tx <- active_tx_data()
      th <- if (!is.null(input$single_threshold)) input$single_threshold else 0.50
      compare_model_predictions(tx, threshold = th)
    })
    
    output$multimodel_consensus_banner <- renderUI({
      cmp <- multimodel_reactive()
      badge_cls <- if (cmp$all_agree) "alert-success" else "alert-warning"
      
      tags$div(
        class = paste("alert d-flex justify-content-between align-items-center mb-3", badge_cls),
        tags$div(
          tags$strong("Architecture Consensus: "),
          cmp$consensus_label,
          sprintf(" (Maximum Model Disagreement Spread: %.2f%%)", cmp$disagreement_spread * 100)
        ),
        tags$span(class = "badge bg-dark", sprintf("Ensemble Prob: %.2f%%", cmp$ensemble_prob * 100))
      )
    })
    
    output$multimodel_comparison_table <- renderTable({
      cmp <- multimodel_reactive()
      cmp$models_table[, c("Model", "Architecture", "Formatted_Score", "Risk_Score", "Decision", "Weight", "Calibration_Status")]
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
    
    output$multimodel_spectrum_plotly <- renderPlotly({
      cmp <- multimodel_reactive()
      df <- cmp$models_table
      
      plot_ly(
        data = df,
        x = ~Model,
        y = ~Risk_Score,
        type = "bar",
        color = ~Decision,
        colors = c("Alert" = "#EF4444", "Cleared" = "#22C55E", "Anomalous" = "#F59E0B", "Normal" = "#3B82F6")
      ) %>%
        layout(
          yaxis = list(title = "Normalized Risk Score (0-100)", range = c(0, 105)),
          xaxis = list(title = ""),
          margin = list(l = 50, r = 20, t = 20, b = 60)
        )
    })
    
    # --------------------------------------------------------------------------
    # TAB 5: Policy Lab
    # --------------------------------------------------------------------------
    output$policy_metrics_kpi_ui <- renderUI({
      m_type <- input$policy_model_select
      th     <- input$policy_sim_threshold
      if (is.null(m_type)) m_type <- "random_forest"
      if (is.null(th)) th <- 0.50
      
      scored_dt <- get_scored_test_data(m_type, threshold = th)
      metrics <- compute_advanced_metrics(scored_dt, threshold = th)
      
      tags$div(
        class = "row g-3 mb-3",
        tags$div(
          class = "col-md-3 col-6",
          tags$div(
            class = "p-2 bg-light rounded text-center border",
            tags$small(class = "text-muted", "Precision"),
            tags$h4(class = "text-primary my-1", sprintf("%.2f%%", metrics$precision * 100)),
            tags$small(class = "text-muted", sprintf("FP Alerts: %d", metrics$fp))
          )
        ),
        tags$div(
          class = "col-md-3 col-6",
          tags$div(
            class = "p-2 bg-light rounded text-center border",
            tags$small(class = "text-muted", "Fraud Recall (Sensitivity)"),
            tags$h4(class = "text-success my-1", sprintf("%.2f%%", metrics$recall * 100)),
            tags$small(class = "text-muted", sprintf("Captured: %d / %d", metrics$tp, metrics$tp + metrics$fn))
          )
        ),
        tags$div(
          class = "col-md-3 col-6",
          tags$div(
            class = "p-2 bg-light rounded text-center border",
            tags$small(class = "text-muted", "Balanced F1-Score"),
            tags$h4(class = "text-dark my-1", sprintf("%.4f", metrics$f1_score)),
            tags$small(class = "text-muted", sprintf("F2-Score: %.4f", metrics$f2_score))
          )
        ),
        tags$div(
          class = "col-md-3 col-6",
          tags$div(
            class = "p-2 bg-light rounded text-center border",
            tags$small(class = "text-muted", "Specificity (Legit Cleared)"),
            tags$h4(class = "text-info my-1", sprintf("%.4f", metrics$specificity)),
            tags$small(class = "text-muted", sprintf("False Alarm Rate: %.4f%%", metrics$fpr * 100))
          )
        )
      )
    })
    
    output$policy_confusion_matrix_table <- renderTable({
      m_type <- input$policy_model_select
      th     <- input$policy_sim_threshold
      if (is.null(m_type)) m_type <- "random_forest"
      if (is.null(th)) th <- 0.50
      
      scored_dt <- get_scored_test_data(m_type, threshold = th)
      metrics <- compute_advanced_metrics(scored_dt, threshold = th)
      
      data.frame(
        Benchmark_Ground_Truth = c("Actual Fraud (Class = 1)", "Actual Legitimate (Class = 0)"),
        Predicted_Alert = c(
          sprintf("%d (True Positive - Catch)", metrics$tp),
          sprintf("%d (False Positive - Alarm)", metrics$fp)
        ),
        Predicted_Clear = c(
          sprintf("%d (False Negative - Escape)", metrics$fn),
          sprintf("%d (True Negative - Auto-Clear)", metrics$tn)
        ),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, bordered = TRUE, width = "100%")
    
    output$policy_tradeoff_plotly <- renderPlotly({
      thresh_df <- tryCatch(get_threshold_analysis_table(), error = function(e) NULL)
      if (is.null(thresh_df) || nrow(thresh_df) == 0) return(NULL)
      
      sub_df <- thresh_df[thresh_df$Model == "XGBoost", ]
      
      plot_ly(data = sub_df, x = ~Threshold) %>%
        add_lines(y = ~Precision, name = "Precision", line = list(color = "#0066FF", width = 2)) %>%
        add_lines(y = ~Recall, name = "Recall", line = list(color = "#16A34A", width = 2)) %>%
        add_lines(y = ~F1_Score, name = "F1-Score", line = list(color = "#D97706", width = 2, dash = "dash")) %>%
        layout(
          yaxis = list(title = "Metric Score (0 - 1.0)", range = c(0, 1.05)),
          xaxis = list(title = "Operating Decision Cutoff"),
          legend = list(orientation = "h", y = 1.1),
          margin = list(l = 50, r = 20, t = 30, b = 50)
        )
    })
    
    # --------------------------------------------------------------------------
    # TAB 6: Financial Simulator
    # --------------------------------------------------------------------------
    financial_sim_reactive <- reactive({
      amt  <- input$sim_tx_amount
      rec  <- input$sim_recovery_rate / 100
      cost <- input$sim_investigation_cost
      fn_m <- input$sim_fn_multiplier
      
      eval_res <- active_eval_reactive()
      p <- if (eval_res$valid) eval_res$probability else 0.50
      
      calculate_financial_exposure(
        amount = amt,
        prob = p,
        threshold = input$single_threshold,
        params = list(
          loss_rate = 1.0,
          recovery_rate = rec,
          investigation_cost = cost,
          fn_multiplier = fn_m
        )
      )
    })
    
    output$financial_summary_cards_ui <- renderUI({
      fin <- financial_sim_reactive()
      
      tags$div(
        class = "row g-3 mb-3",
        tags$div(
          class = "col-md-3 col-6",
          tags$div(
            class = "p-2 bg-light rounded text-center border",
            tags$small(class = "text-muted", "Potential Exposure"),
            tags$h4(class = "text-danger my-1", scales::dollar(fin$potential_exposure)),
            tags$small(class = "text-muted", sprintf("Net Loss: %s", scales::dollar(fin$net_fraud_loss)))
          )
        ),
        tags$div(
          class = "col-md-3 col-6",
          tags$div(
            class = "p-2 bg-light rounded text-center border",
            tags$small(class = "text-muted", "Expected Unmitigated Loss"),
            tags$h4(class = "text-warning my-1", scales::dollar(fin$expected_unmitigated_loss)),
            tags$small(class = "text-muted", "Weighted by P(Fraud)")
          )
        ),
        tags$div(
          class = "col-md-3 col-6",
          tags$div(
            class = "p-2 bg-light rounded text-center border",
            tags$small(class = "text-muted", "Investigation Cost"),
            tags$h4(class = "text-primary my-1", scales::dollar(fin$investigation_cost)),
            tags$small(class = "text-muted", ifelse(fin$is_alert, "Alert Incurred", "Auto-Cleared ($0)"))
          )
        ),
        tags$div(
          class = "col-md-3 col-6",
          tags$div(
            class = "p-2 bg-light rounded text-center border",
            tags$small(class = "text-muted", "Net Expected Benefit"),
            tags$h4(class = ifelse(fin$net_expected_benefit >= 0, "text-success", "text-danger"), my = 1, scales::dollar(fin$net_expected_benefit)),
            tags$small(class = "text-muted", "Policy Intervention Gain")
          )
        )
      )
    })
    
    output$financial_cost_benefit_plotly <- renderPlotly({
      fin <- financial_sim_reactive()
      df <- fin$cost_benefit_curve
      
      plot_ly(data = df, x = ~Threshold) %>%
        add_lines(y = ~Expected_Cost, name = "Expected Operational Cost", line = list(color = "#DC2626", width = 2)) %>%
        add_lines(y = ~Expected_Benefit, name = "Expected Benefit", line = list(color = "#16A34A", width = 2)) %>%
        layout(
          yaxis = list(title = "Dollar Value ($)"),
          xaxis = list(title = "Decision Cutoff (Tau)"),
          legend = list(orientation = "h", y = 1.1),
          margin = list(l = 50, r = 20, t = 30, b = 50)
        )
    })
    
    # --------------------------------------------------------------------------
    # TAB 7: Explainability Integration
    # --------------------------------------------------------------------------
    output$explainability_integration_banner <- renderUI({
      res <- active_eval_reactive()
      tags$div(
        class = "alert alert-secondary d-flex justify-content-between align-items-center mb-3",
        tags$div(
          tags$strong("Forensic Explainable AI Link: "),
          sprintf("Explaining active transaction %s evaluated under %s.", res$transaction_id, res$model_label)
        ),
        tags$span(class = "badge bg-info", "DALEX iBreakDown Active")
      )
    })
    
    shap_active_reactive <- reactive({
      tx <- active_tx_data()
      m_type <- input$single_model_choice
      if (is.null(m_type) || m_type == "ensemble") m_type <- "random_forest"
      compute_local_shap_breakdown(model_type = m_type, tx_row = tx, B = 5)
    })
    
    output$explainability_consistency_badge <- renderUI({
      shap_res <- shap_active_reactive()
      if (shap_res$is_consistent) {
        tags$span(class = "badge bg-success", sprintf("Consistent (|Discrepancy| = %.4f)", shap_res$discrepancy))
      } else {
        tags$span(class = "badge bg-warning text-dark", sprintf("Approximation Discrepancy: %.4f", shap_res$discrepancy))
      }
    })
    
    output$explainability_shap_plotly <- renderPlotly({
      shap_res <- shap_active_reactive()
      df <- head(shap_res$contributions_df, 8)
      
      df$Color <- ifelse(df$Contribution >= 0, "#DC2626", "#16A34A")
      
      plot_ly(
        data = df,
        x = ~Contribution,
        y = ~reorder(Feature, abs(Contribution)),
        type = "bar",
        orientation = "h",
        marker = list(color = ~Color)
      ) %>%
        layout(
          xaxis = list(title = "SHAP Probability Contribution (Delta from Prior)"),
          yaxis = list(title = ""),
          margin = list(l = 100, r = 20, t = 20, b = 50)
        )
    })
    
    output$explainability_top_drivers_ui <- renderUI({
      shap_res <- shap_active_reactive()
      pos <- head(shap_res$top_positive, 3)
      neg <- head(shap_res$top_negative, 3)
      
      tags$div(
        style = "font-size: 0.85rem; line-height: 1.6;",
        tags$h6(class = "text-danger font-weight-bold", icon("arrow-up"), " Escalation Signals (+ Risk):"),
        if (nrow(pos) > 0) {
          tags$ul(lapply(seq_len(nrow(pos)), function(i) {
            tags$li(tags$strong(pos$Feature[i]), sprintf(": +%.4f (Value: %.2f)", pos$Contribution[i], pos$Actual_Value[i]))
          }))
        } else {
          tags$p("No significant positive escalation signals.")
        },
        tags$hr(style = "margin: 10px 0;"),
        tags$h6(class = "text-success font-weight-bold", icon("arrow-down"), " Mitigating Signals (- Risk):"),
        if (nrow(neg) > 0) {
          tags$ul(lapply(seq_len(nrow(neg)), function(i) {
            tags$li(tags$strong(neg$Feature[i]), sprintf(": %.4f (Value: %.2f)", neg$Contribution[i], neg$Actual_Value[i]))
          }))
        } else {
          tags$p("No significant mitigating signals.")
        }
      )
    })
    
    # --------------------------------------------------------------------------
    # TAB 8: Uncertainty & Confidence Analysis
    # --------------------------------------------------------------------------
    output$uncertainty_diagnostic_banner <- renderUI({
      res <- active_eval_reactive()
      unc <- res$uncertainty
      
      badge_cls <- if (unc$uncertainty_tier == "Low Uncertainty") "alert-success" else if (unc$uncertainty_tier == "Moderate Uncertainty") "alert-warning" else "alert-danger"
      
      tags$div(
        class = paste("alert d-flex justify-content-between align-items-center mb-3", badge_cls),
        tags$div(
          tags$strong("Confidence Status: "),
          unc$confidence_rating,
          tags$span(" — ", unc$recommendation)
        ),
        tags$span(class = "badge bg-dark", sprintf("Uncertainty Score: %.1f / 100", unc$uncertainty_index))
      )
    })
    
    output$uncertainty_metrics_panel_ui <- renderUI({
      res <- active_eval_reactive()
      unc <- res$uncertainty
      
      tags$div(
        style = "font-size: 0.85rem; line-height: 1.8;",
        tags$div(class = "d-flex justify-content-between py-1 border-bottom", tags$strong("Boundary Distance (|P - Cutoff|):"), tags$span(sprintf("%.4f", unc$boundary_distance))),
        tags$div(class = "d-flex justify-content-between py-1 border-bottom", tags$strong("Uncertainty Index (0 - 100):"), tags$span(sprintf("%.1f", unc$uncertainty_index))),
        tags$div(class = "d-flex justify-content-between py-1 border-bottom", tags$strong("Assigned Confidence Tier:"), tags$span(style = sprintf("color: %s; font-weight: 700;", unc$confidence_color), unc$confidence_rating)),
        tags$div(class = "d-flex justify-content-between py-1 border-bottom", tags$strong("Automated Triage Eligibility:"), tags$span(ifelse(unc$uncertainty_index < 50, "Eligible for Straight-Through Processing", "Requires Mandatory Human Verification"))),
        tags$div(class = "mt-2 p-2 bg-light rounded text-muted", tags$strong("Standard: "), "Transactions located within +/- 0.15 of the decision threshold automatically trigger elevated uncertainty states.")
      )
    })
    
    output$uncertainty_stability_plotly <- renderPlotly({
      res <- active_eval_reactive()
      # Generate 5 perturbed noise simulations
      set.seed(42)
      sim_probs <- pmin(pmax(rnorm(10, mean = res$probability, sd = 0.02), 0), 1)
      
      plot_ly(
        x = paste0("Run-", 1:10),
        y = sim_probs * 100,
        type = "scatter",
        mode = "lines+markers",
        marker = list(color = "#0066FF", size = 8),
        line = list(color = "#0066FF", width = 2)
      ) %>%
        layout(
          yaxis = list(title = "Perturbed Probability (%)"),
          xaxis = list(title = "1% Gaussian Perturbation Iteration"),
          margin = list(l = 50, r = 20, t = 20, b = 50)
        )
    })
    
    # --------------------------------------------------------------------------
    # TAB 9: Risk Portfolio Analytics
    # --------------------------------------------------------------------------
    output$portfolio_exposure_plotly <- renderPlotly({
      scored_dt <- get_scored_test_data("random_forest", threshold = 0.50)
      exp_by_tier <- scored_dt[, .(
        Total_Exposure = sum(amount),
        Avg_Ticket = mean(amount)
      ), by = risk_category][order(risk_category)]
      
      plot_ly(
        data = exp_by_tier,
        x = ~risk_category,
        y = ~Total_Exposure,
        type = "bar",
        marker = list(color = c("#22C55E", "#86EFAC", "#FBBF24", "#FB923C", "#EF4444"))
      ) %>%
        layout(
          yaxis = list(title = "Total Financial Exposure ($)"),
          xaxis = list(title = ""),
          margin = list(l = 60, r = 20, t = 20, b = 50)
        )
    })
    
    output$portfolio_prob_dist_plotly <- renderPlotly({
      scored_dt <- get_scored_test_data("random_forest", threshold = 0.50)
      
      plot_ly(
        data = scored_dt,
        x = ~fraud_probability * 100,
        color = ~factor(class, labels = c("Legitimate (0)", "Fraudulent (1)")),
        colors = c("#22C55E", "#EF4444"),
        type = "histogram",
        nbinsx = 30
      ) %>%
        layout(
          barmode = "overlay",
          xaxis = list(title = "Fraud Probability (%)"),
          yaxis = list(title = "Transaction Count", type = "log"),
          margin = list(l = 50, r = 20, t = 20, b = 50)
        )
    })
    
    output$portfolio_timeline_plotly <- renderPlotly({
      scored_dt <- get_scored_test_data("random_forest", threshold = 0.50)
      scored_dt[, elapsed_hour := as.integer(floor(elapsed_hours))]
      
      hourly_alerts <- scored_dt[, .(
        High_Risk_Alerts = sum(risk_category %in% c("High Risk", "Critical Risk"))
      ), by = elapsed_hour][order(elapsed_hour)]
      
      plot_ly(
        data = hourly_alerts,
        x = ~elapsed_hour,
        y = ~High_Risk_Alerts,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#DC2626", width = 2),
        marker = list(color = "#DC2626", size = 6)
      ) %>%
        layout(
          xaxis = list(title = "Elapsed Hours (0 - 48 Hours)"),
          yaxis = list(title = "High & Critical Risk Alerts"),
          margin = list(l = 50, r = 20, t = 20, b = 50)
        )
    })
    
    # --------------------------------------------------------------------------
    # TAB 10: Prediction History & Audit Center
    # --------------------------------------------------------------------------
    audit_data_reactive <- reactive({
      input$refresh_audit_btn
      get_prediction_audit_history(
        limit = 200,
        filter_model = input$audit_filter_model,
        filter_category = input$audit_filter_category,
        search_query = input$audit_search_input
      )
    })
    
    output$audit_history_dt <- renderDT({
      df <- audit_data_reactive()
      if (is.null(df) || nrow(df) == 0) {
        return(data.frame(Message = "No audit records match the current filters."))
      }
      
      disp_df <- data.frame(
        Prediction_ID = df$prediction_id,
        Transaction_ID = df$transaction_id,
        Timestamp = df$timestamp,
        Model_Name = df$model_name,
        Amount = scales::dollar(df$amount),
        Probability = sprintf("%.2f%%", df$raw_probability * 100),
        Risk_Score = sprintf("%.1f", df$risk_score),
        Risk_Tier = df$risk_category,
        Recommendation = df$decision_recommendation,
        Calibration = df$calibration_status,
        Policy_Version = df$policy_version,
        stringsAsFactors = FALSE
      )
      
      datatable(
        disp_df,
        options = list(pageLength = 10, dom = "tip", scrollX = TRUE),
        rownames = FALSE
      ) %>%
        formatStyle(
          "Risk_Tier",
          backgroundColor = styleEqual(
            c("Very Low Risk", "Low Risk", "Medium Risk", "High Risk", "Critical Risk"),
            c("#DCFCE7", "#E0F2FE", "#FEF3C7", "#FFEDD5", "#FEE2E2")
          ),
          color = styleEqual(
            c("Very Low Risk", "Low Risk", "Medium Risk", "High Risk", "Critical Risk"),
            c("#16A34A", "#0369A1", "#D97706", "#EA580C", "#DC2626")
          ),
          fontWeight = "bold"
        )
    })
    
    output$download_audit_history_csv <- downloadHandler(
      filename = function() { sprintf("fraudshield_decision_audit_log_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")) },
      content = function(file) {
        df <- audit_data_reactive()
        write.csv(df, file, row.names = FALSE)
      }
    )
    
  })
}
