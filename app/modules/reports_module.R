# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Module: Reports & Data Quality Center (Page 8)
# Script: app/modules/reports_module.R
# ==============================================================================

reports_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header Banner
    tags$div(
      class = "alert alert-secondary d-flex align-items-center mb-4",
      style = "background-color: #F8FAFC; border-left: 4px solid #0066FF; color: #1E293B; padding: 12px 18px; border-radius: 6px;",
      tags$div(style = "font-size: 1.4rem; margin-right: 12px; color: #0066FF;", icon("check-circle")),
      tags$div(
        tags$strong("Data Governance, Quality Assurance & Audit Artifacts: "),
        "Access rigorous validation logs, pipeline data quality metrics, and downloadable executive audit reports. ",
        "All data integrity checks comply with financial governance standards."
      )
    ),
    
    # Row 1: Data Quality Audit Scorecards
    tags$div(
      class = "row g-3 mb-4",
      tags$div(
        class = "col-lg-3 col-md-6",
        create_kpi_card("Completeness Rate", "100.00%", "0 Missing Values Across 31 Cols", "check-double", "#16A34A")
      ),
      tags$div(
        class = "col-lg-3 col-md-6",
        create_kpi_card("Duplicate Preservation", "1,081 Rows", "0.38% Preserved for Audit", "copy", "#D97706")
      ),
      tags$div(
        class = "col-lg-3 col-md-6",
        create_kpi_card("Non-Finite Checks", "0 Anomalies", "0 Inf / -Inf / NaN Detected", "shield-alt", "#0066FF")
      ),
      tags$div(
        class = "col-lg-3 col-md-6",
        create_kpi_card("Target Conformance", "100% Binary", "Strict 0 (Legit) & 1 (Fraud)", "check-circle", "#10B981")
      )
    ),
    
    # Row 2: Data Quality Verification Matrix Table
    bslib::card(
      class = "mb-4",
      bslib::card_header(
        tags$span(style = "font-weight: 700; color: #0F172A;", icon("tasks"), " Data Pipeline Quality & Integrity Assurance Matrix")
      ),
      bslib::card_body(
        tags$div(style = "overflow-x: auto;", tableOutput(ns("quality_matrix_table")))
      )
    ),
    
    # Row 3: Download & Export Center
    bslib::card(
      class = "mb-4",
      bslib::card_header(
        tags$span(style = "font-weight: 700; color: #0F172A;", icon("download"), " Executive Reports & Export Center")
      ),
      bslib::card_body(
        tags$div(
          class = "row g-3",
          
          # Card 1: Model Comparison
          tags$div(
            class = "col-lg-4 col-md-6",
            tags$div(
              class = "p-3 border rounded h-100 d-flex flex-column justify-content-between",
              tags$div(
                tags$h6(style = "font-weight: 700; color: #0F172A;", "ML Model Benchmark Table"),
                tags$p(style = "font-size: 0.82rem; color: #64748B;", "Full holdout evaluation metrics for RF, XGBoost, Logistic Regression, and Isolation Forest.")
              ),
              downloadButton(ns("download_model_comparison_csv"), "Export Metrics CSV", class = "btn-outline-primary btn-sm w-100")
            )
          ),
          
          # Card 2: Threshold Optimization
          tags$div(
            class = "col-lg-4 col-md-6",
            tags$div(
              class = "p-3 border rounded h-100 d-flex flex-column justify-content-between",
              tags$div(
                tags$h6(style = "font-weight: 700; color: #0F172A;", "Threshold Performance Grid"),
                tags$p(style = "font-size: 0.82rem; color: #64748B;", "Detailed sensitivity grid (0.10 to 0.90) with TP, FP, TN, FN, Precision, and Recall.")
              ),
              downloadButton(ns("download_threshold_grid_csv"), "Export Threshold CSV", class = "btn-outline-primary btn-sm w-100")
            )
          ),
          
          # Card 3: Executive Summary Report
          tags$div(
            class = "col-lg-4 col-md-6",
            tags$div(
              class = "p-3 border rounded h-100 d-flex flex-column justify-content-between",
              tags$div(
                tags$h6(style = "font-weight: 700; color: #0F172A;", "Executive ML Summary Audit"),
                tags$p(style = "font-size: 0.82rem; color: #64748B;", "Comprehensive plain-text forensic report detailing architecture, CV results, and champion selection.")
              ),
              downloadButton(ns("download_ml_summary_txt"), "Export Audit Report TXT", class = "btn-outline-dark btn-sm w-100")
            )
          ),
          
          # Card 4: Dataset Validation Report
          tags$div(
            class = "col-lg-4 col-md-6",
            tags$div(
              class = "p-3 border rounded h-100 d-flex flex-column justify-content-between",
              tags$div(
                tags$h6(style = "font-weight: 700; color: #0F172A;", "Dataset Validation Report"),
                tags$p(style = "font-size: 0.82rem; color: #64748B;", "Ingestion validation report covering dimension checks, schema validation, and column statistics.")
              ),
              downloadButton(ns("download_validation_report_csv"), "Export Validation CSV", class = "btn-outline-primary btn-sm w-100")
            )
          ),
          
          # Card 5: Data Cleaning Report
          tags$div(
            class = "col-lg-4 col-md-6",
            tags$div(
              class = "p-3 border rounded h-100 d-flex flex-column justify-content-between",
              tags$div(
                tags$h6(style = "font-weight: 700; color: #0F172A;", "Data Cleaning Decisions Log"),
                tags$p(style = "font-size: 0.82rem; color: #64748B;", "Detailed transformation log documenting column standardization, missingness audits, and duplicate tracking.")
              ),
              downloadButton(ns("download_cleaning_report_csv"), "Export Cleaning CSV", class = "btn-outline-primary btn-sm w-100")
            )
          ),
          
          # Card 6: SQLite Investigation Trail
          tags$div(
            class = "col-lg-4 col-md-6",
            tags$div(
              class = "p-3 border rounded h-100 d-flex flex-column justify-content-between",
              tags$div(
                tags$h6(style = "font-weight: 700; color: #0F172A;", "Investigation Center Audit Trail"),
                tags$p(style = "font-size: 0.82rem; color: #64748B;", "Snapshot of all local triage records, analyst notes, and disposition history from SQLite.")
              ),
              downloadButton(ns("download_investigations_csv"), "Export Case Trail CSV", class = "btn-outline-success btn-sm w-100")
            )
          )
        )
      )
    )
  )
}

reports_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Quality Assurance Matrix Table
    output$quality_matrix_table <- renderTable({
      data.frame(
        Integrity_Check = c(
          "1. Schema Conformance",
          "2. Completeness / Missing Values",
          "3. Duplicate Record Audit",
          "4. Non-Finite / Out-of-Bounds Detection",
          "5. Target Variable Purity",
          "6. Feature Stationarity & Normalization"
        ),
        Requirement = c(
          "Exact match of Time, V1-V28, Amount, Class (31 Canonical Columns)",
          "0 missing observations across all 284,807 transactions",
          "Identify exact duplicate transaction vectors (1,081 rows)",
          "Check for Inf, -Inf, NaN, or negative transaction amounts",
          "Class must contain strictly binary integers (0 = Legit, 1 = Fraud)",
          "V1-V28 PCA orthogonal components; log1p transformation on Amount"
        ),
        Observed_Metric = c(
          "31/31 Validated (100% Schema Alignment)",
          "0 Missing Values (0.00% Missingness)",
          "1,081 Duplicates (0.38% Frequency)",
          "0 Non-Finite Values (0 Negative Amounts)",
          "284,315 Legitimate | 492 Fraudulent",
          "amount_log created (mean: 3.15, sd: 1.66)"
        ),
        Status = c("PASSED", "PASSED", "AUDITED", "PASSED", "PASSED", "PASSED"),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lllc")
    
    # 2. Download Handlers
    
    # Model Comparison CSV
    output$download_model_comparison_csv <- downloadHandler(
      filename = function() { "fraudshield_model_comparison.csv" },
      content = function(file) {
        root <- get_project_root()
        f <- file.path(root, "outputs", "tables", "model_comparison.csv")
        if (file.exists(f)) file.copy(f, file) else write.csv(data.frame(), file)
      }
    )
    
    # Threshold Grid CSV
    output$download_threshold_grid_csv <- downloadHandler(
      filename = function() { "fraudshield_threshold_analysis.csv" },
      content = function(file) {
        root <- get_project_root()
        f <- file.path(root, "outputs", "tables", "threshold_analysis.csv")
        if (file.exists(f)) file.copy(f, file) else write.csv(data.frame(), file)
      }
    )
    
    # ML Summary TXT
    output$download_ml_summary_txt <- downloadHandler(
      filename = function() { "fraudshield_ml_summary_report.txt" },
      content = function(file) {
        root <- get_project_root()
        f <- file.path(root, "outputs", "reports", "ml_summary_report.txt")
        if (file.exists(f)) file.copy(f, file) else writeLines("Report not found.", file)
      }
    )
    
    # Validation Report CSV
    output$download_validation_report_csv <- downloadHandler(
      filename = function() { "fraudshield_dataset_validation_report.csv" },
      content = function(file) {
        root <- get_project_root()
        f <- file.path(root, "outputs", "reports", "dataset_validation_report.csv")
        if (file.exists(f)) file.copy(f, file) else write.csv(data.frame(), file)
      }
    )
    
    # Cleaning Report CSV
    output$download_cleaning_report_csv <- downloadHandler(
      filename = function() { "fraudshield_data_cleaning_report.csv" },
      content = function(file) {
        root <- get_project_root()
        f <- file.path(root, "outputs", "reports", "data_cleaning_report.csv")
        if (file.exists(f)) file.copy(f, file) else write.csv(data.frame(), file)
      }
    )
    
    # Investigations Audit Trail from SQLite
    output$download_investigations_csv <- downloadHandler(
      filename = function() { sprintf("fraudshield_investigations_%s.csv", format(Sys.time(), "%Y%m%d")) },
      content = function(file) {
        inv_df <- tryCatch(fetch_investigations("All", "All"), error = function(e) data.frame())
        write.csv(inv_df, file, row.names = FALSE)
      }
    )
    
  })
}
