# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Module: Investigation Center & Case Management (Page 7)
# Script: app/modules/investigation_module.R
# ==============================================================================

investigation_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Disclaimer Banner
    tags$div(
      class = "alert alert-warning d-flex align-items-center mb-4",
      style = "background-color: #FFFBEB; border-left: 4px solid #F59E0B; color: #92400E; padding: 12px 18px; border-radius: 6px;",
      tags$div(style = "font-size: 1.4rem; margin-right: 12px; color: #F59E0B;", icon("user-shield")),
      tags$div(
        tags$strong("Simulated Case Management & Local Audit Trail: "),
        "All case updates, disposition statuses, and forensic annotations are persisted locally in an embedded ",
        tags$strong("RSQLite database (`data/investigations.sqlite`). "),
        "This triage interface illustrates financial fraud operations and does not connect to live card networks or banking cores."
      )
    ),
    
    # KPI Triage Counters
    uiOutput(ns("investigation_kpi_cards")),
    
    # Filter & Search Toolbar
    bslib::card(
      class = "mb-4",
      bslib::card_header(
        tags$span(style = "font-weight: 700; color: #0F172A;", icon("filter"), " Investigation Queue Filters")
      ),
      bslib::card_body(
        tags$div(
          class = "row g-3 align-items-center",
          tags$div(
            class = "col-md-4",
            selectInput(
              ns("filter_status"),
              "Triage Status:",
              choices = c("All Statuses" = "All", "Pending", "Under Review", "Reviewed", "Escalated"),
              selected = "All"
            )
          ),
          tags$div(
            class = "col-md-4",
            selectInput(
              ns("filter_risk"),
              "Risk Tier:",
              choices = c("All Risk Tiers" = "All", "Critical Risk", "High Risk", "Medium Risk", "Low Risk"),
              selected = "All"
            )
          ),
          tags$div(
            class = "col-md-4 text-end",
            actionButton(ns("refresh_db_btn"), "Refresh Queue", icon = icon("sync"), class = "btn-outline-primary mt-4")
          )
        )
      )
    ),
    
    # Main Investigation Workspace (Queue Table + Detail & Update Drawer)
    tags$div(
      class = "row g-3 mb-4",
      
      # Left Column: Interactive Case Queue
      tags$div(
        class = "col-lg-8 col-md-12",
        bslib::card(
          bslib::card_header(
            tags$div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              tags$span(style = "font-weight: 700; color: #0F172A;", "Flagged Case Triage Queue (Click a row to audit)"),
              tags$span(class = "badge bg-dark", "SQLite Backed")
            )
          ),
          bslib::card_body(
            tags$div(style = "overflow-x: auto;", DTOutput(ns("cases_dt")))
          )
        )
      ),
      
      # Right Column: Case Disposition Editor
      tags$div(
        class = "col-lg-4 col-md-12",
        bslib::card(
          bslib::card_header(
            tags$span(style = "font-weight: 700; color: #0F172A;", icon("edit"), " Analyst Disposition & Annotation")
          ),
          bslib::card_body(
            uiOutput(ns("case_editor_ui"))
          )
        )
      )
    )
  )
}

investigation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive trigger for database refresh
    db_trigger <- reactiveVal(0)
    
    observeEvent(input$refresh_db_btn, {
      db_trigger(db_trigger() + 1)
    })
    
    # Fetch data from SQLite
    cases_data <- reactive({
      db_trigger()
      tryCatch({
        fetch_investigations(
          status_filter = input$filter_status,
          risk_filter = input$filter_risk
        )
      }, error = function(e) data.frame())
    })
    
    # Render KPI Triage Counters
    output$investigation_kpi_cards <- renderUI({
      db_trigger()
      all_cases <- tryCatch(fetch_investigations("All", "All"), error = function(e) data.frame())
      
      total_n     <- nrow(all_cases)
      pending_n   <- sum(all_cases$status == "Pending")
      review_n    <- sum(all_cases$status == "Under Review")
      reviewed_n  <- sum(all_cases$status == "Reviewed")
      escalated_n <- sum(all_cases$status == "Escalated")
      
      tags$div(
        class = "row g-3 mb-4",
        tags$div(class = "col-xl-2 col-md-4 col-sm-6", create_kpi_card("Total Flagged", format_count(total_n), "Audited Cases", "shield-alt", "#0066FF")),
        tags$div(class = "col-xl-2 col-md-4 col-sm-6", create_kpi_card("Pending Queue", format_count(pending_n), "Requires Review", "hourglass-half", "#D97706")),
        tags$div(class = "col-xl-2 col-md-4 col-sm-6", create_kpi_card("Under Review", format_count(review_n), "Active Analyst", "search", "#3B82F6")),
        tags$div(class = "col-xl-2 col-md-4 col-sm-6", create_kpi_card("Reviewed", format_count(reviewed_n), "Cleared / Closed", "check-double", "#16A34A")),
        tags$div(class = "col-xl-2 col-md-4 col-sm-6", create_kpi_card("Escalated", format_count(escalated_n), "Forensics Escalation", "exclamation-triangle", "#DC2626")),
        tags$div(class = "col-xl-2 col-md-4 col-sm-6", create_kpi_card("Storage Engine", "RSQLite", "Embedded Local", "database", "#6366F1"))
      )
    })
    
    # Render Cases DT
    output$cases_dt <- renderDT({
      df <- cases_data()
      if (nrow(df) == 0) {
        return(datatable(data.frame(Message = "No investigation records found in local SQLite database.")))
      }
      
      disp_df <- data.frame(
        ID           = df$id,
        Case_ID      = df$transaction_id,
        Amount       = scales::dollar(df$amount),
        Fraud_Prob   = sprintf("%.2f%%", df$predicted_prob * 100),
        Iso_Score    = sprintf("%.3f", df$anomaly_score),
        Risk_Tier    = df$risk_tier,
        Status       = df$status,
        Updated_At   = df$updated_at,
        stringsAsFactors = FALSE
      )
      
      datatable(
        disp_df,
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 8, dom = "ltip")
      ) %>%
        formatStyle(
          "Risk_Tier",
          backgroundColor = styleEqual(
            c("Low Risk", "Medium Risk", "High Risk", "Critical Risk"),
            c("#DCFCE7", "#FEF3C7", "#FFEDD5", "#FEE2E2")
          ),
          color = styleEqual(
            c("Low Risk", "Medium Risk", "High Risk", "Critical Risk"),
            c("#16A34A", "#D97706", "#EA580C", "#DC2626")
          ),
          fontWeight = "bold"
        ) %>%
        formatStyle(
          "Status",
          backgroundColor = styleEqual(
            c("Pending", "Under Review", "Reviewed", "Escalated"),
            c("#FEF3C7", "#DBEAFE", "#DCFCE7", "#FEE2E2")
          ),
          color = styleEqual(
            c("Pending", "Under Review", "Reviewed", "Escalated"),
            c("#D97706", "#1D4ED8", "#16A34A", "#DC2626")
          ),
          fontWeight = "bold"
        )
    })
    
    # Selected case reactive
    selected_case <- reactive({
      df <- cases_data()
      idx <- input$cases_dt_rows_selected
      if (length(idx) == 0 || nrow(df) == 0) return(NULL)
      df[idx, ]
    })
    
    # Render Case Editor UI
    output$case_editor_ui <- renderUI({
      case <- selected_case()
      
      if (is.null(case)) {
        return(
          tags$div(
            class = "text-center text-muted p-4",
            icon("mouse-pointer", class = "mb-2 fa-2x"),
            tags$p("Select a transaction from the queue table to review details, update disposition, or record audit notes.")
          )
        )
      }
      
      tags$div(
        tags$div(
          class = "p-3 bg-light rounded mb-3",
          tags$h6(style = "font-weight: 700; color: #0F172A; margin: 0;", sprintf("Case File: %s", case$transaction_id)),
          tags$small(class = "text-muted", sprintf("Internal DB Record #%d", case$id)),
          tags$hr(style = "margin: 8px 0;"),
          tags$div(tags$strong("Amount: "), scales::dollar(case$amount)),
          tags$div(tags$strong("Predicted Probability: "), sprintf("%.2f%%", case$predicted_prob * 100)),
          tags$div(tags$strong("Isolation Anomaly: "), sprintf("%.4f", case$anomaly_score)),
          tags$div(tags$strong("Risk Category: "), case$risk_tier),
          tags$div(tags$strong("Actual Historical Class: "),
                   if (is.na(case$actual_class)) "Unlabeled (New Inference)"
                   else if (case$actual_class == 1) "Ground-Truth Fraud (1)"
                   else "Ground-Truth Legit (0)")
        ),
        
        selectInput(
          ns("edit_status"),
          "Update Disposition Status:",
          choices = c("Pending", "Under Review", "Reviewed", "Escalated"),
          selected = case$status
        ),
        
        textAreaInput(
          ns("edit_notes"),
          "Analyst Forensic Notes:",
          value = if (is.na(case$notes)) "" else case$notes,
          rows = 4,
          placeholder = "Enter audit findings, cardholder contact notes, or escalation rationale..."
        ),
        
        actionButton(
          ns("save_case_btn"),
          "Commit Case Update to SQLite",
          icon = icon("save"),
          class = "btn-primary w-100 mt-2"
        )
      )
    })
    
    # Save update event
    observeEvent(input$save_case_btn, {
      case <- selected_case()
      if (is.null(case)) return()
      
      new_st    <- input$edit_status
      new_notes <- input$edit_notes
      
      update_investigation(
        record_id = case$id,
        new_status = new_st,
        new_notes = new_notes
      )
      
      showNotification(sprintf("Case %s successfully updated to '%s' in SQLite!", case$transaction_id, new_st), type = "message")
      db_trigger(db_trigger() + 1)
    })
    
  })
}
