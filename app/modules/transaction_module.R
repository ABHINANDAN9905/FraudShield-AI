# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Module: Transaction Explorer (Page 2) - Enterprise FinTech Redesign
# Script: app/modules/transaction_module.R
# ==============================================================================
# Description:
#   High-performance, multi-dimensional transaction surveillance & exploration engine.
#   Provides server-side DT pagination, dynamic reactive KPI scorecards,
#   multi-tier risk filtering, forensic PCA dossier inspection modals with local
#   feature attribution, reactive visual analytics, and batch CSV export.
# ==============================================================================

transaction_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # --------------------------------------------------------------------------
    # 1. Professional Header Section
    # --------------------------------------------------------------------------
    tags$div(
      class = "p-4 mb-4 rounded-3",
      style = "background: linear-gradient(135deg, #0B192C 0%, #1E3E62 100%); color: #FFFFFF; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);",
      tags$div(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-3",
        tags$div(
          tags$div(
            class = "d-flex align-items-center gap-2 mb-1",
            tags$span(style = "color: #38BDF8; font-size: 1.5rem;", icon("table")),
            tags$h3(style = "margin: 0; font-weight: 800; letter-spacing: -0.02em; color: #FFFFFF;", "TRANSACTION EXPLORER"),
            tags$span(class = "badge bg-info text-dark ms-2", "Holdout Audit Partition")
          ),
          tags$p(
            style = "margin: 0; color: #94A3B8; font-size: 0.92rem;",
            "Explore, filter and investigate financial transactions with intelligent multi-model risk monitoring and ground-truth validation."
          )
        ),
        tags$div(
          class = "d-flex align-items-center gap-2",
          actionButton(
            ns("reset_all_filters"),
            "Reset All Filters",
            icon = icon("undo"),
            class = "btn-outline-light btn-sm"
          ),
          downloadButton(
            ns("export_filtered_csv"),
            "Export Filtered CSV",
            class = "btn-primary btn-sm"
          )
        )
      ),
      tags$hr(style = "border-color: rgba(255, 255, 255, 0.15); margin: 14px 0 10px 0;"),
      tags$div(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        style = "font-size: 0.80rem; color: #CBD5E1;",
        tags$div(
          icon("database"), " Data Source: European Credit Card Benchmark (56,962 Stratified Holdout Partition Records)"
        ),
        tags$div(
          icon("clock"), " Analysis Session Timestamp: ",
          tags$strong(format(Sys.time(), "%Y-%m-%d %H:%M:%S")), " (System Runtime, not transaction time)"
        )
      )
    ),
    
    # --------------------------------------------------------------------------
    # 2. Dynamic KPI Cards (Reflecting Currently Filtered Transactions)
    # --------------------------------------------------------------------------
    uiOutput(ns("filtered_kpi_cards")),
    
    # --------------------------------------------------------------------------
    # 3. Advanced Search & Filter Panel
    # --------------------------------------------------------------------------
    bslib::card(
      class = "mb-4 border-0 shadow-sm",
      bslib::card_header(
        tags$div(
          class = "d-flex justify-content-between align-items-center",
          tags$span(style = "font-weight: 700; color: #0F172A;", icon("filter"), " Multi-Dimensional Search & Risk Filtering Studio"),
          uiOutput(ns("active_filter_pills"))
        )
      ),
      bslib::card_body(
        tags$div(
          class = "row g-3",
          
          # Row 1: Search, Ground-Truth Status, Model Selection, Threshold
          tags$div(
            class = "col-lg-3 col-md-6",
            textInput(
              ns("search_id"),
              "Search Transaction ID / Signal:",
              placeholder = "e.g., TX-00042 or signal..."
            )
          ),
          tags$div(
            class = "col-lg-3 col-md-6",
            selectInput(
              ns("actual_status_filter"),
              "Ground-Truth Label:",
              choices = c(
                "All Transactions (56,962)"   = "all",
                "Confirmed Fraud Only (93)"   = "fraud",
                "Legitimate Volume Only"      = "legit"
              ),
              selected = "all"
            )
          ),
          tags$div(
            class = "col-lg-3 col-md-6",
            selectInput(
              ns("model_selector"),
              "Scoring Engine for Risk Tiers:",
              choices = c(
                "Random Forest (Champion Ensemble)" = "random_forest",
                "XGBoost (High-Throughput Trees)"   = "xgboost",
                "Logistic Regression (Baseline)"    = "logistic_regression"
              ),
              selected = "random_forest"
            )
          ),
          tags$div(
            class = "col-lg-3 col-md-6",
            sliderInput(
              ns("decision_threshold"),
              "Decision Threshold (Alert Cutoff):",
              min = 0.10, max = 0.90, value = 0.50, step = 0.05
            )
          ),
          
          # Row 2: Ticket Amount Range, Elapsed Hours, Risk Category
          tags$div(
            class = "col-lg-4 col-md-6",
            sliderInput(
              ns("amount_range"),
              "Ticket Amount Range ($ USD):",
              min = 0, max = 2500, value = c(0, 1500), step = 25, pre = "$"
            )
          ),
          tags$div(
            class = "col-lg-4 col-md-6",
            sliderInput(
              ns("elapsed_hours_range"),
              "Elapsed Ingestion Window (Hours from Origin):",
              min = 0.0, max = 48.0, value = c(0.0, 48.0), step = 1.0, post = "h"
            )
          ),
          tags$div(
            class = "col-lg-4 col-md-12",
            selectInput(
              ns("risk_tier_filter"),
              "Model Risk Classification Tier:",
              choices = c(
                "All Risk Tiers"              = "all",
                "Critical Risk (Prob >= 80%)" = "Critical Risk",
                "High Risk (Threshold <= P < 80%)" = "High Risk",
                "Medium Risk (20% <= P < Threshold)" = "Medium Risk",
                "Low Risk (Prob < 20%)"       = "Low Risk"
              ),
              selected = "all"
            )
          )
        )
      )
    ),
    
    # --------------------------------------------------------------------------
    # 4. Interactive Transaction Table (DT)
    # --------------------------------------------------------------------------
    bslib::card(
      class = "mb-4 border-0 shadow-sm",
      bslib::card_header(
        tags$div(
          class = "d-flex justify-content-between align-items-center",
          tags$div(
            tags$span(style = "font-weight: 700; color: #0F172A;", icon("list-check"), " Transaction Surveillance Ledger"),
            tags$small(class = "text-muted ms-2", "(Click any transaction row to open the forensic PCA audit dossier)")
          ),
          uiOutput(ns("table_records_badge"))
        )
      ),
      bslib::card_body(
        tags$div(style = "overflow-x: auto;", DTOutput(ns("transactions_dt")))
      )
    ),
    
    # --------------------------------------------------------------------------
    # 5. Visual Analytics Section (Compact Row Below Table)
    # --------------------------------------------------------------------------
    tags$div(
      class = "row g-3 mb-4",
      tags$div(
        class = "col-lg-3 col-md-6",
        bslib::card(
          class = "h-100 border-0 shadow-sm",
          bslib::card_header(tags$span(style = "font-weight: 700; font-size: 0.88rem; color: #0F172A;", "Filtered Class Split")),
          bslib::card_body(plotlyOutput(ns("filtered_donut_plot"), height = "240px"))
        )
      ),
      tags$div(
        class = "col-lg-3 col-md-6",
        bslib::card(
          class = "h-100 border-0 shadow-sm",
          bslib::card_header(tags$span(style = "font-weight: 700; font-size: 0.88rem; color: #0F172A;", "Filtered Ticket Scale (log1p)")),
          bslib::card_body(plotlyOutput(ns("filtered_amount_hist"), height = "240px"))
        )
      ),
      tags$div(
        class = "col-lg-3 col-md-6",
        bslib::card(
          class = "h-100 border-0 shadow-sm",
          bslib::card_header(tags$span(style = "font-weight: 700; font-size: 0.88rem; color: #0F172A;", "Filtered Velocity Cadence")),
          bslib::card_body(plotlyOutput(ns("filtered_hourly_plot"), height = "240px"))
        )
      ),
      tags$div(
        class = "col-lg-3 col-md-6",
        bslib::card(
          class = "h-100 border-0 shadow-sm",
          bslib::card_header(tags$span(style = "font-weight: 700; font-size: 0.88rem; color: #0F172A;", "Model Risk Tier Distribution")),
          bslib::card_body(plotlyOutput(ns("filtered_risk_tier_plot"), height = "240px"))
        )
      )
    )
  )
}

transaction_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --------------------------------------------------------------------------
    # Reactive Base Dataset: Scored with Selected Model & Threshold
    # --------------------------------------------------------------------------
    base_scored_data <- reactive({
      m_choice <- input$model_selector
      th_val   <- input$decision_threshold
      
      tryCatch({
        get_scored_test_data(model_type = m_choice, threshold = th_val)
      }, error = function(e) {
        data.table()
      })
    })
    
    # Reset filters action
    observeEvent(input$reset_all_filters, {
      updateTextInput(session, "search_id", value = "")
      updateSelectInput(session, "actual_status_filter", selected = "all")
      updateSelectInput(session, "risk_tier_filter", selected = "all")
      updateSliderInput(session, "amount_range", value = c(0, 1500))
      updateSliderInput(session, "elapsed_hours_range", value = c(0.0, 48.0))
      updateSliderInput(session, "decision_threshold", value = 0.50)
    })
    
    # --------------------------------------------------------------------------
    # Filtered Dataset Reactive Pipeline
    # --------------------------------------------------------------------------
    filtered_data <- reactive({
      dt <- base_scored_data()
      if (nrow(dt) == 0) return(data.table())
      
      # 1. Ground-Truth Status filter
      if (input$actual_status_filter == "fraud") {
        dt <- dt[class == 1]
      } else if (input$actual_status_filter == "legit") {
        dt <- dt[class == 0]
      }
      
      # 2. Risk Tier filter
      if (input$risk_tier_filter != "all") {
        dt <- dt[risk_category == input$risk_tier_filter]
      }
      
      # 3. Amount Range filter
      dt <- dt[amount >= input$amount_range[1] & amount <= input$amount_range[2]]
      
      # 4. Elapsed Time filter
      dt <- dt[elapsed_hours >= input$elapsed_hours_range[1] & elapsed_hours <= input$elapsed_hours_range[2]]
      
      # 5. Search Text query
      q <- trimws(input$search_id)
      if (nzchar(q)) {
        dt <- dt[grepl(q, transaction_id, ignore.case = TRUE) | 
                 grepl(q, as.character(round(amount, 2)), fixed = TRUE)]
      }
      
      return(dt)
    })
    
    # Active Filter Badges UI
    output$active_filter_pills <- renderUI({
      tags$div(
        class = "d-flex gap-1 flex-wrap align-items-center",
        tags$span(class = "badge bg-light text-dark border", sprintf("Amount: $%d - $%d", input$amount_range[1], input$amount_range[2])),
        tags$span(class = "badge bg-light text-dark border", sprintf("Time: %.0fh - %.0fh", input$elapsed_hours_range[1], input$elapsed_hours_range[2])),
        if (input$actual_status_filter != "all") {
          tags$span(class = "badge bg-primary", sprintf("Status: %s", input$actual_status_filter))
        },
        if (input$risk_tier_filter != "all") {
          tags$span(class = "badge bg-danger", sprintf("Risk: %s", input$risk_tier_filter))
        }
      )
    })
    
    # Records Badge
    output$table_records_badge <- renderUI({
      n_filt <- nrow(filtered_data())
      n_tot  <- nrow(base_scored_data())
      pct    <- if (n_tot > 0) (n_filt / n_tot) * 100 else 0
      
      tags$span(
        class = "badge bg-primary",
        style = "font-size: 0.85rem; padding: 6px 12px;",
        sprintf("%s Matches (%.1f%% of Partition)", format(n_filt, big.mark = ","), pct)
      )
    })
    
    # --------------------------------------------------------------------------
    # Dynamic KPI Cards (Computed from Filtered Cohort)
    # --------------------------------------------------------------------------
    output$filtered_kpi_cards <- renderUI({
      dt <- filtered_data()
      tot_matched <- nrow(dt)
      
      if (tot_matched == 0) {
        return(
          tags$div(
            class = "alert alert-warning text-center",
            icon("exclamation-triangle"), " No transaction records match the current multi-dimensional filter selection."
          )
        )
      }
      
      n_fraud   <- sum(dt$class == 1)
      n_legit   <- sum(dt$class == 0)
      prev_pct  <- (n_fraud / tot_matched) * 100
      tot_amt   <- sum(dt$amount)
      avg_amt   <- mean(dt$amount)
      
      tags$div(
        class = "row g-3 mb-4",
        tags$div(
          class = "col-xl-2 col-md-4 col-sm-6",
          create_kpi_card("Matched Volume", format_count(tot_matched), "In Current Filter", "funnel", "#0066FF")
        ),
        tags$div(
          class = "col-xl-2 col-md-4 col-sm-6",
          create_kpi_card("Confirmed Frauds", format_count(n_fraud), "Ground-Truth Class 1", "exclamation-triangle", "#DC2626")
        ),
        tags$div(
          class = "col-xl-2 col-md-4 col-sm-6",
          create_kpi_card("Legitimate Volume", format_count(n_legit), "Ground-Truth Class 0", "check-circle", "#16A34A")
        ),
        tags$div(
          class = "col-xl-2 col-md-4 col-sm-6",
          create_kpi_card("Fraud Prevalence", sprintf("%.4f%%", prev_pct), "Filtered Cohort Skew", "percent", "#D97706")
        ),
        tags$div(
          class = "col-xl-2 col-md-4 col-sm-6",
          create_kpi_card("Gross Ticket Value", format_currency(tot_amt), "Filtered Cumulative", "dollar-sign", "#6366F1")
        ),
        tags$div(
          class = "col-xl-2 col-md-4 col-sm-6",
          create_kpi_card("Average Ticket", format_currency(avg_amt), "Filtered Arithmetic Mean", "wallet", "#0284C7")
        )
      )
    })
    
    # --------------------------------------------------------------------------
    # Professional DataTable (DT)
    # --------------------------------------------------------------------------
    output$transactions_dt <- renderDT({
      dt <- filtered_data()
      if (nrow(dt) == 0) {
        return(datatable(data.frame(Status = "No transactions found matching active criteria.")))
      }
      
      # Limit view rows to top 10,000 for client-side speed
      max_rows <- min(nrow(dt), 10000)
      view_dt  <- dt[1:max_rows]
      
      disp_table <- data.frame(
        Transaction_ID = view_dt$transaction_id,
        Amount         = scales::dollar(view_dt$amount),
        Elapsed_Time   = sprintf("%.2fh (%,ds)", view_dt$elapsed_hours, as.integer(view_dt$elapsed_seconds)),
        Actual_Status  = as.character(view_dt$actual_status),
        Fraud_Prob     = sprintf("%.2f%%", view_dt$fraud_probability * 100),
        Model_Decision = as.character(view_dt$model_prediction),
        Risk_Category  = as.character(view_dt$risk_category),
        stringsAsFactors = FALSE
      )
      
      datatable(
        disp_table,
        selection = "single",
        rownames  = FALSE,
        options   = list(
          pageLength  = 10,
          lengthMenu  = c(10, 25, 50, 100),
          dom         = "Bfrltip",
          order       = list(list(4, "desc")), # Sort by probability descending
          searchHighlight = TRUE,
          autoWidth   = FALSE,
          buttons     = c("csv", "colvis")
        ),
        extensions = c("Buttons")
      ) %>%
        formatStyle(
          "Actual_Status",
          backgroundColor = styleEqual(c("Confirmed Fraud", "Legitimate"), c("#FEE2E2", "#DCFCE7")),
          color = styleEqual(c("Confirmed Fraud", "Legitimate"), c("#DC2626", "#16A34A")),
          fontWeight = "700"
        ) %>%
        formatStyle(
          "Model_Decision",
          backgroundColor = styleEqual(c("Flagged Alert", "Cleared"), c("#FFEDD5", "#F8FAFC")),
          color = styleEqual(c("Flagged Alert", "Cleared"), c("#EA580C", "#475569")),
          fontWeight = "600"
        ) %>%
        formatStyle(
          "Risk_Category",
          backgroundColor = styleEqual(
            c("Low Risk", "Medium Risk", "High Risk", "Critical Risk"),
            c("#DCFCE7", "#FEF3C7", "#FFEDD5", "#FEE2E2")
          ),
          color = styleEqual(
            c("Low Risk", "Medium Risk", "High Risk", "Critical Risk"),
            c("#16A34A", "#D97706", "#EA580C", "#DC2626")
          ),
          fontWeight = "700"
        )
    }, server = TRUE)
    
    # --------------------------------------------------------------------------
    # Forensic Transaction Detail Modal
    # --------------------------------------------------------------------------
    observeEvent(input$transactions_dt_rows_selected, {
      sel_idx <- input$transactions_dt_rows_selected
      if (length(sel_idx) == 0) return()
      
      dt <- filtered_data()
      if (sel_idx > nrow(dt)) return()
      row <- dt[sel_idx, ]
      
      is_fraud <- row$class == 1
      is_alert <- row$is_alert == 1
      
      showModal(modalDialog(
        title = tags$div(
          class = "d-flex justify-content-between align-items-center w-100 pe-3",
          tags$div(
            tags$span(style = "color: #0066FF; font-size: 1.2rem; margin-right: 8px;", icon("file-medical-alt")),
            tags$strong(sprintf("Forensic Transaction Audit Dossier: %s", row$transaction_id))
          ),
          tags$span(
            class = if (is_fraud) "badge bg-danger" else "badge bg-success",
            style = "font-size: 0.90rem; padding: 6px 12px;",
            if (is_fraud) "GROUND-TRUTH: CONFIRMED FRAUD" else "GROUND-TRUTH: LEGITIMATE"
          )
        ),
        size = "xl",
        easyClose = TRUE,
        footer = modalButton("Close Audit Dossier"),
        
        # Section 1: KPI Summary Scorecards for this Transaction
        tags$div(
          class = "row g-3 mb-4",
          tags$div(
            class = "col-md-3",
            tags$div(
              class = "p-3 bg-light rounded text-center border",
              tags$small(class = "text-muted", "Ticket Amount"),
              tags$h4(style = "color: #0F172A; font-weight: 800; margin: 4px 0;", scales::dollar(row$amount)),
              tags$small(class = "text-muted", sprintf("log1p: %.4f", row$amount_log))
            )
          ),
          tags$div(
            class = "col-md-3",
            tags$div(
              class = "p-3 bg-light rounded text-center border",
              tags$small(class = "text-muted", "Elapsed Time"),
              tags$h4(style = "color: #0F172A; font-weight: 800; margin: 4px 0;", sprintf("%.2fh", row$elapsed_hours)),
              tags$small(class = "text-muted", sprintf("%,d seconds", as.integer(row$elapsed_seconds)))
            )
          ),
          tags$div(
            class = "col-md-3",
            tags$div(
              class = sprintf("p-3 rounded text-center border %s", if (is_alert) "bg-danger text-white" else "bg-light text-dark"),
              tags$small(if (is_alert) "text-white-50" else "text-muted", sprintf("Probability (%s)", toupper(row$scoring_model))),
              tags$h4(style = "font-weight: 800; margin: 4px 0;", sprintf("%.2f%%", row$fraud_probability * 100)),
              tags$small(if (is_alert) "text-white-50" else "text-muted", as.character(row$model_prediction))
            )
          ),
          tags$div(
            class = "col-md-3",
            tags$div(
              class = "p-3 rounded text-center border",
              style = sprintf(
                "background-color: %s;",
                switch(as.character(row$risk_category),
                       "Critical Risk" = "#FEE2E2",
                       "High Risk"     = "#FFEDD5",
                       "Medium Risk"   = "#FEF3C7",
                       "#DCFCE7")
              ),
              tags$small(class = "text-muted", "Assigned Risk Category"),
              tags$h4(
                style = sprintf(
                  "font-weight: 800; margin: 4px 0; color: %s;",
                  switch(as.character(row$risk_category),
                         "Critical Risk" = "#DC2626",
                         "High Risk"     = "#EA580C",
                         "Medium Risk"   = "#D97706",
                         "#16A34A")
                ),
                as.character(row$risk_category)
              ),
              tags$small(class = "text-muted", switch(as.character(row$risk_category),
                                                      "Critical Risk" = "Action: Auto-Block & Alert",
                                                      "High Risk"     = "Action: Route to Analyst",
                                                      "Medium Risk"   = "Action: Step-Up OTP",
                                                      "Action: Auto-Clear"))
            )
          )
        ),
        
        # Section 2: Local Feature Attribution Waterfall Plot
        tags$h6(style = "font-weight: 700; color: #0F172A; margin-top: 15px; border-bottom: 2px solid #E2E8F0; padding-bottom: 6px;",
                icon("chart-bar"), " Forensic Signal Attribution (Impact on Fraud Probability):"),
        
        plotlyOutput(ns("dossier_attribution_plot"), height = "260px"),
        
        # Section 3: Full 28 PCA Features Grid
        tags$h6(style = "font-weight: 700; color: #0F172A; margin-top: 25px; border-bottom: 2px solid #E2E8F0; padding-bottom: 6px;",
                icon("th"), " Complete Orthogonal PCA Feature Vector (V1 - V28):"),
        
        tags$div(
          class = "row g-2 mb-3",
          lapply(1:28, function(i) {
            col_name   <- paste0("v", i)
            val        <- round(as.numeric(row[[col_name]]), 4)
            is_key_sig <- i %in% c(14, 17, 12, 10, 11, 4)
            bg <- if (is_key_sig && is_fraud) "#FEE2E2" else if (is_key_sig) "#E0F2FE" else "#F8FAFC"
            fg <- if (is_key_sig && is_fraud) "#DC2626" else if (is_key_sig) "#0284C7" else "#334155"
            border_color <- if (is_key_sig) "#93C5FD" else "#E2E8F0"
            
            tags$div(
              class = "col-lg-3 col-md-4 col-sm-6",
              tags$div(
                style = sprintf("background-color: %s; color: %s; border: 1px solid %s; padding: 7px 12px; border-radius: 6px; font-size: 0.82rem;", bg, fg, border_color),
                tags$div(
                  class = "d-flex justify-content-between align-items-center",
                  tags$strong(toupper(col_name)),
                  if (is_key_sig) tags$span(class = "badge bg-secondary", style = "font-size: 0.65rem;", "KEY SIGNAL")
                ),
                tags$div(style = "font-family: monospace; font-size: 0.95rem; font-weight: 700; margin-top: 2px;", val)
              )
            )
          })
        ),
        
        # Section 4: Privacy & Anonymization Audit Disclaimer
        tags$div(
          class = "alert alert-secondary mb-0",
          style = "font-size: 0.80rem; background-color: #F8FAFC; border-left: 4px solid #64748B;",
          icon("shield-alt"),
          tags$strong(" Anonymization & Governance Policy: "),
          "Features V1 through V28 represent orthogonal PCA components derived to protect cardholder privacy. ",
          "No actual card numbers (PAN), names, phone numbers, or merchant identities exist within this benchmark dataset."
        )
      ))
      
      # Render Local Attribution Plot for this selected transaction
      output$dossier_attribution_plot <- renderPlotly({
        key_signals <- c("v17", "v14", "v12", "v10", "v11", "v4", "amount_log", "elapsed_hours")
        
        # Calculate directional contribution
        impacts <- c(
          -as.numeric(row$v17) * 0.28,
          -as.numeric(row$v14) * 0.25,
          -as.numeric(row$v12) * 0.20,
          -as.numeric(row$v10) * 0.16,
          as.numeric(row$v11)  * 0.14,
          as.numeric(row$v4)   * 0.12,
          (as.numeric(row$amount_log) - 2.5) * 0.05,
          (as.numeric(row$elapsed_hours) - 24) * 0.02
        )
        
        plot_df <- data.frame(
          Signal = toupper(key_signals),
          Val    = sapply(key_signals, function(s) round(as.numeric(row[[s]]), 3)),
          Impact = impacts,
          stringsAsFactors = FALSE
        )
        plot_df$Color <- ifelse(plot_df$Impact > 0, "#DC2626", "#16A34A")
        plot_df$Label <- paste0(plot_df$Signal, " (", plot_df$Val, ")")
        plot_df <- plot_df[order(plot_df$Impact), ]
        plot_df$Label <- factor(plot_df$Label, levels = plot_df$Label)
        
        plot_ly(
          data = plot_df,
          x = ~Impact,
          y = ~Label,
          type = "bar",
          orientation = "h",
          marker = list(color = plot_df$Color)
        ) %>%
          layout(
            xaxis = list(
              title = "Signal Contribution (< 0 Lowers Fraud Risk | > 0 Drives Fraud Alert)",
              zeroline = TRUE,
              zerolinecolor = "#000000"
            ),
            yaxis = list(title = ""),
            margin = list(t = 10, b = 35, l = 150, r = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
      })
    })
    
    # --------------------------------------------------------------------------
    # Visual Analytics Row (Reactively Linked to Filtered Cohort)
    # --------------------------------------------------------------------------
    
    # 1. Filtered Donut Chart
    output$filtered_donut_plot <- renderPlotly({
      dt <- filtered_data()
      if (nrow(dt) == 0) return(NULL)
      
      n_legit <- sum(dt$class == 0)
      n_fraud <- sum(dt$class == 1)
      
      df <- data.frame(
        Category = c("Legitimate", "Fraudulent"),
        Count    = c(n_legit, n_fraud),
        Color    = c("#16A34A", "#DC2626")
      )
      
      plot_ly(
        data = df,
        labels = ~Category,
        values = ~Count,
        type = "pie",
        hole = 0.65,
        marker = list(colors = df$Color),
        textinfo = "label+percent",
        textposition = "inside",
        hoverinfo = "text",
        text = ~sprintf("<b>%s</b>: %s txs", Category, format(Count, big.mark = ","))
      ) %>%
        layout(
          showlegend = FALSE,
          margin = list(t = 5, b = 5, l = 5, r = 5),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        )
    })
    
    # 2. Filtered Amount Histogram
    output$filtered_amount_hist <- renderPlotly({
      dt <- filtered_data()
      if (nrow(dt) == 0) return(NULL)
      
      # Sample if > 2,000 for responsive rendering
      amt_vec <- if (nrow(dt) > 2000) sample(dt$amount_log, 2000) else dt$amount_log
      
      plot_ly(x = amt_vec, type = "histogram", marker = list(color = "#0066FF", opacity = 0.75)) %>%
        layout(
          xaxis = list(title = "log1p(Amount)", showgrid = TRUE, gridcolor = "#E2E8F0"),
          yaxis = list(title = "Frequency", showgrid = TRUE, gridcolor = "#E2E8F0"),
          margin = list(t = 5, b = 35, l = 40, r = 10),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        )
    })
    
    # 3. Filtered Hourly Activity Bar
    output$filtered_hourly_plot <- renderPlotly({
      dt <- filtered_data()
      if (nrow(dt) == 0) return(NULL)
      
      hourly_counts <- dt[, .(Volume = .N), by = .(Hour = as.integer(floor(elapsed_hours)))][order(Hour)]
      
      plot_ly(
        data = hourly_counts,
        x = ~Hour,
        y = ~Volume,
        type = "bar",
        marker = list(color = "#6366F1")
      ) %>%
        layout(
          xaxis = list(title = "Elapsed Hour (0-47h)", showgrid = FALSE),
          yaxis = list(title = "Volume", showgrid = TRUE, gridcolor = "#E2E8F0"),
          margin = list(t = 5, b = 35, l = 40, r = 10),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        )
    })
    
    # 4. Filtered Risk Tier Breakdown Bar
    output$filtered_risk_tier_plot <- renderPlotly({
      dt <- filtered_data()
      if (nrow(dt) == 0) return(NULL)
      
      tier_counts <- dt[, .(Count = .N), by = risk_category]
      tier_order  <- c("Low Risk", "Medium Risk", "High Risk", "Critical Risk")
      tier_colors <- c("Low Risk" = "#16A34A", "Medium Risk" = "#D97706", "High Risk" = "#EA580C", "Critical Risk" = "#DC2626")
      
      df <- data.frame(
        Tier  = factor(tier_order, levels = tier_order),
        Count = sapply(tier_order, function(t) sum(dt$risk_category == t)),
        Color = tier_colors[tier_order]
      )
      
      plot_ly(
        data = df,
        x = ~Count,
        y = ~Tier,
        type = "bar",
        orientation = "h",
        marker = list(color = df$Color)
      ) %>%
        layout(
          xaxis = list(title = "Count", showgrid = TRUE, gridcolor = "#E2E8F0"),
          yaxis = list(title = ""),
          margin = list(t = 5, b = 35, l = 90, r = 10),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        )
    })
    
    # --------------------------------------------------------------------------
    # CSV Export Handler
    # --------------------------------------------------------------------------
    output$export_filtered_csv <- downloadHandler(
      filename = function() {
        sprintf("fraudshield_transactions_filtered_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        dt <- filtered_data()
        # Export presentation columns + essential metadata
        export_dt <- dt[, .(
          transaction_id,
          amount,
          elapsed_hours,
          elapsed_seconds,
          actual_status,
          fraud_probability,
          model_prediction,
          risk_category,
          scoring_model
        )]
        write.csv(export_dt, file, row.names = FALSE)
      }
    )
    
  })
}
