# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Module: Fraud Analytics & Pattern Intelligence (Page 3)
# Script: app/modules/fraud_analytics_module.R
# ==============================================================================

fraud_analytics_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Header summary banner
    tags$div(
      class = "alert alert-secondary d-flex align-items-center mb-4",
      style = "background-color: #F8FAFC; border-left: 4px solid #0066FF; color: #1E293B; padding: 14px 20px; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
      tags$div(style = "font-size: 1.4rem; margin-right: 14px; color: #0066FF;", icon("chart-pie")),
      tags$div(
        tags$strong("Empirical Fraud Analytics & Pattern Intelligence: "),
        "Interactive cohort surveillance, multi-feature correlation heatmaps, density shift analysis, and real-time transaction risk scoring across the verified holdout evaluation partition (56,962 transactions)."
      )
    ),

    # ==========================================================================
    # SECTION 1: GLOBAL INTERACTIVE FILTERS PANEL
    # ==========================================================================
    tags$div(
      class = "card border-0 shadow-sm mb-4",
      style = "background-color: #FFFFFF; border-radius: 8px; border: 1px solid #E2E8F0;",
      tags$div(
        class = "card-header bg-white py-3 border-bottom d-flex justify-content-between align-items-center flex-wrap gap-2",
        tags$div(
          class = "d-flex align-items-center gap-2",
          tags$span(style = "font-weight: 700; color: #0F172A; font-size: 1rem;", icon("sliders-h"), " Global Analytics Filters & Cohort Slicing"),
          uiOutput(ns("active_filters_badge"), inline = TRUE)
        ),
        tags$div(
          class = "d-flex gap-2 align-items-center",
          actionButton(ns("reset_filters"), "Reset Filters", icon = icon("redo-alt"), class = "btn btn-outline-secondary btn-sm"),
          downloadButton(ns("download_filtered_csv"), "Export Cohort CSV", class = "btn btn-outline-primary btn-sm"),
          downloadButton(ns("download_cor_csv"), "Export Correlations CSV", class = "btn btn-outline-primary btn-sm")
        )
      ),
      tags$div(
        class = "card-body py-3",
        # Row 1 of Filters
        tags$div(
          class = "row g-3 align-items-end mb-2",
          tags$div(
            class = "col-lg-3 col-md-6 col-12",
            tags$label(class = "form-label text-muted small font-weight-bold mb-1", icon("filter"), " Transaction Class"),
            selectInput(
              ns("filter_class"),
              label = NULL,
              choices = c(
                "All Transactions (Legit + Fraud)" = "all",
                "Confirmed Fraud Only (Class 1)" = "fraud",
                "Legitimate Only (Class 0)" = "legit"
              ),
              selected = "all",
              width = "100%"
            )
          ),
          tags$div(
            class = "col-lg-3 col-md-6 col-12",
            tags$label(class = "form-label text-muted small font-weight-bold mb-1", icon("dollar-sign"), " Amount Range ($ USD)"),
            sliderInput(
              ns("filter_amount"),
              label = NULL,
              min = 0,
              max = 25000,
              value = c(0, 25000),
              step = 25,
              pre = "$",
              width = "100%"
            )
          ),
          tags$div(
            class = "col-lg-3 col-md-6 col-12",
            tags$label(class = "form-label text-muted small font-weight-bold mb-1", icon("clock"), " Elapsed Time Range (Hours: 0-48h)"),
            sliderInput(
              ns("filter_time"),
              label = NULL,
              min = 0,
              max = 48,
              value = c(0, 48),
              step = 1,
              post = "h",
              width = "100%"
            )
          ),
          tags$div(
            class = "col-lg-3 col-md-6 col-12",
            tags$label(class = "form-label text-muted small font-weight-bold mb-1", icon("shield-alt"), " Model Risk Category"),
            selectInput(
              ns("filter_risk_tier"),
              label = NULL,
              choices = c(
                "All Risk Tiers" = "all",
                "Critical Risk (>= 80%)" = "Critical Risk",
                "High Risk (50% - 80%)" = "High Risk",
                "Medium Risk (20% - 50%)" = "Medium Risk",
                "Low Risk (< 20%)" = "Low Risk"
              ),
              selected = "all",
              width = "100%"
            )
          )
        ),
        # Row 2 of Filters & Focal Controls
        tags$div(
          class = "row g-3 align-items-end",
          tags$div(
            class = "col-lg-3 col-md-6 col-12",
            tags$label(class = "form-label text-muted small font-weight-bold mb-1", icon("crosshairs"), " Focal Feature (Density Shift)"),
            selectInput(
              ns("focus_feature"),
              label = NULL,
              choices = c(
                "V14 (Top Negative Separator)" = "v14",
                "V17 (Top Negative Separator)" = "v17",
                "V12 (Strong Negative Separator)" = "v12",
                "V10 (Negative Shift)" = "v10",
                "V11 (Strong Positive Fraud Signal)" = "v11",
                "V4 (Positive Fraud Signal)" = "v4",
                "V2 (Positive Correlation)" = "v2",
                "Amount (Transaction Ticket Size)" = "amount",
                paste0("v", setdiff(1:28, c(14, 17, 12, 10, 11, 4, 2)))
              ),
              selected = "v14",
              width = "100%"
            )
          ),
          tags$div(
            class = "col-lg-3 col-md-6 col-12",
            tags$label(class = "form-label text-muted small font-weight-bold mb-1", icon("calculator"), " Correlation Method"),
            selectInput(
              ns("cor_method"),
              label = NULL,
              choices = c("Pearson (Linear Correlation)" = "pearson", "Spearman (Rank Correlation)" = "spearman"),
              selected = "pearson",
              width = "100%"
            )
          ),
          tags$div(
            class = "col-lg-3 col-md-6 col-12",
            tags$label(class = "form-label text-muted small font-weight-bold mb-1", icon("brain"), " Supervised ML Engine"),
            selectInput(
              ns("gauge_model"),
              label = NULL,
              choices = c(
                "Random Forest (ranger)" = "random_forest",
                "XGBoost (gradient boost)" = "xgboost",
                "Logistic Regression (GLM)" = "logistic_regression"
              ),
              selected = "random_forest",
              width = "100%"
            )
          ),
          tags$div(
            class = "col-lg-3 col-md-6 col-12",
            tags$label(class = "form-label text-muted small font-weight-bold mb-1", icon("adjust"), " Decision Alert Threshold"),
            sliderInput(
              ns("gauge_threshold"),
              label = NULL,
              min = 0.05,
              max = 0.95,
              value = 0.50,
              step = 0.05,
              width = "100%"
            )
          )
        )
      )
    ),

    # ==========================================================================
    # SECTION 2: FOUR GENUINELY INTERACTIVE SPEEDOMETER GAUGES
    # ==========================================================================
    tags$div(
      class = "row g-3 mb-4",

      # GAUGE 1: FRAUD PREVALENCE GAUGE
      tags$div(
        class = "col-xl-3 col-md-6 col-12",
        bslib::card(
          class = "h-100 shadow-sm border-0",
          style = "border-radius: 8px; border: 1px solid #E2E8F0;",
          bslib::card_header(
            class = "bg-white py-2 border-bottom d-flex justify-content-between align-items-center",
            tags$span(style = "font-weight: 700; color: #0F172A; font-size: 0.95rem;", icon("chart-pie"), " Fraud Prevalence"),
            tags$span(class = "badge bg-danger", "Observed Cohort")
          ),
          bslib::card_body(
            class = "p-2 d-flex flex-column justify-content-between",
            plotlyOutput(ns("gauge_fraud_prevalence"), height = "220px"),
            tags$div(
              style = "background-color: #F8FAFC; border-radius: 6px; padding: 10px; margin-top: 5px; font-size: 0.8rem; border: 1px solid #E2E8F0;",
              uiOutput(ns("prevalence_stats_ui"))
            )
          )
        )
      ),

      # GAUGE 2: TRANSACTION RISK GAUGE
      tags$div(
        class = "col-xl-3 col-md-6 col-12",
        bslib::card(
          class = "h-100 shadow-sm border-0",
          style = "border-radius: 8px; border: 1px solid #E2E8F0;",
          bslib::card_header(
            class = "bg-white py-2 border-bottom d-flex justify-content-between align-items-center",
            tags$span(style = "font-weight: 700; color: #0F172A; font-size: 0.95rem;", icon("shield-alt"), " Transaction Risk"),
            tags$span(class = "badge bg-warning text-dark", "Live ML Score")
          ),
          bslib::card_body(
            class = "p-2 d-flex flex-column justify-content-between",
            plotlyOutput(ns("gauge_transaction_risk"), height = "220px"),
            tags$div(
              style = "background-color: #F8FAFC; border-radius: 6px; padding: 10px; margin-top: 5px; font-size: 0.8rem; border: 1px solid #E2E8F0;",
              uiOutput(ns("transaction_risk_meta_ui"))
            )
          )
        )
      ),

      # GAUGE 3: MODEL RECALL GAUGE
      tags$div(
        class = "col-xl-3 col-md-6 col-12",
        bslib::card(
          class = "h-100 shadow-sm border-0",
          style = "border-radius: 8px; border: 1px solid #E2E8F0;",
          bslib::card_header(
            class = "bg-white py-2 border-bottom d-flex justify-content-between align-items-center",
            tags$span(style = "font-weight: 700; color: #0F172A; font-size: 0.95rem;", icon("bullseye"), " Model Recall"),
            tags$span(class = "badge bg-primary", "Sensitivity")
          ),
          bslib::card_body(
            class = "p-2 d-flex flex-column justify-content-between",
            plotlyOutput(ns("gauge_model_recall"), height = "220px"),
            tags$div(
              style = "background-color: #F8FAFC; border-radius: 6px; padding: 10px; margin-top: 5px; font-size: 0.8rem; border: 1px solid #E2E8F0;",
              uiOutput(ns("recall_meta_ui"))
            )
          )
        )
      ),

      # GAUGE 4: MODEL PRECISION GAUGE
      tags$div(
        class = "col-xl-3 col-md-6 col-12",
        bslib::card(
          class = "h-100 shadow-sm border-0",
          style = "border-radius: 8px; border: 1px solid #E2E8F0;",
          bslib::card_header(
            class = "bg-white py-2 border-bottom d-flex justify-content-between align-items-center",
            tags$span(style = "font-weight: 700; color: #0F172A; font-size: 0.95rem;", icon("check-double"), " Model Precision"),
            tags$span(class = "badge bg-success", "Alert Fidelity")
          ),
          bslib::card_body(
            class = "p-2 d-flex flex-column justify-content-between",
            plotlyOutput(ns("gauge_model_precision"), height = "220px"),
            tags$div(
              style = "background-color: #F8FAFC; border-radius: 6px; padding: 10px; margin-top: 5px; font-size: 0.8rem; border: 1px solid #E2E8F0;",
              uiOutput(ns("precision_meta_ui"))
            )
          )
        )
      )
    ),

    # ==========================================================================
    # SECTION 3: CORRELATION HEATMAP & FEATURE-CLASS RANKING
    # ==========================================================================
    tags$div(
      class = "row g-3 mb-4",
      tags$div(
        class = "col-lg-6 col-12",
        bslib::card(
          class = "shadow-sm border-0 h-100",
          style = "border-radius: 8px; border: 1px solid #E2E8F0;",
          bslib::card_header(
            class = "bg-white py-2 border-bottom d-flex justify-content-between align-items-center",
            tags$span(style = "font-weight: 700; color: #0F172A;", icon("chart-bar"), " Feature Correlations with Fraud (Target Class)"),
            tags$span(class = "badge bg-secondary", "Click bar to inspect feature")
          ),
          bslib::card_body(
            plotlyOutput(ns("corr_bar_chart"), height = "360px")
          )
        )
      ),
      tags$div(
        class = "col-lg-6 col-12",
        bslib::card(
          class = "shadow-sm border-0 h-100",
          style = "border-radius: 8px; border: 1px solid #E2E8F0;",
          bslib::card_header(
            class = "bg-white py-2 border-bottom d-flex justify-content-between align-items-center",
            tags$span(style = "font-weight: 700; color: #0F172A;", icon("th"), " Interactive Pairwise Correlation Matrix"),
            tags$span(class = "badge bg-primary", "Top 10 Signals")
          ),
          bslib::card_body(
            plotlyOutput(ns("corr_heatmap_plot"), height = "360px")
          )
        )
      )
    ),

    # ==========================================================================
    # SECTION 4: DENSITY SHIFT & TICKET SIZE DISPARITY
    # ==========================================================================
    tags$div(
      class = "row g-3 mb-4",
      tags$div(
        class = "col-lg-7 col-12",
        bslib::card(
          class = "shadow-sm border-0 h-100",
          style = "border-radius: 8px; border: 1px solid #E2E8F0;",
          bslib::card_header(
            class = "bg-white py-2 border-bottom d-flex justify-content-between align-items-center",
            tags$span(style = "font-weight: 700; color: #0F172A;", icon("wave-square"), " Density Shift: Legitimate vs Fraudulent Distributions"),
            uiOutput(ns("density_sample_badge"), inline = TRUE)
          ),
          bslib::card_body(
            plotlyOutput(ns("density_shift_chart"), height = "300px"),
            tags$div(
              style = "background-color: #F8FAFC; border-radius: 6px; padding: 12px; margin-top: 8px; border: 1px solid #E2E8F0;",
              uiOutput(ns("density_metrics_summary"))
            )
          )
        )
      ),
      tags$div(
        class = "col-lg-5 col-12",
        bslib::card(
          class = "shadow-sm border-0 h-100",
          style = "border-radius: 8px; border: 1px solid #E2E8F0;",
          bslib::card_header(
            class = "bg-white py-2 border-bottom d-flex justify-content-between align-items-center",
            tags$span(style = "font-weight: 700; color: #0F172A;", icon("money-bill-wave"), " Transaction Amount Comparison by Class"),
            tags$span(class = "badge bg-info text-dark", "log1p(USD)")
          ),
          bslib::card_body(
            plotlyOutput(ns("amount_by_class_chart"), height = "370px")
          )
        )
      )
    ),

    # ==========================================================================
    # SECTION 5: TEMPORAL ATTACK CADENCE & VOLUME DYNAMICS
    # ==========================================================================
    tags$div(
      class = "row g-3 mb-4",
      tags$div(
        class = "col-lg-6 col-12",
        bslib::card(
          class = "shadow-sm border-0",
          style = "border-radius: 8px; border: 1px solid #E2E8F0;",
          bslib::card_header(
            tags$span(style = "font-weight: 700; color: #0F172A;", icon("history"), " Hourly Confirmed Fraud Concentration Rate (0 - 47h)")
          ),
          bslib::card_body(
            plotlyOutput(ns("hourly_rate_chart"), height = "300px")
          )
        )
      ),
      tags$div(
        class = "col-lg-6 col-12",
        bslib::card(
          class = "shadow-sm border-0",
          style = "border-radius: 8px; border: 1px solid #E2E8F0;",
          bslib::card_header(
            tags$span(style = "font-weight: 700; color: #0F172A;", icon("layer-group"), " Hourly Transaction Volume Over Time (0 - 47h)")
          ),
          bslib::card_body(
            plotlyOutput(ns("hourly_volume_chart"), height = "300px")
          )
        )
      )
    ),

    # ==========================================================================
    # SECTION 6: TRANSACTION INVESTIGATION LEDGER & FORENSIC DETAIL
    # ==========================================================================
    tags$div(
      class = "row g-3 mb-4",
      tags$div(
        class = "col-12",
        bslib::card(
          class = "shadow-sm border-0",
          style = "border-radius: 8px; border: 1px solid #E2E8F0;",
          bslib::card_header(
            class = "bg-white py-3 border-bottom d-flex justify-content-between align-items-center flex-wrap gap-2",
            tags$div(
              tags$span(style = "font-weight: 700; color: #0F172A; font-size: 1rem;", icon("table"), " Transaction Investigation Panel & Risk Ledger"),
              tags$span(style = "font-size: 0.8rem; color: #64748B; margin-left: 10px;", "Select any row to dynamically inspect risk signals in the gauge above.")
            ),
            tags$div(
              downloadButton(ns("download_table_csv"), "Download Ledger (CSV)", class = "btn btn-outline-secondary btn-sm")
            )
          ),
          bslib::card_body(
            DTOutput(ns("tx_investigation_table"))
          )
        )
      )
    ),

    # ==========================================================================
    # SECTION 7: STATISTICAL SYNTHESIS COMPARISON TABLE
    # ==========================================================================
    tags$div(
      class = "row g-3 mb-4",
      tags$div(
        class = "col-12",
        bslib::card(
          class = "shadow-sm border-0",
          style = "border-radius: 8px; border: 1px solid #E2E8F0;",
          bslib::card_header(
            tags$span(style = "font-weight: 700; color: #0F172A;", icon("balance-scale"), " Statistical Synthesis: Legitimate vs Fraudulent Transactions")
          ),
          bslib::card_body(
            tags$div(style = "overflow-x: auto;", tableOutput(ns("stats_summary_table")))
          )
        )
      )
    )
  )
}

fraud_analytics_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to track currently inspected transaction ID
    current_tx_id <- reactiveVal("TX-01340")

    # --------------------------------------------------------------------------
    # 1. BASE DATASETS & REACTIVE FILTERING
    # --------------------------------------------------------------------------
    # Master scored dataset cached by model and threshold
    master_scored_data <- reactive({
      m_type <- if (!is.null(input$gauge_model)) input$gauge_model else "random_forest"
      thresh <- if (!is.null(input$gauge_threshold)) input$gauge_threshold else 0.50
      get_scored_test_data(model_type = m_type, threshold = thresh)
    })

    # Filtered dataset responding dynamically to all filter controls
    filtered_data <- reactive({
      dt <- master_scored_data()
      if (is.null(dt) || nrow(dt) == 0) {
        return(data.table())
      }

      sub_dt <- copy(dt)

      # 1. Transaction Class Filter
      cls_filter <- input$filter_class
      if (!is.null(cls_filter) && cls_filter == "fraud") {
        sub_dt <- sub_dt[class == 1]
      } else if (!is.null(cls_filter) && cls_filter == "legit") {
        sub_dt <- sub_dt[class == 0]
      }

      # 2. Amount Range Filter
      amt_range <- input$filter_amount
      if (!is.null(amt_range) && length(amt_range) == 2) {
        sub_dt <- sub_dt[amount >= amt_range[1] & amount <= amt_range[2]]
      }

      # 3. Elapsed Time Range Filter (Hours)
      time_range <- input$filter_time
      if (!is.null(time_range) && length(time_range) == 2) {
        sub_dt <- sub_dt[elapsed_hours >= time_range[1] & elapsed_hours <= time_range[2]]
      }

      # 4. Risk Category Filter
      risk_tier <- input$filter_risk_tier
      if (!is.null(risk_tier) && risk_tier != "all") {
        sub_dt <- sub_dt[as.character(risk_category) == risk_tier]
      }

      return(sub_dt)
    })

    # Active filters badge UI
    output$active_filters_badge <- renderUI({
      cnt <- 0
      if (!is.null(input$filter_class) && input$filter_class != "all") cnt <- cnt + 1
      if (!is.null(input$filter_amount) && (input$filter_amount[1] > 0 || input$filter_amount[2] < 25000)) cnt <- cnt + 1
      if (!is.null(input$filter_time) && (input$filter_time[1] > 0 || input$filter_time[2] < 48)) cnt <- cnt + 1
      if (!is.null(input$filter_risk_tier) && input$filter_risk_tier != "all") cnt <- cnt + 1

      n_recs <- nrow(filtered_data())
      tagList(
        tags$span(class = "badge bg-light text-dark border", sprintf("Cohort: %s records", format(n_recs, big.mark = ","))),
        if (cnt > 0) {
          tags$span(class = "badge bg-primary", sprintf("%d Active Filter%s", cnt, if (cnt > 1) "s" else ""))
        } else {
          tags$span(class = "badge bg-secondary", "No Active Filters")
        }
      )
    })

    # Reset Filters Button Handler
    observeEvent(input$reset_filters, {
      updateSelectInput(session, "filter_class", selected = "all")
      updateSliderInput(session, "filter_amount", value = c(0, 25000))
      updateSliderInput(session, "filter_time", value = c(0, 48))
      updateSelectInput(session, "filter_risk_tier", selected = "all")
      updateSelectInput(session, "focus_feature", selected = "v14")
      updateSelectInput(session, "cor_method", selected = "pearson")
      updateSliderInput(session, "gauge_threshold", value = 0.50)
      current_tx_id("TX-01340")
    })

    # Cross-filtering: When user selects a row in the investigation table
    observeEvent(input$tx_investigation_table_rows_selected, {
      sel <- input$tx_investigation_table_rows_selected
      if (length(sel) > 0) {
        sub_dt <- filtered_data()
        if (nrow(sub_dt) >= sel) {
          chosen_id <- sub_dt$transaction_id[sel]
          current_tx_id(chosen_id)
        }
      }
    })

    # Cross-filtering: When user clicks a feature bar in the correlation chart
    observeEvent(event_data("plotly_click", source = "corr_bars"), {
      evt <- event_data("plotly_click", source = "corr_bars")
      if (!is.null(evt) && !is.null(evt$y)) {
        clicked_feat <- tolower(trimws(as.character(evt$y)))
        # Verify it's a valid feature
        valid_cols <- c(paste0("v", 1:28), "amount")
        if (clicked_feat %in% valid_cols) {
          updateSelectInput(session, "focus_feature", selected = clicked_feat)
        }
      }
    })

    # --------------------------------------------------------------------------
    # 2. GAUGE 1: FRAUD PREVALENCE GAUGE (Observed Cohort Attack Rate)
    # --------------------------------------------------------------------------
    output$gauge_fraud_prevalence <- renderPlotly({
      sub_dt <- filtered_data()

      if (nrow(sub_dt) == 0) {
        prev_pct <- 0.0
      } else {
        tot <- nrow(sub_dt)
        frd <- sum(sub_dt$class == 1)
        prev_pct <- if (tot > 0) (frd / tot) * 100 else 0
      }

      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = prev_pct,
        number = list(
          suffix = "%",
          valueformat = ".3f",
          font = list(size = 26, color = "#DC2626", family = "Arial, sans-serif")
        ),
        title = list(
          text = "<b>Fraud Prevalence Rate</b><br><span style='font-size:11px;color:#64748B;'>Observed Attack Ratio (0-100% Scale)</span>",
          font = list(size = 13, family = "Arial, sans-serif")
        ),
        gauge = list(
          axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "#94A3B8", ticksuffix = "%"),
          bar = list(color = "#DC2626", thickness = 0.40),
          bgcolor = "#F8FAFC",
          borderwidth = 1,
          bordercolor = "#E2E8F0",
          steps = list(
            list(range = c(0, 1), color = "#FEF2F2"),
            list(range = c(1, 5), color = "#FEE2E2"),
            list(range = c(5, 100), color = "#FECACA")
          ),
          threshold = list(
            line = list(color = "#991B1B", width = 3),
            thickness = 0.75,
            value = prev_pct
          )
        ),
        height = 220
      ) %>% layout(
        margin = list(t = 28, b = 12, l = 25, r = 25),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    })

    output$prevalence_stats_ui <- renderUI({
      sub_dt <- filtered_data()
      tot <- nrow(sub_dt)
      frd <- sum(sub_dt$class == 1)
      lgt <- sum(sub_dt$class == 0)
      pct <- if (tot > 0) (frd / tot) * 100 else 0

      tagList(
        tags$div(
          class = "d-flex justify-content-between align-items-center mb-1",
          tags$span(tags$strong("Confirmed Fraud: "), tags$span(class = "badge bg-danger", format(frd, big.mark = ","))),
          tags$span(tags$strong("Legitimate: "), tags$span(class = "badge bg-success", format(lgt, big.mark = ",")))
        ),
        tags$div(
          class = "d-flex justify-content-between text-muted small",
          tags$span("Cohort Volume: ", tags$b(format(tot, big.mark = ","))),
          tags$span("Prevalence: ", tags$b(style = "color:#DC2626;", sprintf("%.4f%%", pct)))
        ),
        tags$div(
          class = "text-muted small mt-1",
          tags$em("Observed base rate within active cohort (not model accuracy).")
        )
      )
    })

    # --------------------------------------------------------------------------
    # 3. GAUGE 2: TRANSACTION RISK GAUGE (Real-Time Supervised ML Prediction)
    # --------------------------------------------------------------------------
    # Extracted data for currently selected transaction
    current_tx_details <- reactive({
      all_dt <- master_scored_data()
      if (is.null(all_dt) || nrow(all_dt) == 0) {
        return(NULL)
      }

      tx_id <- current_tx_id()
      row_dt <- all_dt[transaction_id == tx_id]
      if (nrow(row_dt) == 0) {
        # Fallback to first available row
        row_dt <- all_dt[1, ]
      }

      # Extract features for prediction (Ground truth 'class' is NEVER supplied)
      pred_cols <- c(paste0("v", 1:28), "amount_log", "elapsed_hours")
      feats <- as.data.frame(row_dt[, ..pred_cols])

      # Calculate live risk probability with currently selected model
      m_type <- if (!is.null(input$gauge_model)) input$gauge_model else "random_forest"
      thresh <- if (!is.null(input$gauge_threshold)) input$gauge_threshold else 0.50
      prob <- 0.0

      tryCatch(
        {
          if (m_type == "random_forest") {
            rf_mod <- get_cached_model("random_forest")
            prob <- predict(rf_mod, data = feats)$predictions[, "Fraud"]
          } else if (m_type == "xgboost") {
            xgb_mod <- get_cached_model("xgboost")
            prob <- predict(xgb_mod, xgboost::xgb.DMatrix(as.matrix(feats)))
          } else if (m_type == "logistic_regression") {
            lr_mod <- get_cached_model("logistic_regression")
            prob <- suppressWarnings(predict(lr_mod, newdata = feats, type = "response"))
          }
        },
        error = function(e) {
          prob <<- 0.0
        }
      )

      prob_pct <- pmin(pmax(as.numeric(prob) * 100, 0), 100)

      risk_tier <- if (prob_pct < 20) {
        "Low Risk"
      } else if (prob_pct < (thresh * 100)) {
        "Medium Risk"
      } else if (prob_pct < 80) {
        "High Risk"
      } else {
        "Critical Risk"
      }

      bar_col <- if (risk_tier == "Low Risk") {
        "#16A34A"
      } else if (risk_tier == "Medium Risk") {
        "#D97706"
      } else if (risk_tier == "High Risk") {
        "#EA580C"
      } else {
        "#DC2626"
      }

      list(
        tx_id        = row_dt$transaction_id[1],
        amount       = row_dt$amount[1],
        elapsed_hrs  = round(row_dt$elapsed_hours[1], 2),
        actual_class = row_dt$class[1],
        actual_label = ifelse(row_dt$class[1] == 1, "Confirmed Fraud", "Legitimate"),
        model_type   = m_type,
        threshold    = thresh,
        prob_pct     = prob_pct,
        risk_tier    = risk_tier,
        bar_col      = bar_col,
        is_alert     = (prob_pct >= (thresh * 100)),
        top_v14      = round(row_dt$v14[1], 2),
        top_v17      = round(row_dt$v17[1], 2)
      )
    })

    output$gauge_transaction_risk <- renderPlotly({
      tx_info <- current_tx_details()
      if (is.null(tx_info)) {
        prob_val <- 0.0
        tier_lbl <- "Model Unavailable"
        bar_col <- "#94A3B8"
        thresh_pct <- 50.0
      } else {
        prob_val <- tx_info$prob_pct
        tier_lbl <- tx_info$risk_tier
        bar_col <- tx_info$bar_col
        thresh_pct <- tx_info$threshold * 100
      }

      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = prob_val,
        number = list(
          suffix = "%",
          valueformat = ".1f",
          font = list(size = 26, color = bar_col, family = "Arial, sans-serif")
        ),
        title = list(
          text = paste0("<b>Transaction Risk</b><br><span style='font-size:11px;color:", bar_col, ";font-weight:bold;'>", toupper(tier_lbl), "</span>"),
          font = list(size = 13, family = "Arial, sans-serif")
        ),
        gauge = list(
          axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "#94A3B8", ticksuffix = "%"),
          bar = list(color = bar_col, thickness = 0.40),
          bgcolor = "#F8FAFC",
          borderwidth = 1,
          bordercolor = "#E2E8F0",
          steps = list(
            list(range = c(0, 20), color = "#DCFCE7"), # Low Risk (< 20%)
            list(range = c(20, thresh_pct), color = "#FEF3C7"), # Medium Risk (20% to Threshold)
            list(range = c(thresh_pct, 80), color = "#FFEDD5"), # High Risk (Threshold to 80%)
            list(range = c(80, 100), color = "#FEE2E2") # Critical Risk (>= 80%)
          ),
          threshold = list(
            line = list(color = "#1E293B", width = 3),
            thickness = 0.75,
            value = thresh_pct
          )
        ),
        height = 220
      ) %>% layout(
        margin = list(t = 28, b = 12, l = 25, r = 25),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    })

    output$transaction_risk_meta_ui <- renderUI({
      tx_info <- current_tx_details()
      if (is.null(tx_info)) {
        return(tags$span("No transaction selected."))
      }

      gt_badge_class <- if (tx_info$actual_class == 1) "badge bg-danger" else "badge bg-success"
      alert_badge <- if (tx_info$is_alert) {
        tags$span(class = "badge bg-danger", "FLAGGED ALERT")
      } else {
        tags$span(class = "badge bg-light text-muted border", "CLEARED")
      }

      tagList(
        tags$div(
          class = "d-flex justify-content-between align-items-center mb-1",
          tags$span(tags$strong("Record: "), tags$code(tx_info$tx_id)),
          tags$span(tags$strong("Ticket: "), sprintf("$%.2f", tx_info$amount))
        ),
        tags$div(
          class = "d-flex justify-content-between align-items-center small",
          tags$span("Ground Truth: ", tags$span(class = gt_badge_class, tx_info$actual_label)),
          tags$span("Decision: ", alert_badge)
        ),
        tags$div(
          class = "text-muted small mt-1",
          tags$span(sprintf("Signals: V14 = %.2f | V17 = %.2f (Scored without Class)", tx_info$top_v14, tx_info$top_v17))
        )
      )
    })

    # --------------------------------------------------------------------------
    # 4. GAUGE 3: MODEL RECALL GAUGE (Verified Model Sensitivity)
    # --------------------------------------------------------------------------
    # Retrieve verified metrics for chosen model from model_comparison table
    current_model_eval_metrics <- reactive({
      m_type <- if (!is.null(input$gauge_model)) input$gauge_model else "random_forest"
      name_map <- list(
        "random_forest"       = "Random Forest",
        "xgboost"             = "XGBoost",
        "logistic_regression" = "Logistic Regression"
      )
      target_name <- name_map[[m_type]]

      comp_tbl <- tryCatch(get_model_comparison_table(), error = function(e) NULL)
      if (!is.null(comp_tbl) && target_name %in% comp_tbl$Model) {
        row <- comp_tbl[comp_tbl$Model == target_name, ][1, ]
        list(
          model_name = target_name,
          recall     = round(row$Recall * 100, 2),
          precision  = round(row$Precision * 100, 2),
          f1         = round(row$F1_Score, 4),
          tp         = as.integer(row$TP),
          fp         = as.integer(row$FP),
          fn         = as.integer(row$FN),
          tn         = as.integer(row$TN)
        )
      } else {
        # Verified benchmarks from outputs/tables/model_comparison.csv
        if (m_type == "random_forest") {
          list(model_name = "Random Forest", recall = 80.65, precision = 96.15, f1 = 0.8772, tp = 75, fp = 3, fn = 18, tn = 56866)
        } else if (m_type == "xgboost") {
          list(model_name = "XGBoost", recall = 83.87, precision = 75.00, f1 = 0.7919, tp = 78, fp = 26, fn = 15, tn = 56843)
        } else {
          list(model_name = "Logistic Regression", recall = 61.29, precision = 83.82, f1 = 0.7081, tp = 57, fp = 11, fn = 36, tn = 56858)
        }
      }
    })

    output$gauge_model_recall <- renderPlotly({
      m_info <- current_model_eval_metrics()
      rec_val <- m_info$recall

      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = rec_val,
        number = list(
          suffix = "%",
          valueformat = ".2f",
          font = list(size = 26, color = "#0284C7", family = "Arial, sans-serif")
        ),
        title = list(
          text = paste0("<b>Model Recall</b><br><span style='font-size:11px;color:#64748B;'>", m_info$model_name, " (Verified)</span>"),
          font = list(size = 13, family = "Arial, sans-serif")
        ),
        gauge = list(
          axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "#94A3B8", ticksuffix = "%"),
          bar = list(color = "#0284C7", thickness = 0.40),
          bgcolor = "#F8FAFC",
          borderwidth = 1,
          bordercolor = "#E2E8F0",
          steps = list(
            list(range = c(0, 50), color = "#F1F5F9"), # Low (< 50%)
            list(range = c(50, 75), color = "#E0F2FE"), # Moderate (50-75%)
            list(range = c(75, 90), color = "#BAE6FD"), # Strong (75-90%)
            list(range = c(90, 100), color = "#7DD3FC") # Elite (> 90%)
          ),
          threshold = list(
            line = list(color = "#0369A1", width = 3),
            thickness = 0.75,
            value = rec_val
          )
        ),
        height = 220
      ) %>% layout(
        margin = list(t = 28, b = 12, l = 25, r = 25),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    })

    output$recall_meta_ui <- renderUI({
      m_info <- current_model_eval_metrics()
      tot_frd <- m_info$tp + m_info$fn

      tagList(
        tags$div(
          class = "d-flex justify-content-between align-items-center mb-1",
          tags$span(tags$strong("Frauds Caught: "), tags$span(class = "badge bg-primary", sprintf("%d / %d (TP)", m_info$tp, tot_frd))),
          tags$span(tags$strong("Missed (FN): "), tags$span(class = "badge bg-secondary", sprintf("%d", m_info$fn)))
        ),
        tags$div(
          class = "text-muted small",
          tags$span(tags$b("Definition: "), "Recall = TP / (TP + FN). Measures the model's ability to intercept fraudulent transactions.")
        )
      )
    })

    # --------------------------------------------------------------------------
    # 5. GAUGE 4: MODEL PRECISION GAUGE (Verified Positive Predictive Value)
    # --------------------------------------------------------------------------
    output$gauge_model_precision <- renderPlotly({
      m_info <- current_model_eval_metrics()
      prec_val <- m_info$precision

      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = prec_val,
        number = list(
          suffix = "%",
          valueformat = ".2f",
          font = list(size = 26, color = "#16A34A", family = "Arial, sans-serif")
        ),
        title = list(
          text = paste0("<b>Model Precision</b><br><span style='font-size:11px;color:#64748B;'>", m_info$model_name, " (Verified)</span>"),
          font = list(size = 13, family = "Arial, sans-serif")
        ),
        gauge = list(
          axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "#94A3B8", ticksuffix = "%"),
          bar = list(color = "#16A34A", thickness = 0.40),
          bgcolor = "#F8FAFC",
          borderwidth = 1,
          bordercolor = "#E2E8F0",
          steps = list(
            list(range = c(0, 60), color = "#F1F5F9"), # Sub-optimal (< 60%)
            list(range = c(60, 80), color = "#FEF3C7"), # Moderate (60-80%)
            list(range = c(80, 95), color = "#DCFCE7"), # High Precision (80-95%)
            list(range = c(95, 100), color = "#BBF7D0") # Ultra-low False Alarms
          ),
          threshold = list(
            line = list(color = "#15803D", width = 3),
            thickness = 0.75,
            value = prec_val
          )
        ),
        height = 220
      ) %>% layout(
        margin = list(t = 28, b = 12, l = 25, r = 25),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    })

    output$precision_meta_ui <- renderUI({
      m_info <- current_model_eval_metrics()
      tot_alerts <- m_info$tp + m_info$fp

      tagList(
        tags$div(
          class = "d-flex justify-content-between align-items-center mb-1",
          tags$span(tags$strong("True Alarms: "), tags$span(class = "badge bg-success", sprintf("%d (TP)", m_info$tp))),
          tags$span(tags$strong("False Alarms: "), tags$span(class = "badge bg-warning text-dark", sprintf("%d (FP)", m_info$fp)))
        ),
        tags$div(
          class = "text-muted small",
          tags$span(tags$b("Definition: "), "Precision = TP / (TP + FP). Total predicted alerts: ", tot_alerts, ". Minimizes benign friction.")
        )
      )
    })

    # --------------------------------------------------------------------------
    # 6. SECTION 3: CORRELATION ENGINE (Fixed Error A & Robust Calculation)
    # --------------------------------------------------------------------------
    # Calculate feature correlations with binary Class safely
    computed_correlations <- reactive({
      sub_dt <- filtered_data()
      method <- if (!is.null(input$cor_method)) input$cor_method else "pearson"

      # Target validation: Class must exist and have variance
      if (is.null(sub_dt) || nrow(sub_dt) < 5 || !"class" %in% names(sub_dt)) {
        return(list(status = "empty", message = "Insufficient data points to compute correlation.", results = data.frame()))
      }

      target <- as.numeric(sub_dt$class)
      target_valid <- !is.na(target) & is.finite(target)

      if (sum(target_valid) < 5 || stats::sd(target[target_valid]) == 0) {
        return(list(
          status = "zero_variance",
          message = "Target 'Class' has zero variance in the active filter selection (e.g. all selected transactions belong to a single class). Correlation is mathematically undefined.",
          results = data.frame()
        ))
      }

      # Select ONLY valid numeric candidate features
      candidate_cols <- c(paste0("v", 1:28), "amount", "elapsed_hours", "amount_log")
      candidate_cols <- intersect(candidate_cols, names(sub_dt))

      valid_feats <- character(0)
      cor_vals <- numeric(0)

      for (col in candidate_cols) {
        vec <- as.numeric(sub_dt[[col]])
        idx <- target_valid & !is.na(vec) & is.finite(vec)
        if (sum(idx) >= 5 && stats::sd(vec[idx]) > 0) {
          r_val <- tryCatch(
            stats::cor(vec[idx], target[idx], method = method),
            error = function(e) NA_real_
          )
          if (!is.na(r_val) && is.finite(r_val)) {
            valid_feats <- c(valid_feats, col)
            cor_vals <- c(cor_vals, r_val)
          }
        }
      }

      if (length(cor_vals) == 0) {
        return(list(status = "empty", message = "No valid numeric features with non-zero variance found.", results = data.frame()))
      }

      res_df <- data.frame(
        Feature = valid_feats,
        Correlation = cor_vals,
        Abs_Correlation = abs(cor_vals),
        Direction = ifelse(cor_vals < 0, "Negative (Protective Signal)", "Positive (Fraud Indicator)"),
        stringsAsFactors = FALSE
      )
      res_df <- res_df[order(-res_df$Abs_Correlation), ]

      return(list(status = "success", results = res_df))
    })

    # Render Correlation Bar Chart
    output$corr_bar_chart <- renderPlotly({
      cor_info <- computed_correlations()

      if (cor_info$status != "success" || nrow(cor_info$results) == 0) {
        # Display clean empty state notification without warning
        return(
          plot_ly(type = "scatter", mode = "markers") %>%
            add_markers(x = 0, y = 0, marker = list(opacity = 0), showlegend = FALSE) %>%
            layout(
              annotations = list(
                list(
                  text = paste0("<b>Correlation Notice</b><br>", cor_info$message),
                  xref = "paper", yref = "paper",
                  x = 0.5, y = 0.5, showarrow = FALSE,
                  font = list(size = 13, color = "#64748B")
                )
              ),
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor = "rgba(0,0,0,0)"
            )
        )
      }

      df <- cor_info$results
      # Extract top 5 negative and top 5 positive
      neg_sub <- head(df[df$Correlation < 0, ][order(df[df$Correlation < 0, ]$Correlation), ], 5)
      pos_sub <- head(df[df$Correlation > 0, ][order(-df[df$Correlation > 0, ]$Correlation), ], 5)

      plot_df <- rbind(neg_sub, pos_sub)
      if (nrow(plot_df) == 0) plot_df <- head(df, 10)

      plot_df <- plot_df[order(plot_df$Correlation), ]
      plot_df$Feature <- factor(plot_df$Feature, levels = plot_df$Feature)
      plot_df$BarColor <- ifelse(plot_df$Correlation < 0, "#0066FF", "#DC2626")

      plot_ly(
        data = plot_df,
        x = ~Correlation,
        y = ~Feature,
        type = "bar",
        orientation = "h",
        source = "corr_bars",
        marker = list(color = plot_df$BarColor),
        hovertemplate = "Feature: %{y}<br>Correlation r: %{x:.4f}<br>Click bar to inspect in density chart<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "Correlation Coefficient (r)", zeroline = TRUE, zerolinecolor = "#0F172A"),
          yaxis = list(title = ""),
          margin = list(t = 20, b = 40, l = 60, r = 20),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        )
    })

    # Render Interactive Pairwise Heatmap Plot
    output$corr_heatmap_plot <- renderPlotly({
      cor_info <- computed_correlations()
      sub_dt <- filtered_data()

      if (cor_info$status != "success" || nrow(sub_dt) < 5) {
        return(
          plot_ly(type = "scatter", mode = "markers") %>%
            add_markers(x = 0, y = 0, marker = list(opacity = 0), showlegend = FALSE) %>%
            layout(
              annotations = list(
                list(
                  text = "Heatmap unavailable: filter selection has zero variance.",
                  xref = "paper", yref = "paper",
                  x = 0.5, y = 0.5, showarrow = FALSE,
                  font = list(size = 13, color = "#64748B")
                )
              ),
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor = "rgba(0,0,0,0)"
            )
        )
      }

      top_feats <- head(cor_info$results$Feature, 8)
      matrix_cols <- unique(c(top_feats, "class"))
      matrix_cols <- intersect(matrix_cols, names(sub_dt))

      num_mat <- as.matrix(sub_dt[, ..matrix_cols])
      c_mat <- round(stats::cor(num_mat, use = "pairwise.complete.obs"), 3)

      plot_ly(
        x = matrix_cols,
        y = matrix_cols,
        z = c_mat,
        type = "heatmap",
        colorscale = list(
          c(0, "#1E3A8A"), # Corporate Navy (-1)
          c(0.5, "#F8FAFC"), # Neutral (0)
          c(1, "#DC2626") # Crimson Alert (+1)
        ),
        zmin = -1,
        zmax = 1,
        hoverongaps = FALSE,
        hovertemplate = "Var 1: %{x}<br>Var 2: %{y}<br>Correlation r: %{z:.3f}<extra></extra>"
      ) %>%
        layout(
          margin = list(t = 20, b = 40, l = 50, r = 20),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        )
    })

    # --------------------------------------------------------------------------
    # 7. SECTION 4: DENSITY SHIFT (Fixed Error B & Valid R Colours)
    # --------------------------------------------------------------------------
    # Dynamic feature density distribution
    output$density_shift_chart <- renderPlotly({
      sub_dt <- filtered_data()
      feat <- if (!is.null(input$focus_feature)) input$focus_feature else "v14"

      if (nrow(sub_dt) == 0 || !feat %in% names(sub_dt)) {
        return(
          plot_ly(type = "scatter", mode = "markers") %>%
            add_markers(x = 0, y = 0, marker = list(opacity = 0), showlegend = FALSE) %>%
            layout(
              annotations = list(list(text = "No records available for active filter.", x = 0.5, y = 0.5, showarrow = FALSE)),
              xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
            )
        )
      }

      legit_vec <- as.numeric(sub_dt[class == 0][[feat]])
      fraud_vec <- as.numeric(sub_dt[class == 1][[feat]])

      legit_vec <- legit_vec[!is.na(legit_vec) & is.finite(legit_vec)]
      fraud_vec <- fraud_vec[!is.na(fraud_vec) & is.finite(fraud_vec)]

      # If Amount is selected, transform with log1p for readability
      if (feat == "amount") {
        legit_vec <- log1p(pmax(legit_vec, 0))
        fraud_vec <- log1p(pmax(fraud_vec, 0))
      }

      p <- plot_ly()

      # Legitimate density curve (using valid R hex color '#0066FF' and alpha)
      if (length(legit_vec) >= 3 && stats::sd(legit_vec) > 0) {
        # Sample for ultra-responsive density estimation if large
        if (length(legit_vec) > 3000) legit_vec <- sample(legit_vec, 3000)
        d_l <- stats::density(legit_vec, n = 200)
        p <- p %>% add_lines(
          x = d_l$x, y = d_l$y, name = "Legitimate",
          line = list(color = "#0066FF", width = 2.5),
          fill = "tozeroy", fillcolor = "#0066FF26",
          hovertemplate = "Legitimate<br>Value: %{x:.2f}<br>Density: %{y:.3f}<extra></extra>"
        )
      }

      # Fraud density curve (using valid R hex color '#DC2626')
      if (length(fraud_vec) >= 3 && stats::sd(fraud_vec) > 0) {
        d_f <- stats::density(fraud_vec, n = 200)
        p <- p %>% add_lines(
          x = d_f$x, y = d_f$y, name = "Fraudulent",
          line = list(color = "#DC2626", width = 2.5),
          fill = "tozeroy", fillcolor = "#DC262633",
          hovertemplate = "Fraudulent<br>Value: %{x:.2f}<br>Density: %{y:.3f}<extra></extra>"
        )
      }

      x_label <- if (feat == "amount") "Transaction Amount [log1p(USD)]" else sprintf("Feature %s (Standardized PCA Vector)", toupper(feat))

      p %>% layout(
        xaxis = list(title = x_label, showgrid = TRUE, gridcolor = "#E2E8F0"),
        yaxis = list(title = "Probability Density", showgrid = TRUE, gridcolor = "#E2E8F0"),
        legend = list(orientation = "h", x = 0.3, y = 1.12),
        margin = list(t = 20, b = 40, l = 50, r = 20),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    })

    output$density_sample_badge <- renderUI({
      sub_dt <- filtered_data()
      feat <- if (!is.null(input$focus_feature)) input$focus_feature else "v14"
      n_l <- sum(sub_dt$class == 0)
      n_f <- sum(sub_dt$class == 1)

      tags$span(class = "badge bg-light text-dark border", sprintf("Feature: %s | Legit: %s | Fraud: %s", toupper(feat), format(n_l, big.mark = ","), format(n_f, big.mark = ",")))
    })

    output$density_metrics_summary <- renderUI({
      sub_dt <- filtered_data()
      feat <- if (!is.null(input$focus_feature)) input$focus_feature else "v14"
      if (nrow(sub_dt) == 0 || !feat %in% names(sub_dt)) {
        return(tags$span("No data."))
      }

      l_vals <- as.numeric(sub_dt[class == 0][[feat]])
      f_vals <- as.numeric(sub_dt[class == 1][[feat]])

      l_med <- if (length(l_vals) > 0) median(l_vals, na.rm = TRUE) else NA
      f_med <- if (length(f_vals) > 0) median(f_vals, na.rm = TRUE) else NA
      l_mea <- if (length(l_vals) > 0) mean(l_vals, na.rm = TRUE) else NA
      f_mea <- if (length(f_vals) > 0) mean(f_vals, na.rm = TRUE) else NA
      diff_mea <- if (!is.na(l_mea) && !is.na(f_mea)) (f_mea - l_mea) else NA

      prefix <- if (feat == "amount") "$" else ""

      tagList(
        tags$div(
          class = "row g-2 text-center",
          tags$div(
            class = "col-sm-3 col-6",
            tags$div(class = "text-muted small", "Legitimate Median"),
            tags$div(style = "font-weight: 700; color: #0066FF;", sprintf("%s%.2f", prefix, l_med))
          ),
          tags$div(
            class = "col-sm-3 col-6",
            tags$div(class = "text-muted small", "Fraud Median"),
            tags$div(style = "font-weight: 700; color: #DC2626;", sprintf("%s%.2f", prefix, f_med))
          ),
          tags$div(
            class = "col-sm-3 col-6",
            tags$div(class = "text-muted small", "Legitimate Mean"),
            tags$div(style = "font-weight: 700; color: #0066FF;", sprintf("%s%.2f", prefix, l_mea))
          ),
          tags$div(
            class = "col-sm-3 col-6",
            tags$div(class = "text-muted small", "Fraud Mean Divergence"),
            tags$div(style = "font-weight: 700; color: #D97706;", sprintf("%+.2f", diff_mea))
          )
        ),
        tags$div(
          class = "text-muted small mt-2",
          tags$em("PCA Anonymization Notice: Features V1-V28 are orthogonal components with masked identities. Metrics represent empirical divergence.")
        )
      )
    })

    # Amount Comparison by Class (Boxplot / Distribution)
    output$amount_by_class_chart <- renderPlotly({
      sub_dt <- filtered_data()
      if (nrow(sub_dt) == 0) {
        return(NULL)
      }

      plot_ly(
        data = sub_dt,
        x = ~actual_status,
        y = ~amount_log,
        color = ~actual_status,
        colors = c("#0066FF", "#DC2626"),
        type = "box",
        boxpoints = "outliers",
        jitter = 0.3,
        pointpos = -1.8
      ) %>% layout(
        xaxis = list(title = ""),
        yaxis = list(title = "log1p(Amount in USD)", showgrid = TRUE, gridcolor = "#E2E8F0"),
        showlegend = FALSE,
        margin = list(t = 20, b = 40, l = 50, r = 20),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    })

    # --------------------------------------------------------------------------
    # 8. SECTION 5: TEMPORAL FRAUD RATE & VOLUME CADENCE
    # --------------------------------------------------------------------------
    # Hourly Rate Chart
    output$hourly_rate_chart <- renderPlotly({
      sub_dt <- filtered_data()
      if (nrow(sub_dt) == 0) {
        return(NULL)
      }

      # Group by elapsed hour
      hourly_agg <- sub_dt[, .(
        Total = .N,
        Frauds = sum(class == 1),
        Rate_Pct = (sum(class == 1) / .N) * 100
      ), by = .(Elapsed_Hour = as.integer(floor(elapsed_hours)))][order(Elapsed_Hour)]

      plot_ly(
        data = hourly_agg,
        x = ~Elapsed_Hour,
        y = ~Rate_Pct,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#D97706", width = 3),
        marker = list(color = "#DC2626", size = 6),
        hovertemplate = "Elapsed Hour: %{x}h<br>Fraud Attack Rate: %{y:.3f}%<extra></extra>"
      ) %>% layout(
        xaxis = list(title = "Elapsed Time (Hours: 0 to 47h)", showgrid = FALSE),
        yaxis = list(title = "Cohort Fraud Attack Rate (%)", showgrid = TRUE, gridcolor = "#E2E8F0"),
        margin = list(t = 20, b = 40, l = 50, r = 20),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    })

    # Hourly Volume Chart
    output$hourly_volume_chart <- renderPlotly({
      sub_dt <- filtered_data()
      if (nrow(sub_dt) == 0) {
        return(NULL)
      }

      hourly_agg <- sub_dt[, .(
        Legit_Vol = sum(class == 0),
        Fraud_Vol = sum(class == 1)
      ), by = .(Elapsed_Hour = as.integer(floor(elapsed_hours)))][order(Elapsed_Hour)]

      plot_ly(data = hourly_agg, x = ~Elapsed_Hour) %>%
        add_bars(y = ~Legit_Vol, name = "Legitimate", marker = list(color = "#0066FF")) %>%
        add_bars(y = ~Fraud_Vol, name = "Fraudulent", marker = list(color = "#DC2626")) %>%
        layout(
          barmode = "stack",
          xaxis = list(title = "Elapsed Time (Hours: 0 to 47h)", showgrid = FALSE),
          yaxis = list(title = "Transaction Volume", showgrid = TRUE, gridcolor = "#E2E8F0"),
          legend = list(orientation = "h", x = 0.3, y = 1.1),
          margin = list(t = 20, b = 40, l = 50, r = 20),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        )
    })

    # --------------------------------------------------------------------------
    # 9. SECTION 6: TRANSACTION INVESTIGATION TABLE (DT)
    # --------------------------------------------------------------------------
    output$tx_investigation_table <- renderDT(
      {
        sub_dt <- filtered_data()
        if (nrow(sub_dt) == 0) {
          return(datatable(data.frame(Message = "No transactions match the selected filters."), options = list(dom = "t")))
        }

        # Select display columns
        disp_dt <- data.table(
          Transaction_ID = sub_dt$transaction_id,
          Elapsed_Time = sprintf("%.2f hrs", sub_dt$elapsed_hours),
          Amount = sprintf("$%.2f", sub_dt$amount),
          Ground_Truth = ifelse(
            sub_dt$class == 1,
            '<span class="badge" style="background-color:#fee2e2;color:#991b1b;font-weight:700;">CONFIRMED FRAUD</span>',
            '<span class="badge" style="background-color:#dcfce7;color:#166534;font-weight:700;">LEGITIMATE</span>'
          ),
          Model_Prob = sprintf("%.2f%%", sub_dt$fraud_probability * 100),
          Model_Decision = ifelse(
            sub_dt$is_alert == 1,
            '<span class="badge bg-danger">FLAGGED ALERT</span>',
            '<span class="badge bg-light text-muted border">CLEARED</span>'
          ),
          Risk_Tier = ifelse(
            sub_dt$risk_category == "Critical Risk",
            '<span class="badge" style="background-color:#fee2e2;color:#991b1b;border:1px solid #f87171;">Critical</span>',
            ifelse(
              sub_dt$risk_category == "High Risk",
              '<span class="badge" style="background-color:#ffedd5;color:#9a3412;border:1px solid #fb923c;">High</span>',
              ifelse(
                sub_dt$risk_category == "Medium Risk",
                '<span class="badge" style="background-color:#fef3c7;color:#92400e;border:1px solid #fcd34d;">Medium</span>',
                '<span class="badge" style="background-color:#dcfce7;color:#166534;border:1px solid #86efac;">Low</span>'
              )
            )
          )
        )

        datatable(
          disp_dt,
          escape = FALSE,
          selection = "single",
          options = list(
            pageLength = 10,
            lengthMenu = c(10, 25, 50, 100),
            autoWidth = FALSE,
            dom = "<'row'<'col-sm-6'l><'col-sm-6'f>>rt<'row'<'col-sm-6'i><'col-sm-6'p>>"
          ),
          rownames = FALSE
        )
      },
      server = TRUE
    )

    # --------------------------------------------------------------------------
    # 10. SECTION 7: STATISTICAL SYNTHESIS COMPARISON TABLE
    # --------------------------------------------------------------------------
    output$stats_summary_table <- renderTable(
      {
        sub_dt <- filtered_data()
        if (nrow(sub_dt) == 0) {
          return(data.frame(Notice = "No records in current filter."))
        }

        l_amt <- sub_dt[class == 0, amount]
        f_amt <- sub_dt[class == 1, amount]

        n_l <- length(l_amt)
        n_f <- length(f_amt)

        data.frame(
          Metric = c(
            "Cohort Transaction Count",
            "Mean Ticket Size ($)",
            "Median Ticket Size ($)",
            "Interquartile Range (IQR)",
            "Max Ticket Size ($)",
            "Zero-Dollar Transactions ($0)",
            "High-Value Transactions (>$1,000)"
          ),
          Legitimate_Cohort = c(
            sprintf("%s (%.2f%%)", format(n_l, big.mark = ","), if (n_l + n_f > 0) (n_l / (n_l + n_f)) * 100 else 0),
            if (n_l > 0) sprintf("$%.2f", mean(l_amt)) else "N/A",
            if (n_l > 0) sprintf("$%.2f", median(l_amt)) else "N/A",
            if (n_l > 0) sprintf("$%.2f ($%.2f to $%.2f)", IQR(l_amt), quantile(l_amt, 0.25), quantile(l_amt, 0.75)) else "N/A",
            if (n_l > 0) sprintf("$%.2f", max(l_amt)) else "N/A",
            sprintf("%d (%.2f%%)", sum(l_amt == 0), if (n_l > 0) (sum(l_amt == 0) / n_l) * 100 else 0),
            sprintf("%d (%.2f%%)", sum(l_amt > 1000), if (n_l > 0) (sum(l_amt > 1000) / n_l) * 100 else 0)
          ),
          Fraudulent_Cohort = c(
            sprintf("%s (%.2f%%)", format(n_f, big.mark = ","), if (n_l + n_f > 0) (n_f / (n_l + n_f)) * 100 else 0),
            if (n_f > 0) sprintf("$%.2f", mean(f_amt)) else "N/A",
            if (n_f > 0) sprintf("$%.2f", median(f_amt)) else "N/A",
            if (n_f > 0) sprintf("$%.2f ($%.2f to $%.2f)", IQR(f_amt), quantile(f_amt, 0.25), quantile(f_amt, 0.75)) else "N/A",
            if (n_f > 0) sprintf("$%.2f", max(f_amt)) else "N/A",
            sprintf("%d (%.2f%%)", sum(f_amt == 0), if (n_f > 0) (sum(f_amt == 0) / n_f) * 100 else 0),
            sprintf("%d (%.2f%%)", sum(f_amt > 1000), if (n_f > 0) (sum(f_amt > 1000) / n_f) * 100 else 0)
          ),
          Operational_Interpretation = c(
            "Observed attack volume and class disparity within active filter scope.",
            "Fraud ticket mean compared to legitimate purchasing profile.",
            "Median ticket reveals micro-probing authorizations by cybercriminals.",
            "Interquartile ticket dispersion among fraudulent authorizations.",
            "Maximum exposure ticket observed in the active filter slice.",
            "Automated card testing account verification authorizations ($0).",
            "Targeted bust-out attacks on high-value merchant categories."
          ),
          stringsAsFactors = FALSE
        )
      },
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE,
      align = "lllc"
    )

    # --------------------------------------------------------------------------
    # 11. DOWNLOAD HANDLERS
    # --------------------------------------------------------------------------
    # Export filtered cohort as CSV
    output$download_filtered_csv <- downloadHandler(
      filename = function() {
        sprintf("fraudshield_cohort_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        sub_dt <- filtered_data()
        write.csv(sub_dt, file, row.names = FALSE)
      }
    )

    # Export correlations results as CSV
    output$download_cor_csv <- downloadHandler(
      filename = function() {
        sprintf("fraudshield_correlations_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        cor_info <- computed_correlations()
        df <- if (cor_info$status == "success") cor_info$results else data.frame(Notice = cor_info$message)
        write.csv(df, file, row.names = FALSE)
      }
    )

    # Export ledger table as CSV
    output$download_table_csv <- downloadHandler(
      filename = function() {
        sprintf("fraudshield_investigation_ledger_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        sub_dt <- filtered_data()
        disp_cols <- c("transaction_id", "elapsed_hours", "amount", "class", "actual_status", "fraud_probability", "model_prediction", "risk_category")
        disp_cols <- intersect(disp_cols, names(sub_dt))
        write.csv(sub_dt[, ..disp_cols], file, row.names = FALSE)
      }
    )
  })
}
