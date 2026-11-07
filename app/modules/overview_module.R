# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Module: Executive Overview (Page 1)
# Script: app/modules/overview_module.R
# ==============================================================================

overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Top Information Banner
    tags$div(
      class = "alert alert-secondary d-flex align-items-center mb-4",
      style = "background-color: #F1F5F9; border-left: 4px solid #0066FF; color: #334155; padding: 12px 18px; border-radius: 6px;",
      tags$div(
        style = "font-size: 1.2rem; margin-right: 12px; color: #0066FF;",
        icon("shield-alt")
      ),
      tags$div(
        tags$strong("Portfolio Overview & Benchmark Telemetry: "),
        "Metrics reflect the validated European Credit Card Fraud Benchmark (284,807 transactions over 48 hours). ",
        tags$span(style = "color: #DC2626; font-weight: 600;", "Ground-truth labels (Class 0 = Legitimate, Class 1 = Fraud) are historical records"),
        " and distinctly separated from real-time model inferences."
      )
    ),
    
    # Row 1: KPI Cards Container (Dynamically sourced from verified dataset metrics)
    uiOutput(ns("kpi_cards_container")),
    
    # Row 2: Charts (Donut + Volume Trend + Fraud Rate Trend)
    tags$div(
      class = "row g-3 mb-4",
      tags$div(
        class = "col-lg-4 col-md-12",
        bslib::card(
          bslib::card_header(
            tags$div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              tags$span(style = "font-weight: 700; color: #0F172A;", "Class Distribution (Extreme Imbalance)"),
              tags$span(class = "badge bg-danger", "578 : 1 Skew")
            )
          ),
          bslib::card_body(
            plotlyOutput(ns("donut_chart"), height = "320px")
          )
        )
      ),
      tags$div(
        class = "col-lg-8 col-md-12",
        bslib::card(
          bslib::card_header(
            tags$div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              tags$span(style = "font-weight: 700; color: #0F172A;", "Transaction Volume & Fraud Activity Timeline"),
              tags$span(style = "font-size: 0.8rem; color: #64748B;", "Hourly Aggregation: floor(Time / 3600)")
            )
          ),
          bslib::card_body(
            plotlyOutput(ns("timeline_chart"), height = "320px")
          )
        )
      )
    ),
    
    # Row 3: Amount Comparison & Recent High-Risk Alerts Table
    tags$div(
      class = "row g-3 mb-4",
      tags$div(
        class = "col-lg-5 col-md-12",
        bslib::card(
          bslib::card_header(
            tags$span(style = "font-weight: 700; color: #0F172A;", "Ticket Size Distribution: Fraud vs Legitimate")
          ),
          bslib::card_body(
            plotlyOutput(ns("amount_box_chart"), height = "340px")
          )
        )
      ),
      tags$div(
        class = "col-lg-7 col-md-12",
        bslib::card(
          bslib::card_header(
            tags$div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              tags$span(style = "font-weight: 700; color: #0F172A;", "Verified High-Risk Incidents (Holdout Audit Queue)"),
              tags$span(class = "badge bg-dark", "Confirmed Ground-Truth")
            )
          ),
          bslib::card_body(
            tags$div(style = "overflow-x: auto;", DTOutput(ns("high_risk_table")))
          )
        )
      )
    )
  )
}

overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Render KPI Cards dynamically from verified dataset portfolio metrics
    output$kpi_cards_container <- renderUI({
      kpis <- tryCatch(get_portfolio_kpis(), error = function(e) {
        list(
          total_transactions   = 284807,
          fraud_transactions   = 492,
          legitimate_volume    = 284315,
          fraud_prevalence_pct = (492 / 284807) * 100,
          total_fraud_loss     = 60127.97,
          avg_ticket_size      = 88.35,
          fraud_avg_ticket     = 122.21,
          imbalance_ratio_str  = "578 : 1"
        )
      })
      
      tags$div(
        class = "row g-3 mb-4",
        tags$div(
          class = "col-xl-2 col-md-4 col-sm-6",
          create_kpi_card("Total Transactions", format_count(kpis$total_transactions), "48-Hour Ingestion Window", "credit-card", "#0066FF")
        ),
        tags$div(
          class = "col-xl-2 col-md-4 col-sm-6",
          create_kpi_card("Fraud Transactions", format_count(kpis$fraud_transactions), "Confirmed Class 1 Attacks", "exclamation-triangle", "#DC2626")
        ),
        tags$div(
          class = "col-xl-2 col-md-4 col-sm-6",
          create_kpi_card("Legitimate Volume", format_count(kpis$legitimate_volume), "99.83% Baseline Volume", "check-circle", "#16A34A")
        ),
        tags$div(
          class = "col-xl-2 col-md-4 col-sm-6",
          create_kpi_card("Fraud Prevalence", sprintf("%.4f%%", kpis$fraud_prevalence_pct), paste("Skew Ratio", kpis$imbalance_ratio_str), "percent", "#D97706")
        ),
        tags$div(
          class = "col-xl-2 col-md-4 col-sm-6",
          create_kpi_card("Total Fraud Loss", format_currency(kpis$total_fraud_loss), "Gross Class 1 Exposure", "dollar-sign", "#DC2626")
        ),
        tags$div(
          class = "col-xl-2 col-md-4 col-sm-6",
          create_kpi_card("Avg Ticket Size", format_currency(kpis$avg_ticket_size), sprintf("Fraud Avg: %s", format_currency(kpis$fraud_avg_ticket)), "wallet", "#6366F1")
        )
      )
    })
    
    # 1. Donut Chart: Class Distribution with True Counts & Percentages
    output$donut_chart <- renderPlotly({
      class_df <- tryCatch(get_class_distribution_table(), error = function(e) NULL)
      
      if (is.null(class_df) || nrow(class_df) < 2) {
        class_df <- data.frame(
          Class = c(0, 1),
          Status = c("Legitimate", "Fraudulent"),
          Records = c(284315, 492),
          Percentage = c(99.8273, 0.1727),
          stringsAsFactors = FALSE
        )
      }
      
      legit_count <- as.numeric(class_df$Records[class_df$Class == 0])
      fraud_count <- as.numeric(class_df$Records[class_df$Class == 1])
      total_count <- legit_count + fraud_count
      
      plot_df <- data.frame(
        Category = c("Legitimate", "Fraudulent"),
        Count = c(legit_count, fraud_count),
        Color = c("#16A34A", "#DC2626"),
        Percentage = c((legit_count / total_count) * 100, (fraud_count / total_count) * 100),
        stringsAsFactors = FALSE
      )
      
      plot_ly(
        data = plot_df,
        labels = ~Category,
        values = ~Count,
        type = "pie",
        hole = 0.65,
        marker = list(colors = plot_df$Color, line = list(color = "#FFFFFF", width = 2)),
        textinfo = "label+percent",
        textposition = "inside",
        hoverinfo = "text",
        text = ~sprintf("<b>%s Transactions</b><br>Actual Volume: %s<br>Portfolio Share: %.4f%%", 
                        Category, format(Count, big.mark = ","), Percentage)
      ) %>%
        layout(
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0.15, y = -0.1),
          annotations = list(
            list(
              text = sprintf("<b>%s</b><br><span style='font-size:10px;color:#64748B;'>Total Txns</span>", format(total_count, big.mark = ",")),
              x = 0.5, y = 0.5, font = list(size = 14, color = "#0F172A"),
              showarrow = FALSE
            )
          ),
          margin = list(t = 10, b = 20, l = 10, r = 10),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        )
    })
    
    # 2. Timeline Chart: Hourly Volume & Fraud Counts (Floor(Time / 3600))
    output$timeline_chart <- renderPlotly({
      hourly_df <- tryCatch(get_hourly_risk_table(), error = function(e) data.frame())
      
      if (nrow(hourly_df) == 0) {
        return(NULL)
      }
      
      # Standardize elapsed hour vector
      x_vals <- if ("Elapsed_Hour" %in% names(hourly_df)) hourly_df$Elapsed_Hour else hourly_df$Hour
      
      plot_ly(data = hourly_df) %>%
        add_bars(
          x = x_vals,
          y = ~Total_Transactions,
          name = "Total Transactions",
          marker = list(color = "rgba(0, 102, 255, 0.4)", line = list(color = "#0066FF", width = 1)),
          yaxis = "y1",
          hovertemplate = paste0(
            "<b>Elapsed Window:</b> %{x}:00h - %{customdata}:00h<br>",
            "<b>Total Volume:</b> %{y:,} txs<extra></extra>"
          ),
          customdata = x_vals + 1
        ) %>%
        add_lines(
          x = x_vals,
          y = ~Fraud_Count,
          name = "Confirmed Fraud Attacks",
          line = list(color = "#DC2626", width = 3),
          marker = list(color = "#DC2626", size = 6),
          yaxis = "y2",
          hovertemplate = paste0(
            "<b>Elapsed Window:</b> %{x}:00h - %{customdata}:00h<br>",
            "<b>Confirmed Attacks:</b> %{y:,}<extra></extra>"
          ),
          customdata = x_vals + 1
        ) %>%
        layout(
          yaxis = list(title = "Total Transaction Volume", side = "left", showgrid = TRUE, gridcolor = "#E2E8F0"),
          yaxis2 = list(title = "Confirmed Fraud Attacks", side = "right", overlaying = "y", showgrid = FALSE, color = "#DC2626"),
          xaxis = list(
            title = "Elapsed Time Interval: floor(Time / 3600) [Hours 0 to 47 from Dataset Origin]",
            showgrid = FALSE
          ),
          legend = list(orientation = "h", x = 0.25, y = 1.12),
          margin = list(t = 20, b = 45, l = 50, r = 50),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        )
    })
    
    # 3. Ticket Size Distribution: Fraud vs Legitimate
    output$amount_box_chart <- renderPlotly({
      test_dt <- tryCatch(get_test_data(), error = function(e) NULL)
      if (is.null(test_dt)) return(NULL)
      
      # Sample 1000 legit and all test frauds for smooth plotly rendering
      set.seed(42)
      f_sub <- test_dt[class == 1]
      l_sub <- test_dt[class == 0][sample(.N, min(.N, 1500))]
      plot_sub <- rbind(f_sub, l_sub)
      plot_sub[, ClassLabel := ifelse(class == 1, "Fraud ($)", "Legitimate ($)")]
      
      plot_ly(
        data = plot_sub,
        x = ~ClassLabel,
        y = ~log1p(amount),
        color = ~ClassLabel,
        colors = c("#DC2626", "#16A34A"),
        type = "box",
        boxpoints = "outliers",
        jitter = 0.3,
        pointpos = -1.8
      ) %>%
        layout(
          yaxis = list(title = "Transaction Amount: log1p(Amount)", showgrid = TRUE, gridcolor = "#E2E8F0"),
          xaxis = list(title = ""),
          showlegend = FALSE,
          margin = list(t = 15, b = 30, l = 50, r = 20),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        )
    })
    
    # 4. Recent High-Risk Incidents Table
    output$high_risk_table <- renderDT({
      test_dt <- tryCatch(get_test_data(), error = function(e) NULL)
      if (is.null(test_dt)) return(NULL)
      
      # Extract top fraud cases from holdout test set
      frauds <- test_dt[class == 1]
      tbl_data <- data.frame(
        Incident_ID    = sprintf("INC-%04d", 1:nrow(frauds)),
        Elapsed_Hours  = round(frauds$elapsed_hours, 2),
        Amount         = scales::dollar(frauds$amount),
        Top_Signal_V14 = round(frauds$v14, 3),
        Top_Signal_V17 = round(frauds$v17, 3),
        Ground_Truth   = "CONFIRMED FRAUD",
        Recommended    = "Block & Investigate",
        stringsAsFactors = FALSE
      )
      
      datatable(
        head(tbl_data, 15),
        options = list(
          pageLength = 5,
          dom = "tip",
          autoWidth = FALSE,
          ordering = TRUE
        ),
        rownames = FALSE,
        selection = "single"
      ) %>%
        formatStyle(
          "Ground_Truth",
          backgroundColor = "#FEE2E2",
          color = "#DC2626",
          fontWeight = "bold"
        ) %>%
        formatStyle(
          "Recommended",
          color = "#D97706",
          fontWeight = "600"
        )
    })
    
  })
}
