tryCatch({
  cand1 <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
  Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))
  options(sass.cache = FALSE)
  
  library(plotly)
  
  # 1. Prevalence gauge (0 to 100% or small percentage formatting)
  # When value is e.g. 0.17% or 0.1633%, on 0-100% scale:
  g1 <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = 0.1727,
    number = list(suffix = "%", valueformat = ".3f", font = list(size = 28, color = "#0F172A", family = "Arial, sans-serif")),
    title = list(text = "<b>Fraud Prevalence</b><br><span style='font-size:11px;color:#64748B;'>Empirical Attack Density</span>", font = list(size = 14)),
    gauge = list(
      axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "#94A3B8", ticksuffix = "%"),
      bar = list(color = "#DC2626", thickness = 0.35),
      bgcolor = "#F8FAFC",
      borderwidth = 1,
      bordercolor = "#E2E8F0",
      steps = list(
        list(range = c(0, 0.5), color = "#FEF2F2"),
        list(range = c(0.5, 2), color = "#FEE2E2"),
        list(range = c(2, 100), color = "#FECACA")
      )
    )
  ) %>% layout(
    margin = list(t = 30, b = 10, l = 25, r = 25),
    height = 220
  )

  # 2. Transaction Risk Gauge (0 to 100%)
  g2 <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = 94.2,
    number = list(suffix = "%", valueformat = ".1f", font = list(size = 28, color = "#DC2626", family = "Arial, sans-serif")),
    title = list(text = "<b>Transaction Risk Score</b><br><span style='font-size:11px;color:#DC2626;font-weight:bold;'>CRITICAL RISK (>= 80%)</span>", font = list(size = 14)),
    gauge = list(
      axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "#94A3B8", ticksuffix = "%"),
      bar = list(color = "#DC2626", thickness = 0.4),
      bgcolor = "#F8FAFC",
      borderwidth = 1,
      bordercolor = "#E2E8F0",
      steps = list(
        list(range = c(0, 20), color = "#DCFCE7"),   # Low
        list(range = c(20, 50), color = "#FEF3C7"),  # Medium
        list(range = c(50, 80), color = "#FFEDD5"),  # High
        list(range = c(80, 100), color = "#FEE2E2")  # Critical
      ),
      threshold = list(
        line = list(color = "#1E293B", width = 3),
        thickness = 0.75,
        value = 50
      )
    )
  ) %>% layout(
    margin = list(t = 30, b = 10, l = 25, r = 25),
    height = 220
  )

  # 3. Model Recall Gauge (0 to 100%)
  g3 <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = 80.65,
    number = list(suffix = "%", valueformat = ".2f", font = list(size = 28, color = "#0284C7", family = "Arial, sans-serif")),
    title = list(text = "<b>Model Recall (Sensitivity)</b><br><span style='font-size:11px;color:#64748B;'>Random Forest (Verified)</span>", font = list(size = 14)),
    gauge = list(
      axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "#94A3B8", ticksuffix = "%"),
      bar = list(color = "#0284C7", thickness = 0.4),
      bgcolor = "#F8FAFC",
      borderwidth = 1,
      bordercolor = "#E2E8F0",
      steps = list(
        list(range = c(0, 60), color = "#F1F5F9"),
        list(range = c(60, 80), color = "#E0F2FE"),
        list(range = c(80, 100), color = "#BAE6FD")
      )
    )
  ) %>% layout(
    margin = list(t = 30, b = 10, l = 25, r = 25),
    height = 220
  )

  # 4. Model Precision Gauge (0 to 100%)
  g4 <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = 96.15,
    number = list(suffix = "%", valueformat = ".2f", font = list(size = 28, color = "#16A34A", family = "Arial, sans-serif")),
    title = list(text = "<b>Model Precision (PPV)</b><br><span style='font-size:11px;color:#64748B;'>Random Forest (Verified)</span>", font = list(size = 14)),
    gauge = list(
      axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "#94A3B8", ticksuffix = "%"),
      bar = list(color = "#16A34A", thickness = 0.4),
      bgcolor = "#F8FAFC",
      borderwidth = 1,
      bordercolor = "#E2E8F0",
      steps = list(
        list(range = c(0, 60), color = "#F1F5F9"),
        list(range = c(60, 85), color = "#FEF3C7"),
        list(range = c(85, 100), color = "#DCFCE7")
      )
    )
  ) %>% layout(
    margin = list(t = 30, b = 10, l = 25, r = 25),
    height = 220
  )

  cat("All 4 gauges generated successfully without error!\n")
}, error = function(e) {
  cat("Error: ", conditionMessage(e), "\n")
})
