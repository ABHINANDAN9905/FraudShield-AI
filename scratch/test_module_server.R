cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))
options(sass.cache = FALSE)

source("app/global.R")
library(shiny)

cat("====================================================================\n")
cat(" Running official shiny::testServer for fraud_analytics_server\n")
cat("====================================================================\n\n")

testServer(fraud_analytics_server, {
  cat("[SESSION INITIALIZED]\n")
  
  # Default state
  cat("  -> Initial model:", input$gauge_model, "\n")
  session$setInputs(gauge_model = "random_forest", gauge_tx_id = "TX-01340", gauge_time_scope = "all", gauge_amount_cohort = "all")
  
  # Trigger reactivity
  session$flushReact()
  
  # Verify prevalence
  prev_data <- filtered_prevalence_data()
  cat("  -> Filtered prevalence data rows:", nrow(prev_data), "\n")
  cat("  -> Fraud count in filter:", sum(prev_data$class == 1), "\n")
  stopifnot(nrow(prev_data) == 56962)
  stopifnot(sum(prev_data$class == 1) == 93)
  
  # Verify transaction risk calculation
  tx_info <- selected_transaction_info()
  cat("  -> Scored TX:", tx_info$tx_id, "| Prob:", tx_info$prob_pct, "% | Tier:", tx_info$risk_tier, "\n")
  stopifnot(tx_info$risk_tier == "Critical Risk")
  stopifnot(tx_info$prob_pct > 80)
  
  # Verify model recall & precision
  m_metrics <- selected_model_metrics()
  cat("  -> Model Recall:", m_metrics$recall, "% | Precision:", m_metrics$precision, "%\n")
  stopifnot(abs(m_metrics$recall - 80.65) < 0.01)
  stopifnot(abs(m_metrics$precision - 96.15) < 0.01)
  
  # Test changing model to XGBoost
  cat("\n[TEST CHANGING MODEL TO XGBOOST]\n")
  session$setInputs(gauge_model = "xgboost")
  session$flushReact()
  
  m_metrics_xgb <- selected_model_metrics()
  cat("  -> XGB Recall:", m_metrics_xgb$recall, "% | Precision:", m_metrics_xgb$precision, "%\n")
  stopifnot(abs(m_metrics_xgb$recall - 83.87) < 0.01)
  stopifnot(abs(m_metrics_xgb$precision - 75.00) < 0.01)
  
  # Test changing transaction to Legitimate
  cat("\n[TEST CHANGING TRANSACTION TO LEGITIMATE (TX-00001)]\n")
  session$setInputs(gauge_tx_id = "TX-00001")
  session$flushReact()
  
  tx_info_legit <- selected_transaction_info()
  cat("  -> Scored TX:", tx_info_legit$tx_id, "| Prob:", tx_info_legit$prob_pct, "% | Tier:", tx_info_legit$risk_tier, "\n")
  stopifnot(tx_info_legit$risk_tier == "Low Risk")
  stopifnot(tx_info_legit$prob_pct < 1.0)
  
  # Test changing Prevalence Filter to Night Spikes
  cat("\n[TEST CHANGING PREVALENCE FILTER TO NIGHT SPIKES]\n")
  session$setInputs(gauge_time_scope = "night")
  session$flushReact()
  
  prev_night <- filtered_prevalence_data()
  cat("  -> Night cohort total:", nrow(prev_night), "| Frauds:", sum(prev_night$class == 1), "| Prev:", round(sum(prev_night$class == 1)/nrow(prev_night)*100, 4), "%\n")
  stopifnot(nrow(prev_night) == 3331)
  stopifnot(sum(prev_night$class == 1) == 27)
  
  cat("\n====================================================================\n")
  cat(" [SHINY TESTSERVER PASSED] All reactive flows behave impeccably!\n")
  cat("====================================================================\n")
})

