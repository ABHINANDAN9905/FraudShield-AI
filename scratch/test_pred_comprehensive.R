# ==============================================================================
# FRAUDSHIELD — ADVANCED RISK SCORING & PREDICTION ENGINE INTEGRATION TEST
# ==============================================================================

cat("=================================================================\n")
cat("Starting Comprehensive Risk Scoring & Prediction Engine Test\n")
cat("=================================================================\n\n")

# 1. Load Global Environment
cat("[Step 1] Loading app/global.R ...\n")
source("app/global.R")
cat("✓ Global environment loaded.\n\n")

# 2. Check Prediction Engine functions
cat("[Step 2] Testing prediction_engine.R core functions...\n")
test_dt <- get_test_data()
sample_fraud <- test_dt[1340, ]
sample_legit <- test_dt[1, ]

# Validation
val_f <- validate_transaction_input(sample_fraud)
stopifnot(val_f$valid)
val_l <- validate_transaction_input(sample_legit)
stopifnot(val_l$valid)
cat("✓ validate_transaction_input() verified on real test cases.\n")

# Single predictions across all 4 models + ensemble
for (m in c("random_forest", "xgboost", "logistic_regression", "isolation_forest")) {
  p <- predict_fraud_probability(val_f$clean_data, m)
  stopifnot(length(p) == 1 && is.finite(p))
  r_score <- calculate_risk_score(p, m)
  stopifnot(r_score >= 0 && r_score <= 100)
  r_cat <- classify_risk_category(r_score)
  stopifnot(as.character(r_cat) %in% c("Very Low Risk", "Low Risk", "Medium Risk", "High Risk", "Critical Risk"))
  pol <- apply_decision_policy(r_score, p, threshold = 0.50)
  stopifnot(!is.null(pol$decision))
  cat(sprintf("  ✓ Model %-20s: Output=%.4f, Risk Score=%.1f, Tier=%s, Decision=%s\n",
              m, p, r_score, as.character(r_cat), pol$decision))
}

# Multi-model comparison & ensemble
cat("  - Testing compare_model_predictions()...\n")
cmp <- compare_model_predictions(val_f$clean_data, threshold = 0.50)
stopifnot(nrow(cmp$models_table) == 5)
stopifnot(cmp$ensemble_prob >= 0 && cmp$ensemble_prob <= 1)
cat(sprintf("  ✓ 5-Row comparison matrix generated. Ensemble Prob: %.4f, Consensus: %s\n",
            cmp$ensemble_prob, cmp$consensus_label))

# Uncertainty analysis
cat("  - Testing compute_prediction_uncertainty()...\n")
unc <- compute_prediction_uncertainty(cmp$ensemble_prob, threshold = 0.50, disagreement_spread = cmp$disagreement_spread)
stopifnot(!is.null(unc$uncertainty_tier))
stopifnot(!is.null(unc$confidence_rating))
cat(sprintf("  ✓ Uncertainty Tier: %s (Index: %.1f), Confidence: %s\n",
            unc$uncertainty_tier, unc$uncertainty_index, unc$confidence_rating))

# Financial Exposure Simulator
cat("  - Testing calculate_financial_exposure()...\n")
fin <- calculate_financial_exposure(sample_fraud$amount, cmp$ensemble_prob, threshold = 0.50)
stopifnot(fin$potential_exposure >= 0)
stopifnot(nrow(fin$cost_benefit_curve) > 0)
cat(sprintf("  ✓ Financial exposure computed: Exposure=$%.2f, Net Benefit=$%.2f, Alert=%s\n",
            fin$potential_exposure, fin$net_expected_benefit, fin$is_alert))

# Batch Scoring
cat("  - Testing score_batch_transactions()...\n")
batch_sample <- test_dt[1:50, ]
batch_out <- score_batch_transactions(batch_sample, model_type = "random_forest", threshold = 0.50)
stopifnot(batch_out$summary$valid_count == 50)
stopifnot(nrow(batch_out$scored_data) == 50)
cat(sprintf("  ✓ Batch scored 50 records in %.2f seconds. Columns created: %s\n",
            batch_out$summary$elapsed_seconds,
            paste(intersect(names(batch_out$scored_data), c("fraud_probability", "risk_score", "risk_category", "decision_recommendation")), collapse = ", ")))

# SQLite Audit Registry
cat("  - Testing SQLite audit registry persistence & queries...\n")
init_prediction_audit_table()
audits <- get_prediction_audit_history(limit = 20)
stopifnot(nrow(audits) >= 20)
cat(sprintf("  ✓ Active SQLite audit registry verified with %d historical records.\n", nrow(audits)))

# 3. Test Prediction UI rendering
cat("\n[Step 3] Testing prediction_ui() DOM generation...\n")
ui_html <- prediction_ui("test_pred")
stopifnot(!is.null(ui_html))
ui_str <- as.character(ui_html)
stopifnot(grepl("1. Intelligence Overview", ui_str))
stopifnot(grepl("2. Single Assessment", ui_str))
stopifnot(grepl("3. Batch Assessment", ui_str))
stopifnot(grepl("4. Multi-Model &amp; Ensemble", ui_str))
stopifnot(grepl("5. Decision Policy Lab", ui_str))
stopifnot(grepl("6. Financial Simulator", ui_str))
stopifnot(grepl("7. Explainability", ui_str))
stopifnot(grepl("8. Uncertainty &amp; Confidence", ui_str))
stopifnot(grepl("9. Portfolio Analytics", ui_str))
stopifnot(grepl("10. Audit History", ui_str))
cat("✓ All 10 tabs confirmed present in prediction_ui() DOM.\n\n")

# 4. Test Full App UI
cat("[Step 4] Sourcing app/ui.R and testing complete application UI...\n")
source("app/ui.R")
app_ui_html <- as.character(ui)
stopifnot(nchar(app_ui_html) > 5000)
cat(sprintf("✓ Full app UI compiled successfully (%d bytes of rendered HTML).\n\n", nchar(app_ui_html)))

# 5. Run existing test suite to ensure zero regressions
cat("[Step 5] Running existing test suite tests/test_shiny_app.R ...\n")
source("tests/test_shiny_app.R")
cat("✓ Existing test suite passed with zero regressions!\n\n")

cat("=================================================================\n")
cat("ALL PREDICTION ENGINE & DASHBOARD TESTS PASSED!\n")
cat("=================================================================\n")

