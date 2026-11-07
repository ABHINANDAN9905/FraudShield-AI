# ==============================================================================
# FRAUDSHIELD — XAI & FORENSIC INTELLIGENCE SUITE VERIFICATION
# ==============================================================================

cat("=================================================================\n")
cat("Starting Comprehensive Explainability & Forensic Intelligence Test\n")
cat("=================================================================\n\n")

# 1. Load Global Environment
cat("[Step 1] Loading app/global.R ...\n")
source("app/global.R")
cat("✓ Global environment loaded.\n\n")

# 2. Check XAI Engine functions
cat("[Step 2] Testing xai_engine.R core functions...\n")
compat <- get_xai_compatibility_matrix()
stopifnot(nrow(compat) == 11)
stopifnot(all(c("Random_Forest", "XGBoost", "Logistic_Regression", "Isolation_Forest") %in% names(compat)))
cat("✓ get_xai_compatibility_matrix() verified with 11 techniques across 4 models.\n")

# Test DALEX Explainer Caching
cat("  - Testing DALEX explainer caching for Random Forest...\n")
exp_rf <- get_cached_explainer("Random Forest")
stopifnot(!is.null(exp_rf))
cat("  ✓ Random Forest explainer verified.\n")

cat("  - Testing DALEX explainer caching for XGBoost...\n")
exp_xgb <- get_cached_explainer("XGBoost")
stopifnot(!is.null(exp_xgb))
cat("  ✓ XGBoost explainer verified.\n")

cat("  - Testing DALEX explainer caching for Logistic Regression...\n")
exp_lr <- get_cached_explainer("Logistic Regression")
stopifnot(!is.null(exp_lr))
cat("  ✓ Logistic Regression explainer verified.\n")

# Test Local SHAP Breakdown
cat("  - Testing compute_local_shap_breakdown() on TX-01340...\n")
tx_sample <- get_test_data()[1340, ]
shap_res <- compute_local_shap_breakdown("Random Forest", tx_sample, B = 5)
stopifnot(!is.null(shap_res$contributions_df))
stopifnot(abs(shap_res$discrepancy) < 0.15)
cat(sprintf("  ✓ SHAP computed. Pred: %.4f, Base: %.4f, Recon: %.4f, Discrepancy: %.6f\n",
            shap_res$predicted_prob, shap_res$base_value, shap_res$reconstructed_pred, shap_res$discrepancy))

# Test Partial Dependence
cat("  - Testing compute_pdp_curve() for v14...\n")
pdp_v14 <- compute_pdp_curve("Random Forest", "v14", grid_size = 15)
stopifnot(nrow(pdp_v14) > 0)
cat(sprintf("  ✓ PDP curve computed with %d evaluation points.\n", nrow(pdp_v14)))

# Test Counterfactual
cat("  - Testing compute_counterfactual_explanation() on TX-01340...\n")
cf_res <- compute_counterfactual_explanation("Random Forest", tx_sample, target_class = 0, threshold = 0.50)
stopifnot(!is.null(cf_res$changes_made))
cat(sprintf("  ✓ Counterfactual search completed. Status: %s, Initial Prob: %.4f, Final Prob: %.4f, Distance: %.4f\n",
            cf_res$status, cf_res$original_prob, cf_res$counterfactual_prob, cf_res$distance))

# Test Cross-Model Explanation
cat("  - Testing compute_cross_model_explanation() on TX-01340...\n")
xm_res <- compute_cross_model_explanation(tx_sample)
stopifnot(nrow(xm_res$predictions) == 3)
stopifnot(nrow(xm_res$attribution_rank_correlation) == 2)
cat("  ✓ Cross-model predictions and Spearman correlations computed:\n")
print(xm_res$predictions[, c("Model", "Probability", "Decision")])
print(xm_res$attribution_rank_correlation)

# Test Reliability Lab
cat("  - Testing compute_explanation_reliability() on TX-01340...\n")
rel_res <- compute_explanation_reliability("Random Forest", tx_sample, n_runs = 5)
stopifnot(!is.null(rel_res$stability_status))
cat(sprintf("  ✓ Reliability diagnostics computed: Divergence: %.4f, Status: %s\n",
            rel_res$max_divergence, rel_res$stability_status))

# 3. Test Explainability UI rendering
cat("\n[Step 3] Testing explainability_ui() DOM generation...\n")
ui_html <- explainability_ui("test_xai")
stopifnot(!is.null(ui_html))
ui_str <- as.character(ui_html)
stopifnot(grepl("Overview &amp; Compatibility", ui_str))
stopifnot(grepl("Global Attribution", ui_str))
stopifnot(grepl("Transaction Investigation", ui_str))
stopifnot(grepl("SHAP Attribution Lab", ui_str))
stopifnot(grepl("Feature Behaviour", ui_str))
stopifnot(grepl("Counterfactual Lab", ui_str))
stopifnot(grepl("Cross-Model Agreement", ui_str))
stopifnot(grepl("Reliability Lab", ui_str))
stopifnot(grepl("Forensic Report Dossier", ui_str))
cat("✓ All 9 tabs confirmed present in explainability_ui() DOM.\n\n")

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
cat("ALL EXPLAINABILITY & FORENSIC INTELLIGENCE TESTS PASSED!\n")
cat("=================================================================\n")
