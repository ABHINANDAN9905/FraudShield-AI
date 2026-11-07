cat("--- Testing app/R/prediction_engine.R ---\n")
source("app/global.R")
source("app/R/prediction_engine.R")

# 1. Validation test
cat("\n[1] Testing input validation...\n")
bad_input <- data.frame(a = 1, b = 2)
v1 <- validate_transaction_input(bad_input)
stopifnot(!v1$valid)
cat("✓ Missing columns caught correctly:\n", paste(v1$errors, collapse = "\n"), "\n")

# Valid test
test_dt <- get_test_data()
sample_tx <- test_dt[1340, ]
v2 <- validate_transaction_input(sample_tx)
stopifnot(v2$valid)
cat("✓ Valid test row passed validation.\n")

# 2. Probability prediction
cat("\n[2] Testing predict_fraud_probability()...\n")
p_rf  <- predict_fraud_probability(v2$clean_data, "random_forest")
p_xgb <- predict_fraud_probability(v2$clean_data, "xgboost")
p_lr  <- predict_fraud_probability(v2$clean_data, "logistic_regression")
s_iso <- predict_fraud_probability(v2$clean_data, "isolation_forest")
cat(sprintf("✓ Predictions: RF=%.4f, XGB=%.4f, LR=%.4f, ISO=%.4f\n", p_rf, p_xgb, p_lr, s_iso))

# 3. Risk scoring & classification
cat("\n[3] Testing risk score & categorization...\n")
r_score <- calculate_risk_score(p_rf, "random_forest")
r_cat   <- classify_risk_category(r_score)
cat(sprintf("✓ RF Risk Score: %.1f -> Category: %s\n", r_score, r_cat))

# 4. Decision policy
cat("\n[4] Testing decision policy...\n")
pol <- apply_decision_policy(r_score, p_rf, threshold = 0.50)
cat(sprintf("✓ Decision: %s (Action: %s)\nReason: %s\n", pol$decision, pol$action_code, pol$reason))

# 5. Multi-model comparison & ensemble
cat("\n[5] Testing compare_model_predictions()...\n")
cmp <- compare_model_predictions(v2$clean_data, threshold = 0.50)
print(cmp$models_table)
cat(sprintf("✓ Consensus: %s | Spread: %.4f | Ensemble Prob: %.4f\n",
            cmp$consensus_label, cmp$disagreement_spread, cmp$ensemble_prob))

# 6. Uncertainty
cat("\n[6] Testing compute_prediction_uncertainty()...\n")
unc <- compute_prediction_uncertainty(p_rf, threshold = 0.50, disagreement_spread = cmp$disagreement_spread)
cat(sprintf("✓ Boundary Dist: %.4f | Uncertainty: %.1f (%s) | Conf: %s\n",
            unc$boundary_distance, unc$uncertainty_index, unc$uncertainty_tier, unc$confidence_rating))

# 7. Financial simulation
cat("\n[7] Testing calculate_financial_exposure()...\n")
fin <- calculate_financial_exposure(sample_tx$amount, p_rf, threshold = 0.50)
cat(sprintf("✓ Amount: $%.2f | Net Benefit: $%.2f | Flagged: %s\n",
            fin$amount, fin$net_expected_benefit, fin$is_alert))

# 8. Audit persistence & retrieval
cat("\n[8] Testing SQLite audit registry...\n")
init_prediction_audit_table()
audits <- get_prediction_audit_history(limit = 10)
cat(sprintf("✓ Successfully retrieved %d audit records from SQLite.\n", nrow(audits)))
print(head(audits[, c("prediction_id", "transaction_id", "model_name", "risk_score", "risk_category", "decision_recommendation")], 3))

# Test saving single audit
rec <- list(
  prediction_id = "PRED-TEST-0001",
  transaction_id = "TX-01340",
  model_name = "Random Forest (Champion)",
  amount = 1.00,
  raw_probability = p_rf,
  anomaly_score = s_iso,
  risk_score = r_score,
  risk_category = as.character(r_cat),
  predicted_class = 1,
  decision_threshold = 0.50,
  decision_recommendation = pol$decision,
  action_code = pol$action_code,
  calibration_status = "Tree Frequency Calibrated",
  policy_version = pol$policy_version
)
save_prediction_audit(rec)
audits_updated <- get_prediction_audit_history(limit = 5, search_query = "PRED-TEST-0001")
stopifnot(nrow(audits_updated) == 1)
cat("✓ Test audit record saved and verified in SQLite.\n")

# 9. Batch scoring
cat("\n[9] Testing score_batch_transactions()...\n")
batch_sample <- test_dt[1:30, ]
batch_res <- score_batch_transactions(batch_sample, model_type = "random_forest", threshold = 0.50)
cat(sprintf("✓ Batch completed: %d total, %d valid, %d high risk, %d critical risk, in %.2f sec\n",
            batch_res$summary$total_uploaded, batch_res$summary$valid_count,
            batch_res$summary$high_risk_count, batch_res$summary$critical_risk_count,
            batch_res$summary$elapsed_seconds))

cat("\n=============================================\n")
cat("ALL PREDICTION ENGINE BACKEND TESTS PASSED!\n")
cat("=============================================\n")

