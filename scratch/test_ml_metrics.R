cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))

source("app/R/dashboard_data.R")

test_dt <- get_scored_test_data("random_forest", threshold = 0.50)

# True labels and predicted probabilities
y_true <- test_dt$class
p_pred <- test_dt$fraud_probability
thresh <- 0.50
y_pred <- as.integer(p_pred >= thresh)

# Confusion Matrix
tp <- sum(y_true == 1 & y_pred == 1)
fp <- sum(y_true == 0 & y_pred == 1)
fn <- sum(y_true == 1 & y_pred == 0)
tn <- sum(y_true == 0 & y_pred == 0)
n  <- length(y_true)

cat(sprintf("TP=%d, FP=%d, FN=%d, TN=%d (N=%d)\n", tp, fp, fn, tn, n))

# Metrics
precision <- tp / (tp + fp)
recall    <- tp / (tp + fn)
spec      <- tn / (tn + fp)
bal_acc   <- (recall + spec) / 2
f1        <- 2 * precision * recall / (precision + recall)
f2        <- 5 * precision * recall / (4 * precision + recall)
f05       <- 1.25 * precision * recall / (0.25 * precision + recall)
mcc       <- (tp * tn - fp * fn) / sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) * as.numeric(tn + fp) * as.numeric(tn + fn))
gmean     <- sqrt(recall * spec)
npv       <- tn / (tn + fn)
fpr       <- fp / (fp + tn)
fnr       <- fn / (fn + tp)

# Probability metrics
brier <- mean((p_pred - y_true)^2)
eps <- 1e-15
p_clipped <- pmin(pmax(p_pred, eps), 1 - eps)
logloss <- -mean(y_true * log(p_clipped) + (1 - y_true) * log(1 - p_clipped))

# Calibration ECE (10 bins)
bins <- cut(p_pred, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
bin_df <- data.table(p = p_pred, y = y_true, bin = bins)[, .(
  count = .N,
  conf = mean(p),
  acc = mean(y)
), by = bin]
ece <- sum(bin_df$count / n * abs(bin_df$acc - bin_df$conf))

cat(sprintf("Precision=%.4f, Recall=%.4f, Specificity=%.4f, F1=%.4f, F2=%.4f\n", precision, recall, spec, f1, f2))
cat(sprintf("Balanced Acc=%.4f, MCC=%.4f, G-Mean=%.4f, NPV=%.4f\n", bal_acc, mcc, gmean, npv))
cat(sprintf("Brier Score=%.6f, Log Loss=%.6f, ECE=%.6f\n", brier, logloss, ece))

# Financial Cost Simulation
fraud_exposure <- sum(test_dt[class == 1, amount])
missed_loss    <- sum(test_dt[class == 1 & is_alert == 0, amount])
caught_loss    <- sum(test_dt[class == 1 & is_alert == 1, amount])
fp_cost        <- fp * 15 # $15 investigation cost per alert
total_cost     <- missed_loss + fp_cost
net_benefit    <- fraud_exposure - total_cost

cat(sprintf("\nFinancial Simulation:\n  Total Exposure: $%.2f\n  Missed Loss (FN): $%.2f\n  Investigated (FP Cost): $%.2f\n  Prevented: $%.2f\n  Net Benefit: $%.2f\n",
            fraud_exposure, missed_loss, fp_cost, caught_loss, net_benefit))

