cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))

source("app/R/dashboard_data.R")

test_dt <- get_scored_test_data("random_forest")
y_true <- test_dt$class
p_pred <- test_dt$fraud_probability

# Stratified or standard bootstrap
set.seed(42)
B <- 100
n <- length(y_true)

fraud_idx <- which(y_true == 1)
legit_idx <- which(y_true == 0)

boot_rec <- numeric(B)
boot_prec <- numeric(B)
boot_f1 <- numeric(B)

for (b in 1:B) {
  s_f <- sample(fraud_idx, length(fraud_idx), replace = TRUE)
  s_l <- sample(legit_idx, min(length(legit_idx), 2000), replace = TRUE) # Subsample legit for speed
  idx <- c(s_f, s_l)
  
  y_b <- y_true[idx]
  p_b <- p_pred[idx]
  pred_b <- as.integer(p_b >= 0.50)
  
  tp <- sum(y_b == 1 & pred_b == 1)
  fp <- sum(y_b == 0 & pred_b == 1)
  fn <- sum(y_b == 1 & pred_b == 0)
  
  rec <- if (tp + fn > 0) tp / (tp + fn) else 0
  prec <- if (tp + fp > 0) tp / (tp + fp) else 0
  f1 <- if (rec + prec > 0) 2 * rec * prec / (rec + prec) else 0
  
  boot_rec[b] <- rec
  boot_prec[b] <- prec
  boot_f1[b] <- f1
}

cat(sprintf("Recall: %.2f%% [95%% CI: %.2f%% - %.2f%%]\n",
            mean(boot_rec)*100, quantile(boot_rec, 0.025)*100, quantile(boot_rec, 0.975)*100))
cat(sprintf("Precision: %.2f%% [95%% CI: %.2f%% - %.2f%%]\n",
            mean(boot_prec)*100, quantile(boot_prec, 0.025)*100, quantile(boot_prec, 0.975)*100))
cat(sprintf("F1-Score: %.4f [95%% CI: %.4f - %.4f]\n",
            mean(boot_f1), quantile(boot_f1, 0.025), quantile(boot_f1, 0.975)))

