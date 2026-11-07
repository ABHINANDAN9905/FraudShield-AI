cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))

source("app/R/dashboard_data.R")
library(DALEX)
library(iBreakDown)

xgb <- get_cached_model("xgboost")
test_dt <- get_test_data()
pred_cols <- c(paste0("v", 1:28), "amount_log", "elapsed_hours")

bg_data <- as.data.frame(test_dt[1:50, ..pred_cols])
target_tx <- as.data.frame(test_dt[1340, ..pred_cols])

p_fun_xgb <- function(model, newdata) {
  predict(model, as.matrix(newdata))
}

exp_xgb <- DALEX::explain(
  model = xgb,
  data = bg_data,
  predict_function = p_fun_xgb,
  label = "XGBoost",
  verbose = FALSE
)

pred_val <- p_fun_xgb(xgb, target_tx)
cat("XGB predicted prob:", pred_val, "\n")
bd_xgb <- iBreakDown::shap(exp_xgb, new_observation = target_tx, B = 5)
cat("XGB SHAP success!\n")
print(head(bd_xgb, 5))

