cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))

pkgs <- c("pROC", "PRROC", "yardstick", "ranger", "xgboost", "isotree", "probably", "vip", "DALEX", "fastshap", "DBI", "RSQLite", "digest", "data.table", "plotly", "DT", "bslib", "shiny")
res <- sapply(pkgs, function(p) requireNamespace(p, quietly = TRUE))
print(res)

