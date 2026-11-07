cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))

library(data.table)
dt <- data.table(v1 = rnorm(10), class = sample(0:1, 10, replace = TRUE))

cat("Test 1: cor(dt$v1, dt[, 'class'])\n")
tryCatch({
  cor(dt$v1, dt[, 'class'])
}, error = function(e) cat("ERR 1:", conditionMessage(e), "\n"))

cat("Test 2: cor(dt[, 'v1'], dt$class)\n")
tryCatch({
  cor(dt[, 'v1'], dt$class)
}, error = function(e) cat("ERR 2:", conditionMessage(e), "\n"))

cat("Test 3: cor(dt[, 1], dt[, 2])\n")
tryCatch({
  cor(dt[, 1], dt[, 2])
}, error = function(e) cat("ERR 3:", conditionMessage(e), "\n"))

cat("Test 4: col2rgb('rgba(0, 102, 255, 0.4)')\n")
tryCatch({
  col2rgb('rgba(0, 102, 255, 0.4)')
}, error = function(e) cat("ERR 4:", conditionMessage(e), "\n"))

