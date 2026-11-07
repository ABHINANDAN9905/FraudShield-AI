# FraudShield - R Project Environment Setup
local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib)) {
    .libPaths(c(proj_lib, .libPaths()))
  }
  cache_dir <- file.path(getwd(), ".Rcache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(R_USER_CACHE_DIR = normalizePath(cache_dir, winslash = "/"))
  options(sass.cache = FALSE)
})
