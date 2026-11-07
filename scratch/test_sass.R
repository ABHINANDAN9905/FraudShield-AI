local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib) && !(proj_lib %in% .libPaths())) {
    .libPaths(c(proj_lib, .libPaths()))
  }
})
dir.create(".Rcache", showWarnings = FALSE)
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/"))
options(sass.cache = FALSE)
library(bslib)
th <- bs_theme(version = 5, bootswatch = "flatly")
cat("Theme created successfully!\n")

