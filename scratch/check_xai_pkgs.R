cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))

pkgs <- c("DALEX", "ingredients", "iBreakDown", "fastshap", "kernelshap", "shapr", "iml", "vip", "pdp")
res <- sapply(pkgs, function(p) requireNamespace(p, quietly = TRUE))
print(res)

