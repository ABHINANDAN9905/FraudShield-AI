cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))

# Test various functions for "argument 1 is not a vector"
f_list <- list(
  is.vector = function() is.vector(),
  as.vector = function() as.vector(),
  vector = function() vector(),
  cor = function() cor(),
  cor.test = function() cor.test()
)

# Test tibble or dplyr
library(dplyr)
library(ggplot2)
library(plotly)

# Let's test if any ggplot / scales / plotly function throws this
# For instance, scale_x_discrete, coord_flip, etc.
cat("Testing functions...\n")

