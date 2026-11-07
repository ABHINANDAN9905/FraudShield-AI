cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))

source("app/R/dashboard_data.R")
library(plotly)

test_dt <- get_test_data()
top_cols <- c("v17", "v14", "v12", "v10", "v16", "v11", "v4", "v2", "amount", "class")

# Calculate pairwise cor matrix safely
numeric_sub <- as.matrix(test_dt[, ..top_cols])
c_mat <- round(cor(numeric_sub, use = "pairwise.complete.obs"), 3)

p <- plot_ly(
  x = top_cols,
  y = top_cols,
  z = c_mat,
  type = "heatmap",
  colorscale = list(
    c(0, "#1E3A8A"),    # Corporate Navy (-1)
    c(0.5, "#F8FAFC"),  # Neutral (0)
    c(1, "#DC2626")     # Crimson Red (+1)
  ),
  zmin = -1,
  zmax = 1,
  hoverongaps = FALSE,
  hovertemplate = "Var 1: %{x}<br>Var 2: %{y}<br>Correlation r: %{z:.3f}<extra></extra>"
) %>% layout(
  margin = list(t = 30, b = 40, l = 50, r = 20),
  paper_bgcolor = "rgba(0,0,0,0)",
  plot_bgcolor = "rgba(0,0,0,0)"
)

cat("Heatmap created successfully without error!\n")

