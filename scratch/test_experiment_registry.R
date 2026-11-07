cand1 <- file.path(getwd(), ".Rlibrary")
if (dir.exists(cand1)) .libPaths(c(cand1, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = normalizePath(".Rcache", winslash = "/", mustWork = FALSE))

library(DBI)
library(RSQLite)

db_path <- file.path(getwd(), "data", "investigations.sqlite")
con <- dbConnect(SQLite(), db_path)

# Initialize experiments table if not exists
dbExecute(con, "
CREATE TABLE IF NOT EXISTS ml_experiments (
  experiment_id TEXT PRIMARY KEY,
  model_name TEXT NOT NULL,
  model_version TEXT NOT NULL,
  paradigm TEXT NOT NULL,
  hyperparameters TEXT,
  random_seed INTEGER,
  training_date TEXT,
  evaluation_date TEXT,
  train_rows INTEGER,
  test_rows INTEGER,
  pr_auc REAL,
  roc_auc REAL,
  recall REAL,
  precision REAL,
  f1_score REAL,
  optimal_threshold REAL,
  experiment_notes TEXT,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
)")

# Seed baseline runs from verified artifacts if table is empty
count <- dbGetQuery(con, "SELECT COUNT(*) as n FROM ml_experiments")$n
cat("Existing experiment records:", count, "\n")

if (count == 0) {
  cat("Seeding official verified model experiments...\n")
  seed_runs <- data.frame(
    experiment_id = c("EXP-2026-001", "EXP-2026-002", "EXP-2026-003", "EXP-2026-004"),
    model_name = c("Random Forest", "XGBoost", "Logistic Regression", "Isolation Forest"),
    model_version = c("v1.2.0-champion", "v1.1.0-challenger", "v1.0.0-baseline", "v0.9.0-unsupervised"),
    paradigm = c("Supervised Ensemble", "Gradient Boosted Trees", "Penalized GLM", "Unsupervised Anomaly"),
    hyperparameters = c(
      "trees=80, mtry=6, min_node_size=5, splitrule=gini",
      "nrounds=100, max_depth=6, eta=0.1, scale_pos_weight=570",
      "family=binomial, link=logit, max_iter=100",
      "ntrees=100, sample_size=256, ndim=1"
    ),
    random_seed = c(42, 42, 42, 42),
    training_date = c("2026-11-05 00:59:37", "2026-11-05 00:59:41", "2026-11-05 00:58:28", "2026-11-05 00:59:42"),
    evaluation_date = c("2026-11-05 01:00:15", "2026-11-05 01:00:15", "2026-11-05 01:00:15", "2026-11-05 01:00:15"),
    train_rows = c(227845, 227845, 227845, 227845),
    test_rows = c(56962, 56962, 56962, 56962),
    pr_auc = c(0.8782, 0.8523, 0.7625, 0.2715),
    roc_auc = c(0.9664, 0.9860, 0.9707, 0.9549),
    recall = c(0.8065, 0.8387, 0.6129, 0.3978),
    precision = c(0.9615, 0.7500, 0.8382, 0.2868),
    f1_score = c(0.8772, 0.7919, 0.7081, 0.3333),
    optimal_threshold = c(0.50, 0.50, 0.50, 0.58),
    experiment_notes = c(
      "Champion architecture: Highest PR-AUC (0.8782) and Precision (96.15%). Only 3 false alarms in test set.",
      "Challenger: Highest ROC-AUC (0.9860) and Recall (83.87%), but 26 false positives.",
      "Baseline GLM: Strong explainability, moderate sensitivity (61.29% recall).",
      "Unsupervised benchmark: Anomaly scoring without labels; evaluated at score percentile 0.58."
    ),
    stringsAsFactors = FALSE
  )
  dbWriteTable(con, "ml_experiments", seed_runs, append = TRUE)
  cat("Seeded 4 official model experiments successfully.\n")
}

exps <- dbGetQuery(con, "SELECT experiment_id, model_name, model_version, pr_auc, recall, precision FROM ml_experiments")
print(exps)
dbDisconnect(con)

