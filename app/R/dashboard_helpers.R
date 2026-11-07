# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Script: app/R/dashboard_helpers.R
# ==============================================================================
# Description:
#   UI design constants, KPI card builders, SQLite persistence helpers,
#   and formatting utilities for the FraudShield Shiny application.
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(htmltools)
  library(RSQLite)
  library(DBI)
  library(scales)
})

# ------------------------------------------------------------------------------
# 1. Fintech Design System & Color Palette
# ------------------------------------------------------------------------------
FS_COLORS <- list(
  navy_dark      = "#0B192C",
  navy_surface   = "#1E3E62",
  primary_blue   = "#0066FF",
  primary_hover  = "#0052CC",
  fraud_red      = "#DC2626",
  fraud_bg       = "#FEE2E2",
  legit_green    = "#16A34A",
  legit_bg       = "#DCFCE7",
  warning_amber  = "#F59E0B",
  warning_bg     = "#FEF3C7",
  card_bg        = "#FFFFFF",
  body_bg        = "#F8FAFC",
  text_primary   = "#0F172A",
  text_muted     = "#64748B",
  border_color   = "#E2E8F0"
)

# ------------------------------------------------------------------------------
# 2. Number & Financial Formatters
# ------------------------------------------------------------------------------
format_currency <- function(val) {
  if (is.null(val) || is.na(val)) return("$0.00")
  scales::dollar(val, accuracy = 0.01)
}

format_count <- function(val) {
  if (is.null(val) || is.na(val)) return("0")
  format(as.numeric(val), big.mark = ",")
}

format_percent <- function(val, digits = 2) {
  if (is.null(val) || is.na(val)) return("0.00%")
  sprintf(paste0("%.", digits, "f%%"), as.numeric(val) * 100)
}

# ------------------------------------------------------------------------------
# 3. KPI Card Component Builder
# ------------------------------------------------------------------------------
#' Construct an enterprise-grade fintech KPI Card
#'
#' @param title Card Header Label
#' @param value Primary Metric Value
#' @param subtitle Secondary Context Label
#' @param icon_name Bootstrap icon identifier or character
#' @param accent_color Hex color code for the left border and icon accent
#' @param bg_color Hex color for card background
#' @return Shiny HTML card element
create_kpi_card <- function(title,
                            value,
                            subtitle = NULL,
                            icon_name = "graph-up",
                            accent_color = "#0066FF",
                            bg_color = "#FFFFFF") {
  tags$div(
    class = "fs-kpi-card",
    style = sprintf(
      "background-color: %s; border-left: 5px solid %s; border-radius: 8px; padding: 18px 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.06); border-top: 1px solid #E2E8F0; border-right: 1px solid #E2E8F0; border-bottom: 1px solid #E2E8F0; height: 100%%;",
      bg_color, accent_color
    ),
    tags$div(
      style = "display: flex; justify-content: space-between; align-items: flex-start;",
      tags$div(
        tags$span(
          style = "font-size: 0.80rem; font-weight: 600; text-transform: uppercase; letter-spacing: 0.05em; color: #64748B;",
          title
        ),
        tags$div(
          style = sprintf("font-size: 1.75rem; font-weight: 700; color: #0F172A; margin: 4px 0;"),
          value
        ),
        if (!is.null(subtitle)) {
          tags$span(style = "font-size: 0.82rem; color: #64748B;", subtitle)
        }
      ),
      tags$div(
        style = sprintf(
          "background-color: %s1A; color: %s; width: 44px; height: 44px; border-radius: 10px; display: flex; align-items: center; justify-content: center; font-size: 1.3rem;",
          accent_color, accent_color
        ),
        icon(icon_name)
      )
    )
  )
}

# ------------------------------------------------------------------------------
# 4. Status & Risk Badge Builder
# ------------------------------------------------------------------------------
render_risk_badge <- function(risk_tier) {
  tier <- as.character(risk_tier)
  style_map <- list(
    "Low Risk"      = list(bg = "#DCFCE7", fg = "#16A34A", border = "#86EFAC"),
    "Medium Risk"   = list(bg = "#FEF3C7", fg = "#D97706", border = "#FDE68A"),
    "High Risk"     = list(bg = "#FFEDD5", fg = "#EA580C", border = "#FDBA74"),
    "Critical Risk" = list(bg = "#FEE2E2", fg = "#DC2626", border = "#FCA5A5")
  )
  cfg <- if (tier %in% names(style_map)) style_map[[tier]] else list(bg = "#F1F5F9", fg = "#475569", border = "#CBD5E1")
  
  sprintf(
    '<span style="background-color: %s; color: %s; border: 1px solid %s; padding: 3px 8px; border-radius: 6px; font-weight: 600; font-size: 0.78rem; text-transform: uppercase;">%s</span>',
    cfg$bg, cfg$fg, cfg$border, tier
  )
}

render_investigation_badge <- function(status) {
  st <- as.character(status)
  style_map <- list(
    "Pending"      = list(bg = "#FEF3C7", fg = "#D97706"),
    "Under Review" = list(bg = "#DBEAFE", fg = "#1D4ED8"),
    "Reviewed"     = list(bg = "#DCFCE7", fg = "#16A34A"),
    "Escalated"    = list(bg = "#FEE2E2", fg = "#DC2626")
  )
  cfg <- if (st %in% names(style_map)) style_map[[st]] else list(bg = "#F1F5F9", fg = "#475569")
  
  sprintf(
    '<span style="background-color: %s; color: %s; padding: 4px 10px; border-radius: 9999px; font-weight: 600; font-size: 0.76rem;">%s</span>',
    cfg$bg, cfg$fg, st
  )
}

# ------------------------------------------------------------------------------
# 5. SQLite Persistence Engine for Investigation Center
# ------------------------------------------------------------------------------
get_db_path <- function() {
  root <- if (exists("get_project_root")) get_project_root() else getwd()
  db_dir <- file.path(root, "data")
  if (!dir.exists(db_dir)) dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(db_dir, "investigations.sqlite")
}

#' Initialize Investigation SQLite Database
init_investigation_db <- function() {
  db_path <- get_db_path()
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  create_table_sql <- "
    CREATE TABLE IF NOT EXISTS investigations (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      transaction_id TEXT UNIQUE,
      timestamp TEXT,
      amount REAL,
      predicted_prob REAL,
      anomaly_score REAL,
      risk_tier TEXT,
      actual_class INTEGER,
      status TEXT,
      notes TEXT,
      updated_at TEXT
    );
  "
  DBI::dbExecute(conn, create_table_sql)
  
  # Check if table has rows, if not, seed with top flagged test records
  count_res <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM investigations")
  if (count_res$n == 0) {
    seed_initial_investigations(conn)
  }
}

seed_initial_investigations <- function(conn) {
  test_dt <- tryCatch(get_test_data(), error = function(e) NULL)
  if (is.null(test_dt)) return(NULL)
  
  # Pick top 20 fraud cases and top 10 suspicious legitimate cases from test set
  set.seed(42)
  fraud_sub <- test_dt[class == 1][1:min(20, sum(class == 1))]
  legit_sub <- test_dt[class == 0][1:10]
  
  seed_records <- rbind(fraud_sub, legit_sub)
  
  # Model scoring
  rf_model <- tryCatch(get_cached_model("random_forest"), error = function(e) NULL)
  xgb_model <- tryCatch(get_cached_model("xgboost"), error = function(e) NULL)
  iso_model <- tryCatch(get_cached_model("isolation_forest"), error = function(e) NULL)
  
  pred_cols <- c(paste0("v", 1:28), "amount_log", "elapsed_hours")
  X_clean <- as.data.frame(seed_records[, ..pred_cols])
  
  probs <- if (!is.null(rf_model)) {
    predict(rf_model, data = X_clean)$predictions[, "Fraud"]
  } else {
    rep(0.5, nrow(seed_records))
  }
  
  anom_scores <- if (!is.null(iso_model)) {
    predict(iso_model, as.matrix(X_clean), type = "score")
  } else {
    rep(0.5, nrow(seed_records))
  }
  
  for (i in seq_len(nrow(seed_records))) {
    p <- round(probs[i], 4)
    a <- round(anom_scores[i], 4)
    tier <- if (p >= 0.80) "Critical Risk" else if (p >= 0.50) "High Risk" else if (p >= 0.20) "Medium Risk" else "Low Risk"
    st <- if (p >= 0.80) "Escalated" else if (p >= 0.50) "Under Review" else "Pending"
    tx_id <- sprintf("TX-%06d", i + 1000)
    
    insert_sql <- "
      INSERT OR IGNORE INTO investigations (
        transaction_id, timestamp, amount, predicted_prob, anomaly_score, risk_tier, actual_class, status, notes, updated_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
    "
    DBI::dbExecute(conn, insert_sql, params = list(
      tx_id,
      format(Sys.time() - (i * 3600), "%Y-%m-%d %H:%M:%S"),
      round(seed_records$amount[i], 2),
      p,
      a,
      tier,
      as.integer(seed_records$class[i]),
      st,
      sprintf("Auto-flagged during batch risk evaluation. High confidence outlier vector."),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ))
  }
}

fetch_investigations <- function(status_filter = "All", risk_filter = "All") {
  db_path <- get_db_path()
  if (!file.exists(db_path)) init_investigation_db()
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  query <- "SELECT * FROM investigations WHERE 1=1"
  params <- list()
  
  if (status_filter != "All") {
    query <- paste0(query, " AND status = ?")
    params <- c(params, status_filter)
  }
  if (risk_filter != "All") {
    query <- paste0(query, " AND risk_tier = ?")
    params <- c(params, risk_filter)
  }
  
  query <- paste0(query, " ORDER BY id DESC")
  
  if (length(params) > 0) {
    DBI::dbGetQuery(conn, query, params = params)
  } else {
    DBI::dbGetQuery(conn, query)
  }
}

update_investigation <- function(record_id, new_status, new_notes) {
  db_path <- get_db_path()
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  now_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  sql <- "UPDATE investigations SET status = ?, notes = ?, updated_at = ? WHERE id = ?"
  DBI::dbExecute(conn, sql, params = list(new_status, new_notes, now_str, record_id))
}

insert_new_investigation <- function(tx_id, amount, prob, anom, risk_tier, actual_class = NA, notes = "") {
  db_path <- get_db_path()
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  now_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  sql <- "
    INSERT INTO investigations (
      transaction_id, timestamp, amount, predicted_prob, anomaly_score, risk_tier, actual_class, status, notes, updated_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, 'Pending', ?, ?);
  "
  DBI::dbExecute(conn, sql, params = list(
    tx_id, now_str, amount, prob, anom, risk_tier, actual_class, notes, now_str
  ))
}

