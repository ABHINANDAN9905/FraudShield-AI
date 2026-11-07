# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Module: Data Loading & Ingestion Pipeline
# Script: R/data_loading.R
# ==============================================================================
# Description:
#   Validates raw data archive existence, inspects archive contents, extracts
#   the raw credit card transactions CSV, loads it into memory using data.table,
#   performs schema & integrity validation, computes baseline class distribution,
#   and outputs an audit summary report.
# ==============================================================================

# Ensure local project library is available if present
local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib) && !(proj_lib %in% .libPaths())) {
    .libPaths(c(proj_lib, .libPaths()))
  }
})

# Verify dependency
if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("The 'data.table' package is required but not installed. Please run install.packages('data.table').")
}

suppressPackageStartupMessages(library(data.table))

# ------------------------------------------------------------------------------
# 1. Archive Inspection & Extraction
# ------------------------------------------------------------------------------

#' Inspect and extract raw transaction data from ZIP archive
#'
#' @param zip_path Character. Path to the ZIP file.
#' @param target_dir Character. Directory to extract into.
#' @param target_csv Character. Expected CSV filename inside the archive.
#' @return Character. Normalized path to the extracted CSV file.
inspect_and_extract_data <- function(zip_path = file.path("data", "raw", "creditcard.csv.zip"),
                                     target_dir = file.path("data", "raw"),
                                     target_csv = "creditcard.csv") {
  cat("\n====================================================================\n")
  cat(" [STAGE 1] ARCHIVE INSPECTION & EXTRACTION\n")
  cat("====================================================================\n")

  # Step 1: Check ZIP file existence
  if (!file.exists(zip_path)) {
    stop(sprintf("Raw archive not found at target path: '%s'", zip_path))
  }
  cat(sprintf(
    "[OK] Found raw archive: %s (Size: %.2f MB)\n",
    zip_path, file.info(zip_path)$size / (1024^2)
  ))

  # Step 2: Inspect ZIP contents
  zip_contents <- tryCatch(
    {
      utils::unzip(zip_path, list = TRUE)
    },
    error = function(e) {
      stop(sprintf("Failed to inspect archive '%s': %s", zip_path, e$message))
    }
  )

  cat("\nArchive Contents:\n")
  print(zip_contents)

  if (!target_csv %in% zip_contents$Name) {
    stop(sprintf(
      "Expected CSV '%s' not found inside archive: %s",
      target_csv, paste(zip_contents$Name, collapse = ", ")
    ))
  }

  csv_path <- file.path(target_dir, target_csv)

  # Step 3: Extract if not already extracted or if size mismatch
  if (file.exists(csv_path)) {
    cat(sprintf(
      "\n[INFO] Target CSV '%s' already exists in '%s'. Skipping re-extraction.\n",
      target_csv, target_dir
    ))
  } else {
    cat(sprintf("\n[INFO] Extracting '%s' into '%s'...\n", target_csv, target_dir))
    extract_status <- tryCatch(
      {
        utils::unzip(zip_path, files = target_csv, exdir = target_dir)
      },
      error = function(e) {
        stop(sprintf("Extraction failed for '%s': %s", zip_path, e$message))
      }
    )

    if (!file.exists(csv_path)) {
      stop(sprintf("Extraction completed but target file '%s' was not created.", csv_path))
    }
    cat(sprintf(
      "[OK] Extracted successfully: %s (Size: %.2f MB)\n",
      csv_path, file.info(csv_path)$size / (1024^2)
    ))
  }

  return(normalizePath(csv_path, winslash = "/"))
}

# ------------------------------------------------------------------------------
# 2. High-Performance CSV Ingestion
# ------------------------------------------------------------------------------

#' Ingest raw CSV dataset via data.table::fread
#'
#' @param csv_path Character. Path to the CSV file.
#' @return data.table containing raw transaction records.
load_raw_data <- function(csv_path) {
  cat("\n====================================================================\n")
  cat(" [STAGE 2] FAST CSV INGESTION (data.table::fread)\n")
  cat("====================================================================\n")

  if (!file.exists(csv_path)) {
    stop(sprintf("CSV file does not exist at: %s", csv_path))
  }

  cat(sprintf("[INFO] Ingesting: %s\n", csv_path))
  t_start <- Sys.time()

  dt <- tryCatch(
    {
      data.table::fread(csv_path, showProgress = FALSE)
    },
    error = function(e) {
      stop(sprintf("Failed to read CSV '%s': %s", csv_path, e$message))
    }
  )

  t_end <- Sys.time()
  cat(sprintf("[OK] Ingestion completed in %.2f seconds.\n", as.numeric(t_end - t_start, units = "secs")))

  return(dt)
}

# ------------------------------------------------------------------------------
# 3. Schema & Integrity Validation
# ------------------------------------------------------------------------------

#' Validate dataset schema against expected columns and properties
#'
#' @param dt data.table. The loaded dataset.
#' @param expected_count Integer. Number of expected columns (default: 31).
#' @return Logical. TRUE if validation passes, stops execution on error.
validate_raw_schema <- function(dt, expected_count = 31) {
  cat("\n====================================================================\n")
  cat(" [STAGE 3] SCHEMA & INTEGRITY VALIDATION\n")
  cat("====================================================================\n")

  # Validate column count
  actual_count <- ncol(dt)
  cat(sprintf("Checking column count: Expected = %d | Actual = %d\n", expected_count, actual_count))
  if (actual_count != expected_count) {
    stop(sprintf(
      "Schema mismatch! Expected %d columns, but found %d columns.",
      expected_count, actual_count
    ))
  }
  cat("[OK] Column count verification PASSED.\n")

  # Expected canonical column names for European cardholders fraud dataset
  expected_cols <- c("Time", paste0("V", 1:28), "Amount", "Class")
  missing_cols <- setdiff(expected_cols, names(dt))
  extra_cols <- setdiff(names(dt), expected_cols)

  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  if (length(extra_cols) > 0) {
    warning(sprintf("Unexpected additional columns detected: %s", paste(extra_cols, collapse = ", ")))
  }
  cat("[OK] Canonical column name alignment PASSED.\n")

  # Validate target column presence and levels
  if (!"Class" %in% names(dt)) {
    stop("Target column 'Class' is missing from the dataset.")
  }

  unique_classes <- sort(unique(dt$Class))
  cat(sprintf(
    "[OK] Target 'Class' column detected. Unique values: %s\n",
    paste(unique_classes, collapse = ", ")
  ))

  return(TRUE)
}

# ------------------------------------------------------------------------------
# 4. Exploratory Data Inspection
# ------------------------------------------------------------------------------

#' Display structural details, sample rows, and class balance
#'
#' @param dt data.table. The loaded dataset.
#' @return List containing summary metrics.
inspect_raw_data <- function(dt) {
  cat("\n====================================================================\n")
  cat(" [STAGE 4] DATA STRUCTURE & DISTRIBUTION INSPECTION\n")
  cat("====================================================================\n")

  # 1. Dimensions
  n_rows <- nrow(dt)
  n_cols <- ncol(dt)
  cat(sprintf(
    "\nDataset Dimensions:\n  Rows    : %s\n  Columns : %s\n",
    format(n_rows, big.mark = ","),
    format(n_cols, big.mark = ",")
  ))

  # 2. Column Names and Types
  col_info <- data.frame(
    Index = seq_along(names(dt)),
    Column = names(dt),
    Type = vapply(dt, typeof, character(1)),
    Class = vapply(dt, function(x) paste(class(x), collapse = "/"), character(1)),
    Missing = vapply(dt, function(x) sum(is.na(x)), integer(1)),
    stringsAsFactors = FALSE
  )
  cat("\nColumn Types & Schema Summary:\n")
  print(col_info, row.names = FALSE)

  # 3. First 6 records
  cat("\nFirst 6 Records (Head):\n")
  print(head(dt, 6))

  # 4. Class Distribution (Fraud vs Legitimate)
  class_counts <- dt[, .N, by = Class][order(Class)]
  class_counts[, Label := ifelse(Class == 1, "Fraudulent", "Legitimate")]
  class_counts[, Percentage := round((N / sum(N)) * 100, 4)]

  cat("\nTarget Class Distribution:\n")
  print(class_counts[, .(Class, Label, Count = format(N, big.mark = ","), `Percentage (%)` = Percentage)])

  legit_count <- dt[Class == 0, .N]
  fraud_count <- dt[Class == 1, .N]
  imbalance_ratio <- legit_count / max(1, fraud_count)

  cat(sprintf("\nImbalance Ratio: %.2f : 1 (Legitimate : Fraudulent)\n", imbalance_ratio))

  return(list(
    rows = n_rows,
    cols = n_cols,
    col_info = col_info,
    class_counts = class_counts,
    legit_count = legit_count,
    fraud_count = fraud_count,
    imbalance_ratio = imbalance_ratio
  ))
}

# ------------------------------------------------------------------------------
# 5. Audit Report Generation
# ------------------------------------------------------------------------------

#' Save import summary report in outputs/reports/
#'
#' @param summary_data List. Metrics returned by inspect_raw_data.
#' @param output_path Character. Target report file path.
#' @return Character. Path to written report.
save_import_summary <- function(summary_data,
                                output_path = file.path("outputs", "reports", "import_summary.txt")) {
  cat("\n====================================================================\n")
  cat(" [STAGE 5] AUDIT REPORT GENERATION\n")
  cat("====================================================================\n")

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  lines <- c(
    "================================================================================",
    " FraudShield - Raw Data Import Audit Summary Report",
    "================================================================================",
    sprintf(" Generated Timestamp : %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sprintf(" R Version           : %s", R.version.string),
    sprintf(" Platform            : %s", R.version$platform),
    "--------------------------------------------------------------------------------",
    " 1. INGESTION METRICS",
    "--------------------------------------------------------------------------------",
    sprintf(" Total Records (Rows): %s", format(summary_data$rows, big.mark = ",")),
    sprintf(" Total Columns       : %d", summary_data$cols),
    sprintf(" Expected Columns    : 31 (Validated: PASS)"),
    "--------------------------------------------------------------------------------",
    " 2. CLASS DISTRIBUTION",
    "--------------------------------------------------------------------------------",
    sprintf(
      " Legitimate (Class 0): %s (%.4f%%)",
      format(summary_data$legit_count, big.mark = ","),
      (summary_data$legit_count / summary_data$rows) * 100
    ),
    sprintf(
      " Fraudulent (Class 1): %s (%.4f%%)",
      format(summary_data$fraud_count, big.mark = ","),
      (summary_data$fraud_count / summary_data$rows) * 100
    ),
    sprintf(" Imbalance Ratio     : %.2f : 1 (Legitimate per 1 Fraud)", summary_data$imbalance_ratio),
    "--------------------------------------------------------------------------------",
    " 3. COLUMN SCHEMA BREAKDOWN",
    "--------------------------------------------------------------------------------"
  )

  col_lines <- capture.output(print(summary_data$col_info, row.names = FALSE))
  lines <- c(
    lines, col_lines,
    "================================================================================",
    " STATUS: ALL PRE-CLEANING VALIDATIONS COMPLETED SUCCESSFULLY.",
    "================================================================================"
  )

  writeLines(lines, con = output_path)
  cat(sprintf("[OK] Import summary saved successfully to: %s\n", output_path))

  return(output_path)
}

# ------------------------------------------------------------------------------
# Main Execution Pipeline
# ------------------------------------------------------------------------------

#' Execute the full data loading and ingestion workflow
#'
#' @param zip_path Character. Path to the raw ZIP archive.
#' @return data.table of raw transactions.
run_data_loading <- function(zip_path = file.path("data", "raw", "creditcard.csv.zip")) {
  cat("\n====================================================================\n")
  cat(" Starting FraudShield Data Ingestion Pipeline\n")
  cat("====================================================================\n")

  # 1. Inspect and extract
  csv_path <- inspect_and_extract_data(zip_path = zip_path)

  # 2. Ingest CSV
  dt <- load_raw_data(csv_path)

  # 3. Validate schema
  validate_raw_schema(dt, expected_count = 31)

  # 4. Inspect data
  summary_metrics <- inspect_raw_data(dt)

  # 5. Save audit report
  save_import_summary(summary_metrics)

  cat("\n[SUCCESS] FraudShield Data Loading module executed cleanly.\n\n")
  return(invisible(dt))
}

# Execute automatically when invoked directly via Rscript (sys.nframe() == 0L)
if (sys.nframe() == 0L) {
  raw_transactions <- run_data_loading()
}
