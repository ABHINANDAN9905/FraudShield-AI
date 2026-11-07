# ==============================================================================
# FraudShield - Financial Fraud Detection & Risk Intelligence Platform
# Module: Data Validation Pipeline
# Script: R/data_validation.R
# ==============================================================================
# Description:
#   Performs automated comprehensive data integrity, schema, missingness,
#   duplicate detection, value distribution, and statistical validation
#   on the FraudShield credit card transactions dataset.
#   Generates a formal CSV validation audit report in outputs/reports/.
# ==============================================================================

# Ensure local project library is available if present
local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib) && !(proj_lib %in% .libPaths())) {
    .libPaths(c(proj_lib, .libPaths()))
  }
})

# Verify data.table dependency
if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required. Please install it with install.packages('data.table').")
}

suppressPackageStartupMessages(library(data.table))

# ------------------------------------------------------------------------------
# Modular Validation Functions
# ------------------------------------------------------------------------------

#' Validate dataset dimensions
#' @param dt data.table/data.frame to inspect.
#' @return list with row and column counts.
check_dimensions <- function(dt) {
  n_rows <- nrow(dt)
  n_cols <- ncol(dt)
  cat(sprintf("[1. DIMENSIONS] Rows: %s | Columns: %s\n",
              format(n_rows, big.mark = ","),
              format(n_cols, big.mark = ",")))
  return(list(rows = n_rows, cols = n_cols))
}

#' Validate required canonical columns
#' @param dt data.table/data.frame to inspect.
#' @param expected_cols Character vector of expected column names.
#' @return list with validation flags and details.
check_required_columns <- function(dt,
                                   expected_cols = c("Time", paste0("V", 1:28), "Amount", "Class")) {
  actual_cols <- names(dt)
  missing_cols <- setdiff(expected_cols, actual_cols)
  extra_cols <- setdiff(actual_cols, expected_cols)
  matches_expected <- (length(missing_cols) == 0 && length(actual_cols) == length(expected_cols))

  cat(sprintf("[2. COLUMNS] Expected: %d | Actual: %d | Matches canonical: %s\n",
              length(expected_cols), length(actual_cols), matches_expected))

  if (length(missing_cols) > 0) {
    cat(sprintf("    ALERT: Missing columns: %s\n", paste(missing_cols, collapse = ", ")))
  }
  if (length(extra_cols) > 0) {
    cat(sprintf("    INFO: Extra columns: %s\n", paste(extra_cols, collapse = ", ")))
  }

  return(list(
    expected_count = length(expected_cols),
    actual_count = length(actual_cols),
    is_valid = matches_expected,
    missing = missing_cols,
    extra = extra_cols
  ))
}

#' Check missing values (NA / NaN) across all columns
#' @param dt data.table/data.frame.
#' @return data.frame with per-column NA counts and overall summary.
check_missing_values <- function(dt) {
  na_per_col <- vapply(dt, function(x) sum(is.na(x)), integer(1))
  total_na <- sum(na_per_col)
  cols_with_na <- sum(na_per_col > 0)

  cat(sprintf("[3. MISSING VALUES] Total NAs: %d across %d columns with missing values\n",
              total_na, cols_with_na))

  return(list(
    total_na = total_na,
    cols_with_na = cols_with_na,
    na_per_col = na_per_col
  ))
}

#' Identify duplicate records
#' @param dt data.table.
#' @return list with duplicate counts and class breakdown.
check_duplicates <- function(dt) {
  dup_flags <- duplicated(dt)
  dup_count <- sum(dup_flags)
  n_rows <- nrow(dt)
  dup_pct <- (dup_count / n_rows) * 100

  dup_by_class <- if ("Class" %in% names(dt)) {
    table(dt$Class[dup_flags])
  } else {
    table(integer(0))
  }

  dup_legit <- if ("0" %in% names(dup_by_class)) as.integer(dup_by_class[["0"]]) else 0L
  dup_fraud <- if ("1" %in% names(dup_by_class)) as.integer(dup_by_class[["1"]]) else 0L

  cat(sprintf("[4. DUPLICATES] Found: %s duplicate rows (%.4f%%)\n",
              format(dup_count, big.mark = ","), dup_pct))
  cat(sprintf("    Breakdown -> Legitimate (Class 0): %s | Fraudulent (Class 1): %s\n",
              format(dup_legit, big.mark = ","),
              format(dup_fraud, big.mark = ",")))

  return(list(
    dup_count = dup_count,
    dup_pct = dup_pct,
    dup_legit = dup_legit,
    dup_fraud = dup_fraud
  ))
}

#' Validate data types across columns
#' @param dt data.table.
#' @return list with data type summary and flag for anomalies.
check_data_types <- function(dt) {
  type_vec <- vapply(dt, typeof, character(1))
  class_vec <- vapply(dt, function(x) class(x)[1], character(1))

  numeric_cols <- sum(type_vec %in% c("double", "integer"))
  non_numeric_cols <- sum(!type_vec %in% c("double", "integer"))

  cat(sprintf("[5. DATA TYPES] Numeric/Double columns: %d | Non-numeric: %d\n",
              numeric_cols, non_numeric_cols))

  return(list(
    types = type_vec,
    classes = class_vec,
    numeric_count = numeric_cols,
    non_numeric_count = non_numeric_cols
  ))
}

#' Calculate fraud and legitimate transaction class distribution
#' @param dt data.table with 'Class' column.
#' @return list with counts, percentages, and imbalance ratio.
check_class_distribution <- function(dt) {
  if (!"Class" %in% names(dt)) {
    stop("Column 'Class' not found in dataset.")
  }

  n_total <- nrow(dt)
  legit_count <- sum(dt$Class == 0, na.rm = TRUE)
  fraud_count <- sum(dt$Class == 1, na.rm = TRUE)
  other_count <- n_total - (legit_count + fraud_count)

  legit_pct <- (legit_count / n_total) * 100
  fraud_pct <- (fraud_count / n_total) * 100
  imbalance_ratio <- legit_count / max(1, fraud_count)

  cat(sprintf("[6 & 7. CLASS COUNTS & FRAUD PERCENTAGE]\n"))
  cat(sprintf("    Legitimate (Class 0) : %s records (%.4f%%)\n",
              format(legit_count, big.mark = ","), legit_pct))
  cat(sprintf("    Fraudulent (Class 1) : %s records (%.4f%%)\n",
              format(fraud_count, big.mark = ","), fraud_pct))
  cat(sprintf("    Imbalance Ratio      : %.2f : 1\n", imbalance_ratio))

  return(list(
    total = n_total,
    legit_count = legit_count,
    legit_pct = legit_pct,
    fraud_count = fraud_count,
    fraud_pct = fraud_pct,
    other_count = other_count,
    imbalance_ratio = imbalance_ratio
  ))
}

#' Compute descriptive statistics for transaction Amount
#' @param dt data.table with 'Amount' and 'Class' columns.
#' @return list of summary statistics for overall, legitimate, and fraud groups.
check_amount_statistics <- function(dt) {
  if (!"Amount" %in% names(dt)) {
    stop("Column 'Amount' not found in dataset.")
  }

  calc_stats <- function(x) {
    list(
      min = min(x, na.rm = TRUE),
      q25 = as.numeric(quantile(x, 0.25, na.rm = TRUE)),
      median = median(x, na.rm = TRUE),
      mean = mean(x, na.rm = TRUE),
      q75 = as.numeric(quantile(x, 0.75, na.rm = TRUE)),
      max = max(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      iqr = IQR(x, na.rm = TRUE),
      zero_count = sum(x == 0, na.rm = TRUE)
    )
  }

  overall_stats <- calc_stats(dt$Amount)
  legit_stats <- if ("Class" %in% names(dt)) calc_stats(dt[Class == 0, Amount]) else list()
  fraud_stats <- if ("Class" %in% names(dt)) calc_stats(dt[Class == 1, Amount]) else list()

  cat("[8. TRANSACTION AMOUNT STATISTICS]\n")
  cat(sprintf("    Overall    -> Min: $%.2f | Median: $%.2f | Mean: $%.2f | Max: $%.2f | SD: $%.2f | IQR: $%.2f\n",
              overall_stats$min, overall_stats$median, overall_stats$mean,
              overall_stats$max, overall_stats$sd, overall_stats$iqr))
  cat(sprintf("    Legitimate -> Min: $%.2f | Median: $%.2f | Mean: $%.2f | Max: $%.2f | SD: $%.2f\n",
              legit_stats$min, legit_stats$median, legit_stats$mean,
              legit_stats$max, legit_stats$sd))
  cat(sprintf("    Fraudulent -> Min: $%.2f | Median: $%.2f | Mean: $%.2f | Max: $%.2f | SD: $%.2f\n",
              fraud_stats$min, fraud_stats$median, fraud_stats$mean,
              fraud_stats$max, fraud_stats$sd))
  cat(sprintf("    Transactions with Amount == 0.00: %s (Fraudulent: %d | Legitimate: %d)\n",
              format(overall_stats$zero_count, big.mark = ","),
              fraud_stats$zero_count, legit_stats$zero_count))

  return(list(
    overall = overall_stats,
    legit = legit_stats,
    fraud = fraud_stats
  ))
}

#' Check for invalid, non-finite, and out-of-bounds domain values
#' @param dt data.table.
#' @return list with non-finite counts and range validity flags.
check_invalid_values <- function(dt) {
  inf_counts <- vapply(dt, function(x) if (is.numeric(x)) sum(is.infinite(x)) else 0L, integer(1))
  nan_counts <- vapply(dt, function(x) if (is.numeric(x)) sum(is.nan(x)) else 0L, integer(1))

  total_inf <- sum(inf_counts)
  total_nan <- sum(nan_counts)

  neg_amounts <- if ("Amount" %in% names(dt)) sum(dt$Amount < 0, na.rm = TRUE) else 0L
  neg_times   <- if ("Time" %in% names(dt)) sum(dt$Time < 0, na.rm = TRUE) else 0L

  cat(sprintf("[9. VALUE VALIDITY & NON-FINITE CHECKS]\n"))
  cat(sprintf("    Infinite values (Inf/-Inf) : %d\n", total_inf))
  cat(sprintf("    NaN values                 : %d\n", total_nan))
  cat(sprintf("    Negative Amount records    : %d\n", neg_amounts))
  cat(sprintf("    Negative Time records      : %d\n", neg_times))

  return(list(
    total_infinite = total_inf,
    total_nan = total_nan,
    neg_amounts = neg_amounts,
    neg_times = neg_times
  ))
}

# ------------------------------------------------------------------------------
# Report Generation
# ------------------------------------------------------------------------------

#' Compile structured CSV validation report
#' @param res Combined results list.
#' @param report_path Character path to target CSV.
#' @return data.frame written to disk.
generate_validation_report_csv <- function(res, report_path) {
  dir.create(dirname(report_path), recursive = TRUE, showWarnings = FALSE)

  report_entries <- list(
    # Dimensions
    list("DIMENSIONS", "Total_Rows", as.character(res$dim$rows), "INFO", "Total record count in dataset"),
    list("DIMENSIONS", "Total_Columns", as.character(res$dim$cols), "INFO", "Total feature/column count"),

    # Column Schema
    list("SCHEMA", "Expected_Columns_Count", as.character(res$cols$expected_count), "INFO", "Expected canonical features (Time, V1-V28, Amount, Class)"),
    list("SCHEMA", "Actual_Columns_Count", as.character(res$cols$actual_count),
         if (res$cols$is_valid) "PASSED" else "ALERT",
         sprintf("Missing: %d | Extra: %d", length(res$cols$missing), length(res$cols$extra))),

    # Missingness
    list("DATA_COMPLETENESS", "Total_NA_Count", as.character(res$na$total_na),
         if (res$na$total_na == 0) "PASSED" else "ALERT", "Count of missing values across all columns"),
    list("DATA_COMPLETENESS", "Columns_With_NA", as.character(res$na$cols_with_na),
         if (res$na$cols_with_na == 0) "PASSED" else "ALERT", "Number of columns containing at least one NA"),

    # Duplicates
    list("INTEGRITY_DUPLICATES", "Duplicate_Records_Count", as.character(res$dups$dup_count),
         if (res$dups$dup_count == 0) "PASSED" else "WARNING", "Identical transactions found in raw dataset"),
    list("INTEGRITY_DUPLICATES", "Duplicate_Records_Percentage", sprintf("%.4f%%", res$dups$dup_pct),
         if (res$dups$dup_count == 0) "PASSED" else "WARNING", "Percentage of rows that are duplicate entries"),
    list("INTEGRITY_DUPLICATES", "Duplicate_Legitimate_Count", as.character(res$dups$dup_legit), "INFO", "Duplicate transactions classified as legitimate (Class 0)"),
    list("INTEGRITY_DUPLICATES", "Duplicate_Fraudulent_Count", as.character(res$dups$dup_fraud), "INFO", "Duplicate transactions classified as fraudulent (Class 1)"),

    # Data Types
    list("DATA_TYPES", "Numeric_Columns_Count", as.character(res$types$numeric_count), "INFO", "Numeric / double / integer columns"),
    list("DATA_TYPES", "Non_Numeric_Columns_Count", as.character(res$types$non_numeric_count),
         if (res$types$non_numeric_count == 0) "PASSED" else "ALERT", "Character, factor, or unsupported data types"),

    # Class Breakdown
    list("CLASS_DISTRIBUTION", "Legitimate_Count_Class_0", as.character(res$class$legit_count), "INFO", "Legitimate transaction records"),
    list("CLASS_DISTRIBUTION", "Fraudulent_Count_Class_1", as.character(res$class$fraud_count), "INFO", "Confirmed fraudulent transaction records"),
    list("CLASS_DISTRIBUTION", "Legitimate_Percentage", sprintf("%.4f%%", res$class$legit_pct), "INFO", "Proportion of legitimate transactions"),
    list("CLASS_DISTRIBUTION", "Fraudulent_Percentage", sprintf("%.4f%%", res$class$fraud_pct), "INFO", "Proportion of fraudulent transactions"),
    list("CLASS_DISTRIBUTION", "Imbalance_Ratio", sprintf("%.2f : 1", res$class$imbalance_ratio), "INFO", "Ratio of legitimate transactions to one fraudulent transaction"),

    # Amount Stats (Overall)
    list("AMOUNT_OVERALL", "Amount_Min", sprintf("%.2f", res$amount$overall$min), "INFO", "Minimum transaction amount"),
    list("AMOUNT_OVERALL", "Amount_Q25", sprintf("%.2f", res$amount$overall$q25), "INFO", "25th percentile of transaction amounts"),
    list("AMOUNT_OVERALL", "Amount_Median", sprintf("%.2f", res$amount$overall$median), "INFO", "Median transaction amount"),
    list("AMOUNT_OVERALL", "Amount_Mean", sprintf("%.2f", res$amount$overall$mean), "INFO", "Mean transaction amount"),
    list("AMOUNT_OVERALL", "Amount_Q75", sprintf("%.2f", res$amount$overall$q75), "INFO", "75th percentile of transaction amounts"),
    list("AMOUNT_OVERALL", "Amount_Max", sprintf("%.2f", res$amount$overall$max), "INFO", "Maximum transaction amount"),
    list("AMOUNT_OVERALL", "Amount_SD", sprintf("%.2f", res$amount$overall$sd), "INFO", "Standard deviation of transaction amounts"),
    list("AMOUNT_OVERALL", "Amount_IQR", sprintf("%.2f", res$amount$overall$iqr), "INFO", "Interquartile range (IQR)"),
    list("AMOUNT_OVERALL", "Amount_Zero_Count", as.character(res$amount$overall$zero_count), "INFO", "Transactions with Amount == $0.00"),

    # Amount Stats by Class
    list("AMOUNT_LEGITIMATE", "Legit_Amount_Mean", sprintf("%.2f", res$amount$legit$mean), "INFO", "Average legitimate transaction amount"),
    list("AMOUNT_LEGITIMATE", "Legit_Amount_Median", sprintf("%.2f", res$amount$legit$median), "INFO", "Median legitimate transaction amount"),
    list("AMOUNT_LEGITIMATE", "Legit_Amount_Max", sprintf("%.2f", res$amount$legit$max), "INFO", "Maximum legitimate transaction amount"),
    list("AMOUNT_FRAUDULENT", "Fraud_Amount_Mean", sprintf("%.2f", res$amount$fraud$mean), "INFO", "Average fraudulent transaction amount"),
    list("AMOUNT_FRAUDULENT", "Fraud_Amount_Median", sprintf("%.2f", res$amount$fraud$median), "INFO", "Median fraudulent transaction amount"),
    list("AMOUNT_FRAUDULENT", "Fraud_Amount_Max", sprintf("%.2f", res$amount$fraud$max), "INFO", "Maximum fraudulent transaction amount"),

    # Non-Finite & Boundaries
    list("VALUE_VALIDITY", "Infinite_Values_Count", as.character(res$invalid$total_infinite),
         if (res$invalid$total_infinite == 0) "PASSED" else "ALERT", "Inf or -Inf values detected"),
    list("VALUE_VALIDITY", "NaN_Values_Count", as.character(res$invalid$total_nan),
         if (res$invalid$total_nan == 0) "PASSED" else "ALERT", "NaN values detected"),
    list("VALUE_VALIDITY", "Negative_Amount_Count", as.character(res$invalid$neg_amounts),
         if (res$invalid$neg_amounts == 0) "PASSED" else "ALERT", "Negative transaction amounts (< $0)"),
    list("VALUE_VALIDITY", "Negative_Time_Count", as.character(res$invalid$neg_times),
         if (res$invalid$neg_times == 0) "PASSED" else "ALERT", "Negative timestamp entries (< 0)")
  )

  report_df <- do.call(rbind, lapply(report_entries, function(x) {
    data.frame(
      Category = x[[1]],
      Metric   = x[[2]],
      Value    = x[[3]],
      Status   = x[[4]],
      Details  = x[[5]],
      stringsAsFactors = FALSE
    )
  }))

  utils::write.csv(report_df, file = report_path, row.names = FALSE)
  cat(sprintf("\n[10. REPORT SAVED] Audit CSV generated: %s (%d metrics)\n",
              report_path, nrow(report_df)))

  return(report_df)
}

# ------------------------------------------------------------------------------
# Master Reusable Execution Function
# ------------------------------------------------------------------------------

#' Execute full automated dataset validation workflow
#'
#' @param dt data.table or data.frame. Defaults to raw_dt from global environment,
#'           or loads from data/raw/creditcard.csv.
#' @param report_path Character. Target destination for validation CSV.
#' @return data.frame containing the structured audit report.
run_data_validation <- function(dt = NULL,
                                report_path = file.path("outputs", "reports", "dataset_validation_report.csv")) {
  cat("\n====================================================================\n")
  cat(" FraudShield - Automated Dataset Validation & Integrity Audit\n")
  cat("====================================================================\n\n")

  # Resolve dataset input
  if (is.null(dt)) {
    if (exists("raw_dt", envir = .GlobalEnv)) {
      cat("[SOURCE] Using 'raw_dt' from active global environment.\n")
      dt <- get("raw_dt", envir = .GlobalEnv)
    } else if (file.exists(file.path("data", "raw", "creditcard.csv"))) {
      cat("[SOURCE] 'raw_dt' not in global environment. Loading from data/raw/creditcard.csv...\n")
      dt <- data.table::fread(file.path("data", "raw", "creditcard.csv"))
    } else if (file.exists(file.path("data", "raw", "creditcard.csv.zip"))) {
      cat("[SOURCE] Extracting and loading raw data via R/data_loading.R...\n")
      source(file.path("R", "data_loading.R"))
      dt <- run_data_loading()
    } else {
      stop("Dataset not found. Please ensure creditcard.csv or creditcard.csv.zip exists in data/raw/.")
    }
  } else {
    cat("[SOURCE] Using user-supplied dataset object.\n")
  }

  # Ensure data.table format without copying unnecessarily
  if (!inherits(dt, "data.table")) {
    dt <- as.data.table(dt)
  }

  # Execute modular validation stages
  res <- list(
    dim     = check_dimensions(dt),
    cols    = check_required_columns(dt),
    na      = check_missing_values(dt),
    dups    = check_duplicates(dt),
    types   = check_data_types(dt),
    class   = check_class_distribution(dt),
    amount  = check_amount_statistics(dt),
    invalid = check_invalid_values(dt)
  )

  # Generate and save CSV report
  report_df <- generate_validation_report_csv(res, report_path = report_path)

  cat("\n[SUCCESS] Dataset validation completed cleanly without altering raw data.\n\n")
  return(invisible(report_df))
}

# Auto-execute when run directly via Rscript (sys.nframe() == 0L)
if (sys.nframe() == 0L) {
  validation_summary <- run_data_validation()
}
