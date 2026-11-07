local({
  proj_lib <- file.path(getwd(), ".Rlibrary")
  if (dir.exists(proj_lib)) .libPaths(c(proj_lib, .libPaths()))
})
dt <- data.table::fread("data/processed/creditcard_cleaned.csv", nrows = 5)
cat("Column names in creditcard_cleaned.csv:\n")
print(names(dt))

full_dt <- data.table::fread("data/processed/creditcard_cleaned.csv", select = c("time", "amount", "class"))
cat("\nTotal rows: ", nrow(full_dt), "\n")
cat("Total Legitimate (class == 0): ", sum(full_dt$class == 0), "\n")
cat("Total Fraudulent (class == 1): ", sum(full_dt$class == 1), "\n")
cat("Fraud Percentage: ", (sum(full_dt$class == 1) / nrow(full_dt)) * 100, "%\n")
cat("Total Fraud Amount: $", format(sum(full_dt$amount[full_dt$class == 1]), big.mark = ","), "\n")
cat("Total Legit Amount: $", format(sum(full_dt$amount[full_dt$class == 0]), big.mark = ","), "\n")
cat("Overall Average Amount: $", round(mean(full_dt$amount), 2), "\n")
cat("Fraud Average Amount: $", round(mean(full_dt$amount[full_dt$class == 1]), 2), "\n")
cat("Legit Average Amount: $", round(mean(full_dt$amount[full_dt$class == 0]), 2), "\n")

# Calculate hourly timeline
full_dt[, elapsed_hour := as.integer(floor(time / 3600))]
hourly_summary <- full_dt[, .(
  Total_Transactions = .N,
  Fraud_Count = sum(class == 1),
  Legit_Count = sum(class == 0),
  Fraud_Rate_Pct = round((sum(class == 1) / .N) * 100, 4)
), by = elapsed_hour][order(elapsed_hour)]

cat("\nHourly Summary (First 6 rows):\n")
print(head(hourly_summary))
cat("\nTotal hourly bins: ", nrow(hourly_summary), "\n")
cat("Sum of Total_Transactions across hourly bins: ", sum(hourly_summary$Total_Transactions), "\n")
cat("Sum of Fraud_Count across hourly bins: ", sum(hourly_summary$Fraud_Count), "\n")

