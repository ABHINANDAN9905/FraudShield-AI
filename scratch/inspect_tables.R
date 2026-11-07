cat("--- model_comparison.csv ---\n")
print(read.csv("outputs/tables/model_comparison.csv"))

cat("\n--- threshold_analysis.csv (first 10 rows) ---\n")
print(head(read.csv("outputs/tables/threshold_analysis.csv"), 10))

