cat("--- Inspecting models/ ---\n")
print(list.files("models", full.names = TRUE))

cat("\n--- Inspecting outputs/tables/ ---\n")
print(list.files("outputs/tables", full.names = TRUE))

cat("\n--- Inspecting data/ ---\n")
print(list.files("data", recursive = TRUE, full.names = TRUE))

cat("\n--- Checking database tables in data/investigations.sqlite ---\n")
if (file.exists("data/investigations.sqlite")) {
  library(DBI)
  library(RSQLite)
  con <- dbConnect(SQLite(), "data/investigations.sqlite")
  print(dbListTables(con))
  for (tbl in dbListTables(con)) {
    cat("Table:", tbl, "\n")
    print(dbGetQuery(con, paste0("PRAGMA table_info(", tbl, ")")))
  }
  dbDisconnect(con)
}

