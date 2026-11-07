files <- list.files("app/modules", full.names = TRUE)
for (f in files) {
  lines <- readLines(f)
  funcs <- grep("<- function\\(", lines, value = TRUE)
  cat("\nFile: ", basename(f), "\n")
  for (fn in funcs) {
    cat("  ", trimws(fn), "\n")
  }
}

