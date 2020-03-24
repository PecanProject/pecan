histfiles <- list.files("data", "history.*.csv", full.names = TRUE)

str_col <- "storage_turnover_rate"

for (hf in histfiles) {
  d <- read.table(hf, header = TRUE, sep = ";")
  if (str_col %in% colnames(d)) {
    d[, str_col] <- 0
  }
  write.table(d, hf, quote = FALSE, sep = ";")
}
