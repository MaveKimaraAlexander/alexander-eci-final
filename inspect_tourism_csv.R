df <- read.csv("Selected-Tourism-Statistics.csv", check.names = FALSE, header = FALSE)
for (i in seq_len(nrow(df))) {
  row <- df[i, ]
  vals <- as.character(unlist(row))
  vals <- vals[!is.na(vals) & nchar(vals) > 0]
  if (length(vals) > 0) cat(i, ":", paste(vals, collapse = " | "), "\n")
}
