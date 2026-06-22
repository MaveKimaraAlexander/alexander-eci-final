library(readxl)
df <- read_excel("Selected_Visitor_Statistics__2012_to_2023.xls",
                 col_names = FALSE, .name_repair = "minimal")
cat("Dimensions:", dim(df), "\n\n")
for (i in seq_len(nrow(df))) {
  vals <- as.character(unlist(df[i, ]))
  vals <- vals[!is.na(vals) & nchar(trimws(vals)) > 0]
  if (length(vals) > 0) cat(i, ":", paste(vals, collapse = " | "), "\n")
}
