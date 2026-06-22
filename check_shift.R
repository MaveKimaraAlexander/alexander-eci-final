tourism_raw <- read.csv("Selected-Tourism-Statistics.csv",
                        header = FALSE, stringsAsFactors = FALSE)
stay_over <- tourism_raw |>
  setNames(paste0("V", seq_len(ncol(tourism_raw)))) |>
  (\(df) df[grepl("Stay-Over Arrivals", df$V2, fixed = TRUE), ])() |>
  transform(
    year   = as.integer(substr(trimws(V5), 1, 4)),
    amount = as.numeric(gsub(",", "", trimws(V6)))
  ) |>
  subset(year %in% c(2010, 2022), select = c(year, amount))

cat("Stay-over arrivals:\n"); print(stay_over)
stay_2010 <- stay_over$amount[stay_over$year == 2010]
stay_2022 <- stay_over$amount[stay_over$year == 2022]
national_shift <- (stay_2022 - stay_2010) / stay_2010
cat("\n2010:", stay_2010, "| 2022:", stay_2022,
    "| Growth:", round(national_shift * 100, 1), "%\n")
