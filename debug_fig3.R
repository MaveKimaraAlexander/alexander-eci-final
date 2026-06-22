library(haven)
library(dplyr)
library(labelled)

raw_2010 <- read_sav("person_house_merged.sav")

hh_2010 <- raw_2010 |>
  mutate(household_id_2010 = paste(DISTRICT, ED, HH, sep = "_")) |>
  distinct(household_id_2010, .keep_all = TRUE) |>
  select(household_id_2010, DISTRICT, NPERS, H13_OWN, H24_BEDROOMS, HWEIGHT)

cat("H13_OWN value counts (before zap_labels):\n")
print(table(as_factor(hh_2010$H13_OWN), useNA = "always"))

cat("\nH13_OWN numeric values:\n")
print(table(zap_labels(hh_2010$H13_OWN), useNA = "always"))

cat("\nHWEIGHT sample:\n")
print(summary(as.numeric(zap_labels(hh_2010$HWEIGHT))))
