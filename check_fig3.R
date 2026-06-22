library(haven)
library(tidyverse)
library(labelled)

# Load data (same as document)
raw_2022 <- read_sav("PersonHHoldMerge 2022 Annon.sav")
hh_2022 <- raw_2022 |>
  distinct(CompositeKey, .keep_all = TRUE) |>
  select(CompositeKey, DISTRICT_ID, Npersons, h2_3a, h2_3b1, h2_15, HHLD_WEIGHT) |>
  rename(household_id = CompositeKey, district_id = DISTRICT_ID,
         household_size = Npersons, tenure = h2_3a,
         monthly_rent = h2_3b1, bedrooms = h2_15, hh_weight = HHLD_WEIGHT) |>
  zap_labels() |>
  mutate(
    across(c(tenure, monthly_rent, bedrooms, household_size),
           ~ if_else(. %in% c(-999999999, 999999999), NA_real_, as.numeric(.))),
    district = case_when(
      district_id %in% 1:3 ~ "Castries",    district_id == 4  ~ "Anse-la-Raye",
      district_id == 5     ~ "Canaries",    district_id == 6  ~ "Soufriere",
      district_id == 7     ~ "Choiseul",    district_id == 8  ~ "Laborie",
      district_id == 9     ~ "Vieux Fort",  district_id == 10 ~ "Micoud",
      district_id == 11    ~ "Dennery",     district_id == 12 ~ "Gros Islet",
      TRUE ~ NA_character_),
    renter = if_else(tenure %in% 7:8, 1, 0)
  ) |> filter(!is.na(district))

raw_2010 <- read_sav("person_house_merged.sav")
hh_2010 <- raw_2010 |>
  mutate(household_id_2010 = paste(DISTRICT, ED, HH, sep = "_")) |>
  distinct(household_id_2010, .keep_all = TRUE) |>
  select(household_id_2010, DISTRICT, NPERS, H13_OWN, H24_BEDROOMS, HWEIGHT) |>
  rename(household_id = household_id_2010, district_id = DISTRICT,
         household_size = NPERS, tenure = H13_OWN,
         bedrooms = H24_BEDROOMS, hh_weight = HWEIGHT) |>
  zap_labels() |>
  mutate(
    across(c(tenure, bedrooms, household_size),
           ~ if_else(. %in% c(-999999999, 999999999), NA_real_, as.numeric(.))),
    district = case_when(
      district_id %in% 1:3 ~ "Castries",    district_id == 4  ~ "Anse-la-Raye",
      district_id == 5     ~ "Canaries",    district_id == 6  ~ "Soufriere",
      district_id == 7     ~ "Choiseul",    district_id == 8  ~ "Laborie",
      district_id == 9     ~ "Vieux Fort",  district_id == 10 ~ "Micoud",
      district_id == 11    ~ "Dennery",     district_id == 12 ~ "Gros Islet",
      TRUE ~ NA_character_),
    renter = if_else(tenure %in% c(3, 4), 1, 0)
  ) |> filter(!is.na(district))

d22 <- hh_2022 |> group_by(district) |>
  summarise(renter_rate_22 = weighted.mean(renter, hh_weight, na.rm = TRUE), .groups = "drop")
d10 <- hh_2010 |> group_by(district) |>
  summarise(renter_rate_10 = weighted.mean(renter, hh_weight, na.rm = TRUE), .groups = "drop")

tpi <- read.csv("TPI_v2_attraction_concentration.csv") |> select(district, tpi = tpi_v2)

dw <- d10 |> left_join(d22, by = "district") |> left_join(tpi, by = "district") |>
  mutate(d_renter = (renter_rate_22 - renter_rate_10) * 100)

cat("district_wide with TPI v2:\n")
print(dw |> arrange(desc(tpi)) |> select(district, tpi, renter_rate_10, renter_rate_22, d_renter))

cat("\nCorrelation tpi vs d_renter:", cor(dw$tpi, dw$d_renter, use = "complete"), "\n")
