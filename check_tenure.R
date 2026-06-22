library(haven); library(dplyr); library(labelled)

raw_2022 <- read_sav("PersonHHoldMerge 2022 Annon.sav")
hh_2022 <- raw_2022 |>
  distinct(CompositeKey, .keep_all = TRUE) |>
  select(CompositeKey, DISTRICT_ID, Npersons, h2_3a, HHLD_WEIGHT) |>
  rename(district_id = DISTRICT_ID, tenure = h2_3a, hh_weight = HHLD_WEIGHT) |>
  zap_labels() |>
  mutate(
    across(c(tenure), ~ if_else(. %in% c(-999999999,999999999), NA_real_, as.numeric(.))),
    district = case_when(
      district_id %in% 1:3 ~ "Castries",    district_id == 4  ~ "Anse-la-Raye",
      district_id == 5     ~ "Canaries",    district_id == 6  ~ "Soufriere",
      district_id == 7     ~ "Choiseul",    district_id == 8  ~ "Laborie",
      district_id == 9     ~ "Vieux Fort",  district_id == 10 ~ "Micoud",
      district_id == 11    ~ "Dennery",     district_id == 12 ~ "Gros Islet",
      TRUE ~ NA_character_),
    owner    = if_else(tenure %in% 1:6,  1, 0),
    renter   = if_else(tenure %in% 7:8,  1, 0),
    rentfree = if_else(tenure == 9,      1, 0)
  ) |> filter(!is.na(district))

raw_2010 <- read_sav("person_house_merged.sav")
hh_2010 <- raw_2010 |>
  mutate(hid = paste(DISTRICT, ED, HH, sep = "_")) |>
  distinct(hid, .keep_all = TRUE) |>
  select(hid, DISTRICT, H13_OWN, HWEIGHT) |>
  rename(district_id = DISTRICT, tenure = H13_OWN, hh_weight = HWEIGHT) |>
  zap_labels() |>
  mutate(
    across(c(tenure), ~ if_else(. %in% c(-999999999,999999999), NA_real_, as.numeric(.))),
    district = case_when(
      district_id %in% 1:3 ~ "Castries",    district_id == 4  ~ "Anse-la-Raye",
      district_id == 5     ~ "Canaries",    district_id == 6  ~ "Soufriere",
      district_id == 7     ~ "Choiseul",    district_id == 8  ~ "Laborie",
      district_id == 9     ~ "Vieux Fort",  district_id == 10 ~ "Micoud",
      district_id == 11    ~ "Dennery",     district_id == 12 ~ "Gros Islet",
      TRUE ~ NA_character_),
    owner    = if_else(tenure %in% c(1, 2), 1, 0),
    renter   = if_else(tenure %in% c(3, 4), 1, 0),
    rentfree = if_else(tenure == 5,         1, 0)
  ) |> filter(!is.na(district))

d22 <- hh_2022 |> filter(!is.na(hh_weight)) |> group_by(district) |>
  summarise(own22 = weighted.mean(owner,    hh_weight, na.rm=TRUE),
            ren22 = weighted.mean(renter,   hh_weight, na.rm=TRUE),
            rf22  = weighted.mean(rentfree, hh_weight, na.rm=TRUE), .groups="drop")

d10 <- hh_2010 |> filter(!is.na(hh_weight)) |> group_by(district) |>
  summarise(own10 = weighted.mean(owner,    hh_weight, na.rm=TRUE),
            ren10 = weighted.mean(renter,   hh_weight, na.rm=TRUE),
            rf10  = weighted.mean(rentfree, hh_weight, na.rm=TRUE), .groups="drop")

tpi <- read.csv("TPI_v2_attraction_concentration.csv") |> select(district, tpi=tpi_v2)

out <- d10 |> left_join(d22, by="district") |> left_join(tpi, by="district") |>
  mutate(d_own = (own22-own10)*100, d_ren = (ren22-ren10)*100, d_rf = (rf22-rf10)*100,
         check = round((own22+ren22+rf22)*100,1)) |>
  arrange(desc(tpi))

cat("Tenure changes by district (pp), ordered by TPI:\n")
print(out |> select(district, tpi,
                    own10, own22, d_own,
                    ren10, ren22, d_ren,
                    rf10,  rf22,  d_rf, check),
      digits=3, n=10)
