library(haven)
library(dplyr)
library(labelled)
library(ggplot2)
library(ggrepel)

raw_2022 <- read_sav("PersonHHoldMerge 2022 Annon.sav")
hh_2022 <- raw_2022 |>
  distinct(CompositeKey, .keep_all = TRUE) |>
  select(CompositeKey, DISTRICT_ID, Npersons, h2_3a, HHLD_WEIGHT) |>
  rename(household_id = CompositeKey, district_id = DISTRICT_ID,
         household_size = Npersons, tenure = h2_3a, hh_weight = HHLD_WEIGHT) |>
  zap_labels() |>
  mutate(
    across(c(tenure, household_size),
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
  select(household_id_2010, DISTRICT, NPERS, H13_OWN, HWEIGHT) |>
  rename(household_id = household_id_2010, district_id = DISTRICT,
         household_size = NPERS, tenure = H13_OWN, hh_weight = HWEIGHT) |>
  zap_labels() |>
  mutate(
    across(c(tenure, household_size),
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

# Show how many valid (non-NA weight) household records per district in 2010
cat("2010 records with valid weight per district:\n")
print(hh_2010 |> filter(!is.na(hh_weight)) |> count(district))

cat("\n2010 all records per district:\n")
print(hh_2010 |> count(district))

# Compute district means — filter to non-NA weights first
d22 <- hh_2022 |>
  filter(!is.na(hh_weight)) |>
  group_by(district) |>
  summarise(renter_rate_22 = weighted.mean(renter, hh_weight, na.rm = TRUE), .groups = "drop")

d10 <- hh_2010 |>
  filter(!is.na(hh_weight)) |>
  group_by(district) |>
  summarise(renter_rate_10 = weighted.mean(renter, hh_weight, na.rm = TRUE), .groups = "drop")

tpi <- read.csv("TPI_v2_attraction_concentration.csv") |> select(district, tpi = tpi_v2)

dw <- d10 |> left_join(d22, by = "district") |> left_join(tpi, by = "district") |>
  mutate(
    d_renter = (renter_rate_22 - renter_rate_10) * 100,
    tourism_tier = case_when(
      tpi >= 0.20 ~ "High", tpi >= 0.09 ~ "Medium", TRUE ~ "Low"
    )
  )

cat("\ndistrict_wide d_renter:\n")
print(dw |> arrange(desc(tpi)) |> select(district, tpi, renter_rate_10, renter_rate_22, d_renter))
cat("\nCorrelation tpi vs d_renter:", cor(dw$tpi, dw$d_renter, use = "complete"), "\n")

# Render figure
p <- dw |>
  mutate(tourism_tier = factor(tourism_tier, levels = c("High", "Medium", "Low"))) |>
  ggplot(aes(x = tpi, y = d_renter, label = district, color = tourism_tier)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey50", fill = "grey85", linewidth = 0.7) +
  geom_point(size = 3, alpha = 0.9) +
  geom_text_repel(size = 2.8, max.overlaps = 12, show.legend = FALSE) +
  scale_color_manual(values = c("High" = "#D7191C", "Medium" = "#FDAE61", "Low" = "#2C7BB6"),
                     name = "Tourism Tier") +
  labs(title = "Figure 3. TPI vs Change in Renter Rate",
       x = "TPI (attraction share)", y = "Δ Renter Rate 2010–2022 (pp)")

ggsave("fig3_check.png", p, width = 7, height = 4.5, dpi = 150, bg = "white")
cat("Saved fig3_check.png\n")
