library(haven)
library(labelled)
library(dplyr)

# ============================================================
# Constituency-Level TPI via Household-Weighted Disaggregation
#
# Method:
#   TPI_c = sum_d [ (HH_{c,d} / HH_{.,d}) * TPI_d ]
#
#   where HH_{c,d} = 2010 weighted households in constituency c
#   that fall within TPI district group d.
#
# This distributes each district's attraction share proportionally
# to the constituencies it contains, weighted by pre-treatment
# household counts (ensuring instrument exogeneity).
# ============================================================

# --- 1. Load district TPI ---
tpi_dist <- read.csv("TPI_v2_attraction_concentration.csv") |>
  select(district, tpi = tpi_v2)

# --- 2. Map census district_id (1-12) to TPI district names ---
# Districts 1-3 are all "Castries" in the TPI (combined)
dist_to_group <- tibble(
  district_id = 1:12,
  tpi_group   = c("Castries", "Castries", "Castries",
                  "Anse-la-Raye", "Canaries",
                  "Soufriere", "Choiseul", "Laborie",
                  "Vieux Fort", "Micoud", "Dennery", "Gros Islet")
)

# --- 3. Constituency code -> name crosswalk ---
cons_names <- c(
  "100"  = "Gros Islet",
  "200"  = "Babonneau",
  "300"  = "Castries North",
  "400"  = "Castries East",
  "500"  = "Castries Central",
  "600"  = "Castries South",
  "700"  = "Anse-la-Raye/Canaries",
  "800"  = "Soufriere",
  "900"  = "Choiseul",
  "1000" = "Laborie",
  "1100" = "Vieux-Fort South",
  "1200" = "Vieux-Fort North",
  "1300" = "Micoud South",
  "1400" = "Micoud North",
  "1500" = "Dennery South",
  "1600" = "Dennery North",
  "1700" = "Castries South-East"
)

# --- 4. Load 2010 census, deduplicate to household level ---
cat("Loading 2010 census...\n")
raw_2010 <- read_sav("person_house_merged.sav")

hh_2010 <- raw_2010 |>
  mutate(hh_id = paste(DISTRICT, ED, HH, sep = "_")) |>
  group_by(hh_id) |>
  mutate(HWEIGHT = suppressWarnings(max(HWEIGHT, na.rm = TRUE)),
         HWEIGHT = if_else(is.infinite(HWEIGHT), NA_real_, HWEIGHT)) |>
  ungroup() |>
  distinct(hh_id, .keep_all = TRUE) |>
  select(poldist, district_id = DISTRICT, hh_weight = HWEIGHT) |>
  zap_labels() |>
  mutate(
    across(c(poldist, district_id, hh_weight),
           ~ if_else(. %in% c(-999999999, 999999999), NA_real_, as.numeric(.)))
  ) |>
  filter(!is.na(poldist), !is.na(district_id), !is.na(hh_weight)) |>
  left_join(dist_to_group, by = "district_id")

cat("2010 households used:", nrow(hh_2010), "\n")

# --- 5. Compute constituency × TPI-group household shares ---
group_totals <- hh_2010 |>
  group_by(tpi_group) |>
  summarise(total_hh = sum(hh_weight, na.rm = TRUE), .groups = "drop")

cons_group_hh <- hh_2010 |>
  group_by(poldist, tpi_group) |>
  summarise(hh = sum(hh_weight, na.rm = TRUE), .groups = "drop")

# --- 6. Compute constituency TPI ---
tpi_constituency <- cons_group_hh |>
  left_join(group_totals, by = "tpi_group") |>
  left_join(tpi_dist |> rename(tpi_group = district), by = "tpi_group") |>
  mutate(tpi_contrib = (hh / total_hh) * tpi) |>
  group_by(poldist) |>
  summarise(tpi = sum(tpi_contrib, na.rm = TRUE), .groups = "drop") |>
  mutate(
    constituency = cons_names[as.character(poldist)],
    tourism_tier = case_when(
      tpi >= 0.10 ~ "High",
      tpi >= 0.04 ~ "Medium",
      TRUE        ~ "Low"
    )
  ) |>
  filter(!is.na(constituency)) |>
  arrange(desc(tpi)) |>
  mutate(rank = row_number()) |>
  select(constituency, poldist, tpi, tourism_tier, rank)

cat("\n=== Constituency TPI (attraction share) ===\n")
print(tpi_constituency, n = 17)
cat("\nSum of TPI:", sum(tpi_constituency$tpi), "\n")

# --- 7. Save ---
write.csv(tpi_constituency, "TPI_constituency.csv", row.names = FALSE)
cat("\nSaved: TPI_constituency.csv\n")
