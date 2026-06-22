# ==============================================================================
# How Short-Term Rentals Affected the St. Lucian Housing Market
# A Bartik Shift-Share Instrumental Variable Analysis
#
# Author : Mave Kimara Alexander | 113077424
# Date   : 2026-06-13
# Source : eci_str_thesis.qmd
#
# Reproducible standalone R script. All data loading, model fitting, figures,
# and tables are produced in sequence from this single file.
#
# REQUIRED DATA FILES (place in working directory or update paths below):
#   PersonHHoldMerge 2022 Annon.sav
#   person_house_merged.sav
#   IDDetail_merged_Anon_Weights_DwellStatus.sav
#   NTE_constituency.csv
#   Selected-Tourism-Statistics.csv
#   natural_amenities_nte_assigned.csv
#   nte_district.csv
#
# NOTE: For the fully formatted PDF with LaTeX tables and embedded figures,
# render MAVE_Thesis_Progress_Update_6.qmd. This script is for replication,
# interactive exploration, and version control on GitHub.
# ==============================================================================

# ── 0. Working Directory ──────────────────────────────────────────────────────
# Update this path if you move the data files
setwd("C:/Users/mavek/OneDrive/Desktop/IMBA Douments/semester 4/Thesis/Thesis Data")

# ── 0. Libraries ──────────────────────────────────────────────────────────────
library(haven)
library(tidyverse)
library(labelled)
library(knitr)
library(scales)
library(ggrepel)
library(kableExtra)
library(fixest)
library(modelsummary)

theme_set(
  theme_minimal(base_size = 10) +
    theme(
      plot.title    = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9,  color = "grey40"),
      plot.caption  = element_text(size = 7,  color = "grey50"),
      axis.text     = element_text(size = 8),
      legend.position = "bottom",
      legend.text   = element_text(size = 8)
    )
)

# ── 1. Constituency Name Map ───────────────────────────────────────────────────
cons_name_map <- c(
  "100"  = "Gros Islet",        "200"  = "Babonneau",
  "300"  = "Castries North",    "400"  = "Castries East",
  "500"  = "Castries Central",  "600"  = "Castries South",
  "700"  = "Anse-la-Raye/Canaries",
  "800"  = "Soufriere",         "900"  = "Choiseul",
  "1000" = "Laborie",           "1100" = "Vieux-Fort South",
  "1200" = "Vieux-Fort North",  "1300" = "Micoud South",
  "1400" = "Micoud North",      "1500" = "Dennery South",
  "1600" = "Dennery North",     "1700" = "Castries South-East"
)

# ── 2. Census Data ─────────────────────────────────────────────────────────────

# 2022 Census
raw_2022 <- read_sav("PersonHHoldMerge 2022 Annon.sav")

hh_2022 <- raw_2022 |>
  distinct(CompositeKey, .keep_all = TRUE) |>
  select(CompositeKey, CONSTITUENCY, Npersons,
         h2_3a, h2_3b1, h2_15, HHLD_WEIGHT) |>
  rename(
    household_id    = CompositeKey,
    constituency_id = CONSTITUENCY,
    household_size  = Npersons,
    tenure          = h2_3a,
    monthly_rent    = h2_3b1,
    bedrooms        = h2_15,
    hh_weight       = HHLD_WEIGHT
  ) |>
  zap_labels() |>
  mutate(
    across(c(tenure, monthly_rent, bedrooms, household_size, constituency_id),
           ~ if_else(. %in% c(-999999999, 999999999), NA_real_, as.numeric(.))),
    constituency        = cons_name_map[as.character(as.integer(constituency_id))],
    owner               = if_else(tenure %in% 1:6, 1, 0),
    renter              = if_else(tenure %in% 7:8, 1, 0),
    rentfree            = if_else(tenure == 9,     1, 0),
    persons_per_bedroom = if_else(bedrooms > 0, household_size / bedrooms, NA_real_)
  ) |>
  filter(!is.na(constituency))

# 2010 Census
raw_2010 <- read_sav("person_house_merged.sav")

hh_2010 <- raw_2010 |>
  mutate(household_id_2010 = paste(DISTRICT, ED, HH, sep = "_")) |>
  group_by(household_id_2010) |>
  mutate(HWEIGHT = suppressWarnings(max(HWEIGHT, na.rm = TRUE)),
         HWEIGHT = if_else(is.infinite(HWEIGHT), NA_real_, HWEIGHT)) |>
  ungroup() |>
  distinct(household_id_2010, .keep_all = TRUE) |>
  select(household_id_2010, poldist, NPERS, H13_OWN, H24_BEDROOMS, HWEIGHT) |>
  rename(
    household_id    = household_id_2010,
    constituency_id = poldist,
    household_size  = NPERS,
    tenure          = H13_OWN,
    bedrooms        = H24_BEDROOMS,
    hh_weight       = HWEIGHT
  ) |>
  zap_labels() |>
  mutate(
    across(c(tenure, bedrooms, household_size, constituency_id),
           ~ if_else(. %in% c(-999999999, 999999999), NA_real_, as.numeric(.))),
    persons_per_bedroom = if_else(bedrooms > 0, household_size / bedrooms, NA_real_),
    constituency        = cons_name_map[as.character(as.integer(constituency_id))],
    owner               = if_else(tenure %in% c(1, 2), 1, 0),
    renter              = if_else(tenure %in% c(3, 4), 1, 0),
    rentfree            = if_else(tenure == 5, 1, 0)
  ) |>
  filter(!is.na(constituency))

# ── 3. Building Enumeration (IDDetail) ────────────────────────────────────────
raw_iddetail <- read_sav("IDDetail_merged_Anon_Weights_DwellStatus.sav") |>
  zap_labels() |>
  mutate(
    district_code  = districtCode,
    str_unit       = if_else(DwellingStatus %in% c(8, 12),    1L, 0L),
    str_unit_broad = if_else(DwellingStatus %in% c(4, 8, 12), 1L, 0L)
  ) |>
  filter(!is.na(district_code)) |>
  mutate(
    district = case_match(as.integer(district_code),
      2  ~ "Castries",   3  ~ "AnselaRaye", 4  ~ "Canaries",
      5  ~ "Soufriere",  6  ~ "Choiseul",   7  ~ "Laborie",
      8  ~ "VieuxFort",  9  ~ "Micoud",     10 ~ "Dennery",
      11 ~ "GrosIslet"
    )
  )

write.csv(
  raw_iddetail |> select(district, str_unit, str_unit_broad),
  "iddetail_str.csv", row.names = FALSE
)

# ── 4. NTE and Tourism Shift ───────────────────────────────────────────────────
nte_data <- read.csv("NTE_constituency.csv") |>
  select(constituency, nte, tourism_tier)

tourism_raw <- read.csv("Selected-Tourism-Statistics.csv",
                        header = FALSE, stringsAsFactors = FALSE)

stay_annual <- tourism_raw |>
  setNames(paste0("V", seq_len(ncol(tourism_raw)))) |>
  filter(grepl("Stay-Over Arrivals", V2, fixed = TRUE)) |>
  mutate(
    year   = as.integer(substr(trimws(V5), 1, 4)),
    amount = as.numeric(gsub(",", "", trimws(V6)))
  ) |>
  filter(!is.na(year), !is.na(amount)) |>
  group_by(year) |>
  summarise(arrivals = sum(amount), .groups = "drop")

pre_avg        <- mean(stay_annual$arrivals[stay_annual$year %in% 2010:2014])
post_avg       <- mean(stay_annual$arrivals[stay_annual$year %in% c(2015:2019, 2022)])
national_shift <- (post_avg - pre_avg) / pre_avg   # Δg = 0.187

# ── 5. STR Counts: District → Constituency Allocation ─────────────────────────
dist_to_cons <- tribble(
  ~district,     ~constituency,
  "Castries",    "Babonneau",
  "Castries",    "Castries North",
  "Castries",    "Castries East",
  "Castries",    "Castries Central",
  "Castries",    "Castries South",
  "Castries",    "Castries South-East",
  "AnselaRaye",  "Anse-la-Raye/Canaries",
  "Canaries",    "Anse-la-Raye/Canaries",
  "Soufriere",   "Soufriere",
  "Choiseul",    "Choiseul",
  "Laborie",     "Laborie",
  "VieuxFort",   "Vieux-Fort South",
  "VieuxFort",   "Vieux-Fort North",
  "Micoud",      "Micoud South",
  "Micoud",      "Micoud North",
  "Dennery",     "Dennery South",
  "Dennery",     "Dennery North",
  "GrosIslet",   "Gros Islet"
)

cons_hh_wts <- hh_2010 |>
  filter(!is.na(hh_weight)) |>
  group_by(constituency) |>
  summarise(hh_wt = sum(hh_weight), .groups = "drop")

make_cons_str <- function(unit_var, count_name) {
  dist_to_cons |>
    left_join(cons_hh_wts, by = "constituency") |>
    left_join(
      raw_iddetail |>
        group_by(district) |>
        summarise(dist_str = sum(.data[[unit_var]], na.rm = TRUE), .groups = "drop"),
      by = "district"
    ) |>
    group_by(district) |>
    mutate(cons_share = hh_wt / sum(hh_wt, na.rm = TRUE)) |>
    ungroup() |>
    group_by(constituency) |>
    summarise(!!count_name := sum(dist_str * cons_share, na.rm = TRUE), .groups = "drop")
}

cons_str       <- make_cons_str("str_unit",       "str_count")
cons_str_broad <- make_cons_str("str_unit_broad",  "str_count_broad")

# ── 6. Constituency and Panel Data ────────────────────────────────────────────
cons_2022 <- hh_2022 |>
  filter(!is.na(hh_weight)) |>
  group_by(constituency) |>
  summarise(
    renter_rate_22   = weighted.mean(renter,              hh_weight, na.rm = TRUE),
    owner_rate_22    = weighted.mean(owner,               hh_weight, na.rm = TRUE),
    rentfree_rate_22 = weighted.mean(rentfree,            hh_weight, na.rm = TRUE),
    hh_size_22       = weighted.mean(household_size,      hh_weight, na.rm = TRUE),
    ppbr_22          = weighted.mean(persons_per_bedroom, hh_weight, na.rm = TRUE),
    .groups = "drop"
  )

cons_2010 <- hh_2010 |>
  filter(!is.na(hh_weight)) |>
  group_by(constituency) |>
  summarise(
    renter_rate_10   = weighted.mean(renter,              hh_weight, na.rm = TRUE),
    owner_rate_10    = weighted.mean(owner,               hh_weight, na.rm = TRUE),
    rentfree_rate_10 = weighted.mean(rentfree,            hh_weight, na.rm = TRUE),
    hh_size_10       = weighted.mean(household_size,      hh_weight, na.rm = TRUE),
    ppbr_10          = weighted.mean(persons_per_bedroom, hh_weight, na.rm = TRUE),
    .groups = "drop"
  )

cons_wide <- cons_2010 |>
  left_join(cons_2022,       by = "constituency") |>
  left_join(nte_data,        by = "constituency") |>
  left_join(cons_str,        by = "constituency") |>
  left_join(cons_str_broad,  by = "constituency") |>
  mutate(
    d_renter  = (renter_rate_22 - renter_rate_10) * 100,
    d_owner   = (owner_rate_22  - owner_rate_10)  * 100,
    d_hh_size = hh_size_22 - hh_size_10,
    d_ppbr    = ppbr_22    - ppbr_10
  )

write.csv(cons_wide, "cons_wide.csv", row.names = FALSE)

panel <- bind_rows(
  cons_2010 |>
    left_join(nte_data, by = "constituency") |>
    mutate(year = 2010, post = 0,
           renter_rate = renter_rate_10, owner_rate = owner_rate_10,
           rentfree_rate = rentfree_rate_10, hh_size = hh_size_10, ppbr = ppbr_10,
           str_count = 0, str_count_broad = 0),
  cons_2022 |>
    left_join(nte_data,       by = "constituency") |>
    left_join(cons_str,       by = "constituency") |>
    left_join(cons_str_broad, by = "constituency") |>
    mutate(year = 2022, post = 1,
           renter_rate = renter_rate_22, owner_rate = owner_rate_22,
           rentfree_rate = rentfree_rate_22, hh_size = hh_size_22, ppbr = ppbr_22)
) |>
  mutate(nte_post = nte * national_shift * post)

hh_panel <- bind_rows(
  hh_2010 |> mutate(year = 2010L, post = 0L),
  hh_2022 |> mutate(year = 2022L, post = 1L)
) |>
  left_join(nte_data |> select(constituency, nte), by = "constituency") |>
  left_join(panel    |> select(constituency, year, str_count, str_count_broad),
            by = c("constituency", "year")) |>
  mutate(nte_post = nte * national_shift * post)

write.csv(
  hh_panel |> select(renter, owner, rentfree, persons_per_bedroom,
                     monthly_rent, tenure, bedrooms, household_size,
                     constituency, year, post, hh_weight,
                     nte, nte_post, str_count, str_count_broad),
  "hh_panel_replication.csv", row.names = FALSE
)

# ── 7. District-Level Data ────────────────────────────────────────────────────
cons_to_dist <- tribble(
  ~constituency,           ~district,
  "Gros Islet",            "Gros Islet",
  "Babonneau",             "Castries",
  "Castries North",        "Castries",
  "Castries East",         "Castries",
  "Castries Central",      "Castries",
  "Castries South",        "Castries",
  "Castries South-East",   "Castries",
  "Anse-la-Raye/Canaries", "Anse-la-Raye",
  "Soufriere",             "Soufriere",
  "Choiseul",              "Choiseul",
  "Laborie",               "Laborie",
  "Vieux-Fort South",      "Vieux Fort",
  "Vieux-Fort North",      "Vieux Fort",
  "Micoud South",          "Micoud",
  "Micoud North",          "Micoud",
  "Dennery South",         "Dennery",
  "Dennery North",         "Dennery"
)

dist_wide <- cons_wide |>
  left_join(cons_to_dist, by = "constituency") |>
  left_join(cons_hh_wts,  by = "constituency") |>
  group_by(district) |>
  summarise(
    renter_rate_10 = weighted.mean(renter_rate_10, hh_wt, na.rm = TRUE),
    renter_rate_22 = weighted.mean(renter_rate_22, hh_wt, na.rm = TRUE),
    d_renter       = (renter_rate_22 - renter_rate_10) * 100,
    .groups = "drop"
  )

write.csv(dist_wide, "dist_wide.csv", row.names = FALSE)

nte_dist <- read.csv("nte_district.csv") |>
  select(district, nte_dist = nte)

dist_plot <- dist_wide |>
  left_join(nte_dist, by = "district")

dist_str_direct <- raw_iddetail |>
  mutate(
    district_h = case_when(
      district %in% c("AnselaRaye", "Canaries") ~ "Anse-la-Raye",
      district == "VieuxFort"                   ~ "Vieux Fort",
      district == "GrosIslet"                   ~ "Gros Islet",
      TRUE                                      ~ district
    )
  ) |>
  group_by(district = district_h) |>
  summarise(
    str_count_d       = sum(str_unit,       na.rm = TRUE),
    str_count_broad_d = sum(str_unit_broad, na.rm = TRUE),
    .groups = "drop"
  )

hh_panel_dist <- hh_panel |>
  left_join(cons_to_dist, by = "constituency") |>
  left_join(nte_dist,      by = "district") |>
  left_join(
    bind_rows(
      dist_str_direct |> mutate(year = 2022L),
      dist_str_direct |> mutate(year = 2010L, str_count_d = 0, str_count_broad_d = 0)
    ),
    by = c("district", "year")
  ) |>
  mutate(nte_post_dist = nte_dist * national_shift * post)

dist_panel_agg <- bind_rows(
  dist_wide |>
    left_join(nte_dist, by = "district") |>
    mutate(year = 2010, post = 0, str_count_d = 0, str_count_broad_d = 0),
  dist_wide |>
    left_join(nte_dist,        by = "district") |>
    left_join(dist_str_direct, by = "district") |>
    mutate(year = 2022, post = 1)
) |>
  mutate(nte_post_dist = nte_dist * national_shift * post)

# ── 8. Core Regression Models ─────────────────────────────────────────────────

# RQ2: Reduced-form LPM — Pr(Renting)
lpm      <- feols(renter ~ nte_post      | constituency + year,
                  data = hh_panel,      weights = ~hh_weight, vcov = ~constituency)
lpm_dist <- feols(renter ~ nte_post_dist | district     + year,
                  data = hh_panel_dist, weights = ~hh_weight, vcov = ~district)

# RQ1: Log rent cross-section (2022 only — no rent data in 2010)
hh_rent_22 <- hh_panel |>
  filter(year == 2022, tenure %in% 7:8,
         !is.na(monthly_rent), monthly_rent > 0, monthly_rent < 99999,
         !is.na(bedrooms), bedrooms > 0, !is.na(household_size)) |>
  mutate(
    log_rent = log(monthly_rent),
    ppbr     = if_else(bedrooms > 0, household_size / bedrooms, NA_real_)
  )

rent1 <- feols(log_rent ~ nte,
               data = hh_rent_22, weights = ~hh_weight, vcov = ~constituency)
rent2 <- feols(log_rent ~ nte + bedrooms + household_size,
               data = hh_rent_22, weights = ~hh_weight, vcov = ~constituency)
rent3 <- feols(log_rent ~ nte + bedrooms + household_size + ppbr,
               data = hh_rent_22, weights = ~hh_weight, vcov = ~constituency)

# First stage and 2SLS
fs_narrow <- feols(str_count       ~ nte_post | constituency + year,
                   data = panel, vcov = "hetero")
fs_broad  <- feols(str_count_broad ~ nte_post | constituency + year,
                   data = panel, vcov = "hetero")
iv_narrow <- feols(renter ~ 1 | constituency + year | str_count       ~ nte_post,
                   data = hh_panel, weights = ~hh_weight, vcov = "hetero")
iv_broad  <- feols(renter ~ 1 | constituency + year | str_count_broad ~ nte_post,
                   data = hh_panel, weights = ~hh_weight, vcov = "hetero")
fs_narrow_dist <- feols(str_count_d ~ nte_post_dist | district + year,
                        data = dist_panel_agg, vcov = "hetero")
iv_narrow_dist <- feols(renter ~ 1 | district + year | str_count_d ~ nte_post_dist,
                        data = hh_panel_dist, weights = ~hh_weight, vcov = "hetero")

# RQ3: Ownership
lpm_owner      <- feols(owner ~ nte_post      | constituency + year,
                        data = hh_panel,      weights = ~hh_weight, vcov = ~constituency)
lpm_owner_dist <- feols(owner ~ nte_post_dist | district     + year,
                        data = hh_panel_dist, weights = ~hh_weight, vcov = ~district)

# RQ4: Crowding
lpm_ppbr      <- feols(persons_per_bedroom ~ nte_post      | constituency + year,
                       data = hh_panel,      weights = ~hh_weight, vcov = ~constituency)
lpm_ppbr_dist <- feols(persons_per_bedroom ~ nte_post_dist | district     + year,
                       data = hh_panel_dist, weights = ~hh_weight, vcov = ~district)

# Key scalars used in text
fmt_p <- function(p) if (p < 0.001) "< 0.001" else paste0("= ", formatC(p, digits = 3, format = "f"))

rf_beta       <- as.numeric(coef(lpm)["nte_post"])
rf_se         <- as.numeric(se(lpm)["nte_post"])
rf_p          <- as.numeric(pvalue(lpm)["nte_post"])
rf_waldf      <- (rf_beta / rf_se)^2
nte_range     <- diff(range(nte_data$nte))
rent_nte_coef <- as.numeric(coef(rent3)["nte"])
rent_nte_p    <- as.numeric(pvalue(rent3)["nte"])
rent_premium_pct <- (exp(rent_nte_coef * nte_range) - 1) * 100
rq3_beta      <- as.numeric(coef(lpm_owner)["nte_post"])
rq3_p         <- as.numeric(pvalue(lpm_owner)["nte_post"])
rq3_beta_dist <- as.numeric(coef(lpm_owner_dist)["nte_post_dist"])
rq3_p_dist    <- as.numeric(pvalue(lpm_owner_dist)["nte_post_dist"])
rq4_beta      <- as.numeric(coef(lpm_ppbr)["nte_post"])
rq4_p         <- as.numeric(pvalue(lpm_ppbr)["nte_post"])
rq4_beta_dist <- as.numeric(coef(lpm_ppbr_dist)["nte_post_dist"])
rq4_p_dist    <- as.numeric(pvalue(lpm_ppbr_dist)["nte_post_dist"])

# ── 9. NTE Construction Tables ────────────────────────────────────────────────
features_raw_nte <- read.csv("natural_amenities_nte_assigned.csv",
                              stringsAsFactors = FALSE)

castries_5 <- c("Castries North", "Castries East", "Castries Central",
                "Castries South", "Castries South-East")

features_exp <- bind_rows(
  features_raw_nte |> filter(constituency != "Castries (equal split)"),
  features_raw_nte |>
    filter(constituency == "Castries (equal split)") |>
    crossing(cons_split = castries_5) |>
    mutate(constituency = cons_split, weight = weight / 5) |>
    select(-cons_split)
)

# Table 5: Weighted score by constituency × feature type
type_wide <- features_exp |>
  mutate(type_label = case_match(
    attr_type,
    "beach"          ~ "Beach",
    "peak"           ~ "Peak",
    "hot_spring"     ~ "Volcanic",
    "waterfall"      ~ "Waterfall",
    "nature_reserve" ~ "NatRes",
    "viewpoint"      ~ "Viewpoint"
  )) |>
  group_by(constituency, type_label) |>
  summarise(score = sum(weight), .groups = "drop") |>
  pivot_wider(names_from = type_label, values_from = score, values_fill = 0) |>
  left_join(nte_data |> select(constituency, nte), by = "constituency") |>
  mutate(
    Total     = Beach + Peak + Volcanic + Waterfall + NatRes + Viewpoint,
    NTE_Share = paste0(round(nte * 100, 1), "%"),
    across(c(Beach, Peak, Volcanic, Waterfall, NatRes, Viewpoint, Total), ~ round(., 1))
  ) |>
  arrange(desc(nte)) |>
  rename(`Nature Res.` = NatRes, `NTE Share` = NTE_Share) |>
  select(Constituency = constituency,
         Beach, Peak, Volcanic, Waterfall, `Nature Res.`, Viewpoint, Total, `NTE Share`)

print(type_wide)

# Table 5a: Named feature inventory
named_features <- features_raw_nte |>
  filter(!is.na(name), trimws(name) != "") |>
  mutate(
    type_label = case_match(
      attr_type,
      "beach"          ~ "Beach",
      "peak"           ~ "Mountain Peak",
      "hot_spring"     ~ "Volcanic / Hot Spring",
      "waterfall"      ~ "Waterfall",
      "nature_reserve" ~ "Nature Reserve",
      "viewpoint"      ~ "Scenic Viewpoint"
    ),
    constituency_disp = if_else(
      constituency == "Castries (equal split)", "Castries (shared)", constituency
    )
  ) |>
  arrange(desc(weight), type_label, name) |>
  select(Feature = name, Type = type_label, Weight = weight, Constituency = constituency_disp)

print(named_features)

# ── 10. Tourism Arrivals Table ────────────────────────────────────────────────
fmt_n <- function(x) format(round(x), big.mark = ",", trim = TRUE)

yrs_pre  <- stay_annual |> filter(year %in% 2010:2014)          |> arrange(year)
yrs_post <- stay_annual |> filter(year %in% c(2015:2019, 2022)) |> arrange(year)
yrs_cov  <- stay_annual |> filter(year %in% 2020:2021)          |> arrange(year)

arrivals_detail <- bind_rows(
  yrs_pre  |> mutate(row_label = as.character(year)),
  tibble(year = NA_integer_, arrivals = pre_avg,  row_label = "Period mean"),
  yrs_post |> mutate(row_label = as.character(year)),
  tibble(year = NA_integer_, arrivals = post_avg, row_label = "Period mean"),
  yrs_cov  |> mutate(row_label = as.character(year))
) |>
  mutate(arrivals_fmt = fmt_n(arrivals)) |>
  select(Year = row_label, `Stay-Over Arrivals` = arrivals_fmt)

cat(sprintf(
  "\nNational shift: (%s - %s) / %s = +%.1f%% (Dg = %.3f)\n",
  fmt_n(post_avg), fmt_n(pre_avg), fmt_n(pre_avg),
  national_shift * 100, national_shift
))
print(arrivals_detail)

# ── 11. Descriptive Tables ────────────────────────────────────────────────────

# Table 3: DwellingStatus distribution
dwellstatus_tbl <- raw_iddetail |>
  count(DwellingStatus) |>
  arrange(DwellingStatus) |>
  mutate(
    pct      = round(n / sum(n) * 100, 2),
    label    = case_match(
      DwellingStatus,
      1  ~ "Occupied",
      2  ~ "Closed — Residents away < 12 months",
      3  ~ "Closed — Residents away > 12 months",
      4  ~ "Vacant — Seasonally",
      5  ~ "Vacant — Non-Seasonally",
      6  ~ "Refused",
      7  ~ "No Contact / Temporarily Absent",
      8  ~ "Short Term Occupation",
      9  ~ "Other",
      10 ~ "Under Construction",
      11 ~ "Demolished",
      12 ~ "AirBNB",
      .default = "Unknown"
    ),
    str_role = case_match(
      DwellingStatus,
      8  ~ "Narrow STR",
      12 ~ "Narrow STR",
      4  ~ "Broad only",
      .default = "—"
    )
  ) |>
  select(Code = DwellingStatus, Label = label, `STR Role` = str_role, N = n, `%` = pct)

print(dwellstatus_tbl)

# Table 6 (thesis): Constituency-level housing outcomes
constituency_outcomes <- cons_wide |>
  arrange(desc(nte)) |>
  mutate(
    across(c(renter_rate_10, renter_rate_22, owner_rate_10, owner_rate_22),
           ~ round(. * 100, 1)),
    nte      = round(nte, 3),
    d_renter = round(d_renter, 1),
    d_owner  = round(d_owner,  1),
    across(c(hh_size_10, hh_size_22), ~ round(., 2))
  ) |>
  select(Constituency = constituency, NTE = nte,
         `Rent. '10` = renter_rate_10, `Rent. '22` = renter_rate_22, DRent = d_renter,
         `Own. '10`  = owner_rate_10,  `Own. '22`  = owner_rate_22,  DOwn  = d_owner,
         `HH Sz '10` = hh_size_10,     `HH Sz '22` = hh_size_22)

print(constituency_outcomes)

# Table 7 (thesis): STR units by constituency
str_by_cons <- cons_str |>
  left_join(nte_data, by = "constituency") |>
  arrange(desc(nte)) |>
  mutate(str_rounded = round(str_count, 1),
         `NTE Share` = paste0(round(nte * 100, 1), "%")) |>
  select(Constituency = constituency, `Tourism Tier` = tourism_tier,
         `NTE Share`, `Est. STR Units` = str_rounded)

print(str_by_cons)

# ── 12. Figures ───────────────────────────────────────────────────────────────
tier_colors <- c("High" = "#D7191C", "Medium" = "#FDAE61", "Low" = "#2C7BB6")

# Figure 3: Renter rate by constituency, 2010 vs. 2022
fig3 <- cons_wide |>
  arrange(nte) |>
  mutate(constituency = factor(constituency, levels = constituency)) |>
  pivot_longer(c(renter_rate_10, renter_rate_22),
               names_to = "year", values_to = "renter_rate") |>
  mutate(year = if_else(year == "renter_rate_10", "2010", "2022")) |>
  ggplot(aes(x = renter_rate, y = constituency, fill = year)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(renter_rate, accuracy = 0.1)),
            position = position_dodge(width = 0.7), hjust = -0.1, size = 2.2) +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.18))) +
  scale_fill_manual(values = c("2010" = "#D7191C", "2022" = "#FDAE61"),
                    name = "Census Year") +
  labs(title    = "Renter Rate by Constituency, 2010 vs. 2022",
       subtitle = "Ordered by NTE (bottom = highest tourism share)",
       x = "Share of Households Renting", y = NULL,
       caption  = "Source: CSO Saint Lucia Population and Housing Census 2010, 2022.")
ggsave("fig3_renter_rate_constituency.png", fig3, width = 8, height = 6, dpi = 300)

# Figure 4: Median monthly rent by constituency, 2022
fig4 <- hh_2022 |>
  filter(tenure %in% 7:8, !is.na(monthly_rent),
         monthly_rent > 0, monthly_rent < 99999) |>
  left_join(cons_wide |> select(constituency, tourism_tier), by = "constituency") |>
  mutate(tourism_tier = factor(tourism_tier, levels = c("High", "Medium", "Low"))) |>
  group_by(constituency, tourism_tier) |>
  summarise(median_rent = median(rep(monthly_rent, times = round(hh_weight))),
            .groups = "drop") |>
  arrange(median_rent) |>
  mutate(constituency = factor(constituency, levels = constituency),
         label = paste0("EC$", format(round(median_rent), big.mark = ","))) |>
  ggplot(aes(x = median_rent, y = constituency, fill = tourism_tier)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = label), hjust = -0.1, size = 2.5) +
  scale_x_continuous(labels = dollar_format(prefix = "EC$"),
                     expand = expansion(mult = c(0, 0.22))) +
  scale_fill_manual(values = tier_colors, name = "Tourism Tier") +
  labs(title    = "Median Monthly Rent by Constituency, 2022",
       subtitle = "Renter-occupied households only; ordered by median rent",
       x = "Median Monthly Rent (EC$)", y = NULL,
       caption  = "Source: CSO Saint Lucia Population and Housing Census 2022.")
ggsave("fig4_median_rent_constituency.png", fig4, width = 8, height = 6, dpi = 300)

# Figure 5: NTE vs. change in renter rate (constituency)
fig5 <- cons_wide |>
  mutate(tourism_tier = factor(tourism_tier, levels = c("High", "Medium", "Low"))) |>
  ggplot(aes(x = nte, y = d_renter, label = constituency, color = tourism_tier)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey50", fill = "grey85", linewidth = 0.7) +
  geom_point(size = 3, alpha = 0.9) +
  geom_text_repel(size = 2.5, max.overlaps = 17, show.legend = FALSE) +
  scale_color_manual(values = tier_colors, name = "Tourism Tier") +
  labs(title    = "Pre-STR Natural Tourism Endowment vs. Change in Renter Rate",
       subtitle = "Each point is one constituency; slope = reduced-form Bartik relationship",
       x = "NTE (pre-STR share)", y = "Change in Renter Rate 2010-2022 (pct. points)",
       caption  = "Source: CSO Saint Lucia Census 2010, 2022; NTE from OSM / Overpass API (May 2026).")
ggsave("fig5_nte_vs_renter_rate.png", fig5, width = 8, height = 6, dpi = 300)

# Figure 6: NTE vs. change in renter rate (district)
fig6 <- dist_plot |>
  ggplot(aes(x = nte_dist, y = d_renter, label = district)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey50", fill = "grey85", linewidth = 0.7) +
  geom_point(size = 3, color = "#2C7BB6", alpha = 0.9) +
  geom_text_repel(size = 2.8, max.overlaps = 10) +
  labs(title    = "District NTE vs. Change in Renter Rate",
       subtitle = "10 districts; same attraction-weighted methodology as constituency NTE",
       x = "District NTE (natural amenity share)", y = "Change in Renter Rate 2010-2022 (pct. points)",
       caption  = "Source: CSO Saint Lucia Census 2010, 2022; NTE from OSM / Overpass API (May 2026).")
ggsave("fig6_nte_vs_renter_rate_district.png", fig6, width = 8, height = 6, dpi = 300)

# Figure 9: NTE vs. change in owner-occupancy rate
fig9 <- cons_wide |>
  mutate(tourism_tier = factor(tourism_tier, levels = c("High", "Medium", "Low"))) |>
  ggplot(aes(x = nte, y = d_owner, label = constituency, color = tourism_tier)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey50", fill = "grey85", linewidth = 0.7) +
  geom_point(size = 3, alpha = 0.9) +
  geom_text_repel(size = 2.5, max.overlaps = 17, show.legend = FALSE) +
  scale_color_manual(values = tier_colors, name = "Tourism Tier") +
  labs(title    = "Pre-STR Natural Tourism Endowment vs. Change in Owner-Occupancy Rate",
       subtitle = "Negative slope = owner-occupancy fell in high-NTE areas",
       x = "NTE (pre-STR share)", y = "Change in Owner-Occupancy Rate 2010-2022 (pct. points)",
       caption  = "Source: CSO Saint Lucia Census 2010, 2022; NTE from OSM / Overpass API (May 2026).")
ggsave("fig9_nte_vs_owner_rate.png", fig9, width = 8, height = 6, dpi = 300)

# Figure 10: NTE vs. change in persons per bedroom
fig10 <- cons_wide |>
  mutate(tourism_tier = factor(tourism_tier, levels = c("High", "Medium", "Low"))) |>
  ggplot(aes(x = nte, y = d_ppbr, label = constituency, color = tourism_tier)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey50", fill = "grey85", linewidth = 0.7) +
  geom_point(size = 3, alpha = 0.9) +
  geom_text_repel(size = 2.5, max.overlaps = 17, show.legend = FALSE) +
  scale_color_manual(values = tier_colors, name = "Tourism Tier") +
  labs(title    = "Pre-STR Natural Tourism Endowment vs. Change in Persons per Bedroom",
       subtitle = "Sign sensitive to geographic aggregation; neither estimate is significant",
       x = "NTE (pre-STR share)", y = "Change in Persons per Bedroom 2010-2022",
       caption  = "Source: CSO Saint Lucia Census 2010, 2022; NTE from OSM / Overpass API (May 2026).")
ggsave("fig10_nte_vs_crowding.png", fig10, width = 8, height = 6, dpi = 300)

# ── 13. Regression Tables ─────────────────────────────────────────────────────
gof_std <- list(
  list(raw = "nobs",          clean = "N",       fmt = 0),
  list(raw = "r.squared",     clean = "R²", fmt = 3),
  list(raw = "adj.r.squared", clean = "Adj. R²", fmt = 3)
)

# Table 8: Log rent on NTE
modelsummary(
  list("(1) No controls" = rent1, "(2) Unit controls" = rent2, "(3) + Crowding" = rent3),
  stars       = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_rename = c(nte = "NTE", bedrooms = "Bedrooms",
                  household_size = "Household size", ppbr = "Persons per bedroom"),
  gof_map     = gof_std,
  title       = "Table 8. Log Monthly Rent on NTE — 2022 Renter Households"
)

# Table 9: Reduced-form LPM
modelsummary(
  list("17 Constituencies" = lpm, "10 Districts" = lpm_dist),
  stars       = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_rename = c(nte_post = "NTE x Arrivals Shift x Post",
                  nte_post_dist = "NTE x Arrivals Shift x Post"),
  gof_map     = gof_std,
  title       = "Table 9. Reduced-Form LPM: Pr(Renting)"
)

# Table 10: First stage
modelsummary(
  list("17 Const. x Narrow" = fs_narrow,
       "17 Const. x Broad"  = fs_broad,
       "10 Dist. x Narrow"  = fs_narrow_dist),
  stars       = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_rename = c(nte_post = "NTE x Arrivals Shift x Post",
                  nte_post_dist = "NTE x Arrivals Shift x Post"),
  gof_map     = gof_std,
  title       = "Table 10. First Stage: NTE Bartik IV on STR Unit Count"
)

# Table 11: 2SLS
modelsummary(
  list("17 Const. x Narrow" = iv_narrow,
       "17 Const. x Broad"  = iv_broad,
       "10 Dist. x Narrow"  = iv_narrow_dist),
  stars       = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_rename = c(fit_str_count       = "STR Units (instrumented)",
                  fit_str_count_broad = "STR Units (instrumented)",
                  fit_str_count_d     = "STR Units (instrumented)"),
  gof_map     = gof_std,
  title       = "Table 11. 2SLS: Instrumented STR Penetration and Pr(Renting)"
)

# Table 12: RQ3 ownership
modelsummary(
  list("17 Constituencies" = lpm_owner, "10 Districts" = lpm_owner_dist),
  stars       = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_rename = c(nte_post = "NTE x Arrivals Shift x Post",
                  nte_post_dist = "NTE x Arrivals Shift x Post"),
  gof_map     = gof_std,
  title       = "Table 12. Reduced-Form LPM: Pr(Owner-Occupied)"
)

# Table 13: RQ4 crowding
modelsummary(
  list("17 Constituencies" = lpm_ppbr, "10 Districts" = lpm_ppbr_dist),
  stars       = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_rename = c(nte_post = "NTE x Arrivals Shift x Post",
                  nte_post_dist = "NTE x Arrivals Shift x Post"),
  gof_map     = gof_std,
  title       = "Table 13. Reduced-Form OLS: Persons per Bedroom"
)

# ── 14. Robustness: Randomization Inference ───────────────────────────────────
set.seed(42)
n_perm    <- 999
obs_coef  <- coef(lpm)["nte_post"]
cons_names <- unique(hh_panel$constituency)

perm_coefs <- replicate(n_perm, {
  perm_nte <- setNames(sample(nte_data$nte), nte_data$constituency)
  hh_perm  <- hh_panel |>
    mutate(nte_p      = perm_nte[constituency],
           nte_post_p = nte_p * national_shift * post)
  coef(feols(renter ~ nte_post_p | constituency + year,
             data = hh_perm, weights = ~hh_weight,
             vcov = ~constituency))["nte_post_p"]
})

perm_p <- mean(abs(perm_coefs) >= abs(obs_coef))
cat(sprintf("\nPermutation p-value (two-sided, 999 perms): %.4f\n", perm_p))

# Figure 7: Permutation distribution
fig7 <- tibble(coef = perm_coefs) |>
  ggplot(aes(x = coef)) +
  geom_histogram(bins = 40, fill = "#ABDDA4", color = "white", alpha = 0.85) +
  geom_vline(xintercept =  obs_coef, color = "#D7191C", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = -obs_coef, color = "#D7191C", linetype = "dashed", linewidth = 1,
             alpha = 0.5) +
  annotate("text", x = obs_coef * 1.05, y = Inf,
           label = paste0("Observed\nb = ", round(obs_coef, 3), "\np = ", round(perm_p, 3)),
           hjust = 0, vjust = 1.4, size = 3, color = "#D7191C") +
  labs(title    = "Randomization Inference: NTE x Arrivals Shift x Post",
       subtitle = paste0("999 permutations across 17 constituencies; exact two-sided p = ",
                         round(perm_p, 3)),
       x = "Permuted reduced-form coefficient", y = "Count",
       caption  = "Source: CSO Saint Lucia Census 2010, 2022; NTE from OSM (May 2026).")
ggsave("fig7_permutation_distribution.png", fig7, width = 7, height = 5, dpi = 300)

# Table 14: Permutation summary
tibble(
  Statistic = c("Observed beta_RF", "Permutation mean (null)",
                "Permutation SD (null)", "p-value (two-sided)", "N permutations"),
  Value     = c(round(obs_coef, 4), round(mean(perm_coefs), 4),
                round(sd(perm_coefs), 4), round(perm_p, 4), n_perm)
) |> print()

# ── 15. Robustness: Leave-One-Out ─────────────────────────────────────────────
loo_results <- map_dfr(cons_names, function(drop_c) {
  fit <- feols(renter ~ nte_post | constituency + year,
               data    = hh_panel |> filter(constituency != drop_c),
               weights = ~hh_weight, vcov = ~constituency)
  ci <- confint(fit)["nte_post", ]
  tibble(dropped = drop_c, coef = coef(fit)["nte_post"],
         ci_lo = ci[[1]], ci_hi = ci[[2]])
})

# Figure 8: Leave-one-out plot
fig8 <- loo_results |>
  left_join(nte_data |> select(constituency, tourism_tier),
            by = c("dropped" = "constituency")) |>
  mutate(tourism_tier = factor(tourism_tier, levels = c("High", "Medium", "Low")),
         dropped = fct_reorder(dropped, coef)) |>
  ggplot(aes(x = coef, y = dropped, color = tourism_tier)) +
  geom_vline(xintercept = obs_coef, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi), width = 0.3, linewidth = 0.6, orientation = "y") +
  geom_point(size = 3) +
  scale_color_manual(values = tier_colors, name = "Tourism Tier") +
  labs(title    = "Leave-One-Out Sensitivity: Reduced-Form Coefficient",
       subtitle = "Each row drops one constituency; dashed line = full-sample estimate",
       x = "Reduced-form coefficient (NTE x Arrivals Shift x Post)", y = NULL,
       caption  = "Source: CSO Saint Lucia Census 2010, 2022; NTE from OSM (May 2026).")
ggsave("fig8_leave_one_out.png", fig8, width = 8, height = 7, dpi = 300)

# ── 16. Robustness: Balance Test ──────────────────────────────────────────────
bal_renter   <- feols(renter_rate_10   ~ nte, data = cons_wide, vcov = "hetero")
bal_owner    <- feols(owner_rate_10    ~ nte, data = cons_wide, vcov = "hetero")
bal_rentfree <- feols(rentfree_rate_10 ~ nte, data = cons_wide, vcov = "hetero")
bal_hhsize   <- feols(hh_size_10       ~ nte, data = cons_wide, vcov = "hetero")
bal_ppbr     <- feols(ppbr_10          ~ nte, data = cons_wide, vcov = "hetero")

modelsummary(
  list("Renter Rate" = bal_renter, "Owner Rate" = bal_owner,
       "Rent-Free"   = bal_rentfree, "HH Size"  = bal_hhsize,
       "Pers./Bdrm"  = bal_ppbr),
  stars       = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_rename = c(nte = "NTE (constituency share)"),
  gof_map     = list(list(raw = "nobs", clean = "N", fmt = 0),
                     list(raw = "r.squared", clean = "R²", fmt = 3)),
  title       = "Table 15. Balance Test: NTE and 2010 Baseline Characteristics (N = 17)"
)

# ── 17. Robustness: Anderson-Rubin Weak-Instrument CI ────────────────────────
f_narrow <- as.numeric((coef(fs_narrow)["nte_post"]          / se(fs_narrow)["nte_post"])^2)
f_broad  <- as.numeric((coef(fs_broad)["nte_post"]           / se(fs_broad)["nte_post"])^2)
f_dist   <- as.numeric((coef(fs_narrow_dist)["nte_post_dist"] / se(fs_narrow_dist)["nte_post_dist"])^2)

b2sls   <- as.numeric(coef(iv_narrow)[1])
ci_2sls <- as.numeric(confint(iv_narrow)[1, ])
se_2sls <- (ci_2sls[2] - ci_2sls[1]) / (2 * 1.96)

beta_seq <- seq(b2sls - 8 * se_2sls, b2sls + 8 * se_2sls, length.out = 500)

ar_pvals <- vapply(beta_seq, function(b0) {
  tmp      <- hh_panel
  tmp$y_ar <- tmp$renter - b0 * tmp$str_count
  ct <- coeftable(
    feols(y_ar ~ nte_post | constituency + year,
          data = tmp, weights = ~hh_weight, vcov = "hetero")
  )
  ct["nte_post", "Pr(>|t|)"]
}, numeric(1))

in_ci <- beta_seq[ar_pvals > 0.05]
ar_lo <- if (length(in_ci) == 0) NA_real_ else min(in_ci)
ar_hi <- if (length(in_ci) == 0) NA_real_ else max(in_ci)

ci_broad <- as.numeric(confint(iv_broad)[1, ])
ci_dist  <- as.numeric(confint(iv_narrow_dist)[1, ])

# Table 16: AR inference
tibble(
  Specification   = c("17 Const. x Narrow (main)", "17 Const. x Broad", "10 Districts x Narrow"),
  `Coef (2SLS)`   = round(c(b2sls,
                              as.numeric(coef(iv_broad)[1]),
                              as.numeric(coef(iv_narrow_dist)[1])), 4),
  `2SLS 95% CI`   = c(sprintf("[%.4f, %.4f]", ci_2sls[1], ci_2sls[2]),
                       sprintf("[%.4f, %.4f]", ci_broad[1], ci_broad[2]),
                       sprintf("[%.4f, %.4f]", ci_dist[1],  ci_dist[2])),
  `AR 95% CI`     = c(sprintf("[%.4f, %.4f]", ar_lo, ar_hi), "n/a", "n/a"),
  `First-stage F` = round(c(f_narrow, f_broad, f_dist), 1)
) |> print()

# ── 18. Session Info ──────────────────────────────────────────────────────────
sessionInfo()
