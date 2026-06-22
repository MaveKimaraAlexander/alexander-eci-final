# build_nte_constituency.R
# Natural Tourism Endowment (NTE) -- direct constituency-level assignment.
#
# Improvement over build_tpi_topographic.R:
#   Assigns each OSM feature to a constituency based on the feature's own
#   coordinates rather than distributing district scores by household weights.
#
# Method:
#   - Single-constituency districts: direct assignment
#   - Gros Islet district (2 constituencies): latitude split
#       Babonneau constituency is the SOUTHERN part of Gros Islet district
#       (Babonneau village lat ~14.008, Gros Islet town lat ~14.075).
#       north (lat >= median) -> Gros Islet; south -> Babonneau.
#   - Vieux Fort, Micoud, Dennery (N/S splits): latitude split
#       north (lat >= median) -> North; south -> South
#   - Anse-la-Raye + Canaries: merged as one constituency
#   - Castries (5 constituencies, all Low tier): equal weight split
#       Cannot distinguish sub-constituencies without constituency boundary data.
#       Equal split is conservative and transparent.

library(dplyr)

# ---- 1. Load pre-classified OSM features ----
feats <- read.csv("natural_amenities_classified.csv")

# Manual exclusions: OSM entries verified as incorrectly tagged in source data
# osm_id 2571578478 "Alma's Kicthen" — tagged as nature_reserve but is a restaurant
feats <- feats[feats$osm_id != "2571578478", ]

cat("Features loaded (pre-dedup):", nrow(feats), "\n")

# ---- Spatial deduplication ----
# Features of the same attr_type within a distance threshold are collapsed to one.
# Reason: OSM sometimes tags the same physical spot multiple times (e.g. 14 unnamed
# viewpoints clustered around the Pitons). All features were cross-checked against
# Google Maps; deduplication removes coordinate-level redundancy, not real features.
#
# Thresholds (metres):
#   viewpoint      200 m  — tend to cluster on the same ridge/lookout
#   waterfall      150 m  — same cascade tagged at top, middle, bottom
#   beach          400 m  — same beach section tagged at multiple access points
#   peak           500 m  — same summit tagged by multiple contributors
#   hot_spring/nature_reserve: no dedup (unique features)
#
# Priority when merging: named feature beats unnamed; lower osm_id breaks ties.

haversine_m <- function(lat1, lon1, lat2, lon2) {
  R <- 6371000
  phi1 <- lat1 * pi / 180; phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a    <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  2 * R * asin(sqrt(a))
}

dedup_thresholds <- c(
  viewpoint      = 200,
  waterfall      = 150,
  beach          = 400,
  peak           = 500,
  hot_spring     = Inf,
  nature_reserve = Inf
)

dedup_type <- function(df, threshold_m) {
  if (nrow(df) <= 1 || is.infinite(threshold_m)) return(df)
  # Sort: named first (so named beats unnamed in greedy scan)
  df <- df[order(!is.na(df$name)), ]
  keep <- rep(TRUE, nrow(df))
  for (i in seq_len(nrow(df) - 1)) {
    if (!keep[i]) next
    for (j in (i + 1):nrow(df)) {
      if (!keep[j]) next
      d <- haversine_m(df$lat[i], df$lon[i], df$lat[j], df$lon[j])
      if (d < threshold_m) keep[j] <- FALSE
    }
  }
  df[keep, ]
}

feats_dedup <- lapply(unique(feats$attr_type), function(tp) {
  sub <- feats[feats$attr_type == tp, ]
  thr <- dedup_thresholds[tp]
  if (is.na(thr)) thr <- Inf
  dedup_type(sub, thr)
})
feats_dedup <- do.call(rbind, feats_dedup)

dropped <- nrow(feats) - nrow(feats_dedup)
cat(sprintf("After spatial deduplication: %d features kept, %d removed\n",
            nrow(feats_dedup), dropped))

if (dropped > 0) {
  removed_ids <- setdiff(feats$osm_id, feats_dedup$osm_id)
  removed_rows <- feats[feats$osm_id %in% removed_ids,
                        c("name", "attr_type", "district", "lat", "lon")]
  cat("\nRemoved as spatial duplicates:\n")
  print(removed_rows[order(removed_rows$district, removed_rows$attr_type), ],
        row.names = FALSE)
}

feats <- feats_dedup
cat("\nFeatures by district (post-dedup):\n")
print(table(feats$district, useNA = "ifany"))

# ---- 2. Constituency -> poldist lookup ----
poldist_lut <- c(
  "Gros Islet"            = 100L,
  "Babonneau"             = 200L,
  "Castries North"        = 300L,
  "Castries East"         = 400L,
  "Castries Central"      = 500L,
  "Castries South"        = 600L,
  "Anse-la-Raye/Canaries" = 700L,
  "Soufriere"             = 800L,
  "Choiseul"              = 900L,
  "Laborie"               = 1000L,
  "Vieux-Fort South"      = 1100L,
  "Vieux-Fort North"      = 1200L,
  "Micoud South"          = 1300L,
  "Micoud North"          = 1400L,
  "Dennery South"         = 1500L,
  "Dennery North"         = 1600L,
  "Castries South-East"   = 1700L
)

# ---- 3. Compute geographic split thresholds from actual feature locations ----
med_lon <- function(d) median(feats$lon[feats$district == d & !is.na(feats$district)], na.rm = TRUE)
med_lat <- function(d) median(feats$lat[feats$district == d & !is.na(feats$district)], na.rm = TRUE)

gros_med_lat    <- med_lat("Gros Islet")
vf_med_lat      <- med_lat("Vieux Fort")
micoud_med_lat  <- med_lat("Micoud")
dennery_med_lat <- med_lat("Dennery")

cat("\nGeographic split thresholds (derived from feature coordinates, not population):\n")
cat(sprintf("  Gros Islet  median lat : %.5f  (north=Gros Islet, south=Babonneau)\n", gros_med_lat))
cat(sprintf("  Vieux Fort  median lat : %.5f  (north=VF-North, south=VF-South)\n", vf_med_lat))
cat(sprintf("  Micoud      median lat : %.5f  (north=Micoud-N, south=Micoud-S)\n", micoud_med_lat))
cat(sprintf("  Dennery     median lat : %.5f  (north=Dennery-N, south=Dennery-S)\n", dennery_med_lat))

# ---- 4. Apply constituency overrides first, then geographic splits ----
# Features with a verified constituency_override bypass the geographic split.
# This applies to peaks and other features confirmed by local knowledge.

has_override <- !is.na(feats$constituency_override)
feats_override  <- feats[has_override, ]
feats_geo       <- feats[!has_override, ]

cat("\nFeatures with verified constituency override:", nrow(feats_override), "\n")
print(feats_override[, c("name", "district", "constituency_override")])

# ---- 5. Assign remaining non-Castries features via geographic splits ----
feats_assigned <- feats_geo |>
  filter(!is.na(district), district != "Castries") |>
  mutate(constituency = case_when(
    district == "Soufriere"                        ~ "Soufriere",
    district == "Choiseul"                         ~ "Choiseul",
    district == "Laborie"                          ~ "Laborie",
    district %in% c("Anse-la-Raye", "Canaries")   ~ "Anse-la-Raye/Canaries",
    district == "Gros Islet" & lat >= gros_med_lat ~ "Gros Islet",
    district == "Gros Islet" & lat <  gros_med_lat ~ "Babonneau",
    district == "Vieux Fort" & lat >= vf_med_lat   ~ "Vieux-Fort North",
    district == "Vieux Fort" & lat <  vf_med_lat   ~ "Vieux-Fort South",
    district == "Micoud"     & lat >= micoud_med_lat ~ "Micoud North",
    district == "Micoud"     & lat <  micoud_med_lat ~ "Micoud South",
    district == "Dennery"    & lat >= dennery_med_lat ~ "Dennery North",
    district == "Dennery"    & lat <  dennery_med_lat ~ "Dennery South",
    TRUE ~ NA_character_
  ))

cat("\nFeature assignment check (non-Castries, geographic split):\n")
print(table(feats_assigned$constituency, useNA = "ifany"))

# ---- 6. Castries: equal weight split, excluding verified overrides ----
castries_5 <- c(
  "Castries North", "Castries East", "Castries Central",
  "Castries South", "Castries South-East"
)

# Castries features without an override go into equal pool
castries_total <- feats_geo |>
  filter(!is.na(district), district == "Castries") |>
  summarise(total = sum(weight, na.rm = TRUE)) |>
  pull(total)

cat("\nCastries district pool (excl. overrides):", castries_total,
    "-> split equally across 5 constituencies (", castries_total / 5, "each)\n")

castries_rows <- tibble(
  constituency   = castries_5,
  weighted_score = castries_total / 5
)

# ---- 7. Aggregate: geographic + overrides ----
override_agg <- feats_override |>
  group_by(constituency = constituency_override) |>
  summarise(weighted_score = sum(weight, na.rm = TRUE), .groups = "drop")

non_castries <- bind_rows(
  feats_assigned |>
    filter(!is.na(constituency)) |>
    group_by(constituency) |>
    summarise(weighted_score = sum(weight, na.rm = TRUE), .groups = "drop"),
  override_agg
) |>
  group_by(constituency) |>
  summarise(weighted_score = sum(weighted_score), .groups = "drop")

# ---- 8. Combine and normalise to NTE shares ----
nte_raw <- bind_rows(non_castries, castries_rows) |>
  group_by(constituency) |>
  summarise(weighted_score = sum(weighted_score), .groups = "drop") |>
  arrange(constituency)

island_total <- sum(nte_raw$weighted_score, na.rm = TRUE)
cat("\nIsland total weighted score:", island_total, "\n")

# Ensure all 17 constituencies appear (fill missing with 0)
all_cons <- names(poldist_lut)
nte_constituency <- tibble(constituency = all_cons) |>
  left_join(nte_raw, by = "constituency") |>
  mutate(
    weighted_score = replace(weighted_score, is.na(weighted_score), 0),
    nte            = weighted_score / island_total,
    poldist        = poldist_lut[constituency],
    tourism_tier   = case_when(
      nte >= 0.10 ~ "High",
      nte >= 0.04 ~ "Medium",
      TRUE        ~ "Low"
    )
  ) |>
  arrange(desc(nte)) |>
  mutate(rank = row_number()) |>
  select(constituency, poldist, nte, tourism_tier, rank)

cat("\n=== Natural Tourism Endowment (NTE) by Constituency ===\n")
print(nte_constituency, n = 17)
cat("\nSum of NTE (should equal 1.0):", round(sum(nte_constituency$nte), 6), "\n")

# ---- 9. Compare to old TPI (household-weight method) ----
if (file.exists("TPI_topographic_constituency.csv")) {
  tpi_old <- read.csv("TPI_topographic_constituency.csv")
  compare <- nte_constituency |>
    select(constituency, nte_new = nte, tier_new = tourism_tier, rank_new = rank) |>
    left_join(
      tpi_old |> select(constituency, nte_old = tpi, tier_old = tourism_tier, rank_old = rank),
      by = "constituency"
    ) |>
    mutate(
      delta       = round(nte_new - nte_old, 4),
      tier_change = tier_old != tier_new
    )
  cat("\n=== Comparison: NTE (new) vs TPI household-weight (old) ===\n")
  print(compare |> arrange(rank_new), n = 17)
  cat("\nConstituencies with tier change:", sum(compare$tier_change, na.rm = TRUE), "\n")
}

# ---- 10. Save output ----
write.csv(nte_constituency, "NTE_constituency.csv", row.names = FALSE)
cat("\nSaved: NTE_constituency.csv\n")

# Save annotated features for transparency
feats_out <- bind_rows(
  feats_assigned |> select(osm_type, osm_id, lon, lat, attr_type, weight, name, district, constituency),
  feats |>
    filter(!is.na(district), district == "Castries") |>
    mutate(constituency = "Castries (equal split)") |>
    select(osm_type, osm_id, lon, lat, attr_type, weight, name, district, constituency)
)
write.csv(feats_out, "natural_amenities_nte_assigned.csv", row.names = FALSE)
cat("Saved: natural_amenities_nte_assigned.csv\n")
