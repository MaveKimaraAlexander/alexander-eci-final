# build_tpi_topographic.R
# Topographic Pull Index (TPI) — natural geographic amenities only.
# Excludes hotels, guesthouses, historic monuments, and dive centres.
# Features: beaches, peaks, hot springs, waterfalls, nature reserves,
#           viewpoints, bays.

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(haven)
library(labelled)

# ---- 1. Fetch natural amenity features from Overpass API ----
q_natural <- '[out:json][timeout:90];
area["ISO3166-1"="LC"]->.sl;
(
  node["natural"="beach"](area.sl);
  way["natural"="beach"](area.sl);
  node["leisure"="beach"](area.sl);
  way["leisure"="beach"](area.sl);
  node["natural"="peak"](area.sl);
  node["natural"="hot_spring"](area.sl);
  node["waterway"="waterfall"](area.sl);
  way["waterway"="waterfall"](area.sl);
  node["leisure"="nature_reserve"](area.sl);
  way["leisure"="nature_reserve"](area.sl);
  relation["leisure"="nature_reserve"](area.sl);
  relation["boundary"="national_park"](area.sl);
  node["tourism"="viewpoint"](area.sl);
  node["natural"="bay"](area.sl);
  way["natural"="bay"](area.sl);
);
out center tags;'

cat("Fetching natural amenity features from Overpass API...\n")
resp <- POST(
  "https://overpass-api.de/api/interpreter",
  body   = list(data = q_natural),
  encode = "form",
  timeout(120)
)

if (status_code(resp) != 200) {
  stop("Overpass API returned status: ", status_code(resp))
}

raw <- fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
cat("Elements returned:", length(raw$elements), "\n")

# ---- 2. Extract coordinates and classify ----
extract_feature <- function(el) {
  tags <- el$tags
  if (is.null(tags)) return(NULL)

  if (el$type == "node") {
    lon <- el$lon; lat <- el$lat
  } else if (!is.null(el$center)) {
    lon <- el$center$lon; lat <- el$center$lat
  } else {
    return(NULL)
  }

  data.frame(
    osm_type = el$type,
    osm_id   = as.character(el$id),
    lon      = lon,
    lat      = lat,
    nat      = if (!is.null(tags$natural))  tags$natural  else NA_character_,
    waterway = if (!is.null(tags$waterway)) tags$waterway else NA_character_,
    leisure  = if (!is.null(tags$leisure))  tags$leisure  else NA_character_,
    boundary = if (!is.null(tags$boundary)) tags$boundary else NA_character_,
    tourism  = if (!is.null(tags$tourism))  tags$tourism  else NA_character_,
    name     = if (!is.null(tags$name))     tags$name     else NA_character_,
    stringsAsFactors = FALSE
  )
}

feats <- do.call(rbind, Filter(Negate(is.null), lapply(raw$elements, extract_feature)))
cat("Features extracted:", nrow(feats), "\n")

feats <- feats |>
  mutate(
    attr_type = case_when(
      nat %in% c("beach") | leisure == "beach" ~ "beach",
      nat == "peak"                             ~ "peak",
      nat == "hot_spring"                       ~ "hot_spring",
      waterway == "waterfall"                   ~ "waterfall",
      leisure  == "nature_reserve"              ~ "nature_reserve",
      boundary == "national_park"               ~ "nature_reserve",
      tourism  == "viewpoint"                   ~ "viewpoint",
      nat      == "bay"                         ~ "bay",
      TRUE                                      ~ "other"
    ),
    weight = case_when(
      attr_type == "beach"          ~ 3.0,
      attr_type == "peak"           ~ 2.5,
      attr_type == "hot_spring"     ~ 2.0,
      attr_type == "waterfall"      ~ 2.0,
      attr_type == "nature_reserve" ~ 2.0,
      attr_type == "viewpoint"      ~ 1.5,
      attr_type == "bay"            ~ 1.0,
      TRUE                          ~ 0
    )
  ) |>
  filter(weight > 0)

cat("Features after filtering:", nrow(feats), "\n")
cat("By type:\n"); print(table(feats$attr_type))

# ---- 3. Assign districts using ray-casting point-in-polygon ----
gj <- fromJSON("lca_districts.geojson", simplifyVector = FALSE)

pip <- function(lon, lat, poly_lon, poly_lat) {
  n <- length(poly_lon)
  inside <- FALSE
  j <- n
  for (i in seq_len(n)) {
    xi <- poly_lon[i]; yi <- poly_lat[i]
    xj <- poly_lon[j]; yj <- poly_lat[j]
    if (((yi > lat) != (yj > lat)) &&
        (lon < (xj - xi) * (lat - yi) / (yj - yi) + xi))
      inside <- !inside
    j <- i
  }
  inside
}

name_fix <- function(x) case_when(
  x == "GrosIslet"  ~ "Gros Islet",
  x == "VieuxFort"  ~ "Vieux Fort",
  x == "Soufrière"  ~ "Soufriere",
  TRUE ~ x
)

districts <- lapply(gj$features, function(f) {
  geom <- f$geometry
  nm   <- name_fix(f$properties$NAME_1)
  rings <- if (geom$type == "Polygon") {
    list(geom$coordinates[[1]])
  } else {
    lapply(geom$coordinates, function(poly) poly[[1]])
  }
  list(name = nm, rings = rings)
})

assign_district <- function(lon, lat) {
  for (d in districts) {
    for (ring in d$rings) {
      pts <- do.call(rbind, lapply(ring, function(p) c(p[[1]], p[[2]])))
      if (pip(lon, lat, pts[, 1], pts[, 2])) return(d$name)
    }
  }
  NA_character_
}

cat("Assigning districts...\n")
feats$district <- mapply(assign_district, feats$lon, feats$lat)
cat("Unassigned:", sum(is.na(feats$district)), "\n")

# Nearest-centroid fallback for coastal POIs on district edges
centroids_ll <- do.call(rbind, lapply(districts, function(d) {
  all_pts <- do.call(rbind, lapply(d$rings, function(ring) {
    do.call(rbind, lapply(ring, function(p) c(p[[1]], p[[2]])))
  }))
  data.frame(district = d$name,
             clon = mean(all_pts[, 1]),
             clat = mean(all_pts[, 2]))
}))

assign_nearest <- function(lon, lat) {
  dists <- sqrt((centroids_ll$clon - lon)^2 + (centroids_ll$clat - lat)^2)
  centroids_ll$district[which.min(dists)]
}

unassigned_idx <- which(is.na(feats$district))
if (length(unassigned_idx) > 0) {
  feats$district[unassigned_idx] <- mapply(
    assign_nearest,
    feats$lon[unassigned_idx],
    feats$lat[unassigned_idx]
  )
  cat("After nearest-centroid fallback, unassigned:", sum(is.na(feats$district)), "\n")
}

# ---- 4. Aggregate to district level and normalise ----
all_districts <- c("Anse-la-Raye", "Canaries", "Castries", "Choiseul",
                   "Dennery", "Gros Islet", "Laborie", "Micoud",
                   "Soufriere", "Vieux Fort")

district_scores <- feats |>
  filter(!is.na(district)) |>
  group_by(district) |>
  summarise(
    n_features     = n(),
    weighted_score = sum(weight),
    n_beaches      = sum(attr_type == "beach"),
    n_peaks        = sum(attr_type == "peak"),
    n_hot_springs  = sum(attr_type == "hot_spring"),
    n_waterfalls   = sum(attr_type == "waterfall"),
    n_reserves     = sum(attr_type == "nature_reserve"),
    n_viewpoints   = sum(attr_type == "viewpoint"),
    n_bays         = sum(attr_type == "bay"),
    .groups = "drop"
  )

district_scores <- tibble(district = all_districts) |>
  left_join(district_scores, by = "district") |>
  mutate(across(where(is.numeric), ~ replace_na(., 0))) |>
  mutate(tpi_district = weighted_score / sum(weighted_score))

cat("\n=== District-level Topographic Pull Scores ===\n")
print(
  district_scores |>
    select(district, n_features, n_beaches, n_peaks, n_viewpoints,
           weighted_score, tpi_district) |>
    arrange(desc(tpi_district)),
  n = 10
)

# ---- 5. Disaggregate district -> constituency (2010 HH weights) ----
dist_to_group <- tibble(
  district_id = 1:12,
  tpi_group   = c("Castries", "Castries", "Castries",
                  "Anse-la-Raye", "Canaries",
                  "Soufriere", "Choiseul", "Laborie",
                  "Vieux Fort", "Micoud", "Dennery", "Gros Islet")
)

cons_names <- c(
  "100"  = "Gros Islet",       "200"  = "Babonneau",
  "300"  = "Castries North",   "400"  = "Castries East",
  "500"  = "Castries Central", "600"  = "Castries South",
  "700"  = "Anse-la-Raye/Canaries",
  "800"  = "Soufriere",        "900"  = "Choiseul",
  "1000" = "Laborie",          "1100" = "Vieux-Fort South",
  "1200" = "Vieux-Fort North", "1300" = "Micoud South",
  "1400" = "Micoud North",     "1500" = "Dennery South",
  "1600" = "Dennery North",    "1700" = "Castries South-East"
)

cat("\nLoading 2010 census for household weights...\n")
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

group_totals <- hh_2010 |>
  group_by(tpi_group) |>
  summarise(total_hh = sum(hh_weight, na.rm = TRUE), .groups = "drop")

cons_group_hh <- hh_2010 |>
  group_by(poldist, tpi_group) |>
  summarise(hh = sum(hh_weight, na.rm = TRUE), .groups = "drop")

tpi_constituency <- cons_group_hh |>
  left_join(group_totals, by = "tpi_group") |>
  left_join(
    district_scores |> select(district, tpi_district) |> rename(tpi_group = district),
    by = "tpi_group"
  ) |>
  mutate(tpi_contrib = (hh / total_hh) * tpi_district) |>
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

cat("\n=== Constituency Topographic Pull Index ===\n")
print(tpi_constituency, n = 17)
cat("\nSum of TPI:", sum(tpi_constituency$tpi), "\n")

# ---- 6. Save outputs ----
write.csv(tpi_constituency, "TPI_topographic_constituency.csv", row.names = FALSE)
cat("\nSaved: TPI_topographic_constituency.csv\n")

write.csv(
  feats |> select(osm_type, osm_id, lon, lat, attr_type, weight, name, district),
  "natural_amenities_classified.csv",
  row.names = FALSE
)
cat("Saved: natural_amenities_classified.csv\n")
