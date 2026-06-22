library(jsonlite)
library(dplyr)
library(readxl)
library(tidyr)

# ============================================================
# TPI v2 — Concentration of Tourism Attractions by District
# Source: OpenStreetMap via Overpass API
# Logic:  count meaningful tourism POIs per district,
#         normalize to island-wide shares (sum = 1)
# ============================================================

# ---- 1. Download OSM tourism + related features ----
download_osm <- function(query, outfile) {
  url <- paste0("https://overpass-api.de/api/interpreter?data=",
                URLencode(query, reserved = TRUE))
  download.file(url, outfile, method = "wininet", quiet = TRUE)
}

# Tourism nodes + ways (with centroid for ways)
q_tourism <- '[out:json][timeout:90];
area["ISO3166-1"="LC"]->.sl;
(
  node["tourism"](area.sl);
  way["tourism"](area.sl);
  node["historic"](area.sl);
  way["historic"](area.sl);
  node["natural"="beach"](area.sl);
  way["natural"="beach"](area.sl);
  node["leisure"="beach"](area.sl);
  way["leisure"="beach"](area.sl);
  node["amenity"="dive_centre"](area.sl);
  way["amenity"="dive_centre"](area.sl);
);
out center tags;'

cat("Downloading OSM attractions...\n")
download_osm(q_tourism, "osm_attractions.json")
cat("Done.\n")

# ---- 2. Parse OSM response ----
raw <- fromJSON("osm_attractions.json", simplifyVector = FALSE)

extract_feature <- function(el) {
  tags  <- el$tags
  if (is.null(tags)) return(NULL)

  # Coordinates: nodes have lat/lon directly; ways have center
  if (el$type == "node") {
    lon <- el$lon; lat <- el$lat
  } else if (!is.null(el$center)) {
    lon <- el$center$lon; lat <- el$center$lat
  } else {
    return(NULL)
  }

  data.frame(
    osm_type  = el$type,
    osm_id    = el$id,
    lon       = lon,
    lat       = lat,
    tourism   = if (!is.null(tags$tourism))   tags$tourism   else NA_character_,
    historic  = if (!is.null(tags$historic))  tags$historic  else NA_character_,
    natural   = if (!is.null(tags$natural))   tags$natural   else NA_character_,
    leisure   = if (!is.null(tags$leisure))   tags$leisure   else NA_character_,
    amenity   = if (!is.null(tags$amenity))   tags$amenity   else NA_character_,
    name      = if (!is.null(tags$name))      tags$name      else NA_character_,
    stringsAsFactors = FALSE
  )
}

feats <- do.call(rbind, Filter(Negate(is.null), lapply(raw$elements, extract_feature)))
cat("Total OSM features extracted:", nrow(feats), "\n")

# ---- 3. Classify and weight attractions ----
# Exclude non-attraction tags (information boards, camp sites, etc.)
tourism_exclude <- c("information", "camp_site", "caravan_site",
                     "picnic_site", "wilderness_hut", "chalet",
                     "apartment", "yes")  # "yes" = unspecified

feats <- feats |>
  mutate(
    # Broad attraction type
    attr_type = case_when(
      tourism %in% c("hotel", "resort", "motel")          ~ "hotel",
      tourism %in% c("guest_house", "hostel", "bed_and_breakfast") ~ "guesthouse",
      tourism %in% c("attraction", "theme_park", "zoo",
                     "aquarium", "museum", "gallery",
                     "arts_centre")                        ~ "attraction",
      tourism == "viewpoint"                               ~ "viewpoint",
      !is.na(historic)                                     ~ "historic",
      natural == "beach" | leisure == "beach"              ~ "beach",
      amenity == "dive_centre"                             ~ "dive_centre",
      TRUE ~ "other"
    ),
    # Weight: larger establishments matter more as pre-Airbnb tourism anchors
    weight = case_when(
      attr_type == "hotel"       ~ 3,
      attr_type == "attraction"  ~ 2,
      attr_type == "beach"       ~ 2,
      attr_type == "viewpoint"   ~ 1.5,
      attr_type == "historic"    ~ 1.5,
      attr_type == "guesthouse"  ~ 1,
      attr_type == "dive_centre" ~ 1,
      TRUE ~ 0   # exclude "other"
    )
  ) |>
  filter(
    weight > 0,
    # Remove tourism=information / camp_site etc. via tourism_exclude
    is.na(tourism) | !tourism %in% tourism_exclude
  )

cat("Features after filtering:", nrow(feats), "\n")
cat("By type:\n"); print(table(feats$attr_type))

# ---- 4. Load district polygons ----
gj <- fromJSON("lca_districts.geojson", simplifyVector = FALSE)

# Ray-casting point-in-polygon
pip <- function(lon, lat, poly_lon, poly_lat) {
  n      <- length(poly_lon)
  inside <- FALSE
  j      <- n
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
  x == "GrosIslet"   ~ "Gros Islet",
  x == "VieuxFort"   ~ "Vieux Fort",
  x == "Soufrière"   ~ "Soufriere",
  TRUE ~ x
)

# Build list of (district_name, list_of_rings)
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

cat("Assigning", nrow(feats), "features to districts...\n")
feats$district <- mapply(assign_district, feats$lon, feats$lat)
cat("Unassigned:", sum(is.na(feats$district)), "\n")

# ---- 4b. Nearest-centroid fallback for coastal POIs ----
# Unassigned features are beaches/hotels right on the coastline edge;
# assign to whichever district centroid is geographically closest.
centroids_ll <- do.call(rbind, lapply(districts, function(d) {
  all_pts <- do.call(rbind, lapply(d$rings, function(ring) {
    do.call(rbind, lapply(ring, function(p) c(p[[1]], p[[2]])))
  }))
  data.frame(district = d$name, clon = mean(all_pts[, 1]), clat = mean(all_pts[, 2]))
}))

assign_nearest <- function(lon, lat) {
  dists <- sqrt((centroids_ll$clon - lon)^2 + (centroids_ll$clat - lat)^2)
  centroids_ll$district[which.min(dists)]
}

unassigned_idx <- which(is.na(feats$district))
if (length(unassigned_idx) > 0) {
  feats$district[unassigned_idx] <- mapply(
    assign_nearest, feats$lon[unassigned_idx], feats$lat[unassigned_idx]
  )
  cat("After nearest-centroid fallback, unassigned:", sum(is.na(feats$district)), "\n")
}

# ---- 5. Aggregate by district ----
district_counts <- feats |>
  filter(!is.na(district)) |>
  group_by(district) |>
  summarise(
    n_attractions    = n(),
    weighted_score   = sum(weight),
    n_hotels         = sum(attr_type == "hotel"),
    n_guesthouses    = sum(attr_type == "guesthouse"),
    n_attractions_ex = sum(attr_type == "attraction"),
    n_beaches        = sum(attr_type == "beach"),
    n_viewpoints     = sum(attr_type == "viewpoint"),
    n_historic       = sum(attr_type == "historic"),
    .groups = "drop"
  )

# Ensure all 10 districts appear (fill zeros for any missing)
all_districts <- c("Anse-la-Raye", "Canaries", "Castries", "Choiseul",
                   "Dennery", "Gros Islet", "Laborie", "Micoud",
                   "Soufriere", "Vieux Fort")

district_counts <- tibble(district = all_districts) |>
  left_join(district_counts, by = "district") |>
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# ---- 6. Normalize to shares (TPI v2) ----
total_weighted <- sum(district_counts$weighted_score)

tpi_v2 <- district_counts |>
  mutate(
    tpi_v2 = weighted_score / total_weighted,
    rank_v2 = rank(-tpi_v2, ties.method = "min")
  ) |>
  arrange(rank_v2)

cat("\n=== TPI v2 — Attraction Concentration Shares ===\n")
print(tpi_v2 |> select(district, n_attractions, weighted_score, tpi_v2, rank_v2),
      n = 10)
cat("\nSum of tpi_v2:", sum(tpi_v2$tpi_v2), "\n")

# ---- 7. Compare with original TPI ----
tpi_orig <- read_excel("TPI_Saint_Lucia.xlsx", skip = 57) |>
  rename_with(tolower) |>
  rename(tpi_v1 = `tpi score`) |>
  select(district, tpi_v1) |>
  mutate(
    district = case_when(
      district == "Anse la Raye" ~ "Anse-la-Raye",
      district == "Soufrière"    ~ "Soufriere",
      TRUE ~ district
    ),
    tpi_v1 = as.numeric(tpi_v1)
  )

comparison <- tpi_v2 |>
  select(district, n_attractions, weighted_score, tpi_v2, rank_v2) |>
  left_join(tpi_orig, by = "district") |>
  mutate(
    rank_v1 = rank(-tpi_v1, ties.method = "min"),
    rank_change = rank_v1 - rank_v2
  ) |>
  arrange(rank_v2)

cat("\n=== Comparison: TPI v1 (distance-based) vs TPI v2 (attraction-based) ===\n")
print(comparison |>
  select(district, tpi_v1, tpi_v2, rank_v1, rank_v2, rank_change),
  n = 10)

# ---- 8. Save results ----
write.csv(comparison, "TPI_v2_attraction_concentration.csv", row.names = FALSE)
cat("\nSaved: TPI_v2_attraction_concentration.csv\n")

# Save cleaned attraction list
write.csv(feats |> select(osm_type, osm_id, lon, lat, attr_type, weight, name, district),
          "osm_attractions_classified.csv", row.names = FALSE)
cat("Saved: osm_attractions_classified.csv\n")
