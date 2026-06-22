# build_nte_map.R
# Creates a choropleth map of Natural Tourism Endowment (NTE) by district.
# Uses jsonlite to parse lca_districts.geojson and ggplot2 geom_polygon
# (no sf / sp packages required).

library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidyr)

# ---- 1. Load NTE constituency data ----
nte <- read.csv("NTE_constituency.csv")

# ---- 2. Aggregate NTE to district level ----
cons_to_dist <- tribble(
  ~constituency,              ~district,
  "Gros Islet",               "Gros Islet",
  "Babonneau",                "Gros Islet",
  "Castries North",           "Castries",
  "Castries East",            "Castries",
  "Castries Central",         "Castries",
  "Castries South",           "Castries",
  "Castries South-East",      "Castries",
  "Anse-la-Raye/Canaries",   "Anse-la-Raye",
  "Anse-la-Raye/Canaries",   "Canaries",
  "Soufriere",                "Soufriere",
  "Choiseul",                 "Choiseul",
  "Laborie",                  "Laborie",
  "Vieux-Fort South",         "Vieux Fort",
  "Vieux-Fort North",         "Vieux Fort",
  "Micoud South",             "Micoud",
  "Micoud North",             "Micoud",
  "Dennery South",            "Dennery",
  "Dennery North",            "Dennery"
)

# Anse-la-Raye/Canaries NTE is shared equally between the two districts
nte_district <- nte |>
  left_join(cons_to_dist, by = "constituency") |>
  mutate(nte_contrib = if_else(constituency == "Anse-la-Raye/Canaries",
                               nte / 2, nte)) |>
  group_by(district) |>
  summarise(nte_dist = sum(nte_contrib), .groups = "drop")

cat("District NTE values:\n")
print(nte_district |> arrange(desc(nte_dist)))

# ---- 3. Parse district GeoJSON into a polygon data frame ----
gj <- fromJSON("lca_districts.geojson", simplifyVector = FALSE)

name_fix <- function(x) {
  switch(x,
    "GrosIslet"  = "Gros Islet",
    "VieuxFort"  = "Vieux Fort",
    "Soufrière"  = "Soufriere",
    "AnselaRaye" = "Anse-la-Raye",
    x
  )
}

extract_rings <- function(feature, district_name, poly_id) {
  geom <- feature$geometry
  # Collect all rings from Polygon or MultiPolygon
  if (geom$type == "Polygon") {
    rings <- list(geom$coordinates)
  } else {
    rings <- geom$coordinates
  }
  rows <- list()
  ring_id <- 0L
  for (poly in rings) {
    outer_ring <- poly[[1]]
    ring_id <- ring_id + 1L
    coords <- do.call(rbind, lapply(outer_ring, function(p) c(p[[1]], p[[2]])))
    rows[[length(rows) + 1L]] <- data.frame(
      long       = coords[, 1],
      lat        = coords[, 2],
      district   = district_name,
      poly_id    = paste0(poly_id, "_", ring_id),
      stringsAsFactors = FALSE
    )
  }
  bind_rows(rows)
}

poly_df <- bind_rows(lapply(seq_along(gj$features), function(i) {
  feat <- gj$features[[i]]
  nm   <- name_fix(feat$properties$NAME_1)
  extract_rings(feat, nm, i)
}))

cat("\nDistricts in GeoJSON:\n")
print(sort(unique(poly_df$district)))

# ---- 4. Join NTE to polygon data ----
map_df <- poly_df |>
  left_join(nte_district, by = "district")

# ---- 5. Compute label positions (centroid of bounding box per district) ----
label_df <- map_df |>
  group_by(district) |>
  summarise(
    long     = mean(range(long)),
    lat      = mean(range(lat)),
    nte_dist = first(nte_dist),
    .groups  = "drop"
  ) |>
  mutate(
    label     = paste0(district, "\n", round(nte_dist * 100, 1), "%"),
    tier      = case_when(
      nte_dist >= 0.10 ~ "High",
      nte_dist >= 0.04 ~ "Medium",
      TRUE             ~ "Low"
    )
  )

# Manual nudges for crowded labels
nudge <- tribble(
  ~district,      ~dx,    ~dy,
  "Castries",      0.01,   0.03,
  "Canaries",     -0.06,   0.00,
  "Anse-la-Raye", -0.06,   0.05,
  "Soufriere",    -0.04,  -0.01,
  "Choiseul",     -0.05,  -0.06,
  "Laborie",       0.08,  -0.02,
  "Vieux Fort",    0.07,  -0.04
)
label_df <- label_df |>
  left_join(nudge, by = "district") |>
  mutate(
    dx = replace_na(dx, 0),
    dy = replace_na(dy, 0),
    long = long + dx,
    lat  = lat  + dy
  )

# ---- 6. Plot ----
tier_colours <- c("High" = "#084594", "Medium" = "#4292C6", "Low" = "#C6DBEF")

p <- ggplot() +
  geom_polygon(
    data    = map_df,
    aes(x = long, y = lat, group = poly_id, fill = nte_dist),
    colour  = "white", linewidth = 0.4
  ) +
  geom_text(
    data = label_df,
    aes(x = long, y = lat, label = label),
    size = 2.4, lineheight = 0.9, colour = "black"
  ) +
  scale_fill_gradient(
    low    = "#C6DBEF",
    high   = "#084594",
    name   = "NTE Share",
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  coord_fixed(ratio = 1) +
  labs(
    title   = "Natural Tourism Endowment (NTE) by District",
    subtitle = "Constituency-level NTE values aggregated to districts; shares sum to 1.0",
    caption = "Source: OpenStreetMap / Overpass API (May 2026). District boundaries: GADM v4.1."
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", size = 12, hjust = 0.5,
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size = 8.5, hjust = 0.5, colour = "grey40",
                                 margin = margin(b = 8)),
    plot.caption  = element_text(size = 7, colour = "grey50",
                                 margin = margin(t = 6)),
    legend.position  = "right",
    legend.title     = element_text(size = 8),
    legend.text      = element_text(size = 7),
    plot.margin      = margin(10, 10, 10, 10)
  )

ggsave("NTE_Map_Saint_Lucia.png", plot = p,
       width = 6, height = 7.5, dpi = 300, bg = "white")
cat("\nSaved: NTE_Map_Saint_Lucia.png\n")
