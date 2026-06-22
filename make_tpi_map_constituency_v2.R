library(jsonlite)
library(ggplot2)
library(dplyr)
library(scales)

# ============================================================
# Build a constituency-level TPI choropleth using the 547
# geoBoundaries ADM2 community polygons.
#
# Steps:
#  1. Assign each community centroid to a district via ray-casting PIP
#  2. Within multi-constituency districts, split by latitude
#  3. Join constituency TPI; plot colored by TPI
# ============================================================

# ---- 1. Load TPI data ----
tpi_const <- read.csv("TPI_constituency.csv") |>
  select(constituency, tpi, tourism_tier, poldist)

# Castries has 5 low-TPI constituencies (2-4%, all Low tier).
# Use weighted average for the whole Castries district polygon area.
castries_avg_tpi <- tpi_const |>
  filter(grepl("Castries", constituency)) |>
  summarise(tpi = mean(tpi)) |>
  pull(tpi)

# ---- 2. Parse district polygons ----
dist_gj <- fromJSON("lca_districts.geojson", simplifyVector = FALSE)

name_fix <- function(x) case_when(
  x == "GrosIslet"                       ~ "Gros Islet",
  x == "VieuxFort"                       ~ "Vieux Fort",
  grepl("Soufri", x, ignore.case = TRUE) ~ "Soufriere",
  TRUE ~ x
)

# Returns list: each element is a matrix of (lon, lat) for one ring
get_rings <- function(feat) {
  geom <- feat$geometry
  coords_list <- if (geom$type == "Polygon") list(geom$coordinates) else geom$coordinates
  lapply(coords_list, function(poly) {
    ring <- poly[[1]]
    do.call(rbind, lapply(ring, function(p) c(p[[1]], p[[2]])))
  })
}

dist_polys <- lapply(dist_gj$features, function(f) {
  list(name = name_fix(f$properties$NAME_1),
       rings = get_rings(f))
})

# ---- 3. Ray-casting point-in-polygon ----
pip <- function(px, py, vx, vy) {
  n <- length(vx)
  inside <- FALSE
  j <- n
  for (i in seq_len(n)) {
    if (((vy[i] > py) != (vy[j] > py)) &&
        (px < (vx[j] - vx[i]) * (py - vy[i]) / (vy[j] - vy[i]) + vx[i]))
      inside <- !inside
    j <- i
  }
  inside
}

point_in_district <- function(px, py) {
  for (dp in dist_polys) {
    for (ring in dp$rings) {
      if (pip(px, py, ring[, 1], ring[, 2])) return(dp$name)
    }
  }
  NA_character_
}

# ---- 4. Parse community polygons and compute centroids ----
cat("Loading community polygons...\n")
comm_gj <- fromJSON("lca_adm2_all/geoBoundaries-LCA-ADM2_simplified.geojson",
                    simplifyVector = FALSE)

extract_comm <- function(feat) {
  nm   <- feat$properties$shapeName
  geom <- feat$geometry
  coords_list <- if (geom$type == "Polygon") list(geom$coordinates) else geom$coordinates
  rows <- lapply(seq_along(coords_list), function(pi) {
    ring <- coords_list[[pi]][[1]]
    pts  <- do.call(rbind, lapply(ring, function(p) c(p[[1]], p[[2]])))
    data.frame(community = nm, piece = pi,
               lon = pts[, 1], lat = pts[, 2],
               stringsAsFactors = FALSE)
  })
  bind_rows(rows)
}

cat("Extracting community polygons...\n")
comm_df <- bind_rows(lapply(comm_gj$features, extract_comm))

# Community centroids for district assignment
comm_centroids <- comm_df |>
  group_by(community) |>
  summarise(clon = mean(lon), clat = mean(lat), .groups = "drop")

# ---- 5. Assign each community to a district ----
cat("Assigning communities to districts (may take ~30s)...\n")
n <- nrow(comm_centroids)
comm_centroids$district <- NA_character_
for (i in seq_len(n)) {
  if (i %% 50 == 0) cat(i, "/", n, "\n")
  comm_centroids$district[i] <- point_in_district(
    comm_centroids$clon[i], comm_centroids$clat[i]
  )
}

# Check coverage
cat("\nDistrict assignment:\n")
print(table(comm_centroids$district, useNA = "ifany"))

# ---- 6. Assign constituency within each district ----
# For single-constituency districts: direct assignment
# For N/S splits (Vieux Fort, Micoud, Dennery): use median latitude
# For Gros Islet: split by longitude (coast vs. inland for Babonneau)
# For Castries: use average TPI across all 5 Castries constituencies

split_lat <- function(df, dist_name, north_const, south_const) {
  lats <- df$clat[df$district == dist_name & !is.na(df$district)]
  if (length(lats) == 0) return(df)
  med <- median(lats)
  df$constituency[df$district == dist_name & !is.na(df$district) & df$clat >= med] <- north_const
  df$constituency[df$district == dist_name & !is.na(df$district) & df$clat <  med] <- south_const
  df
}

comm_centroids$constituency <- NA_character_

# Single-constituency districts
comm_centroids$constituency[comm_centroids$district == "Soufriere"  & !is.na(comm_centroids$district)] <- "Soufriere"
comm_centroids$constituency[comm_centroids$district == "Choiseul"   & !is.na(comm_centroids$district)] <- "Choiseul"
comm_centroids$constituency[comm_centroids$district == "Laborie"    & !is.na(comm_centroids$district)] <- "Laborie"
comm_centroids$constituency[comm_centroids$district == "Anse-la-Raye" & !is.na(comm_centroids$district)] <- "Anse-la-Raye/Canaries"
comm_centroids$constituency[comm_centroids$district == "Canaries"   & !is.na(comm_centroids$district)] <- "Anse-la-Raye/Canaries"

# Castries: all assigned average Castries TPI label
comm_centroids$constituency[comm_centroids$district == "Castries" & !is.na(comm_centroids$district)] <- "Castries (avg)"

# N/S splits
comm_centroids <- split_lat(comm_centroids, "Vieux Fort", "Vieux-Fort North", "Vieux-Fort South")
comm_centroids <- split_lat(comm_centroids, "Micoud",     "Micoud North",     "Micoud South")
comm_centroids <- split_lat(comm_centroids, "Dennery",    "Dennery North",    "Dennery South")

# Gros Islet: coastal (west/north) = Gros Islet, inland (east/south) = Babonneau
# Babonneau is southeast of the Gros Islet district; split by lon
gros_lons <- comm_centroids$clon[comm_centroids$district == "Gros Islet" & !is.na(comm_centroids$district)]
gros_med_lon <- median(gros_lons)
comm_centroids$constituency[comm_centroids$district == "Gros Islet" & !is.na(comm_centroids$district) &
                              comm_centroids$clon <= gros_med_lon] <- "Gros Islet"
comm_centroids$constituency[comm_centroids$district == "Gros Islet" & !is.na(comm_centroids$district) &
                              comm_centroids$clon >  gros_med_lon] <- "Babonneau"

cat("\nConstituency assignment:\n")
print(table(comm_centroids$constituency, useNA = "ifany"))

# ---- 7. Build TPI lookup including Castries average ----
tpi_lookup <- bind_rows(
  tpi_const |> select(constituency, tpi),
  tibble(constituency = "Castries (avg)", tpi = castries_avg_tpi)
)

# ---- 8. Join TPI to community polygons ----
comm_tpi <- comm_centroids |>
  left_join(tpi_lookup, by = "constituency") |>
  select(community, constituency, tpi)

poly_plot <- comm_df |>
  left_join(comm_tpi, by = "community") |>
  filter(!is.na(tpi))

cat("\nPolygon rows for plotting:", nrow(poly_plot), "\n")

# ---- 9. Labels at constituency centroids ----
# For Castries, label with individual constituency names in text
const_labels <- comm_centroids |>
  filter(!is.na(constituency)) |>
  left_join(tpi_lookup, by = "constituency") |>
  group_by(constituency, tpi) |>
  summarise(lon = mean(clon), lat = mean(clat), .groups = "drop") |>
  mutate(label = paste0(
    sub("Castries \\(avg\\)", "Castries*", constituency),
    "\n", percent(tpi, accuracy = 0.1)
  ))

# ---- 10. Plot ----
p <- ggplot(poly_plot,
            aes(x = lon, y = lat, group = interaction(community, piece))) +
  geom_polygon(aes(fill = tpi), colour = NA) +
  # District outlines for reference
  geom_path(
    data = do.call(rbind, lapply(dist_gj$features, function(f) {
      nm <- name_fix(f$properties$NAME_1)
      geom <- f$geometry
      coords_list <- if (geom$type == "Polygon") list(geom$coordinates) else geom$coordinates
      rows <- lapply(seq_along(coords_list), function(pi) {
        ring <- coords_list[[pi]][[1]]
        pts <- do.call(rbind, lapply(ring, function(p) c(p[[1]], p[[2]])))
        data.frame(district = nm, piece = pi, lon = pts[, 1], lat = pts[, 2])
      })
      do.call(rbind, rows)
    })),
    aes(x = lon, y = lat, group = interaction(district, piece)),
    inherit.aes = FALSE,
    colour = "white", linewidth = 0.5
  ) +
  ggrepel::geom_label_repel(
    data          = const_labels,
    aes(x = lon, y = lat, label = label),
    inherit.aes   = FALSE,
    size          = 1.9,
    label.size    = 0.12,
    label.padding = unit(0.10, "lines"),
    min.segment.length = 0.2,
    box.padding   = 0.3,
    fill          = "white", alpha = 0.88,
    lineheight    = 0.82
  ) +
  scale_fill_gradientn(
    colours = c("#FFFDE7", "#FFC107", "#FF5722", "#B71C1C"),
    name    = "TPI Share",
    labels  = percent_format(accuracy = 0.1),
    limits  = c(0, max(tpi_lookup$tpi, na.rm = TRUE))
  ) +
  coord_fixed(ratio = 1.2) +
  labs(
    title    = "Tourism Proximity Index (TPI) by Constituency — Saint Lucia",
    subtitle = "Community polygons shaded by constituency TPI share",
    caption  = paste0(
      "Source: OpenStreetMap (May 2026); 2010 census HH-weighted TPI (CSO); ",
      "geoBoundaries ADM2 community polygons (CC BY 4.0).\n",
      "Constituency assignment by spatial overlay with GADM district boundaries; ",
      "N/S splits by median latitude. *Castries shown as district average (5 constituencies, all Low tier)."
    )
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 8.5, color = "grey40", hjust = 0.5),
    plot.caption  = element_text(size = 6, color = "grey50", hjust = 0.5),
    legend.position   = "right",
    legend.key.height = unit(1.0, "cm"),
    legend.title      = element_text(size = 8),
    plot.margin   = margin(10, 10, 10, 10)
  )

ggsave("TPI_Map_Saint_Lucia_constituency_v2.png",
       plot = p, width = 7, height = 9, dpi = 300, bg = "white")
cat("Saved: TPI_Map_Saint_Lucia_constituency_v2.png\n")
