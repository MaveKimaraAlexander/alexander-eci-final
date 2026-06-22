library(jsonlite)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(scales)

# ---- 1. District TPI (for polygon fill — preserves Update 3 color pattern) ----
tpi_dist <- read.csv("TPI_v2_attraction_concentration.csv") |>
  select(district, dist_tpi = tpi_v2)

# ---- 2. Constituency TPI (for labels) ----
tpi_const <- read.csv("TPI_constituency.csv") |>
  select(constituency, tpi) |>
  arrange(desc(tpi))

# Abbreviated display names to keep labels compact
abbrev <- c(
  "Gros Islet"            = "Gros Islet",
  "Babonneau"             = "Babonneau",
  "Castries North"        = "Cs-North",
  "Castries East"         = "Cs-East",
  "Castries Central"      = "Cs-Central",
  "Castries South"        = "Cs-South",
  "Castries South-East"   = "Cs-S.East",
  "Anse-la-Raye/Canaries" = "Anse/Can.",
  "Soufriere"             = "Soufriere",
  "Choiseul"              = "Choiseul",
  "Laborie"               = "Laborie",
  "Vieux-Fort South"      = "VF-South",
  "Vieux-Fort North"      = "VF-North",
  "Micoud South"          = "Mc-South",
  "Micoud North"          = "Mc-North",
  "Dennery South"         = "Dn-South",
  "Dennery North"         = "Dn-North"
)

# Constituency → GeoJSON district name
# (Anse-la-Raye/Canaries spans both; Babonneau spans Castries+Gros Islet)
const_geo <- c(
  "Gros Islet"            = "Gros Islet",
  "Babonneau"             = "Gros Islet",
  "Castries North"        = "Castries",
  "Castries East"         = "Castries",
  "Castries Central"      = "Castries",
  "Castries South"        = "Castries",
  "Castries South-East"   = "Castries",
  "Anse-la-Raye/Canaries" = "Anse-la-Raye",
  "Soufriere"             = "Soufriere",
  "Choiseul"              = "Choiseul",
  "Laborie"               = "Laborie",
  "Vieux-Fort South"      = "Vieux Fort",
  "Vieux-Fort North"      = "Vieux Fort",
  "Micoud South"          = "Micoud",
  "Micoud North"          = "Micoud",
  "Dennery South"         = "Dennery",
  "Dennery North"         = "Dennery"
)

# Build per-polygon constituency label text
label_by_geo <- tpi_const |>
  mutate(
    geo_district = const_geo[constituency],
    line         = paste0(abbrev[constituency], ": ", percent(tpi, accuracy = 0.1))
  ) |>
  group_by(geo_district) |>
  summarise(label_text = paste(line, collapse = "\n"), .groups = "drop")

# Canaries district shares the Anse-la-Raye/Canaries constituency label
canaries_label <- label_by_geo |>
  filter(geo_district == "Anse-la-Raye") |>
  mutate(geo_district = "Canaries")
label_by_geo <- bind_rows(label_by_geo, canaries_label)

# ---- 3. Parse GeoJSON ----
gj <- fromJSON("lca_districts.geojson", simplifyVector = FALSE)

name_fix <- function(x) case_when(
  x == "GrosIslet"                       ~ "Gros Islet",
  x == "VieuxFort"                       ~ "Vieux Fort",
  grepl("Soufri", x, ignore.case = TRUE) ~ "Soufriere",
  TRUE ~ x
)

extract_polys <- function(feat) {
  nm   <- name_fix(feat$properties$NAME_1)
  geom <- feat$geometry
  coords_list <- if (geom$type == "Polygon") list(geom$coordinates) else geom$coordinates
  rows <- lapply(seq_along(coords_list), function(pi) {
    ring <- coords_list[[pi]][[1]]
    pts  <- do.call(rbind, lapply(ring, function(p) c(lon = p[[1]], lat = p[[2]])))
    data.frame(geo_district = nm, piece = pi,
               lon = pts[, "lon"], lat = pts[, "lat"],
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

poly_df <- do.call(rbind, lapply(gj$features, extract_polys)) |>
  left_join(tpi_dist |> rename(geo_district = district), by = "geo_district") |>
  left_join(label_by_geo, by = "geo_district")

# ---- 4. Centroids for labels ----
centroids <- poly_df |>
  group_by(geo_district) |>
  summarise(lon = mean(lon), lat = mean(lat), .groups = "drop") |>
  left_join(tpi_dist |> rename(geo_district = district), by = "geo_district") |>
  left_join(label_by_geo, by = "geo_district")

# ---- 5. Plot ----
p <- ggplot(poly_df,
            aes(x = lon, y = lat, group = interaction(geo_district, piece))) +
  geom_polygon(aes(fill = dist_tpi), colour = "white", linewidth = 0.45) +
  geom_label_repel(
    data          = centroids,
    aes(x = lon, y = lat, label = label_text),
    inherit.aes   = FALSE,
    size          = 2.0,
    label.size    = 0.15,
    label.padding = unit(0.12, "lines"),
    min.segment.length = 0.25,
    box.padding   = 0.35,
    fill          = "white",
    alpha         = 0.88,
    lineheight    = 0.85
  ) +
  scale_fill_gradientn(
    colours = c("#FFFDE7", "#FFC107", "#FF5722", "#B71C1C"),
    name    = "District\nTPI Share",
    labels  = percent_format(accuracy = 0.1),
    limits  = c(0, max(tpi_dist$dist_tpi, na.rm = TRUE))
  ) +
  coord_fixed(ratio = 1.2) +
  labs(
    title    = "Tourism Proximity Index (TPI) by Constituency — Saint Lucia",
    subtitle = "Labels: constituency shares (HH-weighted). Fill: district attraction share.",
    caption  = paste0(
      "Source: OpenStreetMap (May 2026); GADM v4.1 district polygons.\n",
      "Fill = district-level TPI (379 OSM features). Labels = constituency TPI\n",
      "via 2010 census HH-weighted disaggregation (CSO). N = 17 constituencies."
    )
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 8.5, color = "grey40", hjust = 0.5),
    plot.caption  = element_text(size = 6.5, color = "grey50", hjust = 0.5),
    legend.position   = "right",
    legend.key.height = unit(1.0, "cm"),
    legend.title      = element_text(size = 8),
    plot.margin   = margin(10, 10, 10, 10)
  )

ggsave("TPI_Map_Saint_Lucia_constituency.png",
       plot = p, width = 7, height = 9, dpi = 300, bg = "white")
cat("Saved: TPI_Map_Saint_Lucia_constituency.png\n")
