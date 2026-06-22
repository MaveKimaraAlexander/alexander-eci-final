library(jsonlite)
library(ggplot2)
library(ggrepel)
library(dplyr)

# ---- TPI v2 data ----
tpi <- read.csv("TPI_v2_attraction_concentration.csv") |>
  select(district, tpi_v2, n_attractions, weighted_score) |>
  rename(tpi = tpi_v2)

# ---- Parse GeoJSON ----
gj <- fromJSON("lca_districts.geojson", simplifyVector = FALSE)

name_fix <- function(x) case_when(
  x == "GrosIslet"  ~ "Gros Islet",
  x == "VieuxFort"  ~ "Vieux Fort",
  x == "Soufrière"  ~ "Soufriere",
  TRUE ~ x
)

extract_polys <- function(feat) {
  nm   <- name_fix(feat$properties$NAME_1)
  geom <- feat$geometry
  coords_list <- if (geom$type == "Polygon") list(geom$coordinates) else geom$coordinates
  rows <- lapply(seq_along(coords_list), function(pi) {
    ring <- coords_list[[pi]][[1]]
    pts  <- do.call(rbind, lapply(ring, function(p) c(lon = p[[1]], lat = p[[2]])))
    data.frame(district = nm, piece = pi, lon = pts[, "lon"], lat = pts[, "lat"])
  })
  do.call(rbind, rows)
}

poly_df <- do.call(rbind, lapply(gj$features, extract_polys))

# ---- Centroids ----
centroids <- poly_df |>
  group_by(district) |>
  summarise(lon = mean(lon), lat = mean(lat), .groups = "drop") |>
  left_join(tpi, by = "district") |>
  mutate(label = paste0(district, "\n", scales::percent(tpi, accuracy = 0.1)))

poly_df <- left_join(poly_df, tpi, by = "district")

# ---- Plot ----
p <- ggplot(poly_df, aes(x = lon, y = lat, group = interaction(district, piece))) +
  geom_polygon(aes(fill = tpi), colour = "white", linewidth = 0.4) +
  geom_label_repel(
    data        = centroids,
    aes(x = lon, y = lat, label = label),
    inherit.aes = FALSE,
    size        = 2.6,
    label.size  = 0.2,
    label.padding = unit(0.15, "lines"),
    min.segment.length = 0.3,
    box.padding = 0.4,
    fill        = "white",
    alpha       = 0.85
  ) +
  scale_fill_gradientn(
    colours = c("#FFFDE7", "#FFC107", "#FF5722", "#B71C1C"),
    name    = "TPI Share",
    labels  = scales::percent_format(accuracy = 0.1),
    limits  = c(0, max(tpi$tpi))
  ) +
  coord_fixed(ratio = 1.2) +
  labs(
    title    = "Tourism Proximity Index (TPI) by District — Saint Lucia",
    subtitle = "District share of tourism attraction stock (OSM-based, weighted count)",
    caption  = "Source: OpenStreetMap contributors (Overpass API, May 2026); GADM v4.1.\nWeights: hotel=3, attraction/beach=2, viewpoint/historic=1.5, guesthouse/dive=1.\nN = 379 classified features."
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 9, color = "grey40", hjust = 0.5),
    plot.caption  = element_text(size = 7, color = "grey50", hjust = 0.5),
    legend.position   = "right",
    legend.key.height = unit(1.2, "cm"),
    plot.margin   = margin(10, 10, 10, 10)
  )

ggsave("TPI_Map_Saint_Lucia_v2.png", plot = p, width = 7, height = 9, dpi = 300, bg = "white")
cat("Saved: TPI_Map_Saint_Lucia_v2.png\n")
