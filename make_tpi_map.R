library(jsonlite)
library(ggplot2)
library(ggrepel)
library(readxl)
library(dplyr)

# ---- TPI data ----
tpi <- read_excel("TPI_Saint_Lucia.xlsx", skip = 57) |>
  rename_with(tolower) |>
  rename(tpi = `tpi score`) |>
  select(district, tpi) |>
  mutate(
    district = case_when(
      district == "Anse la Raye" ~ "Anse-la-Raye",
      district == "Soufrière"   ~ "Soufriere",
      district == "Vieux Fort"  ~ "Vieux Fort",
      TRUE ~ district
    ),
    tpi = as.numeric(tpi)
  )

# ---- Parse GeoJSON manually ----
gj <- fromJSON("lca_districts.geojson", simplifyVector = FALSE)

extract_polys <- function(feat) {
  props <- feat$properties
  name_raw <- props$NAME_1
  geom <- feat$geometry

  # normalise district name to match TPI table
  name_clean <- case_when(
    name_raw == "GrosIslet"   ~ "Gros Islet",
    name_raw == "VieuxFort"   ~ "Vieux Fort",
    name_raw == "Soufrière"   ~ "Soufriere",
    TRUE ~ name_raw
  )

  coords_list <- if (geom$type == "Polygon") {
    list(geom$coordinates)
  } else {
    geom$coordinates   # MultiPolygon: list of polygons
  }

  rows <- lapply(seq_along(coords_list), function(pi) {
    ring <- coords_list[[pi]][[1]]   # outer ring only
    pts  <- do.call(rbind, lapply(ring, function(p) c(lon = p[[1]], lat = p[[2]])))
    data.frame(
      district = name_clean,
      piece    = pi,
      lon      = pts[, "lon"],
      lat      = pts[, "lat"]
    )
  })
  do.call(rbind, rows)
}

poly_df <- do.call(rbind, lapply(gj$features, extract_polys))

# ---- Centroids for labels ----
centroids <- poly_df |>
  group_by(district) |>
  summarise(lon = mean(lon), lat = mean(lat), .groups = "drop")

# ---- Join TPI ----
poly_df   <- left_join(poly_df,   tpi, by = "district")
centroids <- left_join(centroids, tpi, by = "district") |>
  mutate(label = paste0(district, "\n", scales::percent(tpi, accuracy = 0.1)))

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
    colours  = c("#FFFDE7", "#FFC107", "#FF5722", "#B71C1C"),
    name     = "TPI Share",
    labels   = scales::percent_format(accuracy = 0.1),
    limits   = c(0, max(tpi$tpi))
  ) +
  coord_fixed(ratio = 1.2) +
  labs(
    title    = "Tourism Proximity Index (TPI) by District — Saint Lucia",
    subtitle = "District share of national hotel-room stock, 2010 (Bartik IV share)",
    caption  = "Source: Saint Lucia Tourism Authority Statistical Digest 2010.\nTPI = district hotel rooms ÷ island total hotel rooms."
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 9, color = "grey40", hjust = 0.5),
    plot.caption  = element_text(size = 7, color = "grey50", hjust = 0.5),
    legend.position  = "right",
    legend.key.height = unit(1.2, "cm"),
    plot.margin   = margin(10, 10, 10, 10)
  )

ggsave("TPI_Map_Saint_Lucia.png", plot = p, width = 7, height = 9, dpi = 300, bg = "white")
cat("Saved: TPI_Map_Saint_Lucia.png\n")
