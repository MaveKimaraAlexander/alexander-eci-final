df <- read.csv("osm_attractions_classified.csv")
unassigned <- df[is.na(df$district), ]
cat("Unassigned count:", nrow(unassigned), "\n")
cat("Lon range:", range(unassigned$lon), "\n")
cat("Lat range:", range(unassigned$lat), "\n")
print(unassigned[, c("attr_type", "weight", "name", "lon", "lat")], row.names = FALSE)
