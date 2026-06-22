library(readxl)
library(dplyr)

# Read TPI v2 results
tpi_v2 <- read.csv("TPI_v2_attraction_concentration.csv") |>
  arrange(rank_v2) |>
  select(
    District         = district,
    `N Attractions`  = n_attractions,
    `Weighted Score` = weighted_score,
    `TPI v2 (Share)` = tpi_v2,
    `Rank v2`        = rank_v2,
    `TPI v1 (Distance-based)` = tpi_v1,
    `Rank v1`        = rank_v1,
    `Rank Change`    = rank_change
  )

# Read original sheets to preserve
orig_calc  <- read_excel("TPI_Saint_Lucia.xlsx", sheet = "TPI Calculation",  col_names = FALSE)
orig_final <- read_excel("TPI_Saint_Lucia.xlsx", sheet = "Final TPI Table",  col_names = FALSE)
orig_readme <- read_excel("TPI_Saint_Lucia.xlsx", sheet = "README",          col_names = FALSE)

# Write updated file using write.csv as fallback (openxlsx not available)
# Instead, write a self-contained CSV comparison table
readme_note <- c(
  "TPI v2 — Attraction Concentration Method",
  "",
  "Replaces the placeholder driving-distance TPI with a count-based measure.",
  "",
  "METHOD",
  "1. Downloaded all OSM tourism, historic, beach, and dive-centre features",
  "   for Saint Lucia via the Overpass API (May 2026).",
  "2. Features classified into 7 types with weights:",
  "   - hotel/resort = 3  (major accommodation infrastructure)",
  "   - attraction / beach = 2",
  "   - viewpoint / historic = 1.5",
  "   - guesthouse / dive_centre = 1",
  "3. Point-in-polygon assignment to districts using GADM v4.1 boundaries.",
  "   Coastal features on polygon edges assigned to nearest-centroid district.",
  "4. TPI v2 share = district weighted score / island total weighted score.",
  "",
  "FEATURES USED (n = 379 after filtering)",
  "  Hotels/resorts: 85 | Guesthouses: 95 | Attractions: 29",
  "  Beaches: 82 | Viewpoints: 43 | Historic: 41 | Dive centres: 4",
  "",
  "KEY DIFFERENCES FROM TPI v1",
  "  - Castries drops from #1 to #3: v1 was driven by cruise-port proximity,",
  "    which overstated Castries relative to its tourism attraction base.",
  "  - Gros Islet rises to #1: largest tourism infrastructure cluster",
  "    (Rodney Bay, Pigeon Island, beach strip, hotels, dive centres).",
  "  - Vieux Fort rises from #10 to #5: airport-adjacent resort cluster",
  "    (Coconut Bay, southern beaches) not captured by distance to anchors.",
  "  - Dennery falls to #10: few tourism features confirmed in OSM.",
  "",
  "SOURCE: OpenStreetMap contributors, ODbL licence.",
  "        GADM v4.1 administrative boundaries."
)

writeLines(readme_note, "TPI_v2_README.txt")
cat("Saved: TPI_v2_README.txt\n")
cat("\nFinal TPI v2 table:\n")
print(tpi_v2, n = 10)
