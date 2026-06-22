q <- '[out:json][timeout:60];area["ISO3166-1"="LC"]->.c;(rel(area.c)["boundary"="administrative"]["admin_level"="6"];rel(area.c)["boundary"="administrative"]["admin_level"="7"];rel(area.c)["boundary"="administrative"]["admin_level"="8"];);out tags;'
url <- paste0("https://overpass-api.de/api/interpreter?data=", URLencode(q, reserved=TRUE))
cat("Downloading...\n")
tryCatch(
  download.file(url, "osm_admin_test.json", method="wininet", quiet=FALSE),
  error = function(e) cat("Error:", conditionMessage(e), "\n")
)
if (file.exists("osm_admin_test.json")) {
  library(jsonlite)
  j <- fromJSON("osm_admin_test.json")
  elems <- j[["elements"]]
  cat("Elements:", nrow(elems), "\n")
  if (!is.null(elems) && nrow(elems) > 0) print(elems[["tags"]])
}
