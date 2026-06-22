q <- '[out:json][timeout:60];area["ISO3166-1"="LC"]->.sl;(node["tourism"](area.sl);way["tourism"](area.sl););out count;'
url <- paste0("https://overpass-api.de/api/interpreter?data=", URLencode(q, reserved = TRUE))
tryCatch(
  download.file(url, "osm_test.json", method = "wininet", quiet = TRUE),
  error = function(e) cat("Error:", e$message, "\n")
)
cat(readLines("osm_test.json"), sep = "\n")
