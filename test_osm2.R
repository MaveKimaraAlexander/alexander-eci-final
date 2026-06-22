q <- '[out:json][timeout:60];area["ISO3166-1"="LC"]->.c;rel(area.c)["boundary"="administrative"];out tags;'
url <- paste0("https://overpass-api.de/api/interpreter?data=", URLencode(q, reserved=TRUE))
download.file(url, "osm_admin_test2.json", method="wininet", quiet=TRUE)
library(jsonlite)
j <- fromJSON("osm_admin_test2.json")
elems <- j[["elements"]]
cat("Total:", length(elems), "\n")
for (x in elems) cat("level:", x[["tags"]][["admin_level"]], " name:", x[["tags"]][["name"]], "\n")
