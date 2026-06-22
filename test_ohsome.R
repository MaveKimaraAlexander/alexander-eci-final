library(jsonlite)

# Ohsome API: count tourism POIs in Saint Lucia as of 2010-01-01
# This gives us the pre-Airbnb (pre-2014) attraction stock
body <- list(
  bboxes    = "-61.08,13.70,-60.87,14.12",   # St. Lucia bounding box
  filter    = "tourism=* and type:node",
  time      = "2010-01-01"
)

url <- "https://api.ohsome.org/v1/elements/count"

tmp <- tempfile(fileext = ".json")
tryCatch({
  download.file(
    url  = paste0(url, "?bboxes=", body$bboxes,
                  "&filter=", URLencode(body$filter, reserved = TRUE),
                  "&time=", body$time),
    destfile = tmp, method = "wininet", quiet = TRUE
  )
  result <- fromJSON(tmp)
  cat("2010 tourism node count:", result$result$value, "\n")
}, error = function(e) cat("Error:", e$message, "\n"))
