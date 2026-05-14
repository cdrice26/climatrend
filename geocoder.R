geocode <- function(loc) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for geocoding.
     Install it with install.packages('httr').")
  }

  url <- "https://nominatim.openstreetmap.org/search"
  ua <- httr::user_agent("climatrend (0.1.0)")
  res <- httr::GET(url, query = list(q = loc, format = "json"), ua)
  httr::stop_for_status(res)

  data <- httr::content(res, as = "parsed", type = "application/json")
  if (length(data) == 0) {
    return(NULL)
  }

  list(
    lat = as.numeric(data[[1]]$lat),
    lon = as.numeric(data[[1]]$lon),
    name = data[[1]]$display_name
  )
}
