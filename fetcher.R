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

fetch_weather <- function(loc, year_range, data_type) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for geocoding.
     Install it with install.packages('httr').")
  }

  url <- "https://archive-api.open-meteo.com/v1/archive"
  ua <- httr::user_agent("climatrend (0.1.0)")
  res <- httr::GET(url, query = list(
    latitude = loc$lat,
    longitude = loc$lon,
    start_date = paste0(year_range[1], "-01-01"),
    end_date = paste0(year_range[2], "-12-31"),
    daily = switch(data_type,
      "Temperature" = "temperature_2m_mean,apparent_temperature_mean",
      "Precipitation" = "precipitation_sum,snowfall_sum",
      "Wind Speed" = "windspeed_10m_max"
    ),
    timezone = "auto"
  ), ua)
  httr::stop_for_status(res)
  data <- httr::content(res, as = "parsed", type = "application/json")
  data$daily
}
