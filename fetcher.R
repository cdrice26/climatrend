# Copyright (C) 2026 Caleb Rice

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
    stop("Package 'httr' is required for weather fetching.
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

  daily <- data$daily
  if (is.null(daily) || length(daily) == 0) {
    stop("No daily data returned from the weather API.")
  }

  time_values <- if (!is.null(daily$time)) {
    as.Date(as.character(
      unlist(daily$time, recursive = TRUE, use.names = FALSE)
    ))
  } else {
    as.Date(character())
  }
  n <- length(time_values)

  normalize_field <- function(field) {
    value <- daily[[field]]
    if (is.null(value)) {
      return(rep(NA, n))
    }
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
    if (length(value) == 0) {
      return(rep(NA, n))
    }
    if (length(value) != n) {
      value <- rep(value, length.out = n)
    }
    value
  }

  df <- data.frame(
    date = time_values,
    location = rep(if (!is.null(loc$name)) loc$name else NA_character_, n),
    temperature = normalize_field("temperature_2m_mean"),
    apparent_temperature = normalize_field("apparent_temperature_mean"),
    precipitation = normalize_field("precipitation_sum"),
    snowfall = normalize_field("snowfall_sum"),
    windspeed = normalize_field("windspeed_10m_max"),
    stringsAsFactors = FALSE
  )
  df
}
