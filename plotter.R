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

library(ggplot2)
library(dplyr)

prepare_df <- function(weather_results) {
  long <- weather_results |>
    select(where(~ !all(is.na(.x)))) |>
    tidyr::pivot_longer(
      cols = -c(date, location),
      names_to = "variable",
      values_to = "value"
    )

  as.data.frame(long)
}

metric_labeller <- function(variable) {
  lookup <- c(
    "temperature" = "Average Daily Temperature (°C)",
    "apparent_temperature" = "Average Daily Apparent Temperature (°C)",
    "precipitation" = "Daily Precipitation (mm)",
    "snowfall" = "Daily Snowfall (cm)",
    "windspeed" = "Daily Max Wind Speed (km/h)"
  )
  lookup[variable]
}

make_plots <- function(long) {
  ggplot(
    data = long,
    mapping = aes(x = date, y = value, color = forecasted)
  ) +
    geom_line() +
    facet_grid(
      location ~ variable,
      scales = "free_y",
      labeller = labeller(variable = metric_labeller)
    ) +
    scale_color_manual(
      values = c("FALSE" = "#1f77b4", "TRUE" = "#ff7f0e"),
      breaks = c(FALSE, TRUE),
      labels = c("Historical", "Forecasted"),
      guide = guide_legend(title = NULL)
    ) +
    labs(x = "Date", y = NULL)
}
