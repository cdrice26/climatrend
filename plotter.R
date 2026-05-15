library(ggplot2)
library(dplyr)

prepare_df <- function(weather_results, seasonal_diff = FALSE) {
  long <- weather_results |>
    select(where(~ !all(is.na(.x)))) |>
    tidyr::pivot_longer(
      cols = -c(date, location),
      names_to = "variable",
      values_to = "value"
    )

  if (seasonal_diff) {
    long <- long |>
      group_by(location, variable) |>
      arrange(date) |>
      mutate(value = c(rep(NA_real_, 365), diff(value, lag = 365))) |>
      ungroup() |>
      filter(!is.na(value))
  }

  long
}

make_plots <- function(long) {
  ggplot(
    data = long,
    mapping = aes(x = date, y = value, color = variable)
  ) +
    geom_line() +
    facet_grid(location ~ variable, scales = "free_y")
}
