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

make_plots <- function(long) {
  ggplot(
    data = long,
    mapping = aes(x = date, y = value, color = forecasted)
  ) +
    geom_line() +
    facet_grid(location ~ variable, scales = "free_y")
}
