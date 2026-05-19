library(shiny)
library(dplyr)
source("locations.R")
source("fetcher.R")
source("analyzer.R")
source("plotter.R")

ui <- fluidPage(
  titlePanel("Climatrend"),
  tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
  tags$script(src = "app.js"),
  div(tags$strong("Locations:")),
  div(id = "location_container"),
  actionButton("add_btn", "Add Another Location"),
  tags$hr(),
  sliderInput(
    "year_range",
    "Time Range:",
    min = 1950,
    max = as.integer(format(Sys.Date(), "%Y")) - 1,
    value = c(2000, 2020), step = 1,
    sep = ""
  ),
  tags$hr(),
  selectInput(
    "data_type",
    "Data Type:",
    choices = c("Temperature", "Precipitation", "Wind Speed")
  ),
  tags$hr(),
  sliderInput(
    "forecast_horizon", "Forecast Horizon (years):",
    min = 1, max = 100, value = 10, step = 1
  ),
  checkboxInput(
    "include_periodic", "Include Periodic Component in Forecast",
    value = FALSE
  ),
  tags$hr(),
  actionButton("submit", "Submit"),
  tags$hr(),
  plotOutput("plot"),
  tags$p(
    HTML(
      'Geocoding via <a
      href="https://nominatim.openstreetmap.org/"
      target="_blank" rel="noopener noreferrer">
      OpenStreetMap Nominatim API</a>. OpenStreetMap data
      is licensed under the <a href="https://www.openstreetmap.org/copyright"
       target="_blank" rel="noopener noreferrer">
       Open Database License</a>.
     Weather data via <a href="https://open-meteo.com/"
      target="_blank" rel="noopener noreferrer">
      Open-Meteo API</a>, which uses climate data
      from the <a href="https://climate.copernicus.eu/"
       target="_blank" rel="noopener noreferrer">
       Copernicus Climate Change Service</a>.'
    )
  )
)

server <- function(input, output, session) {
  vals <- reactiveValues(
    ids = character(),
    data = list()
  )

  # Add a new input
  observeEvent(input$add_btn, {
    add_location(vals)
  })

  # Track changes to each input
  observe({
    update_locations(vals, input)
  })

  # Remove an input
  observe({
    remove_locations(vals, input)
  })

  # Submit handler
  observeEvent(input$submit, {
    if (length(vals$data) == 0) {
      showNotification("Please add at least one location.", type = "error")
      return()
    }
    geocoded <- lapply(vals$data, \(loc) {
      Sys.sleep(1) # Don't send more than 1 request per second
      geocode(loc)
    })
    weather_results <- do.call(rbind, lapply(geocoded, \(loc) {
      if (!is.null(loc)) {
        fetch_weather(loc, input$year_range, input$data_type)
      } else {
        NULL
      }
    }) |> Filter(f = Negate(is.null), x = _))
    if (is.null(weather_results) || nrow(weather_results) == 0) {
      showNotification(
        "No weather data found for the provided locations and time range.",
        type = "error"
      )
      return()
    }
    long <- prepare_df(weather_results)
    forecasts <- long |>
      group_by(location, variable) |>
      arrange(date) |>
      group_modify(
        ~ farima(
          .x,
          h = input$forecast_horizon * 365,
          include_periodic = input$include_periodic
        )
      ) |>
      ungroup()
    output$plot <- renderPlot(
      make_plots(forecasts)
    )
  })
}

shinyApp(ui, server)
