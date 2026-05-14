library(shiny)
library(ggplot2)
source("locations.R")
source("fetcher.R")
source("analyzer.R")

ui <- fluidPage(
  titlePanel("Climatrend"),
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
  actionButton("submit", "Submit"),
  tags$hr(),
  plotOutput("plot")
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
    }))
    weather_results <- weather_results |>
      dplyr::select(dplyr::where(~ !all(is.na(.x))))
    long <- tidyr::pivot_longer(
      weather_results,
      cols = -c(date, location),
      names_to = "variable",
      values_to = "value"
    )
    output$plot <- renderPlot(
      ggplot(
        data = long,
        mapping = aes(x = date, y = value, color = variable)
      ) +
        geom_line() +
        facet_grid(location ~ variable, scales = "free_y") +
        theme_minimal()
    )
  })
}

shinyApp(ui, server)
