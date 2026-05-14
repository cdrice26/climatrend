library(shiny)
source("locations.R")
source("geocoder.R")

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
  verbatimTextOutput("result")
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
      Sys.sleep(1)
      geocode(loc)
    })
    print(geocoded)
    output$result <- renderPrint(
      paste(
        unlist(geocoded),
        unlist(input$year_range),
        unlist(input$data_type),
        sep = " "
      )
    )
  })
}

shinyApp(ui, server)
