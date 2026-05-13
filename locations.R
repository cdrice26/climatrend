make_id <- function() paste0("loc_", sample.int(1e9, 1))

add_location <- function(vals) {
  new_id <- make_id()
  vals$ids <- c(vals$ids, new_id)
  vals$data[[new_id]] <- ""

  insertUI(
    selector = "#location_container",
    where = "beforeEnd",
    ui = fluidRow(
      id = paste0("row_", new_id),
      column(10, textInput(new_id, "Location", value = "")),
      column(2, actionButton(paste0("rm_", new_id), "Delete"))
    )
  )
}

update_locations <- function(vals, input){
  lapply(vals$ids, function(id) {
      observeEvent(input[[id]], {
        vals$data[[id]] <- input[[id]]
      }, ignoreInit = TRUE)
    })
}

remove_locations <- function(vals, input) {
  lapply(vals$ids, function(id) {
      observeEvent(input[[paste0("rm_", id)]], {
        removeUI(selector = paste0("#row_", id))
        vals$ids <- vals$ids[vals$ids != id]
        vals$data[[id]] <- NULL
      }, ignoreInit = TRUE)
    })
}