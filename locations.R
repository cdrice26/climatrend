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

library(shiny)
library(bslib)

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

update_locations <- function(vals, input) {
  lapply(vals$ids, function(id) {
    observeEvent(input[[id]],
      {
        vals$data[[id]] <- input[[id]]
      },
      ignoreInit = TRUE
    )
  })
}

remove_locations <- function(vals, input) {
  lapply(vals$ids, function(id) {
    observeEvent(input[[paste0("rm_", id)]],
      {
        removeUI(selector = paste0("#row_", id))
        vals$ids <- vals$ids[vals$ids != id]
        vals$data[[id]] <- NULL
      },
      ignoreInit = TRUE
    )
  })
}
