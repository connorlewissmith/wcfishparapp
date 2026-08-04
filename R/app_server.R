#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny shinyalert
#' @import leaflet dplyr RColorBrewer stringr
#' @noRd
app_server <- function(input, output, session) {
  # Opening alert
  shinyalert::shinyalert("Welcome!",
                         "If you're new here, we recommend starting with the 'About' tab to get an overview.",
                         type = "info")

  # Load data
  data_path <- system.file("data", "fish_par_app_data.RData", package = "wcfishparapp")
  load(data_path)

  # Your application server logic
  mod_map_server("map")
}
