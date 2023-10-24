#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import leaflet dplyr RColorBrewer stringr
#' @noRd
app_server <- function(input, output, session) {

  # Load data
  data_path <- system.file("data", "fish_par_app_data.RData", package = "wcfishparapp")
  load(data_path)

  # Your application server logic
  mod_map_2017_server("name_of_module1_1")
  mod_map_2020_server("name_of_module2_1")
  mod_map_2023_server("map_2023_1")
}
