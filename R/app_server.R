#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny shinyalert
#' @import leaflet dplyr RColorBrewer stringr
#' @noRd
app_server <- function(input, output, session) {
  # Opening alert
  shinyalert::shinyalert("Welcome to the West Coast Fisheries Participation Surveys App!",
                         "If you're new here, we recommend starting with the 'About' tab to get an overview of our aims and objectives. Next, head over to the 'Survey Documents' tab to get acquainted with how our surveys have evolved over the years.\n\nIn the different year survey tabs, you can explore responses at the county level for different years. While the questions may vary across years, some remain consistent. To make the most out of your experience, we suggest selecting the questions that interest you and toggling between the tabs to compare results.",
                         type = "info")

  # Load data
  data_path <- system.file("data", "fish_par_app_data.RData", package = "wcfishparapp")
  load(data_path)

  # Your application server logic
  mod_map_2017_server("name_of_module1_1")
  mod_map_2020_server("name_of_module2_1")
  mod_map_2023_server("map_2023_1")
}
