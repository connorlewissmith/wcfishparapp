#' map_2023 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_2023_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "outer",
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      leafletOutput(ns("map2023"), width = "100%", height = "100%")
    )
  )
}

#' map_2023 Server Functions
#'
#' @noRd
mod_map_2023_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Create base map
    county_imap_2023 <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% ### base group
      setView(lng = -130.252667, lat = 40.7850, zoom = 5)

    output$map2023 <- leaflet::renderLeaflet({
      county_imap_2023
    })

  })
}

## To be copied in the UI
# mod_map_2023_ui("map_2023_1")

## To be copied in the server
# mod_map_2023_server("map_2023_1")
