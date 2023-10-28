#' survey_docs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_survey_docs_ui <- function(id) {
  ns <- NS(id)

  # Function to create a tabPanel with zoom buttons
  createTabPanelWithZoom <- function(title, img_src, height = 900, width = 700) {
    tabPanel(title, id = "dummy",
             actionButton(ns("zoom_in"), "+"),
             actionButton(ns("zoom_out"), "-"),
             div(class = "center-content", img(class = "img-zoom", height = height, width = width, src = img_src)))
  }

  tagList(
    wellPanel(
      class = "no-background",
      p("These documents are provided for users to familiarize themselves with the different survey structures across years. Navigate through the tabs to explore the documents for each year.")
    ),
    div(
      class = "center-tabs",
      tabsetPanel(
        id = ns("tabs"),
        tabPanel("2023 Survey",
                 value = "A",
                 column(10,
                        tabsetPanel(
                          createTabPanelWithZoom("Page 1", "www/2023p1.png", 800, 600),
                          createTabPanelWithZoom("Page 2", "www/2023p2.png", 800, 600),
                          createTabPanelWithZoom("Page 3", "www/2023p3.png", 800, 600),
                          createTabPanelWithZoom("Page 4", "www/2023p4.png", 800, 600)
                        ))),
        tabPanel("2020 Survey",
                 value = "B",
                 column(10,
                        tabsetPanel(
                          createTabPanelWithZoom("Page 1", "www/2020p1.png"),
                          createTabPanelWithZoom("Page 2", "www/2020p2.png"),
                          createTabPanelWithZoom("Page 3", "www/2020p3.png"),
                          createTabPanelWithZoom("Page 4", "www/2020p4.png", 900, 525)
                        ))),
        tabPanel("2017 Survey",
                 value = "B",
                 column(10,
                        tabsetPanel(
                          createTabPanelWithZoom("Page 1", "www/2017p1.png"),
                          createTabPanelWithZoom("Page 2", "www/2017p2.png"),
                          createTabPanelWithZoom("Page 3", "www/2017p3.png"),
                          createTabPanelWithZoom("Page 4", "www/2017p4.png", 900, 525)
                        )))
      )
    )
  )
}

#' survey_docs Server Functions
#'
#' @noRd
mod_survey_docs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

