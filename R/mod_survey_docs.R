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

  tabsetPanel(
    id = ns("tabs"),
    tabPanel("2023 Survey",
      value = "A",
      column(
        10,
        tabsetPanel(
          tabPanel("Page 1", id = "dummy", img(height = 800, width = 600, src = "www/2023p1.png")),
          tabPanel("Page 2", id = "dummy", img(height = 800, width = 600, src = "www/2023p2.png")),
          tabPanel("Page 3", id = "dummy", img(height = 800, width = 600, src = "www/2023p3.png")),
          tabPanel("Page 4", id = "dummy", img(height = 800, width = 600, src = "www/2023p4.png"))
        )
      )
    ),
    tabPanel("2020 Survey",
      value = "B",
      fluidRow(
        column(
          10,
          tabsetPanel(
            tabPanel("Page 1", id = "dummy", img(height = 900, width = 700, src = "www/2020p1.png")),
            tabPanel("Page 2", id = "dummy", img(height = 900, width = 700, src = "www/2020p2.png")),
            tabPanel("Page 3", id = "dummy", img(height = 900, width = 700, src = "www/2020p3.png")),
            tabPanel("Page 4", id = "dummy", img(height = 900, width = 525, src = "www/2020p4.png"))
          )
        )
      )
    ),
    tabPanel("2017 Survey",
             value = "B",
             fluidRow(
               column(
                 10,
                 tabsetPanel(
                   tabPanel("Page 1", id = "dummy", img(height = 900, width = 700, src = "www/2017p1.png")),
                   tabPanel("Page 2", id = "dummy", img(height = 900, width = 700, src = "www/2017p2.png")),
                   tabPanel("Page 3", id = "dummy", img(height = 900, width = 700, src = "www/2017p3.png")),
                   tabPanel("Page 4", id = "dummy", img(height = 900, width = 525, src = "www/2017p4.png"))
                 )
               )
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

## To be copied in the UI
# mod_survey_docs_ui("survey_docs_1")

## To be copied in the server
# mod_survey_docs_server("survey_docs_1")
