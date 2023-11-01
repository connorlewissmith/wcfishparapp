#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Header row
    div(
      style = "text-align: center; margin-bottom: 20px;",
      h2("About the West Coast Fisheries Participation Survey")
    ),
    # Add CSS class for section-container and gif-center
    div(
      class = "section-container",
      h3("Introduction"),
      p("The West Coast Fisheries Participation Survey collects vital data on fishermen's choices,
         behaviors, and the socio-economic impact on communities across Washington, Oregon, and California.")
    ),
    div(
      style = "text-align: center;",
      img(src = "www/sidebyside.png", class = "responsive-img")
    ),
    div(
      class = "section-container",
      h3("Purpose of the Survey"),
      p("Surveys conducted in 2017, 2020, and 2023 aim to understand why fishermen engage in commercial fishing,
         and how changes in opportunities and profitability affect individual communities.")
    ),
    div(
      class = "section-container",
      h3("How to Navigate the App"),
      p("This application allows you to:",
        tags$ul(
          tags$li("View survey responses by county through interactive maps."),
          tags$li("Use selection tools to delve into specific questions and sub-questions."),
          tags$li("Download the displayed data via a 'Download CSV' button.")
        ),
        "Before comparing different survey years, check the 'Survey Documents' tab to ensure the question exists in each survey."
      ),
      div(style = "text-align: center;", # Centering the GIF
          img(src = "www/demo.gif", class = "responsive-img gif-center") # Added class for the gif
      )
    ),
    div(
      class = "section-container",
      h3("Additional Resources"),
      p("To explore more, visit the additional applications for the ",
        a("2017", href = "https://www.fisheries.noaa.gov/data-tools/west-coast-fisheries-participation-survey-result-tool-2017"),
        " and ",
        a("2020", href = "https://www.fisheries.noaa.gov/data-tools/west-coast-fisheries-participation-survey-result-tool-2020"),
        " survey results. These platforms provide filtering options and allow data downloads."
      )
    ),
    div(
      class = "section-container",  # Added the section-container class
      h3("Acknowledgements"),
      p("We would like to thank every respondent for their time and effort. We credit the ",
        a("US Census American Communities Survey", href = "https://www.census.gov/programs-surveys/acs/"),
        " estimates for county-level metrics are provided through our click function. We additionally credit the ",
        a("US Department of Agriculture", href = "https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/"),
        " for the 2013 Rural-urban continuum codes for the counties that are also displayed through the click function.",
        " The 2017 survey was developed and executed in partnership with ",
        a("Washington Sea Grant", href = "https://wsg.washington.edu/"), ".")
    )
  )
}



# Server Function
mod_about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

