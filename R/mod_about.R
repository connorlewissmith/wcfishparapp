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
    # Image row
    div(
      style = "text-align: center;", # Centering the image
      img(src = "www/sidebyside.png", class = "responsive-img") # Using CSS class for responsive styling
    ),
    # Text row
    div(
      style = "margin: 15px;", # Some margin around the text
      h5("In 2017, 2020, and 2023 we conducted mail surveys to collect information about West Coast fishermen,
          why they choose to participate in commercial fishing, and the benefits they derive from fishing,
          including non-monetary benefits. We also wanted to understand how individuals and communities are
          affected when opportunities and profitability in particular fisheries change. We sent the survey
          to all vessel owners who had commercial fishery landings in Washington, Oregon or California in
          the years preceding the surveys"),
      h5("There are two additional applications for viewing the ", a("2017", href = "https://www.fisheries.noaa.gov/data-tools/west-coast-fisheries-participation-survey-result-tool-2017"),
         " and ", a("2020", href = "https://www.fisheries.noaa.gov/data-tools/west-coast-fisheries-participation-survey-result-tool-2020"), " results. The surveys were largely similar but a
              few questions differ between years. Results can be viewed
              for all respondents or by state of residence of the respondents. The frequency tables as well as the charts can
              be downloaded. Survey design and data collection was produced in partnership with ",
         a("Washington Sea Grant", href = "https://wsg.washington.edu/")),
      textOutput(ns("selected_var"))
    )
  )
}

# Server Function
mod_about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

