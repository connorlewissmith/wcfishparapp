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
      class = "section-container",
      h3("Purpose of the Survey"),
      p("Surveys conducted in 2017, 2020, 2023, and 2026 aim to understand why fishermen engage in commercial fishing,
         and how changes in opportunities and profitability affect individual communities.")
    ),
    div(
      class = "section-container",
      h3("How to Navigate the App"),
      p("This application allows you to:",
        tags$ul(
          tags$li("View survey responses by county through an interactive map."),
          tags$li("Pick a question, then move between survey years to see how it changes. Your question stays selected as you switch years."),
          tags$li("Download the displayed data with 'Download CSV', or save the map itself with the download control on the map.")
        ),
        "The survey renumbered most questions between waves, so questions are listed by wording rather than number, with the number for the
         selected year shown underneath. Where a question was not asked in a given year, or where the wording or scale changed enough that
         the years are not directly comparable, the map says so. The 'Survey Documents' tab has the full instrument for each year."
      )
    ),
    div(
      class = "section-container",
      h3("Holland Indices"),
      p("Four of the map's questions are not single survey items but latent indices from Holland, Abbott & Norman (2020, ",
        em("Ambio"), "): fisher identity, social capital, job quality and livelihood satisfaction. Each combines several survey items",
        " through a measurement model that was fitted once to all four survey waves pooled, so the scale is the same in every year",
        " and a change between years is a real change, not a re-standardisation. Values are in standard deviations of the pooled",
        " respondent population (mean 0), and the map shows the county mean. Most counties have 4-20 respondents, which puts the",
        " standard error of a county mean at roughly 0.25-0.5 SD, so small differences between counties or years should not be",
        " over-read; the respondent count is shown in the hover label. The indices are re-estimated whenever the survey database changes.")
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

