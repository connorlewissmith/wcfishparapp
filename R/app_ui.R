#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny leaflet shinythemes dplyr RColorBrewer stringr
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your customized UI logic with the styling
    bootstrapPage(
      navbarPage(
        theme = shinythemes::shinytheme("flatly"),
        collapsible = TRUE,
        HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Fisheries Participation Survey</a>'),
        id = "nav",
        windowTitle = "Participation Survey",

        # The tab panels go here
        tabPanel("2023 Map", mod_map_2023_ui("map_2023_1")),
        tabPanel("2020 Map", mod_map_2020_ui("name_of_module2_1")),
        tabPanel("2017 Map", mod_map_2017_ui("name_of_module1_1")),
        tabPanel("Survey Documents", mod_survey_docs_ui("survey_docs_1")),
        tabPanel("About", mod_about_ui("about_1"))
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "wcfishparapp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
