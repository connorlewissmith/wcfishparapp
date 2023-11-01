#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny leaflet shinythemes dplyr RColorBrewer stringr shinyalert
#' @noRd
app_ui <- function(request) {
  tagList(
    # Google Tag Manager (noscript) - immediately after the opening <body> tag
    HTML("<!-- Google Tag Manager (noscript) -->
    <noscript><iframe src='https://www.googletagmanager.com/ns.html?id=GTM-W3CR238D'
    height='0' width='0' style='display:none;visibility:hidden'></iframe></noscript>
    <!-- End Google Tag Manager (noscript) -->"),

    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Google Tag Manager - as high in the <head> as possible
    tags$head(
      tags$script(HTML("
      <!-- Google Tag Manager -->
      (function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
      new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
      j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
      'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
      })(window,document,'script','dataLayer','GTM-W3CR238D');
      <!-- End Google Tag Manager -->
      "))
    ),

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
#' @import shiny shinyalert
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
  )
}
