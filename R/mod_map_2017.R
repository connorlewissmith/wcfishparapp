#' name_of_module1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_2017_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "outer",
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      leafletOutput(ns("map2017"), width = "100%", height = "100%"),
      absolutePanel(
        id = "controls", class = "panel panel-default",
        top = 75, left = 55, width = 350, fixed = TRUE,
        draggable = TRUE, height = "auto",
        img(
          height = 80,
          width = 325,
          src = "www/noaalogo.png"
        ),
        span(tags$i(h5("This map displays the 2017 West Coast Fisheries Participation survey responses aggregated to the county level.
                                        Medians are displayed for ordinal statement questions and yes-no questions are displayed as the percent responding yes.
                                        To use the map, you can click, drag, and use the zoom. The selection tool below allows for working through the
                                                       survey questions and subquestions. ")), style = "color:#045a8d"),
        selectInput(
          inputId = ns("select"),
          label = "Select Survey Question",
          choices = question_names_17,
          selected = "Respondent Count"
        ),
        absolutePanel(
          id = ns("controls2"), class = "panel panel-default",
          bottom = 25, left = 55, width = 250, fixed = TRUE,
          draggable = TRUE, height = "auto",
          uiOutput(outputId = ns("image"))
        )
      )
    )
  )
}

#' name_of_module1 Server Functions
#'
#' @noRd
mod_map_2017_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)

      # Create a data frame with configurations
      image_config <- data.frame(
        selecter = c("Q1:", "Q2:", "Q3:", "Q4:", "Q6:", "Q7:", "Q8:", "Q9:", "Q11", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q28", "Q29", "Q31", "Q32", "Q33", "Q34"),
        height = c(75, 200, 250, 100, 75, 75, 200, 75, 350, 350, 100, 250, 250, 300, 300, 150, 250, 250, 100, 150, 100, 100, 300, 100, 100, 400, 300, 75, 75),
        width = c(400, 500, 500, 500, 500, 500, 500, 500, 600, 600, 500, 400, 400, 500, 500, 400, 400, 400, 400, 400, 400, 400, 500, 500, 500, 400, 500, 400, 400),
        src = c("www/20q1.png", "www/20q2.png", "www/20q3.png", "www/17q4.png", "www/17q6.png", "www/17q7.png", "www/17q8.png", "www/20q9.png", "www/20q11.png", "www/17q13.png", "www/17q14.png", "www/17q15.png", "www/17q16.png", "www/17q17.png", "www/17q18.png", "www/17q19.png", "www/17q20.png", "www/17q21.png", "www/17q22.png", "www/17q23.png", "www/17q24.png", "www/17q25.png", "www/17q26.png", "www/17q28.png", "www/17q29.png", "www/17q31.png", "www/17q32.png", "www/17q33.png", "www/17q34.png")
      )

      # Label map for 2017
      label_map_17 <- list(
        "Q1:" = list(bins = c(45, 50, 55, 60, 65, 70), labels = c("45-50", "50-55", "55-60", "60-65", "65-70")),
        "Q2:" = list(bins = c(1, 2, 3, 4, 5, 6), labels = Q2label),
        "Q3:" = list(bins = c(0, 1, 2, 3, 4, 5, 6), labels = Q3label),
        "Q4:" = list(bins = c(1, 2, 3, 4, 5), labels = Q4label),
        "Q7:" = list(bins = c(0, 1, 2, 3, 4, 5, 6), labels = c("0", "1", "2", "3", "4", "5", "6")),
        "Q8:" = list(bins = c(1, 2, 3, 4, 5, 6), labels = Q8label),
        "Q9:" = list(bins = c(0, 1, 2, 3, 4, 5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q11" = list(bins = c(1, 2, 3, 4, 5, 6), labels = Q11label),
        "Q12" = list(bins = c(1, 2, 3, 4, 5, 6), labels = Q12label),
        "Q20" = list(bins = c(1, 2, 3, 4, 5, 6, 7), labels = Q20label),
        "Q26" = list(bins = c(1, 2, 3, 4), labels = Q26label),
        "Q31" = list(bins = c(1, 2, 3, 4, 5), labels = Q31label),
        "Q32" = list(bins = c(1, 2, 3, 4, 5, 6, 7, 8), labels = Q32label),
        "Q6:" = list(bins = c(0, 1, 2, 3, 4, 5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q33" = list(bins = c(0, 1, 2, 3, 4, 5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q22" = list(bins = c(0, 1, 2, 3, 4, 5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q28" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")),
        "Q29" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")),
        "Q25" = list(bins = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c("0-50lbs", "50-100lbs", "100-150lbs", "150-200lbs", "200-250lbs", "250-300lbs", "300-350lbs")),
        "Q34" = list(bins = c(0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c("50-60%", "60-70%", "70-80%", "80-90%", "90-100%")),
        "Q19" = list(bins = c(0.6, 0.7, 0.8, 0.9, 1), labels = c("60-70%", "70-80%", "80-90%", "90-100%")),
        "Res" = list(bins = c(0, 10, 20, 30, 40, 50, 60, 70, 80), labels = c("0 - 10", "20 - 20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", "70 - 80"))
      )

      data_filter <- reactive({
        # Check for NULL and set default to "Respondent Count"
        selected_var <- if (is.null(input$select)) {
          "Respondent Count"
        } else {
          input$select
        }
        county_17_sf %>%
          select(as.symbol(input$select), NAME, geometry)
      })

      pal <- reactive({
        colorNumeric("viridis", data_filter(), domain = NULL)
      })

      # Have to add Q1 as default to work around bug
      county_imap2017 <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% ### base group
        setView(lng = -130.252667, lat = 40.7850, zoom = 5) %>%
        addPolygons(
          data = county_17_sf,
          stroke = F,
          smoothFactor = .2,
          fillOpacity = .7,
          label = ~NAME,
          color = ~ palrepcount_17(`Respondent Count`),
          highlight = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = F
          ),
          dashArray = "3",
          layerId = county_17_sf$NAME,
          group = "RC"
        ) %>%
        leaflet::addLegend(
          data = county_17_sf, ## overlay group 2 legend
          pal = palrepcount_17, values = ~`Respondent Count`,
          opacity = .7,
          title = "Count",
          group = "RC", position = "bottomright",
          className = "info legend aggregate risk"
        )

      output$map2017 <- renderLeaflet({
        county_imap2017 %>%
          clearPopups()
      })

      observe({
        leafletProxy("map2017", data = data_filter()) %>%
          clearShapes() %>%
          clearPopups() %>%
          addPolygons(
            fillColor = ~ pal()
          )
      })

      observeEvent(
        {
          input$select
        },
        {
          selected_column <- paste0(input$select)
          data <- data_filter()[selected_column]

          selecter3 <- ifelse(
            grepl("Q12", str_extract(as.character(input$select), "[^ ]+")),
            substr(str_extract(as.character(input$select), "[^ ]+"), start = 1, stop = 6),
            substr(str_extract(as.character(input$select), "[^ ]+"), start = 1, stop = 3)
          )

          if (selecter3 %in% names(label_map_17)) {
            bins <- label_map_17[[selecter3]]$bins
            labels <- label_map_17[[selecter3]]$labels
          } else {
            # Default bins and labels here
            bins <- c(0, 1, 2, 3, 4, 5) # Replace with your default bins
            labels <- c("0", "1", "2", "3", "4", "5") # Replace with your default labels
          }

          # Reactive legend
          react_leg <- ifelse(selecter3 %in% par17_yn, "Percent Responding Yes",
            ifelse(selecter3 == "Q19", "Percent Prefering Fishing",
              ifelse(selecter3 == "Res", "Count", "Displaying Median")
            )
          )

          pal <- colorBin("YlOrRd", domain = as.numeric(data[[selected_column]]), bins = bins)

          leafletProxy("map2017", data = data) %>%
            clearShapes() %>%
            addPolygons(
              fillColor = pal(as.numeric(data[[selected_column]])),
              weight = 0.0,
              opacity = 1,
              color = "white",
              label = county_17_sf$NAME,
              layerId = county_17_sf$NAME,
              highlight = highlightOptions(
                weight = 2,
                color = "red",
                fillOpacity = 0.7,
                bringToFront = F
              ),
              dashArray = "3",
              fillOpacity = 0.7
            ) %>%
            clearControls() %>%
            addLegend(
              pal = pal,
              values = as.numeric(data[[selected_column]]),
              opacity = 0.7,
              title = react_leg,
              position = "bottomright",
              labFormat = function(type, cuts, p) {
                paste0(labels)
              }
            )

          output$image <- renderUI({
            # Find the corresponding config for selecter3
            config <- subset(image_config, selecter == selecter3)
            # Special conditions for Q12_break and Q12_break2
            if (selecter3 %in% Q12_break) {
              config <- data.frame(height = 350, width = 600, src = "www/20q121.png")
            }
            if (selecter3 %in% Q12_break2) {
              config <- data.frame(height = 350, width = 600, src = "www/20q122.png")
            }
            # If the config exists, create the image tag
            if (nrow(config) == 1) {
              tags$img(
                height = config$height,
                width = config$width,
                src = config$src
              )
            } else {
              return()
            }
          })
        }
      )

      observeEvent(input$map2017_shape_click, {
        click <- input$map2017_shape_click
        selecter.click17 <- as.character(input$select)

        sub <- county_17_df[county_17_df$NAME == input$map2017_shape_click$id, c(
          "NAME", "Population Estimate 2017", "Median Income 2017",
          "Percent College Grad 2017", "Unemployement Rate 2017",
          "Rural-urban Continuum", "Respondent Count"
        )]

        nm1 <- paste(county_17_df[county_17_df$NAME == input$map2017_shape_click$id, c((paste(input$select)))])
        nm <- sub$NAME

        selecter2.click17 <- str_extract(selecter.click17, "[^ ]+")
        income <- format(sub$`Median Income 2017`, big.mark = ",")
        popul <- format(sub$`Population Estimate 2017`, big.mark = ",")
        rural <- sub$`Rural-urban Continuum`
        college <- ((sub$`Percent College Grad 2017`) * 100)
        unemployment <- ((sub$`Unemployement Rate 2017`) * 100)
        respcount <- sub$`Respondent Count`

        popformat <- paste(nm, "County", "<br>", "Responent Count:", respcount, "<br>", "Median income:",
                           income, "<br>", "Population:", popul, "<br>", "Urban to rural:", rural, "<br>",
                           "College grad:", college, "%", "<br>", "Unemployment:", unemployment, "%", "<br>",
                           sep = " "
        )
        if (is.null(click)) {
          leafletProxy("map2017") %>%
            clearPopups()
        } else {
          leafletProxy("map2017") %>%
            addPopups(
              lng = click$lng, lat = click$lat, popup = popformat,
              options = popupOptions(closeOnClick = F)
            )
        }
      })


    }
  )
}

