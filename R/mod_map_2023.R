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
      leafletOutput(ns("map2023"), width = "100%", height = "100%"),
      absolutePanel(
        id = "controls", class = "panel panel-default",
        top = 75, left = 55, width = 350, fixed = TRUE,
        draggable = TRUE, height = "auto",
        img(
          height = 75,
          width = 325,
          src = "www/noaalogo.png"
        ),
        span(tags$i(h5("This map displays the 2023 West Coast Fisheries Participation survey responses aggregated to the county level.
                                        Medians are displayed for ordinal statement questions and yes-no questions are displayed as the percent responding yes.
                                        To use the map, you can click, drag, and use the zoom. The selection tool below allows for working through the
                                                       survey questions and subquestions. ")), style = "color:#045a8d"),
        # Add a download button here
        downloadButton(ns('downloadData3'), 'Download CSV'),
        # Select input tool
        selectInput(
          inputId = ns("select3"),
          label = "Select Survey Question",
          choices = question_names_23,
          selected = "Respondent Count"
        ),
        absolutePanel(
          id = ns("controls3"), class = "panel panel-default",
          bottom = 25, left = 55, width = 250, fixed = TRUE,
          draggable = TRUE, height = "auto",
          uiOutput(outputId = ns("image3"))
        )
      )
    )
  )
}

#' map_2023 Server Functions
#'
#' @noRd
mod_map_2023_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Label map for 2020
    label_map_23 <- list(
      "Q1:" = list(bins = c(45, 50, 55, 60, 65, 70), labels = c("45-50", "50-55", "55-60", "60-65", "65-70")),
      "Q2:" = list(bins = c(1, 2, 3, 4, 5, 6), labels = Q2label_23),
      "Q3:" = list(bins = c(0, 1, 2, 3, 4, 5, 6), labels = Q3label_23),
      "Q4:" = list(bins = c(1, 2, 3, 4, 5), labels = Q4label_23),
      "Q7:" = list(bins = c(0, 1, 2, 3, 4, 5), labels = c("0", "1", "2", "3", "4", "5")),
      "Q8:" = list(bins = c(0, 1, 2, 3, 4, 5, 6), labels = c("0", "1", "2", "3", "4", "5", "6")), # Formally Q7
      "Q9:" = list(bins = c(1, 2, 3, 4, 5, 6), labels = Q9label_23),
      "Q10" = list(bins = c(0, 1, 2, 3, 4, 5), labels = c("0", "1", "2", "3", "4", "5")), # Formally Q9
      "Q12" = list(bins = c(1, 2, 3, 4, 5, 6), labels = Q12label_23),
      "Q13" = list(bins = c(1, 2, 3, 4, 5, 6), labels = Q13label_23),
      "Q14" = list(bins = c(1, 2, 3, 4, 5, 6), labels = Q14label_23),
      "Q15" = list(bins = c(0, 100, 200, 400, 600, 800, 1000), labels = c("less than 100 nm", "100-200 nm", "200-400 nm", "400-600 nm", "600-800 nm", "800-1000 nm")),
      "Q17" = list(bins = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")),
      "Q18" = list(bins = c(1, 2, 3, 4, 5, 6, 7), labels = Q18label_23),
      "Q19" = list(bins = c(0, 1, 2, 3, 4, 5), labels = c("0", "1", "2", "3", "4", "5")),
      "Q20" = list(bins = c(1, 2, 3, 4, 5, 6), labels = Q20label_23),
      "Q22" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")),
      "Q25-1" = list(bins = c(0, .25, .50, .75, 1), labels = c("0-25%", "25-50%", "50-75%", "75-100%")),
      "Q25-2" = list(bins = c(0, 100, 200, 300, 400), labels = c("0-100lbs", "100-200lbs", "200-300lbs", "300-400lbs")),
      "Q26" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")),
      "Q27" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")),
      "Q30" = list(bins = c(0, 1, 2, 3, 4, 5), labels = c("0", "1", "2", "3", "4", "5")),
      "Q31" = list(bins = c(0, 1, 2, 3, 4, 5, 6, 7), labels = Q31label_23),
      "Res" = list(bins = c(0, 10, 20, 30, 40, 50, 60, 70, 80), labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80"))
    )

    # Create a data frame with configurations
    image_lookup <- data.frame(
      selecter = c(
        "Q1:", "Q2:", "Q3:", "Q4:", "Q7:", "Q8:", "Q9:", "Q10:", "Q12",
        "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21",
        "Q22", "Q23", "Q24", "Q25", "Q26", "Q28", "Q29", "Q31", "Q32",
        "Q33", "Q34", "Q27-1", "Q27-2"
      ),
      height = c(
        75, 200, 250, 100, 75, 75, 200, 75, 350, 300, 250, 100, 150, 200,
        250, 100, 200, 75, 100, 125, 200, 100, 100, 75, 75, 200, 250, 75,
        75, 150, 150
      ),
      width = c(
        400, 500, 500, 500, 500, 500, 500, 500, 600, 550, 500, 500, 350, 400,
        400, 400, 400, 400, 400, 400, 400, 400, 400, 500, 500, 400, 400, 400,
        400, 400, 400
      ),
      src = c(
        "www/20q1.png", "www/20q2.png", "www/20q3.png", "www/17q4.png", "www/23q7.png",
        "www/23q8.png", "www/23q9.png", "www/23q10.png", "www/23q12.png", "www/20q13.png",
        "www/20q14.png", "www/20q15.png", "www/20q16.png", "www/20q17.png", "www/20q18.png",
        "www/20q19.png", "www/20q20.png", "www/20q21.png", "www/20q22.png", "www/20q23.png",
        "www/20q24.png", "www/20q25.png", "www/20q26.png", "www/20q28.png", "www/20q29.png",
        "www/20q31.png", "www/20q32.png", "www/20q33.png", "www/20q34.png", "www/20q27.png",
        "www/20q27.png"
      ),
      stringsAsFactors = FALSE
    )

    # Create default reactive filters
    data_filter3 <- reactive({
      # Check for NULL and set default to "Respondent Count"
      selected_var <- if (is.null(input$select3)) {
        "Respondent Count"
      } else {
        input$select3
      }
      county_23_sf %>%
        select(as.symbol(input$select3), NAME, geometry)
    })

    # Create default pals
    pal3 <- reactive({
      colorNumeric("viridis", data_filter3(), domain = NULL)
    })

    # Have to add Q1 as default to work around bug
    county_imap2023 <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% ### base group
      setView(lng = -130.252667, lat = 40.7850, zoom = 5) %>%
      addPolygons(
        data = county_23_sf,
        stroke = F,
        smoothFactor = .2,
        fillOpacity = .7,
        label = ~NAME,
        color = ~ palrepcount_23(`Respondent Count`),
        highlight = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = F
        ),
        dashArray = "3",
        layerId = county_23_sf$NAME,
        group = "RC"
      ) %>%
      leaflet::addLegend(
        data = county_23_sf, ## overlay group 2 legend
        pal = palrepcount_23, values = ~`Respondent Count`,
        opacity = .7,
        title = "Count",
        group = "RC", position = "bottomright",
        className = "info legend aggregate risk"
      )

    output$map2023 <- renderLeaflet({
      county_imap2023 %>%
        clearPopups()
    })

    observe({
      leafletProxy("map2023", data = data_filter3()) %>%
        clearShapes() %>%
        clearPopups() %>%
        addPolygons(
          fillColor = ~ pal3()
        )
    })

    # Create interactive map
    observeEvent(
      {
        input$select3
      },
      {
        selected_column <- paste0(input$select3)
        data <- data_filter3()[selected_column]

        # Clean selection variable
        get_selecter_320 <- function(input_str) {
          if (grepl("Q25", input_str)) {
            return(substr(input_str, start = 1, stop = 5))
          }
          if (grepl("Q13", input_str)) {
            return(substr(input_str, start = 1, stop = 6))
          }
          return(substr(input_str, start = 1, stop = 3))
        }

        selecter320 <- get_selecter_320(str_extract(as.character(input$select3), "[^ ]+"))

        # Lookup the bins and labels based on selecter320
        if (selecter320 %in% names(label_map_23)) {
          bins23 <- label_map_23[[selecter320]]$bins
          labels23 <- label_map_23[[selecter320]]$labels
        } else {
          # Fallback default bins and labels
          bins23 <- c(0, .25, .50, .75, 1)
          labels23 <- c("0-25%", "25-50%", "50-75%", "75-100%")
        }


        # Reactive legend
        react_leg23 <- ifelse(selecter320 %in% par23_yn, "Percent Responding Yes",
                              ifelse(selecter320 == "Q17", "Percent Prefering Fishing",
                                     ifelse(selecter320 == "Res", "Count", "Displaying Median")
                              )
        )

        pal <- colorBin("YlOrRd", domain = as.numeric(data[[selected_column]]), bins = bins23)

        leafletProxy("map2023", data = data) %>%
          clearShapes() %>%
          addPolygons(
            fillColor = pal(as.numeric(data[[selected_column]])),
            weight = 0.0,
            opacity = 1,
            color = "white",
            label = county_23_sf$NAME,
            layerId = county_23_sf$NAME,
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
          clearPopups() %>%
          clearMarkers() %>%
          addLegend(
            pal = pal,
            values = as.numeric(data[[selected_column]]),
            opacity = 0.7,
            # baseSize = 10,
            title = react_leg23,
            position = "bottomright",
            labFormat = function(type, cuts, p) {
              paste0(labels23)
            }
          )

        output$image3 <- renderUI({
          # Special cases for selecter320 %in% Q12_break and Q12_break2
          if (selecter320 %in% Q13_break) {
            return(tags$img(height = 350, width = 600, src = "www/20q121.png"))
          }

          if (selecter320 %in% Q13_break2) {
            return(tags$img(height = 350, width = 600, src = "www/20q122.png"))
          }

          # Find the matching row in the lookup table
          match_row <- subset(image_lookup, selecter == selecter320)

          if (nrow(match_row) == 1) {
            return(tags$img(height = match_row$height, width = match_row$width, src = match_row$src))
          }

          return() # Return NULL if no match is found
        })

      }
    )

    observeEvent(input$map2023_shape_click, {
      click <- input$map2023_shape_click
      selecter.click23 <- as.character(input$select3)

      sub <- county_23_df[county_23_df$NAME == input$map2023_shape_click$id, c(
        "NAME", "Population Estimate 2023", "Median Income 2023",
        "Percent College Grad 2023", "Unemployement Rate 2023",
        "Rural-urban Continuum", "Respondent Count"
      )]

      nm1 <- paste(county_23_df[county_23_df$NAME == input$map2023_shape_click$id, c((paste(input$select3)))])
      nm <- sub$NAME

      selecter2.click23 <- str_extract(selecter.click23, "[^ ]+")
      income <- format(sub$`Median Income 2023`, big.mark = ",")
      popul <- format(sub$`Population Estimate 2023`, big.mark = ",")
      rural <- sub$`Rural-urban Continuum`
      college <- ((sub$`Percent College Grad 2023`) * 100)
      unemployment <- ((sub$`Unemployement Rate 2023`) * 100)
      respcount <- sub$`Respondent Count`

      popformat <- paste(nm, "County", "<br>", "Responent Count:", respcount, "<br>", "Median income:",
                         income, "<br>", "Population:", popul, "<br>", "Urban to rural:", rural, "<br>",
                         "College grad:", college, "%", "<br>", "Unemployment:", unemployment, "%", "<br>",
                         sep = " "
      )
      if (is.null(click)) {
        leafletProxy("map2023") %>%
          clearPopups()
      } else {
        leafletProxy("map2023") %>%
          addPopups(
            lng = click$lng, lat = click$lat, popup = popformat,
            options = popupOptions(closeOnClick = F)
          )
      }
    })

    # Download handler
    output$downloadData3 <- downloadHandler(
      filename = function() {
        paste('county_23_data-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        write.csv(county_23_df, file)
      }
    )

  })
}


