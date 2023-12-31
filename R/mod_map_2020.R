#' name_of_module2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_2020_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "outer",
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      leafletOutput(ns("map2020"), width = "100%", height = "100%"),
      absolutePanel(
        id = "controls", class = "panel panel-default",
        top = 75, left = 55, width = 350, fixed = TRUE,
        draggable = TRUE, height = "auto",
        img(
          height = 75,
          width = 325,
          src = "www/noaalogo.png"
        ),
        span(tags$i(h5("This map displays the 2020 West Coast Fisheries Participation survey responses aggregated to the county level.
                                        Medians are displayed for ordinal statement questions and yes-no questions are displayed as the percent responding yes.
                                        To use the map, you can click, drag, and use the zoom. The selection tool below allows for working through the
                                                       survey questions and subquestions. ")), style = "color:#045a8d"),
        div(
          style = "width: 100%; display: flex;",
          div(
            style = "flex: 1;",
            downloadButton(ns('downloadData2'), 'Download CSV', style = "height: 50px;")
          ),
          div(
            style = "flex: 1;",
            actionButton(inputId = ns("toggle_button2"), label = "Hide Full Selected Question", style = "white-space: normal; font-size: 80%; height: 50px;")
          )
        ),
        selectInput(
          inputId = ns("select2"),
          label = "Select Abbreviated Survey Question",
          choices = question_names_20,
          selected = "Respondent Count"
        ),
        absolutePanel(
          id = ns("controls2"), class = "panel panel-default",
          bottom = 25, left = 55, width = 250, fixed = TRUE,
          draggable = TRUE, height = "auto",
          uiOutput(outputId = ns("image2"))
        )
      )
    ),
    tags$script(
      HTML(
        paste0(
          "$(document).ready(function(){",
          "$('#", ns("toggle_button2"), "').on('click', function(){",
          "$('#", ns("controls2"), "').toggle();",
          "});",
          "});"
        )
      )
    )
  )
}


#' name_of_module2 Server Functions
#'
#' @noRd
mod_map_2020_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)

      # Create a reactive value to store the state of the panel
      showPanel <- reactiveVal(TRUE)

      observeEvent(input$toggle_button2, {
        # Toggle the state
        showPanel(!showPanel())

        # Update the label of the button based on the state
        if (showPanel()) {
          updateActionButton(session, "toggle_button2", label = "Hide Full Selected Question")
        } else {
          updateActionButton(session, "toggle_button2", label = "View Full Selected Questionl")
        }
      })

      # Create a data frame with configurations
      image_lookup <- data.frame(
        selecter = c(
          "Q1:", "Q2:", "Q3:", "Q4:", "Q6:", "Q7:", "Q8:", "Q9:", "Q11",
          "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21",
          "Q22", "Q23", "Q24", "Q25", "Q26", "Q28", "Q29", "Q31", "Q32",
          "Q33", "Q34", "Q27-1", "Q27-2", "Res"
        ),
        height = c(
          75, 200, 250, 100, 75, 75, 200, 75, 350, 300, 250, 100, 150, 200,
          250, 100, 200, 75, 100, 125, 200, 100, 100, 75, 75, 200, 250, 75,
          75, 150, 150, 100
        ),
        width = c(
          400, 500, 500, 500, 500, 500, 500, 500, 600, 550, 500, 500, 350, 400,
          400, 400, 400, 400, 400, 400, 400, 400, 400, 500, 500, 400, 400, 400,
          400, 400, 400, 500
        ),
        src = c(
          "www/20q1.png", "www/20q2.png", "www/20q3.png", "www/17q4.png", "www/17q6.png",
          "www/17q7.png", "www/17q8.png", "www/20q9.png", "www/20q11.png", "www/20q13.png",
          "www/20q14.png", "www/20q15.png", "www/20q16.png", "www/20q17.png", "www/20q18.png",
          "www/20q19.png", "www/20q20.png", "www/20q21.png", "www/20q22.png", "www/20q23.png",
          "www/20q24.png", "www/20q25.png", "www/20q26.png", "www/20q28.png", "www/20q29.png",
          "www/20q31.png", "www/20q32.png", "www/20q33.png", "www/20q34.png", "www/20q27.png",
          "www/20q27.png", "www/all_res.png"
        ),
        stringsAsFactors = FALSE
      )

      # Label map for 2020
      label_map_20 <- list(
        "Q1:" = list(bins = c(45, 50, 55, 60, 65, 70), labels = c("45-50", "50-55", "55-60", "60-65", "65-70")),
        "Q2:" = list(bins = c(1, 2, 3, 4, 5, 6), labels = Q2label_20),
        "Q3:" = list(bins = c(0, 1, 2, 3, 4, 5, 6), labels = Q3label_20),
        "Q4:" = list(bins = c(1, 2, 3, 4, 5), labels = Q4label_20),
        "Q6:" = list(bins = c(0, 1, 2, 3, 4, 5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q7:" = list(bins = c(0, 1, 2, 3, 4, 5, 6), labels = c("0", "1", "2", "3", "4", "5", "6")),
        "Q8:" = list(bins = c(1, 2, 3, 4, 5, 6), labels = Q8label_20),
        "Q9:" = list(bins = c(0, 1, 2, 3, 4, 5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q11" = list(bins = c(1, 2, 3, 4, 5, 6), labels = Q11label_20),
        "Q12" = list(bins = c(1, 2, 3, 4, 5, 6), labels = Q12label),
        "Q13" = list(bins = c(1, 2, 3, 4), labels = Q13label_20),
        "Q14" = list(bins = c(1, 2, 3, 4, 5, 6), labels = Q11label_20), # Assuming labels same as Q11
        "Q17" = list(bins = c(0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c("50-60%", "60-70%", "70-80%", "80-90%", "90-100%")),
        "Q18" = list(bins = c(1, 2, 3, 4, 5, 6, 7), labels = Q18label_20),
        "Q19" = list(bins = c(0, 1, 2, 3, 4, 5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q20" = list(bins = c(1, 2, 3, 4, 5, 6), labels = Q11label_20), # Assuming labels same as Q11
        "Q21" = list(bins = c(0, 100, 200, 400, 600, 800, 1000), labels = c("less than 100 nm", "100-200 nm", "200-400 nm", "400-600 nm", "600-800 nm", "800-1000 nm")),
        "Q24" = list(bins = c(0, 1, 2, 3, 4, 5), labels = Q24label_20),
        "Q28" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")),
        "Q29" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")),
        "Q32" = list(bins = c(0, 1, 2, 3, 4, 5, 6, 7), labels = Q32label_20),
        "Q33" = list(bins = c(0, 1, 2, 3, 4, 5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q34" = list(bins = c(0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c("50-60%", "60-70%", "70-80%", "80-90%", "90-100%")),
        "Res" = list(bins = c(0, 10, 20, 30, 40, 50, 60, 70, 80), labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80"))
      )

      # Create default reactive filters
      data_filter2 <- reactive({
        # Check for NULL and set default to "Respondent Count"
        selected_var <- if (is.null(input$select2)) {
          "Respondent Count"
        } else {
          input$select2
        }
        county_20_sf %>%
          select(as.symbol(input$select2), NAME, geometry)
      })

      # Create default pals
      pal2 <- reactive({
        colorNumeric("viridis", data_filter2(), domain = NULL)
      })

      # Have to add Q1 as default to work around bug
      county_imap2020 <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% ### base group
        setView(lng = -130.252667, lat = 40.7850, zoom = 5) %>%
        addPolygons(
          data = county_20_sf,
          stroke = F,
          smoothFactor = .2,
          fillOpacity = .7,
          label = ~NAME,
          color = ~ palrepcount_20(`Respondent Count`),
          highlight = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = F
          ),
          dashArray = "3",
          layerId = county_20_sf$NAME,
          group = "RC"
        ) %>%
        leaflet::addLegend(
          data = county_20_sf, ## overlay group 2 legend
          pal = palrepcount_20, values = ~`Respondent Count`,
          opacity = .7,
          title = "Count",
          group = "RC", position = "bottomright",
          className = "info legend aggregate risk"
        )

      output$map2020 <- renderLeaflet({
        county_imap2020 %>%
          clearPopups()
      })

      observe({
        leafletProxy("map2020", data = data_filter2()) %>%
          clearShapes() %>%
          clearPopups() %>%
          addPolygons(
            fillColor = ~ pal2()
          )
      })

      # Create interactive map
      observeEvent(
        {
          input$select2
        },
        {
          selected_column <- paste0(input$select2)
          data <- data_filter2()[selected_column]

          selecter2020 <- ifelse(
            grepl("Q12", str_extract(as.character(input$select2), "[^ ]+")),
            substr(str_extract(as.character(input$select2), "[^ ]+"), start = 1, stop = 6),
            substr(str_extract(as.character(input$select2), "[^ ]+"), start = 1, stop = 3)
          )

          # Define your initial 'selecter2020'
          prefix <- str_extract(as.character(input$select2), "[^ ]+")
          if (grepl("^Q12", prefix)) {
            selecter2020 <- substr(prefix, start = 1, stop = 6)
          } else if (grepl("^Q27-1", prefix)) {
            selecter2020 <- "Q27-1"  # Special case for Q15_1, Q15_2, etc. but not Q15_3
          } else if (grepl("^Q27-2", prefix)) {
            selecter2020 <- "Q27-2"  # Special case for Q15_1, Q15_2, etc. but not Q15_3
          } else {
            selecter2020 <- substr(prefix, start = 1, stop = 3)
          }

          if (selecter2020 %in% names(label_map_20)) {
            bins <- label_map_20[[selecter2020]]$bins
            labels <- label_map_20[[selecter2020]]$labels
          } else if (grepl("Q12", selecter2020)) {
            bins <- c(1, 2, 3, 4, 5, 6)
            labels <- Q12label
          } else if (grepl("Q27-2", selecter2020)) {
            bins <- c(0, 50, 100, 150, 200, 250, 300, 501)
            labels <- c("0-50lbs", "50-100lbs", "100-150lbs", "150-200lbs", "200-250lbs", "250-300lbs", "300-500lbs")
          } else {
            # Default bins and labels here
            bins = c(0, .25, .50, .75, 1)
            labels = c("0-25%", "25-50%", "50-75%", "75-100%")
          }

          # Reactive legend
          react_leg20 <- ifelse(selecter2020 %in% par20_yn, "Percent Responding Yes",
            ifelse(selecter2020 == "Q17", "Percent Prefering Fishing",
              ifelse(selecter2020 == "Res", "Count", "Displaying Median")
            )
          )

          pal <- colorBin("YlOrRd", domain = as.numeric(data[[selected_column]]), bins = bins)

          leafletProxy("map2020", data = data) %>%
            clearShapes() %>%
            addPolygons(
              fillColor = pal(as.numeric(data[[selected_column]])),
              weight = 0.0,
              opacity = 1,
              color = "white",
              label = county_20_sf$NAME,
              layerId = county_20_sf$NAME,
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
              title = react_leg20,
              position = "bottomright",
              labFormat = function(type, cuts, p) {
                paste0(labels)
              }
            )

          output$image2 <- renderUI({
            # Special cases for selecter2020 %in% Q12_break and Q12_break2
            if (selecter2020 %in% Q12_break) {
              return(tags$img(height = 350, width = 600, src = "www/20q121.png"))
            }

            if (selecter2020 %in% Q12_break2) {
              return(tags$img(height = 350, width = 600, src = "www/20q122.png"))
            }

            # Find the matching row in the lookup table
            match_row <- subset(image_lookup, selecter == selecter2020)

            if (nrow(match_row) == 1) {
              return(tags$img(height = match_row$height, width = match_row$width, src = match_row$src))
            }

            return() # Return NULL if no match is found
          })

        }
      )

      observeEvent(input$map2020_shape_click, {
        click <- input$map2020_shape_click
        selecter.click17 <- as.character(input$select2)

        sub <- county_20_df[county_20_df$NAME == input$map2020_shape_click$id, c(
          "NAME", "Population Estimate 2020", "Median Income 2020",
          "Percent College Grad 2020", "Unemployement Rate 2020",
          "Rural-urban Continuum", "Respondent Count"
        )]

        nm1 <- paste(county_20_df[county_20_df$NAME == input$map2020_shape_click$id, c((paste(input$select2)))])
        nm <- sub$NAME

        selecter2.click17 <- str_extract(selecter.click17, "[^ ]+")
        income <- format(sub$`Median Income 2020`, big.mark = ",")
        popul <- format(sub$`Population Estimate 2020`, big.mark = ",")
        rural <- sub$`Rural-urban Continuum`
        college <- ((sub$`Percent College Grad 2020`) * 100)
        unemployment <- ((sub$`Unemployement Rate 2020`) * 100)
        respcount <- sub$`Respondent Count`

        popformat <- paste('<center><b>', nm, " County", '</b></center>', "Responent Count: ", respcount, "<br>", "Median income: ",
                           income, "<br>", "Population: ", popul, "<br>", "Urban to rural: ", rural, "<br>",
                           "College grad: ", college, "%", "<br>", "Unemployment: ", unemployment, "%", "<br>",
                           sep = ""
        )

        if (is.null(click)) {
          leafletProxy("map2020") %>%
            clearPopups()
        } else {
          leafletProxy("map2020") %>%
            addPopups(
              lng = click$lng, lat = click$lat, popup = popformat,
              options = popupOptions(closeOnClick = F)
            )
        }
      })

      # Download handler
      output$downloadData2 <- downloadHandler(
        filename = function() {
          paste('county_20_data-', Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
          write.csv(county_20_df, file)
        }
      )

    }
  )
}

