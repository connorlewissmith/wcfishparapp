#' Shared county map module
#'
#' One module drives every survey wave. The wave is chosen inside the map rather
#' than by switching tabs, so the selected question stays put when you move
#' between years. Everything wave specific comes from map_config().
#'
#' The selector is keyed on the question wording, which is stable across waves,
#' and question_lookup_YY turns that back into the column name for the chosen
#' year. question_compare supplies the warning shown when an item is missing
#' from a wave or is not safely comparable.
#'
#' @param id Module id.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id) {
  ns <- NS(id)
  years <- rev(names(map_config()))
  tagList(
    div(
      class = "outer",
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      leafletOutput(ns("map"), width = "100%", height = "100%"),
      absolutePanel(
        class = "panel panel-default controls",
        top = 75, left = 55, width = 350, fixed = TRUE,
        draggable = TRUE, height = "auto",
        img(
          height = 75,
          width = 325,
          src = "www/noaalogo.png"
        ),
        span(tags$i(h5("These maps display the West Coast Fisheries Participation survey responses aggregated to the county level.
                                        Medians are displayed for ordinal statement questions and yes-no questions are displayed as the percent responding yes.
                                        Pick a question, then move between survey years to see how it changes. To use the map, you can click, drag,
                                                       and use the zoom. ")), style = "color:#045a8d"),
        downloadButton(ns("downloadData"), "Download CSV", style = "width: 100%; margin-bottom: 6px;"),
        downloadButton(ns("downloadMap"), "Download Map", style = "width: 100%; margin-bottom: 8px;"),
        selectInput(
          inputId = ns("select"),
          label = "Select Survey Question",
          choices = question_choices_all,
          selected = "Respondent Count"
        ),
        radioButtons(
          inputId = ns("year"),
          label = "Survey Year",
          choices = years,
          selected = years[1],
          inline = TRUE
        ),
        checkboxInput(ns("compare"), "Compare with another year", value = FALSE),
        conditionalPanel(
          condition = "input.compare == true", ns = ns,
          radioButtons(
            inputId = ns("year_b"),
            label = "Compare against",
            choices = years,
            selected = years[2],
            inline = TRUE
          )
        ),
        div(style = "margin-bottom: 6px;",
            actionLink(ns("show_question"), "View the question as it was asked")),
        uiOutput(ns("question_note"))
      )
    ),
  )
}


#' Hover text for a county: name, the value shown, and the sample it rests on
#'
#' A county median from four responses looks identical to one from seventy on a
#' choropleth, so the count travels with the value.
#'
#' @noRd
hover_label <- function(sf_obj, col, question) {
  d <- sf::st_drop_geometry(sf_obj)
  val <- d[[col]]
  n <- d[["Respondent Count"]]
  shown <- ifelse(is.na(val), "no data", format(val, trim = TRUE))
  lapply(
    paste0("<b>", d$NAME, " County</b><br/>", question, ": ", shown,
           "<br/><span style='color:#777'>", n, " respondents</span>"),
    htmltools::HTML
  )
}

#' The survey crop for a selecter key, or NULL when there isn't one
#'
#' @noRd
question_image <- function(cfg, sel) {
  for (b in cfg$breaks) {
    if (sel %in% b$keys) {
      return(list(src = b$src, width = b$width, height = b$height))
    }
  }
  row <- subset(cfg$images, selecter == sel)
  if (nrow(row) == 1) {
    return(list(src = row$src, width = row$width, height = row$height))
  }
  NULL
}

#' Rebuild the on screen map as a static plot for download
#'
#' Rendered server side with ggplot2 rather than screenshotting the leaflet
#' widget, so it does not depend on a headless browser being present wherever
#' the app is deployed.
#'
#' @param st The recorded draw state, or NULL when nothing is mapped.
#' @param all_counties Every west coast county, drawn underneath.
#'
#' @noRd
map_plot <- function(st, all_counties) {
  base <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = all_counties, fill = "#f0f0f0",
                     colour = "#bdbdbd", linewidth = 0.15) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(caption = "Counties with fewer than 4 respondents are not reported")

  if (is.null(st)) {
    return(base + ggplot2::labs(title = "Not asked in this survey year"))
  }

  d <- st$shp
  d$..band <- cut(st$value, st$bins, include.lowest = TRUE,
                  labels = if (is.null(st$labels)) NULL else st$labels)

  base +
    ggplot2::geom_sf(data = d, ggplot2::aes(fill = .data$..band),
                     colour = "white", linewidth = 0.15) +
    ggplot2::scale_fill_brewer(palette = st$palette, drop = FALSE,
                               direction = if (isTRUE(st$reverse)) -1 else 1,
                               na.value = "#f0f0f0", name = st$legend) +
    ggplot2::labs(title = st$title, subtitle = st$subtitle)
}

#' Shared county map Server Function
#'
#' @noRd
mod_map_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cfgs <- map_config()
    lookups <- list(
      "2017" = question_lookup_17, "2020" = question_lookup_20,
      "2023" = question_lookup_23, "2026" = question_lookup_26
    )

    # The survey crop for the current question. Shown on request in a modal,
    # because every crop is 350-600px wide and the old floating panel was 250px.
    question_img <- reactiveVal(NULL)

    # What the map is currently showing, so the PNG export can rebuild it
    draw_state <- reactiveVal(NULL)

    observeEvent(input$show_question, {
      im <- question_img()
      showModal(modalDialog(
        title = input$select,
        if (is.null(im)) {
          div(style = "color:#777;", "No image of this question is available.")
        } else {
          div(style = "text-align:center;",
              tags$img(src = im$src, width = im$width, height = im$height,
                       style = "max-width:100%; height:auto;"))
        },
        footer = modalButton("Close"),
        easyClose = TRUE, size = "l"
      ))
    })

    cur_cfg <- reactive({
      req(input$year)
      cfgs[[input$year]]
    })

    # The column name in the selected year, or NA when that wave did not ask it
    cur_col <- reactive({
      req(input$year, input$select)
      lk <- lookups[[input$year]]
      if (input$select %in% names(lk)) unname(lk[[input$select]]) else NA_character_
    })

    # Basemap only. Everything else is drawn through the proxy so switching year
    # does not reset the pan and zoom.
    # Every west coast county is drawn once, underneath. Counties are dropped in
    # cleaning when they have three or fewer respondents, which mixes genuine
    # zeros with small samples held back, so this layer says "not reported"
    # rather than implying nobody was surveyed there.
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -130.252667, lat = 40.7850, zoom = 5) %>%
        addPolygons(
          data = counties_all_sf,
          group = "basecounties",
          fillColor = "#f0f0f0", fillOpacity = 0.55,
          weight = 0.5, color = "#bdbdbd", opacity = 1,
          label = ~ paste0(NAME, " County - fewer than 4 respondents, not reported")
        )
    })

    output$question_note <- renderUI({
      req(input$year, input$select)
      col <- cur_col()
      cmp <- question_compare[question_compare$label == input$select, ]

      if (is.na(col)) {
        waves <- if (nrow(cmp)) cmp$waves[1] else ""
        return(div(
          style = "background:#fcf3cf; border-left:4px solid #d4ac0d; padding:6px 8px; margin-bottom:8px; font-size:85%;",
          tags$b(paste0("Not asked in the ", input$year, " survey.")),
          if (nzchar(waves)) tags$div(paste0("Asked in: ", waves)) else NULL
        ))
      }

      qnum <- sub(":?%?$", "", sub(" .*$", "", col))
      note <- if (nrow(cmp) && !is.na(cmp$note[1])) cmp$note[1] else NULL

      warn <- function(txt, bold = NULL) div(
        style = "background:#fcf3cf; border-left:4px solid #d4ac0d; padding:6px 8px; margin-bottom:8px; font-size:85%;",
        if (!is.null(bold)) tags$b(bold) else NULL, txt)

      st <- cmp_state()
      cmp_msg <- switch(st,
        same_year = warn("Pick a different year to compare against.", "Comparing a year with itself. "),
        missing   = warn(paste0("This question was not asked in both ", input$year, " and ", input$year_b,
                                ", so there is nothing to difference. Showing ", input$year, " on its own."),
                         "Cannot compare. "),
        caveat    = warn(paste0("The map is showing the difference, but read the note below first, because the two years",
                                " are not measured the same way."), "Difference shown with caution. "),
        on        = div(style = "font-size:85%; color:#555; margin-bottom:6px;",
                        paste0("Showing change: ", input$year, " minus ", input$year_b,
                               ". Blue is higher in ", input$year, ".")),
        NULL)

      tagList(
        cmp_msg,
        div(style = "font-size:85%; color:#555; margin-bottom:6px;",
            paste0(input$year, " survey question ", qnum)),
        if (!is.null(note)) {
          div(style = "background:#fcf3cf; border-left:4px solid #d4ac0d; padding:6px 8px; margin-bottom:8px; font-size:85%;",
              tags$b("Compare across years with care. "), note)
        } else NULL
      )
    })

    # Column for the comparison year, NA when that wave did not ask it
    cmp_col <- reactive({
      req(input$year_b, input$select)
      lk <- lookups[[input$year_b]]
      if (input$select %in% names(lk)) unname(lk[[input$select]]) else NA_character_
    })

    # Comparison is only offered when both waves asked the question and the
    # wording and scale are close enough for a difference to mean anything
    cmp_state <- reactive({
      if (!isTRUE(input$compare)) return("off")
      if (identical(input$year, input$year_b)) return("same_year")
      if (is.na(cur_col()) || is.na(cmp_col())) return("missing")
      st <- question_compare$status[question_compare$label == input$select]
      if (length(st) && st[1] == "caveat") return("caveat")
      "on"
    })

    observe({
      req(input$year, input$select)
      cfg <- cur_cfg()
      col <- cur_col()

      # ---- difference view ----
      if (cmp_state() %in% c("on", "caveat")) {
        cfg_b <- cfgs[[input$year_b]]
        a <- sf::st_drop_geometry(cfg$sf)[, c("COUNTY", col)]
        b <- sf::st_drop_geometry(cfg_b$sf)[, c("COUNTY", cmp_col())]
        names(a) <- c("COUNTY", "a"); names(b) <- c("COUNTY", "b")
        d <- merge(a, b, by = "COUNTY")
        d$diff <- d$a - d$b
        shp <- merge(cfg$sf[, c("COUNTY", "NAME", "geometry")], d, by = "COUNTY")

        rng <- max(abs(shp$diff), na.rm = TRUE)
        rng <- if (!is.finite(rng) || rng == 0) 1 else rng
        brks <- pretty(c(-rng, rng), n = 6)
        if (length(brks) < 3) brks <- c(-rng, 0, rng)
        # RdBu runs red to blue, so positive change lands on blue, matching the caption
        pal <- colorBin("RdBu", domain = shp$diff, bins = brks)

        lbl <- lapply(paste0(
          "<b>", shp$NAME, " County</b><br/>",
          input$year, ": ", format(shp$a, trim = TRUE), "<br/>",
          input$year_b, ": ", format(shp$b, trim = TRUE), "<br/>",
          "<b>change: ", sprintf("%+.2f", shp$diff), "</b>"), htmltools::HTML)

        leafletProxy("map", data = shp) %>%
          clearGroup("choropleth") %>% clearControls() %>% clearPopups() %>%
          addPolygons(
            group = "choropleth",
            fillColor = pal(shp$diff), weight = 0, opacity = 1, color = "white",
            label = lbl, layerId = shp$NAME, fillOpacity = 0.75, dashArray = "3",
            highlight = highlightOptions(weight = 2, color = "red", fillOpacity = 0.7, bringToFront = FALSE)
          ) %>%
          addLegend(
            pal = pal, values = shp$diff, opacity = 0.75, position = "bottomright",
            title = paste0("Change<br/>", input$year, " minus ", input$year_b)
          )

        draw_state(list(
          mode = "diff", shp = shp, value = shp$diff, bins = brks,
          labels = NULL, palette = "RdBu", reverse = FALSE,
          legend = paste0("Change: ", input$year, " minus ", input$year_b),
          title = input$select,
          subtitle = paste0(input$year, " minus ", input$year_b,
                            " - West Coast Fisheries Participation Survey")))
        return(invisible(NULL))
      }

      # Not asked this wave: show the counties in neutral grey rather than a
      # stale choropleth, and let the note above explain why
      if (is.na(col)) {
        question_img(NULL)
        draw_state(NULL)
        leafletProxy("map", data = cfg$sf) %>%
          clearGroup("choropleth") %>% clearControls() %>% clearPopups() %>%
          addPolygons(
            group = "choropleth",
            fillColor = "#d9d9d9", weight = 0, opacity = 1, color = "white",
            label = paste0(cfg$sf$NAME, " County - question not asked in ", input$year),
            fillOpacity = 0.6, dashArray = "3"
          )
        return(invisible(NULL))
      }

      data <- cfg$sf %>% select(as.symbol(col), NAME, geometry)
      prefix <- str_extract(as.character(col), "[^ ]+")
      sel <- cfg$selecter(prefix)

      if (sel %in% names(cfg$label_map)) {
        bins <- cfg$label_map[[sel]]$bins
        labels <- cfg$label_map[[sel]]$labels
      } else {
        extra <- cfg$extra_bins(sel)
        if (!is.null(extra)) {
          bins <- extra$bins
          labels <- extra$labels
        } else {
          bins <- cfg$default_bins$bins
          labels <- cfg$default_bins$labels
        }
      }

      react_leg <- if (sel %in% cfg$yn) {
        "Percent Responding Yes"
      } else if (!is.na(cfg$legend$prefer) && sel == cfg$legend$prefer) {
        "Percent Prefering Fishing"
      } else if (!is.na(cfg$legend$na) && sel == cfg$legend$na) {
        "Percent Responding NA"
      } else if (!is.na(cfg$legend$mean) && sel == cfg$legend$mean) {
        "Displaying Mean"
      } else if (sel == "Res") {
        "Count"
      } else {
        "Displaying Median"
      }

      pal <- colorBin("YlOrRd", domain = as.numeric(data[[col]]), bins = bins)

      leafletProxy("map", data = data) %>%
        clearGroup("choropleth") %>%
        addPolygons(
          group = "choropleth",
          fillColor = pal(as.numeric(data[[col]])),
          weight = 0.0,
          opacity = 1,
          color = "white",
          label = hover_label(cfg$sf, col, input$select),
          layerId = cfg$sf$NAME,
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
          values = as.numeric(data[[col]]),
          opacity = 0.7,
          title = react_leg,
          position = "bottomright",
          labFormat = function(type, cuts, p) {
            paste0(labels)
          }
        )

      draw_state(list(
        mode = "single", shp = data, value = as.numeric(data[[col]]), bins = bins,
        labels = labels, palette = "YlOrRd", reverse = FALSE,
        legend = react_leg, title = input$select,
        subtitle = paste0(input$year, " West Coast Fisheries Participation Survey")))

      question_img(question_image(cfg, sel))
    })

    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      cfg <- cur_cfg()
      yr <- cfg$year
      map_df <- cfg$df
      census <- c(
        pop = paste("Population Estimate", yr), inc = paste("Median Income", yr),
        grad = paste("Percent College Grad", yr), unemp = paste("Unemployement Rate", yr)
      )

      sub <- map_df[map_df$NAME == click$id, c(
        "NAME", census[["pop"]], census[["inc"]], census[["grad"]], census[["unemp"]],
        "Rural-urban Continuum", "Respondent Count"
      )]
      if (!nrow(sub)) {
        return(invisible(NULL))
      }

      popformat <- paste("<center><b>", sub$NAME, " County", "</b></center>",
        "Survey year: ", yr, "<br>",
        "Responent Count: ", sub$`Respondent Count`, "<br>",
        "Median income: ", format(sub[[census[["inc"]]]], big.mark = ","), "<br>",
        "Population: ", format(sub[[census[["pop"]]]], big.mark = ","), "<br>",
        "Urban to rural: ", sub$`Rural-urban Continuum`, "<br>",
        "College grad: ", (sub[[census[["grad"]]]]) * 100, "%", "<br>",
        "Unemployment: ", (sub[[census[["unemp"]]]]) * 100, "%", "<br>",
        sep = ""
      )

      leafletProxy("map") %>%
        addPopups(
          lng = click$lng, lat = click$lat, popup = popformat,
          options = popupOptions(closeOnClick = F)
        )
    })

    output$downloadMap <- downloadHandler(
      filename = function() {
        paste0("fisheries_participation_map_", input$year, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        st <- draw_state()
        p <- map_plot(st, counties_all_sf)
        ggplot2::ggsave(file, p, width = 7, height = 8, dpi = 150, bg = "white")
      }
    )

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("county_", substr(cur_cfg()$year, 3, 4), "_data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(cur_cfg()$df, file)
      }
    )
  })
}
