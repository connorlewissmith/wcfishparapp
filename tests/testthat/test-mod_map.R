# Tests for the shared map module.
#
# Several of these guard defects that were live in the four per-year modules
# this replaced, so they are worth keeping rather than trimming.

test_that("map ui builds", {
  ui <- mod_map_ui(id = "test")
  golem::expect_shinytaglist(ui)
  expect_true("id" %in% names(formals(mod_map_ui)))
})

test_that("every wave has a complete config", {
  cfg <- map_config()
  expect_setequal(names(cfg), c("2017", "2020", "2023", "2026"))
  for (n in names(cfg)) {
    cc <- cfg[[n]]
    expect_true(all(c("year", "sf", "df", "choices", "pal_count", "yn", "legend",
                      "selecter", "extra_bins", "default_bins", "breaks",
                      "label_map", "images") %in% names(cc)))
    expect_gt(nrow(cc$sf), 0)
    expect_gt(length(cc$label_map), 0)
  }
})

test_that("selector labels resolve to a column in the wave that asked them", {
  lk <- list("2017" = question_lookup_17, "2020" = question_lookup_20,
             "2023" = question_lookup_23, "2026" = question_lookup_26)
  cfg <- map_config()
  for (y in names(lk)) {
    expect_true(all(unname(lk[[y]]) %in% names(cfg[[y]]$sf)),
                info = paste("wave", y))
  }
  # every label offered in the selector exists in at least one wave
  labs <- unlist(question_choices_all, use.names = FALSE)
  expect_true(all(vapply(labs, function(l)
    any(vapply(lk, function(k) l %in% names(k), logical(1))), logical(1))))
})

test_that("discrete questions use half point bin edges", {
  # Bin edges sitting on the values put every ordinal map one category low.
  # On 2026 Q4 that affected 41 of 44 counties.
  for (cc in map_config()) {
    for (entry in cc$label_map) {
      b <- entry$bins
      if (length(b) > 1 && all(abs(diff(b) - 1) < 1e-9)) {
        expect_false(all(abs(b - round(b)) < 1e-9),
                     label = "integer bin edges on a discrete question")
      }
    }
  }
})

test_that("legend labels line up with the colour bands", {
  for (cc in map_config()) {
    for (entry in cc$label_map) {
      expect_equal(length(entry$labels), length(entry$bins) - 1)
    }
  }
})

test_that("question stays selected across a year change", {
  testServer(mod_map_server, {
    session$setInputs(select = "Captains own vessel", year = "2026", compare = FALSE)
    session$setInputs(year = "2017")
    expect_equal(input$select, "Captains own vessel")
  })
})

test_that("a wave that did not ask a question says so", {
  testServer(mod_map_server, {
    # underserved communities was asked in 2023 only
    session$setInputs(select = "Underserved community", year = "2026", compare = FALSE)
    html <- as.character(output$question_note$html)
    expect_match(html, "Not asked in the 2026 survey")
    expect_null(draw_state())
  })
})

test_that("questions that are not safely comparable carry a warning", {
  flagged <- question_compare$label[question_compare$status == "caveat" &
                                      question_compare$n_waves == 4]
  expect_gt(length(flagged), 0)
  testServer(mod_map_server, {
    for (lab in flagged) {
      session$setInputs(select = lab, year = "2026", compare = FALSE)
      expect_match(as.character(output$question_note$html),
                   "Compare across years with care", info = lab)
    }
  })
})

test_that("comparison refuses when it would be meaningless", {
  testServer(mod_map_server, {
    session$setInputs(select = "Captains own vessel", year = "2026", compare = TRUE, year_b = "2026")
    expect_match(as.character(output$question_note$html), "Comparing a year with itself")

    session$setInputs(select = "Underserved community", year = "2023", year_b = "2026")
    expect_match(as.character(output$question_note$html), "Cannot compare")
  })
})

test_that("the map export builds for every question", {
  testServer(mod_map_server, {
    session$setInputs(year = "2026", compare = FALSE)
    for (lab in names(question_lookup_26)) {
      session$setInputs(select = lab)
      expect_no_error(ggplot2::ggplot_build(map_plot(draw_state(), counties_all_sf)))
    }
  })
})
