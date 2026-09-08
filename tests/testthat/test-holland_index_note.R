test_that("holland_index_note describes each index and falls back gracefully", {
  for (lab in c("Holland index: Fisher identity", "Holland index: Social capital",
                "Holland index: Job quality", "Holland index: Livelihood satisfaction")) {
    note <- holland_index_note(lab)
    expect_s3_class(note, "shiny.tag")
    txt <- as.character(note)
    expect_true(grepl("Holland, Abbott", txt))
    expect_false(grepl("its survey items", txt))   # every real label has its own item list
  }
  expect_true(grepl("its survey items", as.character(holland_index_note("Holland index: Nonsense"))))
})

test_that("every wave routes a Holland index to the shared SD bins", {
  cfg <- map_config()
  for (w in names(cfg)) {
    sel <- cfg[[w]]$selecter("Holland")
    expect_equal(sel, "Hol", info = w)
    expect_equal(cfg[[w]]$label_map[[sel]]$bins, c(-3, -0.5, -0.2, 0.2, 0.5, 3), info = w)
  }
})
