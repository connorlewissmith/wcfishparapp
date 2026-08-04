# 04_snapshot_map.R
#
# Capture what the map actually renders for every question in every wave, so a
# refactor can be proved not to have changed anything.
#
# It works by swapping colorBin and addLegend inside the package's imports
# environment, so the values come from the module itself rather than from a
# reimplementation of its logic. That distinction matters: an earlier attempt
# to verify by reimplementing the bin rules missed 56 questions whose bins came
# from the fallback chain rather than the label map.
#
#   Rscript dev/04_snapshot_map.R before.rds     # on the current code
#   ... make changes ...
#   Rscript dev/04_snapshot_map.R after.rds
#   Rscript dev/04_snapshot_map.R before.rds after.rds   # compare
#
# A clean refactor produces no differences at all.

suppressMessages({
  library(shiny); library(sf); library(dplyr)
  library(stringr); library(leaflet)
})

args <- commandArgs(trailingOnly = TRUE)

# ---- compare mode ----------------------------------------------------------
if (length(args) == 2 && all(file.exists(args))) {
  a <- readRDS(args[1]); b <- readRDS(args[2])
  rownames(a) <- NULL; rownames(b) <- NULL
  stopifnot(identical(paste(a$wave, a$question), paste(b$wave, b$question)))
  ok <- TRUE
  for (f in c("bins", "legend_title", "legend_labels", "image")) {
    d <- which(a[[f]] != b[[f]] | (is.na(a[[f]]) != is.na(b[[f]])))
    cat(sprintf("  %-14s identical: %-5s (%d differing)\n", f, length(d) == 0, length(d)))
    if (length(d)) {
      ok <- FALSE
      for (i in head(d, 8)) {
        cat(sprintf("      %s  %s\n        was: %s\n        now: %s\n",
                    a$wave[i], substr(a$question[i], 1, 46), a[[f]][i], b[[f]][i]))
      }
    }
  }
  cat(sprintf("\n  %s\n", if (ok) "NO CHANGE" else "DIFFERENCES FOUND"))
  quit(status = if (ok) 0 else 1)
}

out_file <- args[1]
if (is.na(out_file)) stop("usage: Rscript dev/04_snapshot_map.R <out.rds> [<other.rds>]")

suppressWarnings(suppressMessages(
  pkgload::load_all(".", export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)))

R <- new.env()
R$wave <- NA; R$bins <- NA_character_
R$title <- NA_character_; R$labels <- NA_character_; R$rows <- list()

o_cb <- leaflet::colorBin; o_al <- leaflet::addLegend
imp <- parent.env(asNamespace("wcfishparapp"))

unlockBinding("colorBin", imp)
assign("colorBin", function(palette, domain, bins = 7, ...) {
  R$bins <- if (is.numeric(bins)) paste(bins, collapse = ",") else as.character(bins)
  o_cb(palette, domain, bins = bins, ...)
}, envir = imp)

unlockBinding("addLegend", imp)
assign("addLegend", function(map, ..., title = NULL, labFormat = NULL) {
  R$title  <- if (is.null(title)) NA_character_ else as.character(title)
  R$labels <- tryCatch(paste(labFormat("bin", NULL, NULL), collapse = "|"),
                       error = function(x) NA_character_)
  a <- list(map, ...)
  if (!is.null(title)) a$title <- title
  if (!is.null(labFormat)) a$labFormat <- labFormat
  do.call(o_al, a)
}, envir = imp)

lookups <- list("2017" = question_lookup_17, "2020" = question_lookup_20,
                "2023" = question_lookup_23, "2026" = question_lookup_26)

# Accumulate into an environment: plain assignment inside testServer is local.
record <- function(col, img) {
  R$rows[[length(R$rows) + 1]] <- data.frame(
    wave = R$wave, question = col, bins = R$bins,
    legend_title = R$title, legend_labels = R$labels,
    image = if (is.null(img)) NA_character_ else img$src,
    stringsAsFactors = FALSE)
}

shiny::testServer(mod_map_server, {
  for (y in names(lookups)) {
    R$wave <- y
    lk <- lookups[[y]]
    for (lab in names(lk)) {
      R$bins <- NA_character_; R$title <- NA_character_; R$labels <- NA_character_
      suppressWarnings(session$setInputs(year = y, select = lab, compare = FALSE))
      record(unname(lk[[lab]]), tryCatch(question_img(), error = function(x) NULL))
    }
    cat(sprintf("  %s: %d questions\n", y, length(lk)))
  }
})

res <- do.call(rbind, R$rows)
res <- res[order(res$wave, res$question), ]
saveRDS(res, out_file)
cat(sprintf("\nwrote %s : %d rows\n", out_file, nrow(res)))
cat(sprintf("  bins %d | titles %d | labels %d | images %d\n",
            sum(!is.na(res$bins)), sum(!is.na(res$legend_title)),
            sum(!is.na(res$legend_labels)), sum(!is.na(res$image))))
