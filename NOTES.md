# Working notes

Picking-up notes for the county map app. Last substantial work: August 2026,
adding the 2026 wave and consolidating the map.

---

## Data refresh 2026-08-28

`data/fish_par_app_data.RData` rebuilt with the 14 late 2026 paper returns
(`fish_par_survey` now reads `fish_par_26_with_late.xlsx`; 2026 n = 940). 12 of the
14 land on the map (860 respondents across the same 44 counties; no county crossed
the 4-respondent threshold). `dev/04_snapshot_map.R` shows no change because it
snapshots bins/legends/crops, not county values. Needs a redeploy to Connect.

## Holland indices added 2026-08-28

Four new map questions under the group "Holland Indices": county means of the
pooled four-wave latent scores (see README "Data"). Shared bins ±0.2/±0.5 SD;
legend "County mean, SD units (pooled four-wave scale)"; modal shows a text
description instead of a crop; About tab has a section. Snapshot harness
now 313 rows (297 + 4 indices x 4 waves). `tests/testthat/test-holland_index_note.R`
covers the routing. Needs a Connect redeploy.

## Open items, roughly in priority order

### 1. Confirm the R version on the deployment server — possible live breakage

`manifest.json` now pins **R 4.6.0** (it was 4.4.1). It was regenerated because
the old manifest listed three deleted modules and omitted `R/mod_map.R`
entirely, so a git-backed deploy would have built an app with no map.

If Connect is git-backed off this repo, the last push may already have
triggered a rebuild. **If Connect does not have R 4.6.0, that deploy fails at
package restore.** A failed deploy leaves the existing content serving, so
nothing breaks for users, but nothing updates either.

- Check the Connect deploy logs.
- Ask IT which R versions are available.
- If it is still 4.4.x, regenerate the manifest under a 4.4 install and push.

Also note `rsconnect/` holds a **shinyapps.io** record (`clewis-smith`,
appId 10377706) from Nov 2023 — that is the personal scratch copy, not the
NOAA deployment. `rsconnect/` is gitignored, so it does not travel.

### 2. Dan has not seen the interface change

The four year tabs are gone, replaced by one **Map** tab with a year control
inside it. That is the most visible change here and the most likely to come
back. It is deliberately isolated in commit `8aa4ec0`, so `git revert` puts
the year tabs back without touching the correctness fixes.

Worth naming when you show him: the tool used to answer *"what does this county
look like?"* and now answers *"what changed here?"*. That is a different
product, not just a layout tweak.

### 3. Things only a human can check

- **Map export PNG.** Server-side ggplot render, verified headlessly, but nobody
  has clicked the button in a browser.
- **The question modal.** Opens the survey crop at natural size. The old
  floating panel was 250px wide while every crop is 350–600px, so all 133
  overflowed. Worth confirming the battery crops (600×350) look right.
- **About tab.** Now four short sections. The Introduction and Purpose
  paragraphs arguably say the same thing twice.

---

## What changed, and why it matters

### Corrections to what the maps were showing

These were live and wrong, not cosmetic:

- **Ordinal questions were labelled one category low.** Bin edges sat on the
  values, so a county whose median was "Always" displayed as "Mostly". On 2026
  Q4 that was 41 of 44 counties. Edges are now at half points.
- **The contacts question was reversed in 2017 and 2020.** Its raw coding
  flipped between 2020 and 2023; the app assumed one orientation throughout.
  Evidence: linking the same vessel across waves, 2020 and 2023 answers mirror
  59% of the time against a 3% mirror rate on the stable Q4 control.
- **An observer wiped the map and never repainted it.** It called the palette
  with no argument, which errors, but only after `clearShapes()` had been sent.
  It raced the real draw. This was the intermittent "colours don't work" bug.
- **2017 age bins stopped at 70** while one county sits at 73.5, so it rendered
  grey.
- **`Q3label_23` was missing its seventh entry**, leaving six counties with a
  value that had no legend label.

### Structure

Four near-identical per-year modules became `R/mod_map.R` (drawing logic, once)
plus `R/map_config.R` (everything that differs by wave). 1,499 lines to 661.

The driver was maintenance, not tidiness: the same defect had to be fixed in
four places three separate times in one week.

`map_config.R` holds the label maps and image tables **as source text lifted
from the old modules**, so their references to the label vectors built in
`06_prep_app_data.R` still resolve. Do not replace those with evaluated values
— that silently breaks the link to the data pipeline.

### Interface

- Questions are listed by wording, not number, because the survey renumbers
  most of them between waves. The wave's own number shows underneath.
- The selector value is the **wording**, not the column name. That is what lets
  a question survive a year change. `question_lookup_YY` maps wording back to
  the column for the chosen year, returning `NA` when a wave did not ask it.
- Warnings come from `question_compare` in the app data: which waves asked a
  question, and whether the years are safely comparable.
- Difference view, gated: refuses when a wave lacks the question, warns when
  the two years are not measured the same way.

---

## Testing

`tests/testthat/test-mod_map.R` covers the module and deliberately locks in the
defects above — the half-point bin rule and the legend/band alignment are
regression guards, not style checks.

`dev/04_snapshot_map.R` captures what the map actually renders (bins, legend
title, legend labels, question image) for all 297 question-and-wave
combinations. Use it around any refactor:

```
Rscript dev/04_snapshot_map.R before.rds
# ... make changes ...
Rscript dev/04_snapshot_map.R after.rds
Rscript dev/04_snapshot_map.R before.rds after.rds
```

It shims `colorBin` and `addLegend` in the package's imports environment so the
values come from the module rather than a reimplementation. That distinction is
the point: an earlier attempt to verify by reimplementing the bin rules missed
56 questions whose bins came from the fallback chain rather than the label map.

**Two pre-existing failures in `test-golem_utils_ui.R`** are Shiny version
drift — newer Shiny wraps button labels in `<span class="action-label">`. Not
related to this work, but the suite is not green until they are updated.

---

## Data pipeline

App data is built by `code/cleaning/06_prep_app_data.R` in the **fish_par_survey**
repo, then copied here to `data/fish_par_app_data.RData`. There is no automated
link — if the labels or question sets change there, copy the file across.

Counties are dropped when they have **three or fewer respondents**, in the
cleaning scripts. The west coast restriction is separate and later, in
`assign_county_sf.R`. Across all four waves exactly one county outside CA/OR/WA
clears the threshold: Petersburg, Alaska, 2020, n=4. So the geography
restriction is methodological and costs almost nothing.

The map draws all 133 west coast counties underneath, with the dropped ones
labelled **"fewer than 4 respondents, not reported"** — not "not surveyed",
which was wrong, since it mixes genuine zeros with suppressed small samples.

---

## Known rough edges, not addressed

- `test-golem_utils_ui.R` fails on Shiny version drift (above).
- The About tab's Introduction and Purpose sections overlap.
- The map export is a clean static render, not a screenshot of the leaflet
  view: no basemap tiles, and it always shows the full coast rather than the
  current pan and zoom. A true screenshot needs a client-side route and a
  browser to debug.
- Side-by-side comparison was considered and not built. The difference map was
  chosen because it needs no layout change and can be verified without a
  browser. Side-by-side remains straightforward to add.
