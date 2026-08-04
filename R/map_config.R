#' Per wave configuration for the shared map module
#'
#' One entry per survey wave. The drawing logic lives in mod_map.R; everything
#' that differs between waves lives here. The label_map and image tables are
#' copied verbatim from the old per year modules, including their references to
#' the label vectors built in 06_prep_app_data.R.
#'
#' @noRd
map_config <- function() {
  list(
    `2017` = list(
      year = "2017",
      sf = county_17_sf,
      df = county_17_df,
      choices = question_choices_17,
      pal_count = palrepcount_17,
      yn = par17_yn,
      legend = list(prefer = "Q19", na = NA_character_, mean = NA_character_),
      selecter = 
        function(p) if (grepl("Q12", p)) substr(p, 1, 6) else substr(p, 1, 3)
,
      extra_bins = 
        function(sel) {
              if (grepl("Q12", sel)) list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q12label) else NULL
            }
,
      default_bins = list(bins = c(0, .25, .50, .75, 1),
                          labels = c("0-25%", "25-50%", "50-75%", "75-100%")),
      breaks =
        list(list(keys = Q12_break,  src = "www/20q121.png", height = 350, width = 600),
                 list(keys = Q12_break2, src = "www/20q122.png", height = 350, width = 600))
,
      label_map =
        list(
        "Q1:" = list(bins = c(45, 50, 55, 60, 65, 70, 75), labels = c("45-50", "50-55", "55-60", "60-65", "65-70", "70-75")), # Top bin added, one county sits at 73.5
        "Q2:" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q2label),
        "Q3:" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q3label),
        "Q4:" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5), labels = Q4label),
        "Q7:" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5), labels = c("0", "1", "2", "3", "4", "5", "6")),
        "Q8:" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q8label),
        "Q9:" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q11" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q11label),
        "Q12" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q12label),
        "Q13" = list(bins = c(0, .25, .50, .75, 1), labels = c("0-25%","25-50%","50-75%","75-100%")),
        "Q20" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5), labels = Q20label),
        "Q26" = list(bins = c(0.5, 1.5, 2.5, 3.5), labels = Q26label),
        "Q31" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5), labels = Q31label),
        "Q32" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5), labels = Q32label),
        "Q6:" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q33" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q22" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q28" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")),
        "Q29" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")),
        "Q25" = list(bins = c(0, 50, 100, 150, 200, 250, 300, 501), labels = c("0-50lbs", "50-100lbs", "100-150lbs", "150-200lbs", "200-250lbs", "250-300lbs", "300-500lbs")),
        "Q34" = list(bins = c(0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c("50-60%", "60-70%", "70-80%", "80-90%", "90-100%")),
        "Q19" = list(bins = c(0.6, 0.7, 0.8, 0.9, 1), labels = c("60-70%", "70-80%", "80-90%", "90-100%")),
        "Res" = list(bins = c(0, 10, 20, 30, 40, 50, 60, 70, 80), labels = c("0 - 10", "20 - 20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", "70 - 80"))
        )
,
      images =
        data.frame(
        selecter = c(
          "Q1:", "Q2:", "Q3:", "Q4:", "Q6:", "Q7:", "Q8:", "Q9:", "Q11",
          "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21",
          "Q22", "Q23", "Q24", "Q25", "Q26", "Q28", "Q29", "Q31", "Q32",
          "Q33", "Q34", "Res"
        ),
        height = c(
          75, 200, 250, 100, 75, 75, 200, 75, 350, 350, 100, 250, 200,
          300, 300, 150, 250, 250, 100, 150, 100, 100, 300, 100, 100, 400,
          300, 75, 75, 100
        ),
        width = c(
          400, 500, 500, 500, 500, 500, 500, 500, 600, 600, 500, 400, 400, 500,
          500, 400, 400, 400, 400, 400, 400, 400, 500, 500, 500, 400, 500, 400,
          400, 500
        ),
        src = c(
          "www/20q1.png", "www/20q2.png", "www/20q3.png", "www/17q4.png", "www/17q6.png",
          "www/17q7.png", "www/17q8.png", "www/20q9.png", "www/20q11.png", "www/17q13.png",
          "www/17q14.png", "www/17q15.png", "www/17q16.png", "www/17q17.png", "www/17q18.png",
          "www/17q19.png", "www/17q20.png", "www/17q21.png", "www/17q22.png", "www/17q23.png",
          "www/17q24.png", "www/17q25.png", "www/17q26.png", "www/17q28.png", "www/17q29.png",
          "www/17q31.png", "www/17q32.png", "www/17q33.png", "www/17q34.png", "www/all_res.png"
        )
        )
    ),
    `2020` = list(
      year = "2020",
      sf = county_20_sf,
      df = county_20_df,
      choices = question_choices_20,
      pal_count = palrepcount_20,
      yn = par20_yn,
      legend = list(prefer = "Q17", na = NA_character_, mean = NA_character_),
      selecter = 
        function(p) {
              if (grepl("^Q12", p)) substr(p, 1, 6)
              else if (grepl("^Q27-1", p)) "Q27-1"
              else if (grepl("^Q27-2", p)) "Q27-2"
              else substr(p, 1, 3)
            }
,
      extra_bins = 
        function(sel) {
              if (grepl("Q12", sel)) list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q12label)
              else if (grepl("Q27-2", sel)) list(bins = c(0, 50, 100, 150, 200, 250, 300, 501),
                labels = c("0-50lbs", "50-100lbs", "100-150lbs", "150-200lbs", "200-250lbs", "250-300lbs", "300-500lbs"))
              else NULL
            }
,
      default_bins = list(bins = c(0, .25, .50, .75, 1),
                          labels = c("0-25%", "25-50%", "50-75%", "75-100%")),
      breaks =
        list(list(keys = Q12_break,  src = "www/20q121.png", height = 350, width = 600),
                 list(keys = Q12_break2, src = "www/20q122.png", height = 350, width = 600))
,
      label_map =
        list(
        "Q1:" = list(bins = c(45, 50, 55, 60, 65, 70), labels = c("45-50", "50-55", "55-60", "60-65", "65-70")),
        "Q2:" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q2label_20),
        "Q3:" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q3label_20),
        "Q4:" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5), labels = Q4label_20),
        "Q6:" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q7:" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5), labels = c("0", "1", "2", "3", "4", "5", "6")),
        "Q8:" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q8label_20),
        "Q9:" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q11" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q11label_20),
        "Q12" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q12label),
        "Q13" = list(bins = c(0.5, 1.5, 2.5, 3.5), labels = Q13label_20),
        "Q14" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q11label_20), # Assuming labels same as Q11
        "Q17" = list(bins = c(0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c("50-60%", "60-70%", "70-80%", "80-90%", "90-100%")),
        "Q18" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5), labels = Q18label_20),
        "Q19" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q20" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q11label_20), # Assuming labels same as Q11
        "Q21" = list(bins = c(0, 100, 200, 400, 600, 800, 1000), labels = c("less than 100 nm", "100-200 nm", "200-400 nm", "400-600 nm", "600-800 nm", "800-1000 nm")),
        "Q24" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5), labels = Q24label_20),
        "Q28" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")),
        "Q29" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")),
        "Q32" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5), labels = Q32label_20),
        "Q33" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q34" = list(bins = c(0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c("50-60%", "60-70%", "70-80%", "80-90%", "90-100%")),
        "Res" = list(bins = c(0, 10, 20, 30, 40, 50, 60, 70, 80), labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80"))
        )
,
      images =
        data.frame(
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
    ),
    `2023` = list(
      year = "2023",
      sf = county_23_sf,
      df = county_23_df,
      choices = question_choices_23,
      pal_count = palrepcount_23,
      yn = par23_yn,
      legend = list(prefer = "Q17", na = "Q15_3", mean = "Q22-5"),
      selecter = 
        function(p) {
              if (grepl("^Q13", p)) substr(p, 1, 6)
              else if (grepl("^Q15_1", p)) "Q15_1"
              else if (grepl("^Q15_2", p)) "Q15_2"
              else if (grepl("^Q15_3", p)) "Q15_3"
              else if (grepl("^Q22_3", p)) "Q22_3"
              else if (grepl("^Q25-1", p)) "Q25-1"
              else if (grepl("^Q25-2", p)) "Q25-2"
              else substr(p, 1, 3)
            }
,
      extra_bins = 
        function(sel) {
              # Q12label, not Q13label_23, is what the 2023 module has always used here
              if (grepl("Q13", sel)) list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q12label)
              else if (sel %in% c("Q15_1", "Q15_2")) list(bins = c(0, 10, 20, 30, 50, 75, 100, 2000),
                labels = c("less than 10 miles", "10-20 miles", "20-30 miles", "30-50 miles", "50-75 miles", "75-100 miles", "over 100 miles"))
              else if (sel == "Q15_3") list(bins = c(0, .25, .50, .75, 1), labels = c("0-25%", "25-50%", "50-75%", "75-100%"))
              else if (sel == "Q22_3") list(bins = c(0, 20, 40, 60, 80, 100, 150, 500),
                labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%", "100-150%", "over 150%"))
              else if (grepl("Q25-1", sel)) list(bins = c(0, .25, .50, .75, 1), labels = c("0-25%", "25-50%", "50-75%", "75-100%"))
              else if (grepl("Q25-2", sel)) list(bins = c(0, 50, 100, 150, 200, 250, 300, 501),
                labels = c("0-50lbs", "50-100lbs", "100-150lbs", "150-200lbs", "200-250lbs", "250-300lbs", "300-500lbs"))
              else NULL
            }
,
      default_bins = list(bins = c(0, .25, .50, .75, 1),
                          labels = c("0-25%", "25-50%", "50-75%", "75-100%")),
      breaks =
        list(list(keys = Q13_break,  src = "www/23q131.png", height = 350, width = 600),
                 list(keys = Q13_break2, src = "www/23q132.png", height = 350, width = 600))
,
      label_map =
        list(
        "Q1:" = list(bins = c(45, 50, 55, 60, 65, 70), labels = c("45-50", "50-55", "55-60", "60-65", "65-70")),
        "Q2:" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q2label_23),
        "Q3:" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5), labels = Q3label_23),
        "Q4:" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5), labels = Q4label_23),
        "Q7:" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q8:" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5), labels = c("0", "1", "2", "3", "4", "5", "6")), # Formally Q7
        "Q9:" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q9label_23),
        "Q10" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("0", "1", "2", "3", "4", "5")), # Formally Q9
        "Q12" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q12label_23),
        "Q13" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q12label),
        "Q14" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q14label_23),
        "Q17" = list(bins = c(0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c("50-60%", "60-70%", "70-80%", "80-90%", "90-100%")),
        "Q18" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5), labels = Q18label_23),
        "Q19" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q20" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q20label_23),
        "Q22" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")),
        "Q23" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("Very unlikely", "Unlikely", "Neutral", "Likely", "Very likely")),
        "Q26" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")),
        "Q27" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")),
        "Q30" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("0", "1", "2", "3", "4", "5")),
        "Q31" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5), labels = Q31label_23),
        "Res" = list(bins = c(0, 10, 20, 30, 40, 50, 60, 70, 80), labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80"))
        )
,
      images =
        data.frame(
        selecter = c(
        "Q1:", "Q2:", "Q3:", "Q4:", "Q7:", "Q8:", "Q9:", "Q10", "Q12",
        "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21",
        "Q22", "Q23", "Q24", "Q25-1", "Q26", "Q28", "Q29", "Q31", "Q32",
        "Q33", "Q34", "Q27-1", "Q27-2", "Q15_1", "Q15_2", "Q15_3", "Q22_3",
        "Q27", "Q25-2", "Q30", "Res"
        ),
        height = c(
        75, 200, 250, 100, 75, 75, 200, 75, 350, 300, 250, 300, 100, 200,
        250, 100, 200, 200, 175, 225, 100, 150, 100, 75, 200, 200, 75, 300,
        300, 150, 150, 300, 300, 300, 175, 100, 150, 75, 100
        ),
        width = c(
        400, 500, 500, 500, 500, 500, 500, 500, 600, 550, 500, 500, 500, 400,
        400, 400, 400, 500, 500, 500, 400, 500, 400, 500, 400, 400, 400, 500,
        500, 400, 400, 500, 500, 500, 500, 400, 500, 400, 500
        ),
        src = c(
        "www/20q1.png", "www/20q2.png", "www/20q3.png", "www/17q4.png", "www/23q7.png",
        "www/23q8.png", "www/23q9.png", "www/23q10.png", "www/23q12.png", "www/20q13.png",
        "www/23q14.png", "www/23q15.png", "www/23q16.png", "www/20q17.png", "www/20q18.png",
        "www/20q19.png", "www/20q20.png", "www/23q21.png", "www/23q22.png", "www/23q23.png",
        "www/23q24.png", "www/23q25.png", "www/23q26.png", "www/23q28.png", "www/23q29.png",
        "www/23q31.png", "www/23q32.png", "www/23q33.png", "www/20q34.png", "www/20q27.png",
        "www/20q27.png", "www/23q15.png", "www/23q15.png", "www/23q15.png", "www/23q22.png",
        "www/23q27.png", "www/23q25.png", "www/23q30.png", "www/all_res.png"
        ),
        stringsAsFactors = FALSE
        )
    ),
    `2026` = list(
      year = "2026",
      sf = county_26_sf,
      df = county_26_df,
      choices = question_choices_26,
      pal_count = palrepcount_26,
      yn = par26_yn,
      legend = list(prefer = "Q16", na = "Q14_3", mean = "Q21-5"),
      selecter = 
        function(p) {
              if (grepl("^Q12", p)) substr(p, 1, 6)
              else if (grepl("^Q14_1", p)) "Q14_1"
              else if (grepl("^Q14_2", p)) "Q14_2"
              else if (grepl("^Q14_3", p)) "Q14_3"
              else if (grepl("^Q21_3", p)) "Q21_3"
              else if (grepl("^Q24-1", p)) "Q24-1"
              else if (grepl("^Q24-2", p)) "Q24-2"
              else substr(p, 1, 3)
            }
,
      extra_bins = 
        function(sel) {
              if (grepl("Q12", sel)) list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q12label_26)
              else if (sel %in% c("Q14_1", "Q14_2")) list(bins = c(0, 10, 20, 30, 50, 75, 100, 2000),
                labels = c("less than 10 miles", "10-20 miles", "20-30 miles", "30-50 miles", "50-75 miles", "75-100 miles", "over 100 miles"))
              else if (sel == "Q14_3") list(bins = c(0, .25, .50, .75, 1), labels = c("0-25%", "25-50%", "50-75%", "75-100%"))
              else if (sel == "Q21_3") list(bins = c(0, 20, 40, 60, 80, 100, 150, 500),
                labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%", "100-150%", "over 150%"))
              else if (grepl("Q24-1", sel)) list(bins = c(0, .25, .50, .75, 1), labels = c("0-25%", "25-50%", "50-75%", "75-100%"))
              else if (grepl("Q24-2", sel)) list(bins = c(0, 50, 100, 150, 200, 250, 300, 1300),
                labels = c("0-50lbs", "50-100lbs", "100-150lbs", "150-200lbs", "200-250lbs", "250-300lbs", "over 300lbs"))
              else NULL
            }
,
      default_bins = list(bins = c(0, .25, .50, .75, 1),
                          labels = c("0-25%", "25-50%", "50-75%", "75-100%")),
      breaks =
        list(list(keys = Q12_break_26,  src = "www/23q131.png", height = 350, width = 600),
                 list(keys = Q12_break2_26, src = "www/23q132.png", height = 350, width = 600))
,
      label_map =
        list(
        "Q1:" = list(bins = c(45, 50, 55, 60, 65, 70, 75, 80), labels = c("45-50", "50-55", "55-60", "60-65", "65-70", "70-75", "75-80")),
        "Q2:" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q2label_26),
        "Q3:" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q3label_26),
        "Q4:" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5), labels = Q4label_26),
        "Q6:" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("0", "1", "2", "3", "4", "5")), # Formally Q7
        "Q7:" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5), labels = c("0", "1", "2", "3", "4", "5", "6")), # Formally Q8
        "Q8:" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q8label_26), # Formally Q9
        "Q9:" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("0", "1", "2", "3", "4", "5")), # Formally Q10
        "Q11" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q11label_26), # Formally Q12
        "Q12" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q12label_26), # Formally Q13
        "Q13" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q13label_26), # Formally Q14
        "Q16" = list(bins = c(0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c("50-60%", "60-70%", "70-80%", "80-90%", "90-100%")), # Formally Q17
        "Q17" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5), labels = Q17label_26), # Formally Q18
        "Q18" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("0", "1", "2", "3", "4", "5")), # Formally Q19
        "Q19" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = Q19label_26), # Formally Q20
        "Q21" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")), # Formally Q22
        "Q22" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("Very unlikely", "Unlikely", "Neutral", "Likely", "Very likely")), # Formally Q23
        "Q25" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")), # Formally Q26
        "Q26" = list(bins = c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")), # Formally Q27
        "Q29" = list(bins = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels = c("0", "1", "2", "3", "4", "5")), # Formally Q30
        "Q30" = list(bins = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5), labels = Q30label_26), # Formally Q31
        "Res" = list(bins = c(0, 10, 20, 30, 40, 50, 60, 70, 80), labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80"))
        )
,
      images =
        data.frame(
        selecter = c(
        "Q1:", "Q2:", "Q3:", "Q4:", "Q6:", "Q7:", "Q8:", "Q9:", "Q11",
        "Q13", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22",
        "Q23", "Q24-1", "Q24-2", "Q25", "Q26", "Q28", "Q29", "Q30", "Q31",
        "Q14_1", "Q14_2", "Q14_3", "Q21_3", "Res"
        ),
        height = c(
        75, 200, 250, 100, 75, 75, 200, 75, 350,
        250, 100, 200, 250, 100, 200, 200, 175, 225,
        100, 150, 150, 100, 100, 200, 75, 200, 75,
        300, 300, 300, 175, 100
        ),
        width = c(
        400, 500, 500, 500, 500, 500, 500, 500, 600,
        500, 500, 400, 400, 400, 400, 500, 500, 500,
        400, 500, 500, 400, 400, 400, 400, 400, 400,
        500, 500, 500, 500, 500
        ),
        src = c(
        "www/20q1.png", "www/20q2.png", "www/26q3.png", "www/17q4.png", "www/23q7.png",
        "www/23q8.png", "www/23q9.png", "www/23q10.png", "www/23q12.png",
        "www/23q14.png", "www/23q16.png", "www/20q17.png", "www/20q18.png", "www/20q19.png",
        "www/20q20.png", "www/23q21.png", "www/23q22.png", "www/23q23.png",
        "www/23q24.png", "www/23q25.png", "www/23q25.png", "www/23q26.png", "www/23q27.png",
        "www/23q29.png", "www/23q30.png", "www/23q31.png", "www/23q32.png",
        "www/23q15.png", "www/23q15.png", "www/23q15.png", "www/23q22.png", "www/all_res.png"
        ),
        stringsAsFactors = FALSE
        )
    )
  )
}

