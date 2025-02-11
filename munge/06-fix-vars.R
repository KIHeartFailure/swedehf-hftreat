# cut at 3 years
# outcomes
rsdata <- rsdata %>%
  mutate(
    sos_outtime_deathadhere = case_when(
      sos_outtime_death <= 365 ~ NA_real_,
      sos_outtime_death <= global_followup ~ sos_outtime_death - 365,
      TRUE ~ global_followup
    ),
    sos_out_deathcvadhere = case_when(
      is.na(sos_outtime_deathadhere) ~ NA_character_,
      sos_outtime_death <= global_followup ~ as.character(sos_out_deathcv),
      TRUE ~ "No"
    ),
    sos_outtime_hosphfadhere = case_when(
      sos_outtime_hosphfadhere < 0 ~ NA_real_,
      TRUE ~ sos_outtime_hosphfadhere
    ),
    sos_out_hosphfadhere = case_when(
      is.na(sos_outtime_hosphfadhere) ~ NA_character_,
      TRUE ~ as.character(sos_out_hosphfadhere)
    ),
    sos_out_deathcvhosphfadhere = ynfac(case_when(
      is.na(sos_out_deathcvadhere) | is.na(sos_out_hosphfadhere) ~ NA_real_,
      sos_out_deathcvadhere == "Yes" | sos_out_hosphfadhere == "Yes" ~ 1,
      TRUE ~ 0
    )),
    sos_outtime_deathdisc = case_when(
      sos_outtime_death <= 426 ~ NA_real_,
      sos_outtime_death <= global_followup ~ sos_outtime_death - 426,
      TRUE ~ global_followup
    ),
    sos_out_deathcvdisc = case_when(
      is.na(sos_outtime_deathdisc) ~ NA_character_,
      sos_outtime_death <= global_followup ~ as.character(sos_out_deathcv),
      TRUE ~ "No"
    ),
    sos_outtime_hosphfdisc = case_when(
      sos_outtime_hosphfdisc < 0 ~ NA_real_,
      TRUE ~ sos_outtime_hosphfdisc
    ),
    sos_out_hosphfdisc = case_when(
      is.na(sos_outtime_hosphfdisc) ~ NA_character_,
      TRUE ~ as.character(sos_out_hosphfdisc)
    ),
    sos_out_deathcvhosphfdisc = ynfac(case_when(
      is.na(sos_out_deathcvdisc) | is.na(sos_out_hosphfdisc) ~ NA_real_,
      sos_out_deathcvdisc == "Yes" | sos_out_hosphfdisc == "Yes" ~ 1,
      TRUE ~ 0
    ))
  )

rsdata <- cut_surv(rsdata, sos_out_deathcvhosphf, sos_outtime_hosphf, global_followup, cuttime = FALSE, censval = "No")
rsdata <- cut_surv(rsdata, sos_out_hosphf, sos_outtime_hosphf, global_followup, cuttime = TRUE, censval = "No")
rsdata <- cut_surv(rsdata, sos_out_deathcv, sos_outtime_death, global_followup, cuttime = TRUE, censval = "No")

treatvars <- c("sos_lm_rasiarni", paste0("sos_lm_", lmvars %>% filter(!is.na(atc)) %>% pull(var)))

rsdata <- rsdata %>%
  mutate(
    shf_ef = droplevels(shf_ef),
    shf_diabetestype = if_else(shf_diabetes == "No" & !is.na(shf_diabetes), "No", shf_diabetestype),
    shf_indexyear_cat = case_when(
      shf_indexyear <= 2020 ~ "2016-2020",
      shf_indexyear <= 2023 ~ "2021-2023"
    ),
    sos_prevhfh1yr = factor(if_else(sos_timeprevhosphf <= 365 & !is.na(sos_timeprevhosphf) | sos_location == "HF in-patient", 1, 0), levels = 0:1, labels = c("No", "Yes")),
    sos_prevhfh3mo = factor(if_else(sos_timeprevhosphf <= 30.5 * 3 & !is.na(sos_timeprevhosphf) | sos_location == "HF in-patient", 1, 0), levels = 0:1, labels = c("No", "Yes")),
    shf_bpsys_cat = factor(
      case_when(
        shf_bpsys < 100 ~ 2,
        shf_bpsys < 140 ~ 1,
        shf_bpsys >= 140 ~ 3
      ),
      levels = 1:3, labels = c("100-139", "<100", ">=140")
    ),
    shf_bpsys_cateli = factor(
      case_when(
        shf_bpsys < 100 ~ 1,
        shf_bpsys >= 100 ~ 2
      ),
      levels = 1:2, labels = c("<100", ">=100")
    ),
    shf_heartrate_cat = factor(
      case_when(
        shf_heartrate < 50 ~ 2,
        shf_heartrate <= 70 ~ 1,
        shf_heartrate > 70 ~ 3
      ),
      levels = 1:3, labels = c("50-70", "<50", ">70")
    ),
    shf_heartrate_cateli = factor(
      case_when(
        shf_heartrate < 50 ~ 1,
        shf_heartrate >= 50 ~ 2
      ),
      levels = 1:2, labels = c("<50", ">=50")
    ),
    shf_gfrckdepi_cat = factor(
      case_when(
        is.na(shf_gfrckdepi) ~ NA_real_,
        shf_gfrckdepi < 30 ~ 3,
        shf_gfrckdepi < 60 ~ 2,
        shf_gfrckdepi >= 60 ~ 1,
      ),
      levels = 1:3,
      labels = c(">=60", "30-59", "<30")
    ),
    shf_gfrckdepi_cateli = factor(
      case_when(
        shf_gfrckdepi < 30 ~ 1,
        shf_gfrckdepi >= 30 ~ 2
      ),
      levels = 1:2, labels = c("<30", ">=30")
    ),
    shf_potassium_cateli = factor(
      case_when(
        shf_potassium > 5 ~ 1,
        shf_potassium <= 5 ~ 2
      ),
      levels = 1:2, labels = c(">5", "<=5")
    ),
    shf_diabetestypeeli = ynfac(case_when(
      is.na(shf_diabetestype) ~ NA_real_,
      shf_diabetestype == "Type I" ~ 1,
      TRUE ~ 0
    )),
    elall = factor(
      case_when(
        is.na(shf_bpsys_cateli) |
          is.na(shf_gfrckdepi_cateli) |
          is.na(shf_diabetestype) |
          is.na(shf_potassium_cateli) |
          is.na(shf_heartrate_cateli) ~ NA_real_,
        shf_bpsys_cateli == ">=100" &
          shf_gfrckdepi_cateli == ">=30" &
          shf_diabetestype != "Type I" &
          shf_potassium_cateli == "<=5" &
          shf_heartrate_cateli == ">=50" ~ 1,
        TRUE ~ 0
      ),
      levels = 0:1, labels = c("Not eligible", "Eligible according to all")
    ),
    sos_com_charlsonci_cat = factor(
      case_when(
        sos_com_charlsonci <= 3 ~ 1,
        sos_com_charlsonci <= 7 ~ 2,
        sos_com_charlsonci >= 8 ~ 3
      ),
      levels = 1:3,
      labels = c(
        "1-3",
        "4-7",
        ">=8"
      )
    ),
    # meds
    sos_lm_sglt2i = if_else(shf_indexyear <= 2020, 0, sos_lm_sglt2i),
    sos_lm_rasiarni = if_else(sos_lm_acei == 1 | sos_lm_arb == 1 | sos_lm_arni == 1, 1, 0),
    fu_sos_lm_rasiarni = if_else(fu_sos_lm_acei == 1 | fu_sos_lm_arb == 1 | fu_sos_lm_arni == 1, 1, 0),
    sos_lm_gdmt = factor(case_when(
      shf_indexyear <= 2020 ~ sos_lm_bbl + sos_lm_rasiarni + sos_lm_mra,
      shf_indexyear >= 2021 ~ sos_lm_bbl + sos_lm_rasiarni + sos_lm_mra + sos_lm_sglt2i
    ), levels = 0:4, labels = c("None", "Mono", "Double", "Triple", "Quadruple"))
  ) %>%
  mutate(across(all_of(treatvars), ~ ynfac(.x))) %>%
  mutate(
    sos_lm_gdmttype = factor(
      case_when(
        sos_lm_bbl == "No" & sos_lm_rasiarni == "No" & sos_lm_mra == "No" & sos_lm_sglt2i == "No" ~ 0,
        # Mono
        sos_lm_bbl == "Yes" & sos_lm_rasiarni == "No" & sos_lm_mra == "No" & sos_lm_sglt2i == "No" ~ 1,
        sos_lm_bbl == "No" & sos_lm_rasiarni == "Yes" & sos_lm_mra == "No" & sos_lm_sglt2i == "No" ~ 2,
        sos_lm_bbl == "No" & sos_lm_rasiarni == "No" & sos_lm_mra == "Yes" & sos_lm_sglt2i == "No" ~ 3,
        sos_lm_bbl == "No" & sos_lm_rasiarni == "No" & sos_lm_mra == "No" & sos_lm_sglt2i == "Yes" ~ 4,
        # Double
        sos_lm_bbl == "Yes" & sos_lm_rasiarni == "Yes" & sos_lm_mra == "No" & sos_lm_sglt2i == "No" ~ 5,
        sos_lm_bbl == "Yes" & sos_lm_rasiarni == "No" & sos_lm_mra == "Yes" & sos_lm_sglt2i == "No" ~ 6,
        sos_lm_bbl == "Yes" & sos_lm_rasiarni == "No" & sos_lm_mra == "No" & sos_lm_sglt2i == "Yes" ~ 7,
        sos_lm_bbl == "No" & sos_lm_rasiarni == "Yes" & sos_lm_mra == "Yes" & sos_lm_sglt2i == "No" ~ 8,
        sos_lm_bbl == "No" & sos_lm_rasiarni == "Yes" & sos_lm_mra == "No" & sos_lm_sglt2i == "Yes" ~ 9,
        sos_lm_bbl == "No" & sos_lm_rasiarni == "No" & sos_lm_mra == "Yes" & sos_lm_sglt2i == "Yes" ~ 10,
        # Tripple
        sos_lm_bbl == "Yes" & sos_lm_rasiarni == "Yes" & sos_lm_mra == "Yes" & sos_lm_sglt2i == "No" ~ 11,
        sos_lm_bbl == "Yes" & sos_lm_rasiarni == "Yes" & sos_lm_mra == "No" & sos_lm_sglt2i == "Yes" ~ 12,
        sos_lm_bbl == "Yes" & sos_lm_rasiarni == "No" & sos_lm_mra == "Yes" & sos_lm_sglt2i == "Yes" ~ 13,
        sos_lm_bbl == "No" & sos_lm_rasiarni == "Yes" & sos_lm_mra == "Yes" & sos_lm_sglt2i == "Yes" ~ 14,
        # Quadruple
        sos_lm_bbl == "Yes" & sos_lm_rasiarni == "Yes" & sos_lm_mra == "Yes" & sos_lm_sglt2i == "Yes" ~ 15
      ),
      levels = 0:15,
      labels = c(
        "None",
        "Only Beta-blocker",
        "Only ACEi/ARB/ARNi",
        "Only MRA",
        "Only SGLT2i",
        "Beta-blocker + ACEi/ARB/ARNi",
        "Beta-blocker + MRA",
        "Beta-blocker + SGLT2i",
        "ACEi/ARB/ARNi + MRA",
        "ACEi/ARB/ARNi + SGLT2i",
        "MRA + SGLT2i",
        "Beta-blocker + ACEi/ARB/ARNi + MRA",
        "Beta-blocker + ACEi/ARB/ARNi + SGLT2i",
        "Beta-blocker + MRA + SGLT2i",
        "ACEi/ARB/ARNi + MRA + SGLT2i",
        "Beta-blocker + ACEi/ARB/ARNi + MRA + SGLT2i"
      )
    ),
    sos_lmadhere_rasiarni = pmax(sos_lmadhere_acei, sos_lmadhere_arb, na.rm = T),
    sos_lmadhere_rasiarni = pmax(sos_lmadhere_rasiarni, sos_lmadhere_arni, na.rm = T),
    sos_lmadhere_bbl = if_else(sos_lm_bbl == "No", NA_real_, sos_lmadhere_bbl),
    sos_lmadhere_mra = if_else(sos_lm_mra == "No", NA_real_, sos_lmadhere_mra),
    sos_lmadhere_rasiarni = if_else(sos_lm_rasiarni == "No", NA_real_, sos_lmadhere_rasiarni),
    sos_lmadhere_acei = if_else(sos_lm_acei == "No", NA_real_, sos_lmadhere_acei),
    sos_lmadhere_arb = if_else(sos_lm_arb == "No", NA_real_, sos_lmadhere_arb),
    sos_lmadhere_arni = if_else(sos_lm_arni == "No", NA_real_, sos_lmadhere_arni),
    sos_lmadhere_sglt2i = if_else(sos_lm_sglt2i == "No", NA_real_, sos_lmadhere_sglt2i),
    sos_lmadhere_bbl_cat = factor(if_else(sos_lmadhere_bbl >= 80, 1, 0), levels = 0:1, labels = c("<80", ">=80")),
    sos_lmadhere_mra_cat = factor(if_else(sos_lmadhere_mra >= 80, 1, 0), levels = 0:1, labels = c("<80", ">=80")),
    sos_lmadhere_sglt2i_cat = factor(if_else(sos_lmadhere_sglt2i >= 80, 1, 0), levels = 0:1, labels = c("<80", ">=80")),
    sos_lmadhere_rasiarni_cat = factor(if_else(sos_lmadhere_rasiarni >= 80, 1, 0), levels = 0:1, labels = c("<80", ">=80")),
    sos_lmadhere_acei_cat = factor(if_else(sos_lmadhere_acei >= 80, 1, 0), levels = 0:1, labels = c("<80", ">=80")),
    sos_lmadhere_arb_cat = factor(if_else(sos_lmadhere_arb >= 80, 1, 0), levels = 0:1, labels = c("<80", ">=80")),
    sos_lmadhere_arni_cat = factor(if_else(sos_lmadhere_arni >= 80, 1, 0), levels = 0:1, labels = c("<80", ">=80")),
    # fu only for dapa and biso
    fu_sos_lm_6mo = case_when(
      censdtm < shf_indexdtm + 244 ~ NA_real_,
      !is.na(dosefu_sos_lm_C07AB07_6mo) & !is.na(dosefu_sos_lm_A10BK01_6mo) ~ 1,
      TRUE ~ 0
    ),
    fu_sos_lm_1yr = case_when(
      censdtm < shf_indexdtm + 426 ~ NA_real_,
      !is.na(dosefu_sos_lm_C07AB07_1yr) & !is.na(dosefu_sos_lm_A10BK01_1yr) ~ 1,
      TRUE ~ 0
    ),
    fu_sos_lm_bbl = factor(if_else(sos_lm_bbl == "No" | sos_outtime_death < 426, NA_real_, fu_sos_lm_bbl), levels = 0:1, labels = c("No", "Yes")),
    fu_sos_lm_mra = factor(if_else(sos_lm_mra == "No" | sos_outtime_death < 426, NA_real_, fu_sos_lm_mra), levels = 0:1, labels = c("No", "Yes")),
    fu_sos_lm_acei = factor(if_else(sos_lm_acei == "No" | sos_outtime_death < 426, NA_real_, fu_sos_lm_acei), levels = 0:1, labels = c("No", "Yes")),
    fu_sos_lm_arb = factor(if_else(sos_lm_arb == "No" | sos_outtime_death < 426, NA_real_, fu_sos_lm_arb), levels = 0:1, labels = c("No", "Yes")),
    fu_sos_lm_arni = factor(if_else(sos_lm_arni == "No" | sos_outtime_death < 426, NA_real_, fu_sos_lm_arni), levels = 0:1, labels = c("No", "Yes")),
    fu_sos_lm_rasiarni = factor(if_else(sos_lm_rasiarni == "No" | sos_outtime_death < 426, NA_real_, fu_sos_lm_rasiarni), levels = 0:1, labels = c("No", "Yes")),
    fu_sos_lm_sglt2i = factor(if_else(sos_lm_sglt2i == "No" | sos_outtime_death < 426, NA_real_, fu_sos_lm_sglt2i), levels = 0:1, labels = c("No", "Yes"))
  )

# income
inc <- rsdata %>%
  reframe(incsum = list(enframe(quantile(scb_dispincome,
    probs = c(0.33, 0.66),
    na.rm = TRUE
  ))), .by = shf_indexyear) %>%
  unnest(cols = c(incsum)) %>%
  pivot_wider(names_from = name, values_from = value)

rsdata <- left_join(
  rsdata,
  inc,
  by = "shf_indexyear"
) %>%
  mutate(
    scb_dispincome_cat = factor(
      case_when(
        scb_dispincome < `33%` ~ 1,
        scb_dispincome < `66%` ~ 2,
        scb_dispincome >= `66%` ~ 3
      ),
      levels = 1:3,
      labels = c("1st tertile within year", "2nd tertile within year", "3rd tertile within year")
    )
  ) %>%
  select(-`33%`, -`66%`)

# ntprobnp

nt <- rsdata %>%
  reframe(ntmed = list(enframe(quantile(shf_ntprobnp,
    probs = c(0.33, 0.66),
    na.rm = TRUE
  )))) %>%
  unnest(cols = c(ntmed)) %>%
  pivot_wider(names_from = name, values_from = value)

rsdata <- rsdata %>%
  mutate(
    shf_ntprobnp_cat = factor(
      case_when(
        shf_ntprobnp < nt$`33%` ~ 1,
        shf_ntprobnp < nt$`66%` ~ 2,
        shf_ntprobnp >= nt$`66%` ~ 3
      ),
      levels = 1:3,
      labels = c("1st tertile", "2nd tertile", "3rd tertile")
    )
  )


rsdata <- rsdata %>%
  mutate(across(where(is_character), factor))
