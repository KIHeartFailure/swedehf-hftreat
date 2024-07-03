# Variables for baseline tables -----------------------------------------------

tabvars <- c(
  # demo
  "shf_indexyear",
  "shf_sex",
  "shf_age",
  "shf_age_cat",

  # organizational
  "shf_durationhf",
  "sos_prevhfh3mo",
  "sos_hospward_3mo",
  "sos_prevhfh1yr",
  "shf_followuphfunit",
  "shf_followuplocation_cat",

  # clinical factors and lab measurements
  "shf_ef",
  "shf_nyha",
  "shf_nyha_cat",
  "shf_bmi",
  "shf_bmi_cat",
  "shf_bpsys",
  "shf_bpsys_cat",
  "shf_bpsys_cateli",
  "shf_bpdia",
  "shf_map",
  "shf_map_cat",
  "shf_heartrate",
  "shf_heartrate_cat",
  "shf_heartrate_cateli",
  "shf_gfrckdepi",
  "shf_gfrckdepi_cat",
  "shf_gfrckdepi_cateli",
  "shf_potassium",
  "shf_potassium_cat",
  "shf_potassium_cateli",
  "shf_hb",
  "shf_ntprobnp",
  "shf_ntprobnp_cat",

  # comorbs
  "shf_smoke_cat",
  "shf_sos_com_diabetes",
  "shf_diabetestype",
  "shf_sos_com_hypertension",
  "shf_sos_com_ihd",
  "sos_com_mi",
  "shf_revasc",
  "sos_com_dcm",
  "sos_com_pad",
  "shf_sos_com_af",
  "sos_com_stroke",
  "sos_com_tia",
  "shf_anemia",
  "sos_com_valvular",
  "sos_com_hyperkalemia",
  "sos_com_dialysis",
  "sos_com_liver",
  "sos_com_copd",
  "sos_com_cancer3y",
  "sos_com_muscoloskeletal3y",
  "sos_com_alcohol",
  "sos_com_charlsonci",
  "sos_com_charlsonci_cat",

  # treatments
  "sos_lm_rasiarni",
  "sos_lm_bbl",
  "sos_lm_mra",
  "sos_lm_sglt2i",
  "sos_lm_gdmt",
  "sos_lm_nitrate",
  "sos_lm_digoxin",
  "sos_lm_ivabradin",
  "sos_lm_loopdiuretics",
  "n_othermeds",
  "n_othermeds_cat",
  "shf_device_cat",

  # socec
  "scb_famtype",
  "scb_maritalstatus",
  "scb_child",
  "scb_education",
  "scb_dispincome_cat",
  "shf_qol",
  "shf_qol_cat"
)

# Variables for models (imputation, log, cox reg) ----------------------------

tabvars_not_in_mod <- c(
  # others
  "shf_age",
  "shf_nyha",
  "shf_ef",
  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
  "shf_map_cat",
  "shf_heartrate",
  "shf_gfrckdepi",
  "shf_hb",
  "shf_ntprobnp",
  "shf_potassium",
  "shf_bmi",
  "sos_com_liver", # to few obs
  "sos_com_charlsonci",
  "sos_com_charlsonci_cat",
  "shf_qol",
  "shf_qol_cat",
  "scb_maritalstatus",
  "shf_revasc",
  "shf_location",
  "sos_com_mi",
  "sos_com_dcm",
  "sos_com_dialysis",
  "sos_lm_rasiarni",
  "sos_lm_bbl",
  "sos_lm_mra",
  "sos_lm_sglt2i",
  "sos_lm_gdmt",
  "sos_lm_nitrate",
  "sos_lm_digoxin",
  "sos_lm_ivabradin",
  "sos_lm_loopdiuretics",
  "n_othermeds",
  "sos_hospward_3mo",
  "sos_prevhfh1yr",
  "shf_diabetestype",
  "shf_bpsys_cateli",
  "shf_heartrate_cateli",
  "shf_gfrckdepi_cateli",
  "shf_potassium_cateli",
  "sos_com_tia"
)

modvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]

stratavars <- c("shf_location")

tabvarsel <- c(
  "sos_prevhfh3mo",
  "sos_hospward_3mo",
  "shf_age_cat",
  "shf_sex",
  "sos_com_charlsonci_cat",
  "n_othermeds_cat"
)

outvars <- tibble(
  var = c("sos_out_deathcv", "sos_out_hosphf", "sos_out_deathcvhosphf"),
  time = c("sos_outtime_death", "sos_outtime_hosphf", "sos_outtime_hosphf"),
  shortname = c("CVD", "1st HFH", "CVD/1st HFH"),
  name = c("CV death", "First HF hospitalization", "CV death/first HF hospitalization"),
  composite = c(F, F, T),
  rep = c(F, F, F),
  primary = c(F, F, T),
  order = c(1, 2, 3)
) %>%
  arrange(order)
