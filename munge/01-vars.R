# Variables for baseline tables -----------------------------------------------

tabvars <- c(
  # demo
  "shf_indexyear",
  "shf_indexyear_cat",
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
  "shf_bpdia",
  "shf_map",
  "shf_heartrate",
  "shf_heartrate_cat",
  "shf_gfrckdepi",
  "shf_gfrckdepi_cat",
  "shf_potassium",
  "shf_potassium_cat",
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
  "sos_lm_acei",
  "sos_lm_arb",
  "sos_lm_arni",
  "sos_lm_bbl",
  "sos_lm_mra",
  "sos_lm_sglt2i",
  "sos_lm_gdmt",
  "sos_lm_ivabradin",
  "sos_lm_loopdiuretics",
  "sos_lm_digoxin",
  "sos_lm_nitrate",
  "sos_lm_n_othermeds",
  "sos_lm_n_othermeds_cat",
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
  "shf_indexyear_cat",
  "shf_age",
  "shf_nyha",
  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
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
  "sos_com_hyperkalemia",
  "sos_com_dialysis",
  "sos_lm_rasiarni",
  "sos_lm_acei",
  "sos_lm_arb",
  "sos_lm_arni",
  "sos_lm_bbl",
  "sos_lm_mra",
  "sos_lm_sglt2i",
  "sos_lm_gdmt",
  "sos_lm_nitrate",
  "sos_lm_digoxin",
  "sos_lm_ivabradin",
  "sos_lm_loopdiuretics",
  "sos_lm_n_othermeds",
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
  "shf_gfrckdepi_cat",
  "sos_com_charlsonci_cat",
  "sos_lm_n_othermeds_cat"
)

outvars <- tibble(
  var = c("sos_out_deathcv", "sos_out_hosphf", "sos_out_deathcvhosphf"),
  time = c("sos_outtime_death", "sos_outtime_hosphf", "sos_outtime_hosphf"),
  shortname = c("CVM", "1st HFH", "CVM/1st HFH"),
  name = c("CV mortality", "First HF hospitalization", "CV mortality/first HF hospitalization"),
  composite = c(F, F, T),
  rep = c(F, F, F),
  primary = c(F, F, T),
  order = c(1, 2, 3)
) %>%
  arrange(order)

lmvars <- tibble(
  var = c(
    "acei",
    "arb",
    "arni",
    # "rasiarni",
    "bbl",
    "mra",
    "sglt2i",
    "gdmt",
    "ivabradin",
    "loopdiuretics",
    "digoxin",
    "nitrate",
    "n_othermeds"
  ),
  label = c(
    "ACEi",
    "ARB",
    "ARNi",
    # "RASi/ARNi",
    "Beta-blocker",
    "MRA",
    "SGLT2i",
    "Guideline-directed medical therapy",
    "Ivabradin",
    "Loop diuretics",
    "Digoxin",
    "Nitrate",
    "Number of other CV and Non-CV medications"
  ),
  atc = c(
    "C09AA01|C09AA02|C09AA03|C09AA05|C09BA02|C09BA03|C09BA05",
    "C09CA06|C09CA01|C09CA03",
    "C09DX04",
    # "C09AA01|C09AA02|C09AA03|C09AA05|C09CA06|C09CA01|C09CA03|C09DX04|C09BA02|C09BA03|C09BA05",
    "C07AB07|C07AG02|C07AB02|C07FB02",
    "C03DA01|C03DA04|C03DA05",
    "A10BK01|A10BK03",
    NA,
    "C01EB17",
    "C03C",
    "C01AA05",
    "C01DA",
    NA
  ),
  gdmt = c(T, T, T, T, T, T, rep(F, 6))
)

lmvarsub <- tibble(
  atc = c(
    "C07AB07",
    "C07AG02",
    "C07AB02",
    "C07FB02",
    "C09AA01",
    "C09AA02",
    "C09AA03",
    "C09AA05",
    "C09CA06",
    "C09CA01",
    "C09CA03",
    "C09DX04",
    "C09BA02",
    "C09BA03",
    "C09BA05",
    "C03DA01",
    "C03DA04",
    "C03DA05",
    "A10BK01",
    "A10BK03",
    "C01EB17",
    "C03CA01",
    "C03CA02",
    "C03CA04",
    "C01AA05",
    "C01DA02",
    "C01DA14"
  ),
  sub = c(
    "Bisoprolol",
    "Carvedilol",
    "Metoprolol",
    "Metoprolol/felodipine",
    "Captopril",
    "Enalapril",
    "Lisinopril",
    "Ramipril",
    "Candesartan",
    "Losartan",
    "Valsartan",
    "Sacubitril/Valsartan",
    "Enalapril/hydrochlorothiazide",
    "Lisinopril/hydrochlorothiazide",
    "Ramipril/hydrochlorothiazide",
    "Spironolactone",
    "Eplerenone",
    "Finerenone",
    "Dapagliflozin",
    "Empagliflozin",
    "Ivabradin",
    "Furosemid",
    "Bumetanid",
    "Torasemid",
    "Digoxin",
    "Glyceryltrinitrat",
    "Isosorbidmononitrat"
  ),
  med = c(
    rep("Beta-blocker", 4),
    rep("ACEi/ARB/ARNi", 11),
    rep("MRA", 3),
    rep("SGLT2i", 2),
    "Ivabradin",
    rep("Loop diuretics", 3),
    "Digoxin",
    rep("Nitrate", 2)
  )
)

metavars <- bind_rows(
  metavars,
  tibble(
    variable = c(
      "sos_hospward_3mo",
      "sos_prevhfh3mo",
      "sos_prevhfh1yr",
      paste0("sos_lm_", lmvars$var),
      "sos_lm_rasiarni"
    ),
    label = c(
      "Area of previous HFH within 3 months",
      "Previous HFH within 3 months",
      "Previous HFH within 1 year",
      lmvars$label,
      "ACEi/ARB/ARNi"
    )
  )
)

metavars <- metavars %>%
  mutate(label = case_when(
    variable == "shf_diabetestype" ~ "Diabetes type",
    TRUE ~ label
  ))
