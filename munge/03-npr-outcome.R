# Additional variables from NPR -------------------------------------------

load(file = paste0(shfdbpath, "/data/", datadate, "/patregrsdata.RData"))

# HFH after 1 year

rsdata <- rsdata %>%
  mutate(shf_indexdtmadhere = shf_indexdtm + 365)

rsdata <- create_sosvar(
  sosdata = patregrsdata %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtmadhere,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  name = "hosphfadhere",
  diakod = global_hficd,
  stoptime = global_followup,
  censdate = censdtm,
  valsclass = "fac",
  warnings = FALSE
)

# Place of hospitalization --------------------------------------

hfhospsos <- patregrsdata %>%
  filter(sos_source == "sv") %>%
  mutate(tmp_hfhospsos = stringr::str_detect(HDIA, global_hficd)) %>%
  filter(tmp_hfhospsos)

hfhosp <- inner_join(
  rsdata %>% select(lopnr, shf_indexdtm, shf_indexhosptime),
  hfhospsos,
  by = "lopnr"
) %>%
  mutate(
    tmp_sosdtm = coalesce(UTDATUM, INDATUM),
    tmp_indexstartdtm = if_else(!is.na(shf_indexhosptime), shf_indexdtm - shf_indexhosptime, shf_indexdtm),
    keep = case_when(
      (tmp_indexstartdtm >= INDATUM & tmp_indexstartdtm <= UTDATUM |
        shf_indexdtm >= INDATUM & shf_indexdtm <= UTDATUM) ~ 1,
      tmp_sosdtm <= shf_indexdtm & tmp_sosdtm >= shf_indexdtm - 30.5 * 3 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(keep == 1) %>%
  mutate(sos_hospward_3mo = case_when(
    str_detect(MVO, "107|231") ~ 1,
    str_detect(MVO, "101") ~ 2,
    TRUE ~ 3
  )) %>%
  group_by(lopnr) %>%
  arrange(sos_hospward_3mo) %>%
  slice(1) %>%
  ungroup() %>%
  select(lopnr, sos_hospward_3mo)

rsdata <- left_join(
  rsdata,
  hfhosp,
  by = c("lopnr")
) %>%
  mutate(
    # sos_hospward_3mo = if_else(is.na(sos_hospward_3mo), 4, sos_hospward_3mo),
    sos_hospward_3mo = factor(sos_hospward_3mo, levels = 1:3, labels = c("Cardiology", "Internal medicine", "Other"))
  )

rm(patregrsdata)
gc()
