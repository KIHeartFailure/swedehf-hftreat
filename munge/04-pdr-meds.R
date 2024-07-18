load(here(shfdbpath, "data", datadate, "/lmswedehf.RData"))

# Select ATC codes --------------------------------------------------------

lm <- lmswedehf %>%
  filter(
    ANTAL >= 0 &
      !is.na(forpddd)
  )

lm <- left_join(
  rsdata %>%
    select(lopnr, shf_indexdtm),
  lm,
  by = "lopnr",
  relationship = "many-to-many"
)

lmsel <- lm %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff >= -122 & diff <= 14) %>%
  select(lopnr, shf_indexdtm, EDATUM, ATC)

rm(lmswedehf)
gc()

lmlev3 <- lmsel %>%
  mutate(ATC3 = str_sub(ATC, 1, 3)) %>%
  filter(str_detect(ATC, paste0("^(", paste0(lmvars %>% filter(gdmt) %>% pull(atc), collapse = "|"), ")"), negate = T)) %>%
  group_by(lopnr, ATC3) %>%
  slice(1) %>%
  ungroup() %>%
  select(lopnr, ATC3) %>%
  group_by(lopnr) %>%
  summarise(sos_lm_n_othermeds = n()) %>%
  ungroup()

rsdata <- left_join(rsdata, lmlev3, by = "lopnr") %>%
  mutate(
    sos_lm_n_othermeds = replace_na(sos_lm_n_othermeds, 0),
    sos_lm_n_othermeds_cat = factor(case_when(
      sos_lm_n_othermeds <= 1 ~ 1,
      sos_lm_n_othermeds <= 4 ~ 2,
      sos_lm_n_othermeds <= 6 ~ 3,
      sos_lm_n_othermeds <= 8 ~ 4,
      TRUE ~ 5
    ), levels = 1:5, labels = c("0-1", "2-4", "5-6", "7-8", ">=9"))
  )
