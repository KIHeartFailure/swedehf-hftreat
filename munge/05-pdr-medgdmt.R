lmgdmt <- haven::read_sas(here::here("data/raw-data/lmgdmtdose_421.sas7bdat"))

lmgdmt <- lmgdmt %>%
  filter(
    !is.na(forpddd)
  ) %>%
  select(-atc3, -atc4, -atc5) %>%
  select(-OTYP, -contains("SPKOD"), -VERKS, -ar)

lmgdmt <- left_join(
  rsdata %>%
    select(lopnr, shf_indexdtm, censdtm),
  lmgdmt,
  by = "lopnr",
  relationship = "many-to-many"
) %>%
  arrange(lopnr, EDATUM, ATC) %>%
  mutate(post = 1:n())

# remove neg incorrect prescriptions

## iterative since could not find another way...

# calcs the max number of neg disp per lopnr, ATC, ANTAL, forpddd, antnum (= number of runs needed)
lmgdmtremove <- lmgdmt %>%
  filter(ANTAL < 0) %>%
  rename(
    ANTALneg = ANTAL,
    EDATUMneg = EDATUM
  ) %>%
  mutate(ANTAL = abs(ANTALneg)) %>%
  select(lopnr, EDATUMneg, ATC, ANTAL, ANTALneg, forpddd, antnum) %>%
  group_by(lopnr, ATC, ANTAL, forpddd, antnum) %>%
  mutate(postremove = row_number()) %>%
  ungroup() %>%
  arrange(lopnr, ATC, EDATUMneg, ANTAL, forpddd, antnum, postremove)

lmgdmt2 <- lmgdmt %>%
  filter(ANTAL >= 0)

for (i in 1:max(lmgdmtremove$postremove)) {
  tmp <- inner_join(
    lmgdmt2,
    lmgdmtremove %>%
      filter(postremove == i),
    by = c("lopnr", "ATC", "ANTAL", "forpddd", "antnum")
  ) %>%
    mutate(diff = as.numeric(EDATUMneg - EDATUM)) %>%
    filter(diff >= 0) %>% # the negative disp must be same date or after the first (incorrect) disp
    group_by(lopnr, ATC, ANTAL, forpddd, antnum) %>%
    arrange(diff) %>%
    slice(1) %>%
    ungroup() %>%
    select(post)

  tmp <- anti_join(
    lmgdmt2,
    tmp,
    by = "post"
  )

  # output to the global environment
  lmgdmt2 <<- tmp
}

lmgdmt3 <- lmgdmt2 %>%
  mutate(
    diff = as.numeric(EDATUM - shf_indexdtm)
  ) %>%
  filter(diff >= -365 & diff <= 426) %>% # added 428 for the discontinuation calc, add - 365 for sequence calculation
  mutate(
    med = case_when(
      str_detect(ATC, paste0("^(", lmvars$atc[1], ")")) ~ lmvars$var[1],
      str_detect(ATC, paste0("^(", lmvars$atc[2], ")")) ~ lmvars$var[2],
      str_detect(ATC, paste0("^(", lmvars$atc[3], ")")) ~ lmvars$var[3],
      str_detect(ATC, paste0("^(", lmvars$atc[4], ")")) ~ lmvars$var[4],
      str_detect(ATC, paste0("^(", lmvars$atc[6], ")")) ~ lmvars$var[6],
      str_detect(ATC, paste0("^(", lmvars$atc[7], ")")) ~ lmvars$var[7],
      str_detect(ATC, paste0("^(", lmvars$atc[8], ")")) ~ lmvars$var[8],
      str_detect(ATC, paste0("^(", lmvars$atc[9], ")")) ~ lmvars$var[9]
    )
  ) %>%
  filter(!is.na(med))

# tab / day
lmgdmt4 <- lmgdmt3 %>%
  mutate(
    doserlower = tolower(DOSER),
    doserlower = str_squish(doserlower),
    doserlower = str_remove_all(doserlower, "filmdragerad|filmdrag|vattendrivande|vätskedrivande|konv"),
    doserlower = str_squish(doserlower),
    doserlower = str_replace_all(doserlower, "\\-(| )tab|depotkapsel|depottabletter|depottablett|depottabl\\.|depotab|depottabl|depot-tab|depot- tabletter|depot tab|tabletter|tablett|tablet|tabl\\.|tabl|tab\\.|tabt|kapsel|tbl\\.|tbl|talett|kapslar|tb|talb|tqblett| ta |dosering| t ", "tab"),
    doserlower = str_remove_all(doserlower, "varje "),
    doserlower = str_replace_all(doserlower, "ggr|gånger", "gång"),
    doserlower = str_replace_all(doserlower, " \\+ |\\+", ","),
    doserlower = str_replace_all(doserlower, " - | -|- ", "-"),
    doserlower = str_replace_all(doserlower, "1 ½|1½|1 1/2|1,5| en och en halv|^(en och en halv)", "1.5"),
    doserlower = str_replace_all(doserlower, " st | stycken |^st", " "),
    doserlower = str_replace_all(doserlower, " varje ", " "),
    doserlower = str_replace_all(doserlower, "^en | en |1st", " 1 "),
    doserlower = str_replace_all(doserlower, "^två| två |2st| twice ", " 2 "),
    doserlower = str_replace_all(doserlower, "^tre | tre |3st", " 3 "),
    doserlower = str_replace_all(doserlower, "½|1/2|0,5|halv|half|1 halv|en halv", "0.5"),
    doserlower = str_replace_all(doserlower, "samt| o |0ch", "och"),
    doserlower = str_remove_all(doserlower, "\\/|per"),
    doserlower = str_replace_all(doserlower, "(?<=[:digit:])tab", " tab"),
    doserlower = str_replace_all(doserlower, "(?<=[:digit:]),(?=[:digit:])", "."),
    doserlower = str_replace_all(doserlower, "kl\\.|klockan", "kl"),
    doserlower = str_replace_all(doserlower, "kl 08:00|kl 8\\.00|kl 08|kl 8|kl08:00|kl08\\.00|kl08|kl8|kl 7:00|till frukost|på förmiddagen", "på morgonen"),
    doserlower = str_replace_all(doserlower, "på morgonen|på morgon", "morgon"),
    doserlower = str_replace_all(doserlower, "kl20:00|kl 20|kl 17| kl 16|kl 14|till middag|till natten|på kvällen|kl20|på lunch|till lunch", "till kvällen"),
    doserlower = str_replace_all(doserlower, "till kvällen", "kväll"),
    doserlower = str_replace_all(doserlower, "(?<=\\D),(?=\\D)", " och "),
    doserlower = str_remove_all(doserlower, "[:blank:]"),
    tabday = case_when(
      str_detect(doserlower, "4tabmorgonoch4|4tabmorgonochkväll") ~ 8,
      str_detect(doserlower, "4tabmorgonoch3|3tabmorgonoch4") ~ 7,
      str_detect(doserlower, "3tabmorgonoch3|3tab2gång|3tabmorgonochkväll|6tab|2tabmorgonoch4|2tab3gång") ~ 6,
      str_detect(doserlower, "3tabmorgonoch2|2tabmorgonoch3|2\\.0\\.3|2\\.5tabmorgonochkväll|2\\.1\\.2|^5tab|3\\.0\\.2|4tabmorgonoch1|2\\.5(tab|)2gång") ~ 5,
      str_detect(doserlower, "2tabmorgonoch2|2tab2gång|4tab|2\\.0\\.2|2tabmorgonochkväll|1\\.1\\.1\\.1|3\\.0\\.1|1\\.1\\.2|2x2") ~ 4,
      str_detect(doserlower, "2tabmorgonoch1\\.5|1\\.5tabmorgon(och|,|)2|3\\.5tab|2\\.5tabmorgonoch1tab") ~ 3.5,
      str_detect(doserlower, "3tab|2(tab|)morgonoch1|1tab3gång|1\\.5tab2gång|1\\.5tabmorgonoch1\\.5|1\\.5tabmorgonochkv|1tabmorgonoch1tabkvälloch1tab|1\\.1\\.1|2\\.0\\.1|1\\.0\\.2|1-2tab2gång|1-2tab1-3gång|2\\.1\\.0|2-0-1|1\\.5\\.0\\.1\\.5|1tabmorgonoch2|2tabmorgon,1tab") ~ 3,
      str_detect(doserlower, "2\\.5tab|1\\.5tabmorgon(och||,)1|1\\.0\\.1\\.5|2\\.0\\.0\\.5|1\\.5\\.0\\.1|1(tab|)tabmorgon(och||,)1\\.5|2tabmorgonoch0\\.5") ~ 2.5,
      str_detect(doserlower, "0\\.5tab2gång|0\\.5tabmorgonoch0\\.5|0\\.5\\.0\\.5tab|0\\.5tabmorgonochkväll|1tab1gång|1hjärtat|1\\.0\\\\.för hjärtat|0\\.5\\.0\\.5|0.5tabx2gång") ~ 1,
      str_detect(doserlower, "1\\.5(tab|dag)|0\\.5tabmorgon(och||,|\\.)1|1-2tab|1tabmorgonoch0\\.5|0\\.5(tab|)morgonoch1|1(tab|)morgon(och|,|)0\\.5|0\\.5\\.0\\.1|1\\.0\\.0\\.5|1\\.0\\.5tab|0\\.5-1tab2gång|1tab1-2gång|1-2tabdag|1\\.0\\.0\\.0\\.5|1\\.0\\.5\\.0|1\\.51gång|1\\.5x1") ~ 1.5,
      str_detect(doserlower, "1tabmorgonoch0\\.25|1\\.25tab|0\\.25tabmorgonoch1") ~ 1.25,
      str_detect(doserlower, "0\\.5-1tab|0\\.5tab1-2gång|1tabvarannandag,0\\.5tabvarannandag|0\\.5tabvarannandagoch1tabvarannandag") ~ 0.75,
      str_detect(doserlower, "1\\.0\\.0\\.5-1 tab") ~ 0.75,
      str_detect(doserlower, "0\\.5tab|1tabvarannan|05tab|0\\.5st|0\\.5dag|1tabmorgonvarannan|1tabkvällvarannan|1tab1gångvarannan|0\\.5tab|1varannandag|0\\.5x1|0\\.25tabmorgonochkväll") ~ 0.5,
      str_detect(doserlower, "0\\.25tab|0\\.5(tab|)varannandag") ~ 0.25,
      str_detect(doserlower, "2tab|2(1|)gång|1(tab|)morgonoch1|morgonochkväll|1x2|1\\.0\\.1|1tab2gång|1\\.0\\.1|1\\.1|2morgon|2kväll|2\\.0\\.0|2morgon|2dag|0\\.5tabmorgonoch0\\.5tabmittpådagen1tabkväll") ~ 2,
      str_detect(doserlower, "1tab|1x1|0\\.5morgonoch0\\.5|1(tab|)dag|0\\.5x2|1\\.0\\.0|0\\.1\\.0|1\\.0|0\\.5(tab|)2gång|0\\.5\\.0\\.5|0\\.5morgonochkväll|1(morgon|kväll)|0\\.5\\.0\\.0\\.5|1(var|om)(morgon|kväll|dag)|11gång") ~ 1
    )
  )


# koll <- lmgdmt4 %>%
#  count(DOSER, tabday)


# koll <- lmgdmt4 %>%
#   filter(is.na(tabday)) %>%
#  count(DOSER)

# koll <- lmgdmt4 %>%
#  count(is.na(tabday)) %>%
#  mutate(p = n / sum(n) * 100)

# koll <- lmgdmt4 %>%
#  mutate(koll = str_detect(doserlower, "enligttidig") & is.na(tabday)) %>%
#  count(is.na(tabday), koll) %>%
#  mutate(p = n / sum(n) * 100)

# impute if missing tabsday

impgdmt <- lmgdmt4 %>%
  group_by(ATC) %>%
  summarize(imptabday = round(mean(tabday, na.rm = T), 2)) %>%
  ungroup() %>%
  mutate(imptabday = case_when(
    is.na(imptabday) ~ 1,
    imptabday < 1 ~ imptabday,
    TRUE ~ round(imptabday, 0)
  ))

lmgdmt4 <- left_join(lmgdmt4, impgdmt, by = "ATC") %>%
  mutate(
    imptab = if_else(is.na(tabday), 1, 0),
    tabday = if_else(is.na(tabday), imptabday, tabday)
  )

misstabday <- lmgdmt4 %>%
  count(imptab) %>%
  mutate(p = n / sum(n) * 100) %>%
  filter(imptab == 1)

# strenghts
lmgdmt4 <- lmgdmt4 %>%
  mutate(
    ddddose = forpddd / antnum,
    ddd = case_when(
      # bbl
      ATC == "C07AB07" ~ 10, # exists in 1.25, 2.5, 5, 10, so 2 digits
      ATC == "C07AG02" ~ 37.5, # exists in 3.125, 6.25, 12.5, 25, so 3 digits
      ATC == "C07AB02" ~ 150,
      ATC == "C07FB02" ~ NA_real_, # combination with metoprolol, exist in 5 and 10, assume 10

      # rasiarni
      ATC == "C09AA01" ~ 50,
      ATC == "C09AA02" ~ 10, # exists in 2.5, 5, 10, 20, so 1 digits
      ATC == "C09AA03" ~ 10,
      ATC == "C09AA05" ~ 2.5, # exists in 1.25, 2.5, 5, 10, 20, so 2 digits

      ATC == "C09BA02" ~ NA_real_, # combination with enalapril, only exists 20 mg
      ATC == "C09BA03" ~ NA_real_, # combination with lisinopril, only exists 10 mg
      ATC == "C09BA05" ~ NA_real_, # combination ramipril, most pats take 5, assume this

      ATC == "C09CA06" ~ 8,
      ATC == "C09CA01" ~ 50, # exists in 12.5, 50, 100 so 1 digits
      ATC == "C09CA03" ~ 80,
      ATC == "C09DX04" ~ NA_real_,

      # mra
      ATC == "C03DA01" ~ 75,
      ATC == "C03DA04" ~ 50,
      ATC == "C03DA05" ~ 20,

      # sglt2
      ATC == "A10BK01" ~ 10,
      ATC == "A10BK03" ~ 17.5,

      # digoxin
      ATC == "C01AA05" ~ .25,

      # ivabradin
      ATC == "C01EB17" ~ 10,

      # nitrate
      ATC == "C01DA02" ~ 5,
      ATC == "C01DA08" ~ 60,
      ATC == "C01DA14" ~ 40,

      # loopdiuretics
      ATC == "C03CA01" ~ 40,
      ATC == "C03CA02" ~ 1,
      ATC == "C03CA04" ~ 15
    ),
    strength = ddddose * ddd,
    # for the combinations
    strength = case_when(
      !is.na(strength) ~ strength,
      ATC == "C07FB02" ~ 100,
      ATC == "C09BA02" ~ 20,
      ATC == "C09BA03" ~ 10,
      ATC == "C09BA05" ~ 5,

      # arni assumption
      ATC == "C09DX04" & antnum %in% c(28, 56) ~ 51.5, # (real dose is 51 mg but to adjust to the so the target dose is correct)
      ATC == "C09DX04" & antnum %in% c(168) ~ 103
    ),
    strength = if_else(ATC == "C09DX04" & tabday > 2, 51.5, strength), # 103/2 (real dose is 51 mg but to adjust to the so the target dose is correct)
    strength = if_else(ATC == "C09DX04" & tabday > 4, 25.75, strength), # 103/5 (real dose is 26 mg but to adjust to the so the target dose is correct)
    # fix rounding errors
    tmpstrength = round(strength, 0),
    strength = case_when(
      tmpstrength %in% c(1, 6) ~ round(strength, 2),
      tmpstrength %in% c(2, 12) ~ round(strength, 1),
      tmpstrength %in% c(3) ~ round(strength, 3),
      TRUE ~ tmpstrength
    ),
    dose = strength * tabday
  )

koll <- lmgdmt4 %>%
  filter(is.na(dose)) %>%
  count(ATC)

# 1. mean strength if on same day and same ATC

lmgdmt5 <- lmgdmt4 %>%
  group_by(lopnr, shf_indexdtm, EDATUM, ATC, med, diff) %>%
  summarise(dose = mean(dose, na.rm = T)) %>%
  ungroup()

# baseline variables

lmsel <- lmgdmt5 %>%
  filter(diff >= -122 & diff <= 14) %>%
  group_by(lopnr, med) %>%
  arrange(desc(diff)) %>%
  slice(1) %>%
  ungroup() %>%
  select(lopnr, med, dose, ATC) %>%
  mutate(var = 1)

lmgdmt6 <- lmsel %>%
  pivot_wider(names_from = c("med"), values_from = c("var", "dose", "ATC"), names_prefix = c("sos_lm_"))

colnames(lmgdmt6) <- str_remove_all(colnames(lmgdmt6), "var_")

rsdata <- left_join(rsdata, lmgdmt6, by = "lopnr")

treatvars <- c(
  "sos_lm_bbl", "sos_lm_rasiarni", "sos_lm_mra", "sos_lm_sglt2i", "sos_lm_nitrate",
  "sos_lm_digoxin", "sos_lm_ivabradin", "sos_lm_loopdiuretics"
)
rsdata <- rsdata %>%
  mutate(across(all_of(treatvars), ~ replace_na(.x, 0)))

# adherence

lmadhere <- lmgdmt4 %>%
  mutate(
    tabs = ANTAL * antnum,
    stopdtm = pmin(shf_indexdtm + 365, censdtm)
  ) %>%
  filter(diff >= -122 & EDATUM < stopdtm & med %in% c("bbl", "rasiarni", "mra", "sglt2i"))

lmadhere2 <- lmadhere %>%
  group_by(lopnr, EDATUM, med, stopdtm) %>%
  summarise(
    tabday = mean(tabday, na.rm = T),
    tabs = sum(tabs, na.rm = T)
  ) %>%
  ungroup()

lmadhere3 <- lmadhere2 %>%
  group_by(lopnr, med) %>%
  arrange(EDATUM) %>%
  mutate(
    leadEDATUM = lead(EDATUM),
    last = row_number() == n()
  ) %>%
  ungroup() %>%
  arrange(lopnr, med, EDATUM)

lmadhere4 <- lmadhere3 %>%
  mutate(
    time = as.numeric(leadEDATUM - EDATUM),
    time = if_else(last, as.numeric(stopdtm - EDATUM), time),
    tabdaytaken = tabs / time,
    adherence = tabdaytaken / tabday,
    adherence = if_else(adherence > 1 & last, 1, adherence),
    adherence = adherence * 100
  ) %>%
  arrange(lopnr, med, EDATUM)

lmadhere5 <- lmadhere4 %>%
  group_by(lopnr, med) %>%
  summarise(adherence = mean(adherence, na.rm = T)) %>%
  ungroup() %>%
  mutate(adherence = if_else(adherence > 100, 100, adherence))

lmadhere6 <- lmadhere5 %>%
  pivot_wider(names_from = c("med"), values_from = c("adherence"), names_prefix = c("sos_lmadhere_"))

rsdata <- left_join(rsdata, lmadhere6, by = "lopnr")

# dose at 6 mo and 1 year for dapa and biso
lmfu <- lmgdmt4 %>%
  mutate(
    fu = case_when(
      diff >= 122 & diff <= 244 ~ "6mo",
      diff >= 304 & diff <= 426 ~ "1yr"
    ),
    diffsort = case_when(
      fu == "6mo" ~ diff - 365 / 2,
      fu == "1yr" ~ diff - 365
    )
  ) %>%
  filter(!is.na(fu) & ATC %in% c("C07AB07", "A10BK01")) %>%
  group_by(lopnr, ATC, fu) %>%
  arrange(diffsort) %>%
  slice(1) %>%
  ungroup() %>%
  select(lopnr, dose, ATC, fu)

lmfu <- lmfu %>%
  pivot_wider(names_from = c("ATC", "fu"), values_from = c("dose"), names_prefix = c("dosefu_sos_lm_"))

rsdata <- left_join(rsdata, lmfu, by = "lopnr")

# discontinuation at 1 year for bbl rasiarni, mra, sglt2i (objective 10)
lmfu4 <- lmgdmt4 %>%
  filter(diff >= 304 & diff <= 426) %>%
  mutate(
    diffsort = diff - 365
  ) %>%
  filter(med %in% c("bbl", "rasiarni", "mra", "sglt2i")) %>%
  group_by(lopnr, med) %>%
  arrange(diffsort) %>%
  slice(1) %>%
  ungroup() %>%
  select(lopnr, med) %>%
  mutate(value = 1)

lmfu4 <- lmfu4 %>%
  pivot_wider(names_from = c("med"), values_from = c("value"), names_prefix = c("fu_sos_lm_"))

rsdata <- left_join(rsdata, lmfu4, by = "lopnr")
treatvars <- c(
  "fu_sos_lm_bbl", "fu_sos_lm_rasiarni", "fu_sos_lm_mra", "fu_sos_lm_sglt2i"
)
rsdata <- rsdata %>%
  mutate(across(all_of(treatvars), ~ replace_na(.x, 0)))


# initiation pattern sequence (objective 4)
lmprevious <- lmgdmt4 %>%
  filter(diff >= -365 & diff < -122 & med %in% c("bbl", "rasiarni", "mra", "sglt2i")) %>%
  group_by(lopnr, med) %>%
  slice(1) %>%
  ungroup() %>%
  select(lopnr, med) %>%
  mutate(value = 1)

lmprevious <- lmprevious %>%
  pivot_wider(names_from = c("med"), values_from = c("value"), names_prefix = c("previous_sos_lm_"))

lmseq <- lmgdmt4 %>%
  filter(diff >= -122 & diff <= 14 & med %in% c("bbl", "rasiarni", "mra", "sglt2i")) %>%
  group_by(lopnr, med) %>%
  arrange(EDATUM) %>%
  slice(1) %>%
  ungroup() %>%
  select(lopnr, med, EDATUM) %>%
  arrange(lopnr, EDATUM)

lmseq2 <- lmseq %>%
  group_by(lopnr, EDATUM) %>%
  arrange(med) %>%
  summarise(medvars = paste(med, collapse = "/")) %>%
  ungroup()

lmseq3 <- lmseq2 %>%
  group_by(lopnr) %>%
  arrange(EDATUM) %>%
  summarise(sos_lm_seq = paste(medvars, collapse = "-")) %>%
  ungroup()

lmseq4 <- anti_join(lmseq3, lmprevious, by = "lopnr") %>%
  select(lopnr, sos_lm_seq)

rsdata <- left_join(rsdata, lmseq4, by = "lopnr") %>%
  mutate(sos_lm_seq = if_else(!is.na(shf_durationhf) & shf_durationhf == "<6", sos_lm_seq, NA_character_))
