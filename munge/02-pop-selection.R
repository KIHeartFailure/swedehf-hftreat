# Inclusion/exclusion criteria --------------------------------------------------------

flow <- flow[c(1:8, 10), 1:2]

names(flow) <- c("Criteria", "N")

flow <- flow %>%
  mutate(Criteria = case_when(
    Criteria == "Exclude posts with with index date > 2023-12-31 (SwedeHF)/2021-12-31 (NPR HF, Controls)" ~ "Exclude posts with index date > 2023-12-31",
    Criteria == "Exclude posts with with index date > 2023-12-31" ~ "Exclude posts with index date > 2023-12-31",
    Criteria == "Exclude posts censored end fu < index" ~ "Exclude posts with end of follow-up < index",
    TRUE ~ Criteria
  ))


rsdata <- rsdata421 %>%
  filter(!is.na(shf_ef_cat))
flow <- rbind(flow, c("Exclude posts with missing EF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_ef_cat == "HFrEF")
flow <- rbind(flow, c("Include posts with EF < 40%", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_indexyear >= 2016)
flow <- rbind(flow, c("Include posts with indexyear >= 2016", nrow(rsdata)))

rsdata <- rsdata %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm) %>%
  slice(n()) %>%
  ungroup()

flow <- rbind(flow, c("Include the last post / patient", nrow(rsdata)))

names(flow) <- c("Inclusion/exclusion criteria", "N")

rm(rsdata421)
gc()
