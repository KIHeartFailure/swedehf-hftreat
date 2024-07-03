# Inclusion/exclusion criteria --------------------------------------------------------

flow <- flow[c(1:8, 10), 1:2]

names(flow) <- c("Criteria", "N")

flow <- flow %>%
  mutate(Criteria = if_else(
    Criteria == "Exclude posts with with index date > 2023-12-31 (SwedeHF)/2021-12-31 (NPR HF, Controls)",
    "Exclude posts with with index date > 2023-12-31", Criteria
  ))

flow <- rbind(c("General inclusion/exclusion criteria", ""), flow)

flow <- rbind(flow, c("Project specific inclusion/exclusion criteria", ""))

rsdata <- rsdata421 %>%
  filter(!is.na(shf_ef_cat))
flow <- rbind(flow, c("Exclude post with missing EF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_ef_cat == "HFrEF")
flow <- rbind(flow, c("Include post with EF < 40%", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_indexyear >= 2016)
flow <- rbind(flow, c("Include post with indexyear >= 2016", nrow(rsdata)))

rsdata <- rsdata %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm) %>%
  slice(n()) %>%
  ungroup()

flow <- rbind(flow, c("Last post / patient", nrow(rsdata)))

rm(rsdata421)
gc()
