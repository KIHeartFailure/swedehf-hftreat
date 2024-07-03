# default is to use tidyverse functions
select <- dplyr::select
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete
fixed <- stringr::fixed

# used for calculation of ci
global_z05 <- qnorm(1 - 0.025)

shfdbpath <- "F:/STATISTIK/Projects/20210525_shfdb4/dm/"
datadate <- "20240423"

global_cols <- RColorBrewer::brewer.pal(7, "Dark2")

global_followup_year <- 3
global_followup <- 3 * 365.25

global_hficd <- " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R570| 414W| 425E| 425F| 425G| 425H| 425W| 425X| 428"

global_atc_rasiarni <- "C09AA01|C09AA02|C09AA03|C09AA05|C09CA06|C09CA01|C09CA03|C09DX04|C09BA02|C09BA03|C09BA05"
global_atc_bbl <- "C07AB07|C07AG02|C07AB02|C07FB02"
global_atc_mra <- "C03DA01|C03DA04|C03DA05"
global_atc_sglt2i <- "A10BK01|A10BK03"
