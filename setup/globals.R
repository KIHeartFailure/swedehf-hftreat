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
global_followup <- 1095

global_hficd <- " I110| I130| I132| I255| I420| I423| I42[5-9]| I43| I50| J81| K761| R570| 414W| 425E| 425F| 425G| 425H| 425W| 425X| 428"
