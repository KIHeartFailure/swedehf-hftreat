# report
library(rmarkdown)
library(kableExtra)

# data import/export
library(haven)
library(openxlsx)

# general dm
library(purrr)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(lubridate)
library(forcats)
library(hfmisc)
library(here)

# desk stat
library(tableone)

# outcomes
library(survival)
library(MASS) # neg bin model
library(epitools)
library(survminer) # check assumptions
library(splines)
library(reda) # for MCF (repeated events)

# plots
library(ggplot2)
library(ggrepel)
library(patchwork)
library(scales)

# imputation
library(mice)
library(miceadds)
library(parallel)
library(doParallel)
library(foreach)
