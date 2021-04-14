rm(list=ls())

library(tidyverse)
library(zoo)
# remotes::install_github("joachim-gassen/tidycovid19")
library(tidycovid19)


# see https://joachim-gassen.github.io/2021/01/vaccination-data-an-outlier-tale/
##########################################################
git.path <- "/Users/katiehampson/Github/"
reinfections <- readRDS(paste0(git.path, "BGD_COVID-19/B.1.351_resurgence/data/a2i_reinfection_summary.rda"))

plot(reinfections$days, reinfections$reinfections_long)
plot(reinfections$days, reinfections$reinfections_mid)

reinfect_late <- table(cases$reinfected[which(cases$days>"2021-01-27" & !is.na(cases$days))])
reinfect_early <- table(cases$reinfected[which(cases$days<"2021-01-27" & !is.na(cases$days))])
prop.table(reinfect_late)
prop.table(reinfect_early)
sum(reinfect_early)
sum(reinfect_late)

