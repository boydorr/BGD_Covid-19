library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(ggpubr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(deSolve)
library(data.table)
library(forcats)


source("R/calc_fractions.R")

# Parameters for baseline scenario (with no interventions)
source("R/pars_baseline.R")

# For some reason, update**Input() functions are struggling with a named vector, but happy with a list!!
parms_baseline_list <- as.list(parms_baseline)

# Set barplot palette
barplot_pal <- c("#323232", "red")


source("R/worker_days_lost.R")
source("R/mortality.R")
source("R/initial_conds.R")
source("R/covid_model.R")
source("R/amalgamate_cats.R")
source("R/costs.R")
source("R/server_prep.R")
source("R/triple_barplot_base.R")
source("R/bangladesh_covid_data.R")
source("R/epi_params_table.R")
source("R/pop_params_table.R")
source("R/intervention_params_table.R")
