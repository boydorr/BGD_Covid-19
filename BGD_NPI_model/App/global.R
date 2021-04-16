library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(ggpubr)

source("R/calc_fractions.R")

# Parameters for baseline scenario (with no interventions)
source("R/pars_baseline.R")

# For some reason, update**Input() functions are struggling with a named vector, but happy with a list!!
parms_baseline_list <- as.list(parms_baseline)

# Set barplot palette
barplot_pal <- c("#323232", "red")
