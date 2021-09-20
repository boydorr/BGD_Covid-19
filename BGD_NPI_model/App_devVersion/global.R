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

source("R/create_vax.R")
source("R/mortality.R")
source("R/initial_conds.R")
source("R/covid_model.R")
source("R/amalgamate_cats.R")
source("R/triple_barplot_base.R")
source("R/bangladesh_covid_data.R")


# Dates for plotting
date_ticks <- as.numeric(seq(start_date,end_date+1,by="month") - start_date)
date_labels <- paste(month.abb[month(seq(start_date,end_date+1,by="month"))], (year(seq(start_date,end_date,by="month")))-2000,sep="")

date_ticks_forecast <- as.numeric(seq(start_date_forecast-14,end_date+15,by="month") - start_date)
date_labels_forecast <- paste(month.abb[month(seq(start_date_forecast-14,end_date+15,by="month"))], (year(seq(start_date_forecast,end_date,by="month")))-2000,sep="")

date_ticks_initial <- as.numeric(seq(start_date,start_date_forecast+1,by="month") - start_date)
date_labels_initial <- paste(month.abb[month(seq(start_date,start_date_forecast+1,by="month"))], (year(seq(start_date,end_date,by="month")))-2000,sep="")

