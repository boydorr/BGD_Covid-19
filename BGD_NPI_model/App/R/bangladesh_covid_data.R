
# devtools::install_github("RamiKrispin/coronavirus") # Install coronavirus package - to show JH data
# ALTERNATIVE DATA THAT CAN BE UPDATED IN REAL-TIME:
library(coronavirus); #system.time(update_dataset(silence=T))
update_dataset()
library(dplyr)
library(tidyr)

# Coronavirus package
BGD <- coronavirus %>%
  filter(country == "Bangladesh") %>% 
  spread(type, cases) %>% 
  arrange(date) %>%
  mutate(deaths = death, cases = confirmed,date=as.Date(date,format("%Y-%m-%d"))) %>% 
  mutate(cumulative_death = cumsum(deaths), cumulative_cases = cumsum(cases)) %>% 
  ungroup()


# ECDC - now archived though
# read the Dataset sheet into R. The dataset will be called "data".
data <- read.csv("data/ecdc_data.csv")
# data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

cases <- data %>%
  transmute(date = as.Date(dateRep, "%d/%m/%Y"), 
            cases = cases, deaths = deaths, country = countriesAndTerritories) %>%
  group_by(country) %>%
  arrange(date) %>%
  mutate(cumulative_death = cumsum(deaths),
         cumulative_cases = cumsum(cases),
         date = date-1) %>%  #dates are recorded as day after they happened.  Change to day they happened
  ungroup()
bangladesh = subset(cases, country == "Bangladesh")


## Proportion cases in Dhaka (from the dashboard)
district_cases <- read.csv("data/district_cases_dashboard_19.11.csv")
propDhaka <- district_cases$X.[which(district_cases$Distirct=="Dhaka")]/sum(district_cases$X.)

bangladesh$dhaka_cases <- bangladesh$cases*propDhaka
bangladesh$dhaka_deaths <- bangladesh$deaths*propDhaka
bangladesh$dhaka_cum_deaths <- bangladesh$cumulative_death*propDhaka

BGD$dhaka_cases <- BGD$cases*propDhaka
BGD$dhaka_deaths <- BGD$deaths*propDhaka
BGD$dhaka_cum_deaths <- BGD$cumulative_death*propDhaka

propDhaka2021 <- 0.62
BGD$dhaka_cases_2021 <- BGD$cases*propDhaka2021
BGD$dhaka_deaths_2021 <- BGD$deaths*propDhaka2021
BGD$dhaka_cum_deaths_2021 <- BGD$cumulative_death*propDhaka2021


