
# devtools::install_github("RamiKrispin/coronavirus") # Install coronavirus package - to show JH data
# ALTERNATIVE DATA THAT CAN BE UPDATED IN REAL-TIME:
library(coronavirus); #system.time(coronavirus::update_dataset(silence=T))
library(dplyr)
library(tidyr)

# 1. Johns Hopkins direct
# cases & deaths
covid19_df <- refresh_coronavirus_jhu()
BGD <- covid19_df %>%
  filter(location == "Bangladesh") %>% 
  spread(data_type, value) %>% 
  arrange(date) %>%
  mutate(deaths = deaths_new, cases = cases_new) %>% 
  mutate(cumulative_death = cumsum(deaths), cumulative_cases = cumsum(cases)) %>% 
  ungroup()

# Vaccination
BGD_vax <- covid19_vaccine %>%
  filter(country_region == "Bangladesh" & is.na(province_state)) 
if(length(which(BGD_vax$date=="2021-05-10"))==0){ #fill in missing day of data
  BGD_vax <- rbind(BGD_vax[which(BGD_vax$date<"2021-05-10"),],
                   BGD_vax[which(BGD_vax$date=="2021-05-09"),],
                   BGD_vax[which(BGD_vax$date>"2021-05-10"),])
  BGD_vax$date[which(duplicated(BGD_vax$date))] <- as.Date("2021-05-10")
}
dhakaDiv_vax <- covid19_vaccine %>%
  filter(country_region == "Bangladesh" & province_state == "Dhaka") 
# plot(BGD_vax$doses_admin~BGD_vax$date,type="l")
# lines(BGD_vax$people_partially_vaccinated~BGD_vax$date,col=2)
# lines(BGD_vax$people_fully_vaccinated~BGD_vax$date,col=3)
# lines(c(BGD_vax$people_fully_vaccinated+BGD_vax$people_partially_vaccinated)~BGD_vax$date,col=4)
# lines(dhakaDiv_vax$doses_admin~dhakaDiv_vax$date,col=2)
# lines(c(BGD_vax$doses_admin*(dhakadivpop2011/bdeshpop2011))~BGD_vax$date,col=3) # vax in dhaka div higher than expected based on pop
dhakaDiv_vax <- rbind(BGD_vax[which(BGD_vax$date<as.Date("2021-05-28")),],dhakaDiv_vax)
dhakaDiv_vax$doses_admin[which(dhakaDiv_vax$date<as.Date("2021-05-28"))] <- dhakaDiv_vax$doses_admin[which(dhakaDiv_vax$date<as.Date("2021-05-28"))]*(max(dhakaDiv_vax$doses_admin)/max(BGD_vax$doses_admin))
# lines(dhakaDiv_vax$doses_admin~dhakaDiv_vax$date,col=2)
propDhakaDivInDistrict <- dhakapop2011/dhakadivpop2011
dhaka_vax <- dhakaDiv_vax
dhaka_vax$doses_admin <- dhakaDiv_vax$doses_admin*propDhakaDivInDistrict
BGD_vax$prop_doses_dhaka <- dhaka_vax$doses_admin/BGD_vax$doses_admin;BGD_vax$prop_doses_dhaka[1]<-0
dhaka_vax$people_fully_vaccinated <- BGD_vax$people_fully_vaccinated*BGD_vax$prop_doses_dhaka
dhaka_vax$people_partially_vaccinated <- BGD_vax$people_partially_vaccinated*BGD_vax$prop_doses_dhaka
while(any(diff(dhaka_vax$people_fully_vaccinated)<0)){
  dhaka_vax$people_fully_vaccinated[which(diff(dhaka_vax$people_fully_vaccinated)<0)+1] <- dhaka_vax$people_fully_vaccinated[which(diff(dhaka_vax$people_fully_vaccinated)<0)]
}
while(any(diff(dhaka_vax$people_partially_vaccinated)<0)){
  dhaka_vax$people_partially_vaccinated[which(diff(dhaka_vax$people_partially_vaccinated)<0)+1] <- dhaka_vax$people_partially_vaccinated[which(diff(dhaka_vax$people_partially_vaccinated)<0)]
}
# plot(dhaka_vax$doses_admin~dhaka_vax$date,col=1,type="l")
# lines(dhaka_vax$people_fully_vaccinated~dhaka_vax$date,col=2)
# lines(dhaka_vax$people_partially_vaccinated~dhaka_vax$date,col=3)
# lines((dhaka_vax$people_partially_vaccinated+dhaka_vax$people_fully_vaccinated)~dhaka_vax$date,col=3)
dhaka_vax <- dhaka_vax %>% 
  mutate(dailyVax1 = c(0,diff(people_partially_vaccinated)),
         Vax1=people_partially_vaccinated+people_fully_vaccinated,
         dailyVax2 = c(0,diff(people_fully_vaccinated)),
         Vax2=people_fully_vaccinated,
         VaxDosesUsed=Vax1+Vax2,
         Vax1prop = people_partially_vaccinated/dhakapop2020,
         Vax2prop = people_fully_vaccinated/dhakapop2020,
         unVaxProp = 1-Vax1prop-Vax2prop) %>% 
  select(date,dailyVax1,Vax1,dailyVax2,Vax2,VaxDosesUsed,Vax1prop,Vax2prop,unVaxProp) %>%
  filter(date>=start_date)
# barplot(t(as.matrix(dhaka_vax[,7:9])),beside = F,names.arg = dhaka_vax$date,
#         border=c("navy","dodgerblue","orange"),col=c("navy","dodgerblue","orange"))

# # 2. Coronavirus package (also from JHU)
# data("coronavirus")
# BGD_df <- coronavirus %>% 
#   filter(country == "Bangladesh") %>% 
#   spread(type, cases) %>% 
#   arrange(date) %>%
#   mutate(deaths = death, cases = confirmed) %>% 
#   mutate(cumulative_death = cumsum(death), cumulative_cases = cumsum(confirmed)) %>% 
#   ungroup()

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

# # COMPARE DATASETS - everythign seems to align
# plot(BGD_df$date, BGD_df$cases, type = "l")
# lines(BGD$date, BGD$cases, type = "l", col="red")
# lines(bangladesh$date, bangladesh$cases, col="green")

# plot(BGD_df$date, BGD_df$deaths, type="l")
# lines(BGD$date, BGD$deaths, col="red", type="l")
# lines(bangladesh$date, bangladesh$deaths, col="green")

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


