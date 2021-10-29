# Plot the timeseries of covid-19 confirmed cases and deaths as well as key events
git.path <- "/Users/katiehampson/Github/"

# Load libraries
library(tidyverse)
library(coronavirus); #update_dataset()
library(zoo)
library(ggthemes)
library(lubridate)

# theme
theme <- theme_clean() + theme(axis.title=element_text(size=15), axis.text=element_text(size=13), 
                               legend.text=element_text(size=15), legend.title=element_text(size=15), 
                               plot.background = element_rect(color = "white"), legend.background = element_rect(color = NA),
                               strip.text=element_text(size=15))
# data
data("coronavirus")
covid19_df <- refresh_coronavirus_jhu()

data <- covid19_df %>%
  filter(location == "Bangladesh") %>% 
  spread(data_type, value) %>% 
  arrange(date) %>%
  mutate(deaths = deaths_new, cases = cases_new) %>% 
  mutate(cumulative_death = cumsum(deaths), cumulative_cases = cumsum(cases)) %>% 
  mutate(cases_07 = floor(rollmean(cases, k = 7, fill = NA, align="right")),
         cases_14 = floor(rollmean(cases, k = 14, fill = NA, align="right")),
         deaths_07 = floor(rollmean(deaths, k = 7, fill = NA, align="right")),
         deaths_14 = floor(rollmean(deaths, k = 14, fill = NA, align="right")))  %>%
  ungroup()

# data <- coronavirus %>% 
#           filter(country == "Bangladesh") %>% 
#           spread(type, cases) %>% arrange(date) %>%
#             transmute(date, cases = confirmed, deaths = death, recovered) %>% 
#             mutate(cum_cases = cumsum(cases), cum_deaths = cumsum(deaths), cum_recovered = cumsum(recovered)) %>% 
#             mutate(cases_07 = floor(rollmean(cases, k = 7, fill = NA, align="right")),
#                    cases_14 = floor(rollmean(cases, k = 14, fill = NA, align="right")),
#                    deaths_07 = floor(rollmean(deaths, k = 7, fill = NA, align="right")),
#                    deaths_14 = floor(rollmean(deaths, k = 14, fill = NA, align="right")))#
data[is.na(data)] <- 0

# Write up to date case and death data (incl.rolling avgs)
write.csv(data, paste0(git.path, "BGD_Covid-19/Jul2021/output/covid_summary_", Sys.Date(), ".csv"), row.names = FALSE)

# keep range of dates for the submitted figure
data <- data %>% 
  filter(date > ymd("2020-01-21")) # %>% filter(date < ymd("2021-04-15")) # Revisit on 17 April 2021

# reinfection data
# reinfections <- readRDS(paste0(git.path, "BGD_COVID-19/B.1.351_resurgence/data/a2i_reinfection_summary.rda"))

#'------------------------------------------------------------------
#'**
startdate = ymd("2020-01-01")
enddate = Sys.Date()
# enddate = ymd("2021-04-21")

ld1 = data.frame(date = c(ymd("2020-03-26"), ymd("2020-05-31")), ylow = c(0, 0), yhigh = c(8000, 8000))
ld2 = data.frame(date = c(ymd("2021-04-05"), ymd("2021-04-21")), ylow = c(0, 0), yhigh = c(8000, 8000))

main <- ggplot(data, aes(x=date)) +         
  # geom_ribbon(data = ld1, aes(ymin=ylow, ymax=yhigh), color="grey90", fill="grey90") + 
  # geom_ribbon(data = ld2, aes(ymin=ylow, ymax=yhigh), color="grey90", fill="grey90") + 

  geom_segment(aes(x = ymd("2020-03-08"), y = 18000, xend = ymd("2020-03-08"), yend = 0, color="SARS-CoV-2 first detected (08/03/2020)"), size=0.6, linetype = "solid") +
  geom_segment(aes(x = ymd("2020-12-31"), y = 18000, xend = ymd("2020-12-31"), yend = 0, color="Alpha 1st record (31/12/2020)"), size=0.7, linetype = "solid") +
  geom_segment(aes(x = ymd("2021-01-24"), y = 18000, xend = ymd("2021-01-24"), yend = 0, color="Beta 1st record (24/01/2021)"), size=0.7, linetype = "solid") +
  geom_segment(aes(x = ymd("2021-04-28"), y = 18000, xend = ymd("2021-04-28"), yend = 0, color="Delta 1st record (28/04/2021)"), size=0.7, linetype = "solid") +
  
  geom_line(data = data, aes(y = cases), color="black", alpha=0.2) +
  geom_line(aes(y = cases_07), color="black", alpha=1, size=1) +
  geom_line(aes(y = deaths*30), color="darkred", alpha=0.2) +
  geom_line(aes(y = deaths_07*30), color="darkred", alpha=1, size=1) +  
  
#  geom_point(data = reinfections, aes(x = days, y = reinfections_90*30), color="black", alpha=1, size=1.3, shape=9) + 
  
        scale_x_date(limits=c(startdate, enddate), date_breaks="2 month", date_labels = "%m/%Y") + 
#        scale_y_continuous(sec.axis = sec_axis(~ ./30, name = "Deaths (7-day rolling mean) & reinfections")) + 
  scale_y_continuous(sec.axis = sec_axis(~ ./30, name = "Deaths (7-day rolling mean)")) + 
        ggthemes::scale_color_few() +
        labs(x="", y="Cases (7-day rolling mean)", color="Key events") + theme 
main

ggsave(main, file = paste0(git.path, "BGD_Covid-19/Jul2021/output/Fig_1_trends_events.svg"), units = "cm", dpi = "retina", width = 35, height = 15)
ggsave(main, file = paste0(git.path, "BGD_Covid-19/Jul2021/output/Fig_1_trends_events.png"), units = "cm", dpi = "retina", width = 35, height = 15)

##########################################################
# Clean plot for showing on presentations
p1 <- ggplot(data, aes(x=date)) + 

  geom_line(aes(y = cases), color="black", alpha=0.2) +
  geom_line(aes(y = cases_07), color="black", alpha=1, size=1) +
  geom_line(aes(y = deaths*30), color="darkred", alpha=0.2) +
  geom_line(aes(y = deaths_07*30), color="darkred", alpha=1, size=1) +  
  
  scale_x_date(limits=c(ymd("2020-01-01"), ymd("2021-07-17")), date_breaks="2 month", date_labels = "%m/%Y") + 
  scale_y_continuous(sec.axis = sec_axis(~ ./30, name = "Deaths (7-day avg) & reinfections")) + 
  ggthemes::scale_color_few() +
  labs(x="", y="Cases (7-day avg)", color="Key events") + theme 
p1
ggsave(p1, file = paste0(git.path, "BGD_Covid-19/Jul2021/output/Timeline_clean.png"), units = "cm", dpi = "retina", width = 35, height = 20)

##########################################################
# Quick stats on monthly and annual cases and deaths
monthly_BGD <- BGD %>% 
  mutate(year = year(date), 
         month = factor(floor_date(date, "month"))) %>% 
  group_by(month, year) %>%
  summarise(case_n = sum(cases),
            death_n = sum(deaths))
monthly_BGD

yearly_BGD <- BGD %>% 
  mutate(year = year(date), 
         month = factor(floor_date(date, "month"))) %>% 
  group_by(year) %>%
  summarise(case_n = sum(cases),
            death_n = sum(deaths))
yearly_BGD
sum(yearly_BGD$case_n)

# 2021 cases 
61446/138142 # Dhaka
11731/138142 # Chittagong
# March 2021 cases 
45844/65079 # Dhaka 
6499/65079 # Chittagong
