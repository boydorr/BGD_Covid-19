# Plot the timeseries of covid-19 confirmed cases and deaths as well as key events
# Yacob Haddou - 12 April 2021
git.path <- "insert your git folder here/"
git.path <- "D:/GITHUB/"
git.path <- "/Users/katiehampson/Github/"

# Load libraries
#devtools::install_github("RamiKrispin/coronavirus")
library(tidyverse)
library(coronavirus) # update_dataset()
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

data <- coronavirus %>% 
          filter(country == "Bangladesh") %>% 
          spread(type, cases) %>% arrange(date) %>%
            transmute(date, cases = confirmed, deaths = death, recovered) %>% 
            mutate(cum_cases = cumsum(cases), cum_deaths = cumsum(deaths), cum_recovered = cumsum(recovered)) %>% 
            mutate(cases_07 = floor(rollmean(cases, k = 7, fill = NA, align="right")),
                   cases_14 = floor(rollmean(cases, k = 14, fill = NA, align="right")),
                   deaths_07 = floor(rollmean(deaths, k = 7, fill = NA, align="right")),
                   deaths_14 = floor(rollmean(deaths, k = 14, fill = NA, align="right")))#
data[is.na(data)] <- 0

# keep range of dates for the submitted figure
data <- data %>% filter(date > ymd("2020-01-21")) %>% 
                        filter(date < ymd("2021-04-10"))

#'------------------------------------------------------------------
#'**

p1 <- ggplot(data, aes(x=date)) + 
        geom_segment(aes(x = ymd("2020-03-26"), y = 8000, xend = ymd("2020-03-26"), yend = 0, color="1st lockdown (26/03/2020 - 31/05/2020)"), size=0.6, linetype = "solid") +
        geom_segment(aes(x = ymd("2020-03-26"), y = 0, xend = ymd("2020-05-31"), yend = 0, color="1st lockdown (26/03/2020 - 31/05/2020)"), size=0.7, linetype = "solid") +
        geom_segment(aes(x = ymd("2020-05-31"), y = 8000, xend = ymd("2020-05-31"), yend = 0, color="1st lockdown (26/03/2020 - 31/05/2020)"),size=0.7, linetype = "solid") +   
        
        geom_segment(aes(x = ymd("2021-04-05"), y = 8000, xend = ymd("2021-04-05"), yend = 0, color="2nd lockdown (05/04/2021 - 12/04/2021)"), size=0.7, linetype = "solid") +
        geom_segment(aes(x = ymd("2021-04-05"), y = 0, xend = ymd("2021-04-12"), yend = 0, color="2nd lockdown (05/04/2021 - 12/04/2021)"), size=0.7, linetype = "solid") +
        geom_segment(aes(x = ymd("2021-04-12"), y = 8000, xend = ymd("2021-04-12"), yend = 0, color="2nd lockdown (05/04/2021 - 12/04/2021)"),size=0.7, linetype = "solid") + 
        
        geom_segment(aes(x = ymd("2020-03-08"), y = 8000, xend = ymd("2020-03-08"), yend = 0, color="SARS-CoV-2 first detected (08/03/2020)"), size=0.6, linetype = "solid") +
        
        geom_segment(aes(x = ymd("2020-12-31"), y = 8000, xend = ymd("2020-12-31"), yend = 0, color="B.1.1.7 first record (31/12/2020)"), size=0.7, linetype = "solid") +
        geom_segment(aes(x = ymd("2021-01-24"), y = 8000, xend = ymd("2021-01-24"), yend = 0, color="B.1.351 first record (24/01/2021)"), size=0.7, linetype = "solid") +
        
        geom_line(aes(y = cases), color="black", alpha=0.2) +
                geom_line(aes(y = cases_07), color="black", alpha=1, size=1) +
                geom_line(aes(y = deaths*30), color="darkred", alpha=0.2) +
                geom_line(aes(y = deaths_07*30), color="darkred", alpha=1, size=1) + 
        
        scale_x_date(limits=c(ymd("2020-01-01"), ymd("2021-04-12")), date_breaks="2 month", date_labels = "%m/%Y") + 
        scale_y_continuous(sec.axis = sec_axis(~ ./30, name = "Deaths (7-day rolling mean)")) + 
        ggthemes::scale_color_few() +
        labs(x="", y="Cases (7-day rolling mean)", color="Key events") + theme 

#p1 
ggsave(p1, file = paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/output/Fig_1_trends_events.svg"), units = "cm", dpi = "retina", width = 35, height = 20)
ggsave(p1, file = paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/output/Fig_1_trends_events.png"), units = "cm", dpi = "retina", width = 35, height = 20)


