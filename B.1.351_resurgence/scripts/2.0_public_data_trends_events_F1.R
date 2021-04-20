# Plot the timeseries of covid-19 confirmed cases and deaths as well as key events
# Yacob Haddou - 12 April 2021
git.path <- "insert your git folder here/"
git.path <- "D:/GITHUB/"
git.path <- "/Users/katiehampson/Github/"

# Load libraries
#devtools::install_github("RamiKrispin/coronavirus")
library(tidyverse)
library(coronavirus); update_dataset()
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
                        filter(date < ymd("2021-04-10")) # Revisit on 17 April 2021

# reinfection data
reinfections <- readRDS(paste0(git.path, "BGD_COVID-19/B.1.351_resurgence/data/a2i_reinfection_summary.rda"))

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
        
        geom_point(data = reinfections, aes(x = days, y = reinfections_90*30), color="black", alpha=1, size=.5) + 
  
        scale_x_date(limits=c(ymd("2020-01-01"), ymd("2021-04-12")), date_breaks="2 month", date_labels = "%m/%Y") + 
        scale_y_continuous(sec.axis = sec_axis(~ ./30, name = "Deaths (7-day rolling mean) & reinfections")) + 
        ggthemes::scale_color_few() +
        labs(x="", y="Cases (7-day rolling mean)", color="Key events") + theme 
p1

#p1 
ggsave(p1, file = paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/output/Fig_1_trends_events.svg"), units = "cm", dpi = "retina", width = 35, height = 20)
ggsave(p1, file = paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/output/Fig_1_trends_events.png"), units = "cm", dpi = "retina", width = 35, height = 20)

##########################################################
p1 <- ggplot(data, aes(x=date)) + 
  geom_segment(aes(x = ymd("2020-12-31"), y = 8000, xend = ymd("2020-12-31"), yend = 0, color="B.1.1.7 first record (31/12/2020)"), size=0.7, linetype = "solid") +
  geom_segment(aes(x = ymd("2021-01-24"), y = 8000, xend = ymd("2021-01-24"), yend = 0, color="B.1.351 first record (24/01/2021)"), size=0.7, linetype = "solid") +
  
  geom_line(aes(y = cases), color="black", alpha=0.2) +
  geom_line(aes(y = cases_07), color="black", alpha=1, size=1) +
  geom_line(aes(y = deaths*30), color="darkred", alpha=0.2) +
  geom_line(aes(y = deaths_07*30), color="darkred", alpha=1, size=1) +  
  
  geom_point(data = reinfections, aes(x = days, y = reinfections_90*30), color="black", alpha=1, size=.5) + 
  
  scale_x_date(limits=c(ymd("2020-01-01"), ymd("2021-04-12")), date_breaks="2 month", date_labels = "%m/%Y") + 
  scale_y_continuous(sec.axis = sec_axis(~ ./30, name = "Deaths (7-day avg) & reinfections")) + 
  ggthemes::scale_color_few() +
  labs(x="", y="Cases (7-day avg)", color="Key events") + theme 
p1
ggsave(p1, file = paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/output/Timeline_clean.png"), units = "cm", dpi = "retina", width = 35, height = 20)

##########################################################


plot(reinfections$days, reinfections$reinfections_90, pch=20, xlim=c(as.Date("2021-01-10"), as.Date("2021-04-10")))

voc = read.csv(paste0(git.path,"BGD_Covid-19/B.1.351_resurgence/output/variants.csv"))
B.1.351 = voc$X20H.501Y.V2[11:13] # Jan - March (first detected on 24th Jan!)
sequences = voc$freq[11:13] 
var_freq = binconf(x=B.1.351, n=sequences)

B.1.351 = var_m_df$`20H/501Y.V2`[10:13] # Jan - March (first detected on 24th Jan!)
sequences = var_m_df$freq[10:13] 
var_freq = as.data.frame(binconf(x=B.1.351, n=sequences))
var_freq$month <- as.Date(var_m_df$month[10:13])+20
var_freq$voc <- "B.1.351"

# Plot frequency of variants over time and emergence of reinfections
ggplot(var_freq, aes(x=month, y=PointEst)) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=2, colour = "black") +
  geom_line(position = pd, colour = "black") +
  geom_point(colour = "black") + 
  geom_point(data = reinfections, aes(x = days, y = reinfections_90/100)) + 
  scale_x_date(limits=c(ymd("2020-12-15"), ymd("2021-04-12")), date_breaks="2 week", date_labels = "%m/%Y") + 
  theme

dec <- which(sequences$`Collection Data`>"2020-11-30" & sequences$`Collection Data`<"2021-01-01" )
jan <- which(sequences$`Collection Data`>"2020-12-31" & sequences$`Collection Data`<"2021-02-01" )
feb <- which(sequences$`Collection Data`>"2021-01-31" & sequences$`Collection Data`<"2021-03-01" )
mar <- which(sequences$`Collection Data`>"2021-02-28" & sequences$`Collection Data`<"2021-04-01" )

require(lubridate)
par(mfrow=cs(2,2))
hist(yday(sequences$`Collection Data`[dec]), breaks = 0:367)
hist(yday(sequences$`Collection Data`[jan]), breaks = 0:120)
hist(yday(sequences$`Collection Data`[feb]), breaks = 0:120)
hist(yday(sequences$`Collection Data`[mar]), breaks = 0:120)

# Plot reinfections and rise in cases
ggplot() + 
  geom_point(data = reinfections, aes(x = days, y = reinfections_90)) + 
  geom_line(data = data, aes(x = date, y = deaths), color="red", alpha=0.2) + theme

oct <- reinfections$reinfections_90[which(reinfections$days>"2020-09-30" & reinfections$days<"2020-11-01" )]
mar <- reinfections$reinfections_90[which(reinfections$days>"2021-02-28" & reinfections$days<"2021-04-01" )]
apr <- reinfections$reinfections_90[which(reinfections$days>"2021-03-15" & reinfections$days<"2021-04-10" )]


oct_cases <- data$cases[which(data$date>"2020-09-30" & data$date<"2020-11-01" )]
mar_cases <- data$cases[which(data$date>"2021-02-28" & data$date<"2021-04-01" )]
apr_cases <- data$cases[which(data$date>"2021-03-15" & data$date<"2021-04-10" )]

sum(oct)/sum(oct_cases)
sum(mar)/sum(mar_cases)
sum(apr)/sum(apr_cases)