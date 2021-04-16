# these libraries need to be loaded
library(tidyverse)

# read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv("data/ecdc_data.csv")

cases <- data %>%
  transmute(date = as.Date(dateRep, "%d/%m/%Y"), 
            cases = cases, deaths = deaths, country = countriesAndTerritories) %>%
  group_by(country) %>%
  arrange(date) %>%
  mutate(cumulative_death = cumsum(deaths),
         cumulative_cases = cumsum(cases),
         date = date-1) %>%  #dates are recorded as day after they happened.  Change to day they happened
  ungroup()

# first_cases <- cases %>%
#   filter(cases>0) %>%
#   group_by(country) %>%
#   summarise(min(date))
# length(which(first_cases$`min(date)`<as.Date("2020-02-01")))

bangladesh = subset(cases, country == "Bangladesh")
bangladesh$cumulative_cases <- cumsum(bangladesh$cases)

# par(mfrow=c(1,1), mgp=c(2,1,0))
# # set up the axis limits
# xmin <- min(bangladesh$date)
# xmax <- max(bangladesh$date)
# ymax <- max(bangladesh$cases)
# xtick <- seq(xmin, xmax, by=7)
# 
# plot(bangladesh$date, bangladesh$cases, type='l',lwd=3,
#      main="New Reported Cases", xlab="Date", ylab="Cases per day",
#      xlim=c(xmin,xmax),  ylim=c(0,ymax), col='blue', xaxt="n")
# axis(side=1, labels = FALSE)
# text(x=xtick,  y=-150, labels = format(xtick,"%b-%d"), srt = 0, xpd = TRUE, cex=0.75)
# points(bangladesh$date, bangladesh$deaths, pch=19, col='red')
# points(bangladesh$date, bangladesh$cumulative_death, pch=19, col='red')
# points(bangladesh$date, bangladesh$cumulative_death*10, pch=19, col='red', cex=0.25) # 10x underdetection

# startdate <- as.Date("2020-01-01") 
# time <- as.Date(out0[,1]+startdate)

