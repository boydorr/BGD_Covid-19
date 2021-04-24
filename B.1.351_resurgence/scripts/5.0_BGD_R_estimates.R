# Estimate Rt
# Katie Hampson- 9 April 2021
git.path <- "insert your git folder here/"
git.path <- "/Users/katiehampson/Github/"

# devtools::install_github("RamiKrispin/coronavirus") # Install coronavirus package - to show JH data
library(coronavirus); update_dataset()
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(EpiEstim)
library(EpiNow2)
library("caTools")
library(zoo)
library(ggthemes)
library(svglite)
library(Hmisc)

theme <- theme_clean() + theme(axis.title=element_text(size=15), axis.text=element_text(size=12), 
                               legend.text=element_text(size=14), legend.title=element_text(size=15), 
                               plot.background = element_rect(color = "white"), 
                               strip.text=element_text(size=15), title=element_text(size=17))

# Coronavirus package (also from JHU)
data("coronavirus")
covid19_df <- refresh_coronavirus_jhu()

BGD <- covid19_df %>%
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

# reinfection data
reinfections <- readRDS(paste0(git.path, "BGD_COVID-19/B.1.351_resurgence/data/a2i_reinfection_summary.rda"))
reinfections <- reinfections %>%
  mutate(reinfections_07da = floor(rollmean(reinfections_60, k = 7, fill = NA, align="right")),
         reinfections_07da_90 = floor(rollmean(reinfections_90, k = 7, fill = NA, align="right"))) %>%
  ungroup()
######################################################################
# Examine Rt overcourse of pandemic
days = 70:440
dates = BGD$date[days-5] # 1 April 2020 - 5 April 2021
Iday = BGD$cases[days]; nday <- length(Iday)
Iday_07da = BGD$cases_07[(days)-2]; nday_07da <- length(Iday_07da)

window <- 7
t_start <- seq(2, nday-window) # starting at 2 as conditional on the past observations
t_end <- t_start + window # adding 7 to get 7-day windows as bounds included in window

######################################################################
# Approxumate serial interval distribution from Ferguson et al
si_distr = dgamma(0:30, shape = 2.3669, scale = 2.7463)/sum(dgamma(0:30, shape = 2.3669, scale = 2.7463))

######################################################################
Rs.weekly <- estimate_R(Iday, method="non_parametric_si",
                        config = make_config(list(si_distr = si_distr, t_start = t_start, t_end = t_end)))
temp <- Rs.weekly$R %>% select( mean = "Mean(R)", median = "Median(R)", sd = "Std(R)",
                                lower.CI = "Quantile.0.025(R)", upper.CI = "Quantile.0.975(R)")
temp$date <- dates[t_end]
temp <- temp %>% filter(mean > 0)
latest.R <- round(temp$median[nrow(temp)],2)

p1 <- ggplot(data=temp, aes(x=date)) +
  geom_ribbon(aes(ymin=lower.CI, ymax=upper.CI), color="grey70", fill="grey90") +
  geom_line(aes(y=median), color="black") + 
  theme + ylim(0, 3) + labs(x="Time", y=paste0("R (",window," d window)"), 
                            title=paste0("Bangladesh, latest median R number = ",latest.R))
p1

######################################################################
t_start <- seq(2, nday_07da-window) # starting at 2 as conditional on the past observations
t_end <- t_start + window # adding 7 to get 7-day windows as bounds included in window

Rs.weekly <- estimate_R(Iday_07da, method="non_parametric_si",
                        config = make_config(list(si_distr = si_distr, t_start = t_start, t_end = t_end)))
temp <- Rs.weekly$R %>% select( mean = "Mean(R)", median = "Median(R)", sd = "Std(R)",
                                lower.CI = "Quantile.0.025(R)", upper.CI = "Quantile.0.975(R)")
temp$date <- dates[t_end]
temp <- temp %>% filter(mean > 0)
latest.R <- round(temp$median[nrow(temp)],2)

Rt <- ggplot(data=temp, aes(x=date)) +
  geom_ribbon(aes(ymin=lower.CI, ymax=upper.CI), color="grey70", fill="grey90") +
  geom_line(aes(y=median), color="black") + 
  theme + ylim(0, 4) + labs(x="", y=paste0("R (",window," d window)"), title="")
Rt
ggsave(Rt, file = paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/output/Rt.svg"), units = "cm", dpi = "retina", width = 15, height = 9)
ggsave(Rt, file = paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/output/Rt.png"), units = "cm", dpi = "retina", width = 15, height = 9)

#
#####################################################################
# Reinfections
days = which(reinfections$days > as.Date("2020-12-17"))
dates <- reinfections$days[days-5]
Iday = reinfections$reinfections_60[days]; nday <- length(Iday)
Iday_07 = reinfections$reinfections_07da[days]

window <- 7
t_start <- seq(7, nday-window) # starting at 2 as conditional on the past observations
t_end <- t_start + window 

# From daily reinfections
Rs.weekly <- estimate_R(Iday, method="non_parametric_si",
                        config = make_config(list(si_distr = si_distr, t_start = t_start, t_end = t_end)))
temp <- Rs.weekly$R %>% select( mean = "Mean(R)", median = "Median(R)", sd = "Std(R)",
                                lower.CI = "Quantile.0.025(R)", upper.CI = "Quantile.0.975(R)")
temp$date <- dates[t_end]
temp <- temp %>% filter(mean > 0)
latest.R <- round(temp$median[nrow(temp)],2)

Rt_reinfections <- ggplot(data=temp, aes(x=date)) +
  geom_ribbon(aes(ymin=lower.CI, ymax=upper.CI), color="grey70", fill="grey90") +
  geom_line(aes(y=median), color="black") + 
  theme + ylim(0, 3) + labs(x="", y="R", title="Reinfections")
Rt_reinfections

## From daily reinfections - 7 day rolling avg
Rs.weekly <- estimate_R(Iday_07, method="non_parametric_si",
                        config = make_config(list(si_distr = si_distr, t_start = t_start, t_end = t_end)))
temp <- Rs.weekly$R %>% select( mean = "Mean(R)", median = "Median(R)", sd = "Std(R)",
                                lower.CI = "Quantile.0.025(R)", upper.CI = "Quantile.0.975(R)")
temp$date <- dates[t_end]
temp <- temp %>% filter(mean > 0)
latest.R <- round(temp$median[nrow(temp)],2)

Rt_reinfections <- ggplot(data=temp, aes(x=date)) +
  geom_ribbon(aes(ymin=lower.CI, ymax=upper.CI), color="grey70", fill="grey90") +
  geom_line(aes(y=median), color="black") + 
  theme + ylim(0, 4) + labs(x="", y="R", title="Reinfections")
Rt_reinfections
#
#####################################################################
# 2nd wave - Variant info
voc <- read.csv(paste0(git.path,"BGD_Covid-19/B.1.351_resurgence/output/var_freq_20210418.csv"))
startdates <- as.Date(c("2020-12-17", "2020-12-03","2021-01-06")) 
enddate <- as.Date("2021-04-11") 

# 2nd wave - Incidence and dates
# proportion of cases
prop_cases = c(seq(from = 0, to =  voc[11,1], length.out = 15+31), 
  seq(from = voc[11,1], to =  voc[12,1], length.out = 28),
  seq(from = voc[12,1], to =  voc[13,1], length.out = 31),
  seq(from = voc[13,1], to =  voc[14,1], length.out = 11))

prop_cases_min = c(seq(from = 0, to =  voc[11,2], length.out = 29+31), 
               seq(from = voc[11,2], to =  voc[12,2], length.out = 28),
               seq(from = voc[12,2], to =  voc[13,2], length.out = 31),
               seq(from = voc[13,2], to =  voc[14,2], length.out = 11))

prop_cases_max = c(seq(from = 0, to =  voc[11,3], length.out = 26),
                   seq(from = voc[11,3], to =  voc[12,3], length.out = 28),
                   seq(from = voc[12,3], to =  voc[13,3], length.out = 31),
                   seq(from = voc[13,3], to =  voc[14,3], length.out = 11))

# time series of days
d1 = which(BGD$date == startdates[1]):which(BGD$date == enddate)
d2 = which(BGD$date == startdates[2]):which(BGD$date == enddate)
d3 = which(BGD$date == startdates[3]):which(BGD$date == enddate)

length(prop_cases); length(d1)
length(prop_cases_min); length(d2)
length(prop_cases_max); length(d3)

# time series of cases
Iday <- round(BGD$cases[d1]*prop_cases); Iday[1] <- 1
Iday_min <- round(BGD$cases[d2]*prop_cases_min); Iday_min[1] <-1
Iday_max <- round(BGD$cases[d3]*prop_cases_max); Iday_max[1] <-1

nday <- length(Iday)
nday_min <- length(Iday_min)
nday_max <- length(Iday_max)

window <- 7

t_start <- seq(2, nday-window) # starting at 2 as conditional on the past observations
t_end <- t_start + window 
Rs <- estimate_R(Iday, method="non_parametric_si",
                        config = make_config(list(si_distr = si_distr, t_start = t_start, t_end = t_end)))
temp <- Rs$R %>% select( mean = "Mean(R)", median = "Median(R)", sd = "Std(R)", lower.CI = "Quantile.0.025(R)", upper.CI = "Quantile.0.975(R)")
temp$date <- BGD$date[d1[-c(1:8)]]

t_start <- seq(7, nday_min-window) # starting at 2 as conditional on the past observations
t_end <- t_start + window 
Rs_min <- estimate_R(Iday_min, method="non_parametric_si",
                 config = make_config(list(si_distr = si_distr, t_start = t_start, t_end = t_end)))
temp_min <- Rs_min$R %>% select( mean = "Mean(R)", median = "Median(R)", sd = "Std(R)", lower.CI = "Quantile.0.025(R)", upper.CI = "Quantile.0.975(R)")
temp_min$date <- BGD$date[d2[-c(1:13)]]

t_start <- seq(7, nday_max-window) # starting at 2 as conditional on the past observations
t_end <- t_start + window 
Rs_max <- estimate_R(Iday_max, method="non_parametric_si",
                     config = make_config(list(si_distr = si_distr, t_start = t_start, t_end = t_end)))
temp_max <- Rs_max$R %>% select( mean = "Mean(R)", median = "Median(R)", sd = "Std(R)", lower.CI = "Quantile.0.025(R)", upper.CI = "Quantile.0.975(R)")
temp_max$date <- BGD$date[d3[-c(1:13)]]

Rt_voc <- ggplot(data=temp, aes(x=date)) +
  geom_ribbon(data = temp_min, aes(ymin=lower.CI, ymax=upper.CI), color="grey70", fill="grey90") +
  geom_line(data = temp_min, aes(y=median), color="black", linetype = "dashed") + 
  geom_ribbon(data = temp_max, aes(ymin=lower.CI, ymax=upper.CI), color="grey70", fill="grey90") +
  geom_line(data = temp_max, aes(y=median), color="black", linetype = "dashed") + 
  geom_ribbon(data=temp, aes(ymin=lower.CI, ymax=upper.CI), color="grey70", fill="grey70") +
  geom_line(data=temp, aes(y=median), color="black") + 
  theme + ylim(0, 4) + xlim(as.Date("2021-01-15"), as.Date("2021-04-11")) + labs(x="", y="", title="B.1.351")
Rt_voc

# Put all the plots together
p1 <- Rt + 
  annotation_custom(ggplotGrob(Rt_reinfections), xmin = ymd("2020-05-01"), xmax = ymd("2020-11-01"), ymin = 2, ymax = 4.5) +
  annotation_custom(ggplotGrob(Rt_voc), xmin = ymd("2020-10-30"), xmax = ymd("2021-04-15"), ymin = 2, ymax = 4.5)
p1

#p1 
ggsave(p1, file = paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/output/Rt.svg"), units = "cm", dpi = "retina", width = 15, height = 10)
ggsave(p1, file = paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/output/Rt.png"), units = "cm", dpi = "retina", width = 15, height = 10)


# Convert R to understand R0 of new variant
seropos <- c(0.45,0.74) # serology
est_I <- c(sum(Iday[1:96]),sum(Iday[97:280])) # to end of June, and from July-Dec
HI = c(seropos[1], 
       seropos[1] + (seropos[1]*(est_I[2]*.5/est_I[1]))) # Assume 50% loss of initial immunity
dhaka_pop <- 13000000
sum(est_infections)*.7/dhaka_pop
S <- 1-HI
temp$median[70]/S




