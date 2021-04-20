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
BGD <- coronavirus %>% 
  filter(country == "Bangladesh") %>% 
  spread(type, cases) %>% 
  arrange(date) %>%
  mutate(deaths = death, cases = confirmed) %>% 
  mutate(cumulative_death = cumsum(death), cumulative_cases = cumsum(confirmed)) %>% 
  mutate(case_05da = round(zoo::rollmean(cases, k = 5, fill = NA, align = "right"),0),
         case_07da = round(zoo::rollmean(cases, k = 7, fill = NA, align = "right"),0),
         case_10da = round(zoo::rollmean(cases, k = 10, fill = NA, align = "right"),0),
         case_14da = round(zoo::rollmean(cases, k = 14, fill = NA, align = "right"),0),
         death_05da = round(zoo::rollmean(deaths, k = 5, fill = NA, align = "right"),0),
         death_07da = round(zoo::rollmean(deaths, k = 7, fill = NA, align = "right"),0),
         death_10da = round(zoo::rollmean(deaths, k = 10, fill = NA, align = "right"),0),
         death_14da = round(zoo::rollmean(deaths, k = 14, fill = NA, align = "right"),0)) %>%
  ungroup()

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

# reinfection data
reinfections <- readRDS(paste0(git.path, "BGD_COVID-19/B.1.351_resurgence/data/a2i_reinfection_summary.rda"))

######################################################################
# Examine Rt overcourse of pandemic
days = 71:440
dates = BGD$date[days-5] # 1 April 2020 - 5 April 2021
Iday = BGD$cases[days]; nday <- length(Iday)
Iday_07da = BGD$case_07da[(days)-2]; nday_07da <- length(Iday_07da)

window <- 7
t_start <- seq(2, nday-window) # starting at 2 as conditional on the past observations
t_end <- t_start + window # adding 7 to get 7-day windows as bounds included in window

######################################################################
si_distr = dgamma(0:30, shape = 2.3669, scale = 2.7463)/sum(dgamma(0:30, shape = 2.3669, scale = 2.7463))
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

p1 <- ggplot(data=temp, aes(x=date)) +
  geom_ribbon(aes(ymin=lower.CI, ymax=upper.CI), color="grey70", fill="grey90") +
  geom_line(aes(y=median), color="black") + 
  theme + ylim(0, 3) + labs(x="Time", y=paste0("R (",window," d window)"), 
                            title=paste0("Bangladesh, latest median R number = ",latest.R))
p1
#
#####################################################################
# Reinfections
days = which(reinfections$days > (as.Date("2021-01-23")+10))
dates <- reinfections$days[days-5]
Iday = reinfections$reinfections_90[days]; nday <- length(Iday)
window <- 7
t_start <- seq(7, nday-window) # starting at 2 as conditional on the past observations
t_end <- t_start + window 

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
  theme + ylim(0, 3) + labs(x="Time", y=paste0("R (",window," d window)"), title="R estimated from Reinfections")
p1
#
ggsave(p1, file = paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/output/R_from_reinfections.png"), units = "cm", dpi = "retina", width = 35, height = 20)



#####################################################################
t_start <-seq(2, length(days) -13)   
t_end <- t_start + 13                
res <- estimate_R(incid = BGD$case_10da[days], 
                  method = "non_parametric_si",
                  config = make_config(list(
                    si_distr = si_distr, 
                    t_start = t_start, 
                    t_end = t_end)))
plot(t_end, res$R$`Mean(R)`, type="l", ylim=c(0,4))
lines(t_end, res$R$`Quantile.0.025(R)`, type="l", col="grey")
lines(t_end, res$R$`Quantile.0.975(R)`, type="l", col="grey")
lines(t_end, rep(1, length(t_end)))
lines(t_end, rep(2, length(t_end)))


## EpiFilter: provides formally smoothed and exact estimates
# Method based on Bayesian recursive filtering and smoothing
# Source functions
source(paste0(git.path, "EpiFilter/epiFilter.R"))
source("EpiFilter/recursPredict.R")
source("EpiFilter/epiSmoother.R")
source("EpiFilter/plotEpiFilter.R")
######################################################################
# Incidence and dates - 1st wave
days = 71:440
dates = BGD$date[days-5] # 1 April 2020 - 5 April 2021
Iday = BGD$cases[days]
Iday_05da = BGD$case_05da[(days)-2]
Iday_10da = BGD$case_10da[(days)-5]

# 2nd wave - Variant info
voc = read.csv(paste0(f.path,"variants.csv"))
B.1.351 = voc$X20H.501Y.V2[11:13] # Jan - March (first detected on 24th Jan!)
sequences = voc$freq[11:13] 
var_freq = binconf(x=B.1.351, n=sequences)
# 2nd wave - Incidence and dates
extrapolate_days <- 15+14+14+30
days2 = (length(days) - extrapolate_days - 4): (length(days)-4)
var_cases = round(Iday_10da[days2] *
  c(seq(from = var_freq[1,1], to = var_freq[2,1], length.out= 15+14),
    seq(from = var_freq[2,1], to = var_freq[3,1], length.out= 14+31)), 0) 
var_max = round(Iday_10da[days2] *
  c(seq(from = var_freq[1,2], to = var_freq[2,1], length.out= 15+14),
    seq(from = var_freq[2,1], to = var_freq[3,3], length.out= 14+31)), 0) 
var_min = round(Iday_10da[days2] *
  c(seq(from = var_freq[1,3], to = var_freq[2,1], length.out= 15+14),
    seq(from = var_freq[2,1], to = var_freq[3,2], length.out= 14+31)), 0)
dates2 = dates[days2]
plot(dates2,var_cases)

# Total infectiousness
# Wave 1 
Lday = rep(0, nday) 
for(i in 2:nday){Lday[i] = sum(Iday[seq(i-1, 1, -1)]*wdist[1:(i-1)])}
plot(tday, Lday, "l")

# Wave 2
Lday2 = Lday_min = Lday_max <- rep(0, length(days2))
for(i in 2:length(days2)){
  Lday2[i] = sum(var_cases[seq(i-1, 1, -1)]*wdist[1:(i-1)])
  Lday_min[i] = sum(var_min[seq(i-1, 1, -1)]*wdist[1:(i-1)])
  Lday_max[i] = sum(var_max[seq(i-1, 1, -1)]*wdist[1:(i-1)])
}
plot(days2, Lday2, "l")
lines(days2, Lday_min, col="light gray")
lines(days2, Lday_max, col="light gray")

# Approxumate serial interval distribution from Ferguson et al
wdist = dgamma(tday, shape = 2.3669, scale = 2.7463)
wdist2 = dgamma(tday, shape = (3.6/3.1)^2, rate = 3.6/(3.1^2)) # mean 3.6 days and SD of 3.1 days
gen_cov = wdist[1:21]/sum(wdist[1:21])
plot(tday, wdist,type="l", col="red",  xlim = c(0,30), ylim = c(0,.4))
lines(tday, wdist2)
  
# Time series - entire trajectory
nday = length(dates); tday = 1:nday
plot(tday, Iday, "l")
lines(tday, Iday_10da, col = "grey")

nday2 = length(dates2); tday2 = 1:nday2
plot(tday2, var_cases, "l")
lines(tday2, var_min, col = "grey")
lines(tday2, var_max, col = "grey")

# Setup grid and noise parameters
Rmin = 0.01; Rmax = 10; eta = 0.1
# Uniform prior over grid of size m
m = 200; pR0 = (1/m)*rep(1, m)
# Delimited grid defining space of R
Rgrid = seq(Rmin, Rmax, length.out = m)

# Filtered (causal) estimates as list [Rmed, Rhatci, Rmean, pR, pRup, pstate]
Rfilt = epiFilter(Rgrid, m, eta, pR0, nday, Lday[tday], Iday[tday], 0.025)
Rfilt_10da = epiFilter(Rgrid, m, eta, pR0, nday, Lday[tday], Iday_10da[tday], 0.025)
Rfilt_var_cases = epiFilter(Rgrid, m, eta, pR0, nday2, Lday2[tday2], var_cases[tday2], 0.025)
Rfilt_var_min = epiFilter(Rgrid, m, eta, pR0, nday2, Lday2[tday2], var_min[tday2], 0.025)
Rfilt_var_max = epiFilter(Rgrid, m, eta, pR0, nday2, Lday2[tday2], var_max[tday2], 0.025)

# Causal predictions from filtered estimates [pred predci]
Ifilt = recursPredict(Rgrid, Rfilt[[4]], Lday[tday], Rfilt[[3]], 0.025)
Ifilt_10da = recursPredict(Rgrid, Rfilt_10da[[4]], Lday[tday], Rfilt_10da[[3]], 0.025)
Ifilt_var_cases = recursPredict(Rgrid, Rfilt_var_cases[[4]], Lday2[tday2], Rfilt_var_cases[[3]], 0.025)
Ifilt_var_min = recursPredict(Rgrid, Rfilt_var_min[[4]], Lday2[tday2], Rfilt_var_min[[3]], 0.025)
Ifilt_var_max = recursPredict(Rgrid, Rfilt_var_max[[4]], Lday2[tday2], Rfilt_var_max[[3]], 0.025)

# Smoothed estimates as list of [Rmed, Rhatci, Rmean, qR]
Rsmooth_10da = epiSmoother(Rgrid, m, Rfilt_10da[[4]], Rfilt_10da[[5]], nday, Rfilt_10da[[6]], 0.025)
Rsmooth_var = epiSmoother(Rgrid, m, Rfilt_var_cases[[4]], Rfilt_var_cases[[5]], nday2, Rfilt_var_cases[[6]], 0.025)
Rsmooth_vmin = epiSmoother(Rgrid, m, Rfilt_var_min[[4]], Rfilt_var_min[[5]], nday2, Rfilt_var_min[[6]], 0.025)
Rsmooth_vmax = epiSmoother(Rgrid, m, Rfilt_var_max[[4]], Rfilt_var_max[[5]], nday2, Rfilt_var_max[[6]], 0.025)

# Smoothed predictions from filtered estimates [pred predci]
Ismooth_10da = recursPredict(Rgrid, Rsmooth_10da[[4]], Lday[tday], Rsmooth_10da[[3]], 0.025)
Ismooth_var = recursPredict(Rgrid, Rsmooth_var[[4]], Lday2[tday2], Rsmooth_var[[3]], 0.025)
Ismooth_vmin = recursPredict(Rgrid, Rsmooth_vmin[[4]], Lday2[tday2], Rsmooth_vmin[[3]], 0.025)
Ismooth_vmax = recursPredict(Rgrid, Rsmooth_vmax[[4]], Lday2[tday2], Rsmooth_vmax[[3]], 0.025)

par(mfrow=c(2,1), mar=c(2,2,1,1))
plot(BGD_df$date, BGD_df$cases, type = "l")
lines(BGD_df$date, BGD_df$case_10da, col="grey") #lines(BGD_df$date, BGD_df$case_14da)
plot(dates, Rfilt[[1]], type="l", ylim=c(0,6))
lines(dates, rep(1,length(days)), col="grey")
lines(dates, Rfilt_10da[[1]], col="red")

folres <- paste0("./results/")

# Reprod. num estimates and confidence interval
pdf(paste0(f.path, "estimated_R_ranges.pdf"), 4, 5)

par(mfrow=c(2,1), mar=c(2,2,1,1))

Rhat <- Rfilt[[3]][2:nday]
Rhatci <- Rfilt[[2]][, 2:nday]
plot(dates[-1], Rhat, type = 'l', bty = 'l', lwd = 1.5, col='blue', main = "Rfilt",
     xlab = paste0("time (eta = ", eta, ")"), ylab = "reprod. number", ylim=c(0,4))
polygon(c(dates[-1], rev(dates[-1])), c(Rhatci[1,], rev(Rhatci[2,])), 
        col = adjustcolor("dodgerblue", alpha.f = 0.70), border = NA)
polygon(c(dates[-1], rev(dates[-1])), c(Rhatci[3,], rev(Rhatci[4,])), 
        col =  adjustcolor("dodgerblue", alpha.f = 0.70), border = NA)
lines(dates[-1], rep(1, length(dates[-1])), lwd = 2, col = 'black', lty = 'dashed')

Rhat_smooth <- Rsmooth_10da[[3]][2:nday]
Rhatci_smooth <- Rsmooth_10da[[2]][, 2:nday]
plot(dates[-1], Rhat_smooth, type = 'l', bty = 'l', lwd = 1.5, col='blue',
     xlab = paste0("time (eta = ", eta, ")"), ylab = "reprod. number", ylim=c(0,4))
polygon(c(dates[-1], rev(dates[-1])), c(Rhatci_smooth[1,], rev(Rhatci_smooth[2,])), 
        col =  adjustcolor("dodgerblue", alpha.f = 0.50), border = NA)
polygon(c(dates[-1], rev(dates[-1])), c(Rhatci_smooth[3,], rev(Rhatci_smooth[4,])), 
        col =  adjustcolor("dodgerblue", alpha.f = 0.70), border = NA)
lines(dates[-1], rep(1, length(dates[-1])), lwd = 2, col = 'black', lty = 'dashed')

Rhat <- Rfilt_var_cases[[3]][2:nday2]
Rhatci <- Rfilt_var_cases[[2]][, 2:nday2]
Rhatci_min <- Rfilt_var_min[[2]][, 2:nday2]
Rhatci_max <- Rfilt_var_max[[2]][, 2:nday2]

plot(dates2[-1], Rhat, type = 'l', bty = 'l', lwd = 1.5, col='blue', main = "Rfilt var",
     xlab = paste0("time (eta = ", eta, ")"), ylab = "reprod. number", ylim=c(0,4))
polygon(c(dates2[-1], rev(dates2[-1])), c(Rhatci[1,], rev(Rhatci[2,])), 
        col = adjustcolor("dodgerblue", alpha.f = 0.20), border = NA)
polygon(c(dates2[-1], rev(dates2[-1])), c(Rhatci[3,], rev(Rhatci[4,])), 
        col =  adjustcolor("dodgerblue", alpha.f = 0.50), border = NA)
polygon(c(dates2[-1], rev(dates2[-1])), c(Rhatci_min[1,], rev(Rhatci_min[2,])), 
        col = adjustcolor("red", alpha.f = 0.20), border = NA)
polygon(c(dates2[-1], rev(dates2[-1])), c(Rhatci_min[3,], rev(Rhatci_min[4,])), 
        col =  adjustcolor("red", alpha.f = 0.50), border = NA)
polygon(c(dates2[-1], rev(dates2[-1])), c(Rhatci_max[1,], rev(Rhatci_max[2,])), 
        col = adjustcolor("green", alpha.f = 0.20), border = NA)
polygon(c(dates2[-1], rev(dates2[-1])), c(Rhatci_max[3,], rev(Rhatci_max[4,])), 
        col =  adjustcolor("green", alpha.f = 0.50), border = NA)
lines(dates2[-1], rep(1, length(dates2[-1])), lwd = 2, col = 'black', lty = 'dashed')

Rhat <- Rsmooth_var[[3]][2:nday2]
Rhatci <- Rsmooth_var[[2]][, 2:nday2]
Rhatci_min <- Rsmooth_vmin[[2]][, 2:nday2]
Rhatci_max <- Rsmooth_vmax[[2]][, 2:nday2]

plot(dates2[-1], Rhat, type = 'l', bty = 'l', lwd = 1.5, col='blue', main = "",
     xlab = paste0("time (eta = ", eta, ")"), ylab = "reprod. number", ylim=c(0,3), 
     xlim=c(as.Date("2021-02-10"),as.Date("2021-04-01")))
polygon(c(dates2[-1], rev(dates2[-1])), c(Rhatci[1,], rev(Rhatci[2,])), 
        col = adjustcolor("dodgerblue", alpha.f = 0.20), border = NA)
polygon(c(dates2[-1], rev(dates2[-1])), c(Rhatci[3,], rev(Rhatci[4,])), 
        col =  adjustcolor("dodgerblue", alpha.f = 0.50), border = NA)
polygon(c(dates2[-1], rev(dates2[-1])), c(Rhatci_min[1,], rev(Rhatci_max[2,])), 
        col = adjustcolor("red", alpha.f = 0.20), border = NA)
polygon(c(dates2[-1], rev(dates2[-1])), c(Rhatci_min[3,], rev(Rhatci_max[4,])), 
        col =  adjustcolor("red", alpha.f = 0.50), border = NA)
lines(dates2[-1], rep(1, length(dates2[-1])), lwd = 2, col = 'black', lty = 'dashed')
dev.off()

# Convert R to understand R0 of new variant
seropos <- c(0.45,0.74) # serology
est_I <- c(sum(Iday[1:96]),sum(Iday[97:280])) # to end of June, and from July-Dec
HI = c(seropos[1], 
       seropos[1] + (seropos[1]*(est_I[2]*.5/est_I[1]))) # Assume 50% loss of initial immunity
dhaka_pop <- 13000000
sum(est_infections)*.7/dhaka_pop
S <- 1-HI
temp$median[70]/S


# Plot estimates and predictions from filtering
plotEpiFilter(Rfilt[[3]][2:nday], Rfilt[[2]][, 2:nday], Ifilt[[1]], Ifilt[[2]],
              'EpiFilter', Iday[2:nday], folres, eta)

# Plot estimates and predictions from smoothing
plotEpiFilter(Rsmooth[[3]][2:nday], Rsmooth[[2]][, 2:nday], Ismooth[[1]], Ismooth[[2]],
              'EpiSmooth', Iday[2:nday], folres, eta)




pdf(paste0(f.path, "estimated_R.pdf"), 4, 5)
par(mfrow=c(2,1))
# Reprod. num estimates and confidence interval
Rhat <- Rfilt_14da[[3]][2:nday]
Rhatci <- Rfilt_14da[[2]][, 2:nday]

plot(dates[-1], Rhat, type = 'l', bty = 'l', lwd = 1.5, col='blue',
     xlab = paste0("time (eta = ", eta, ")"), ylab = "reprod. number", ylim=c(0,4))
polygon(c(tset, rev(tset)), c(Rhatci[1,], rev(Rhatci[2,])), 
        col =  adjustcolor("dodgerblue", alpha.f = 0.50), border = NA)
polygon(c(tset, rev(tset)), c(Rhatci[3,], rev(Rhatci[4,])), 
        col =  adjustcolor("dodgerblue", alpha.f = 0.70), border = NA)
lines(tset, rep(1, length(tset)), lwd = 2, col = 'black', lty = 'dashed')

Rhat_smooth <- Rsmooth_10da[[3]][2:nday]
Rhatci_smooth <- Rsmooth_10da[[2]][, 2:nday]
plot(dates[-1], Rhat_smooth, type = 'l', bty = 'l', lwd = 1.5, col='blue',
     xlab = paste0("time (eta = ", eta, ")"), ylab = "reprod. number", ylim=c(0,2))
polygon(c(tset, rev(tset)), c(Rhatci[1,], rev(Rhatci[2,])), 
        col =  adjustcolor("dodgerblue", alpha.f = 0.50), border = NA)
polygon(c(tset, rev(tset)), c(Rhatci[3,], rev(Rhatci[4,])), 
        col =  adjustcolor("dodgerblue", alpha.f = 0.70), border = NA)
lines(tset, rep(1, length(tset)), lwd = 2, col = 'black', lty = 'dashed')
dev.off()

Rhat <- Rsmooth[[3]][2:nday]
Rhatci <- Rsmooth[[2]][, 2:nday]
plot(dates[-1], Rhat, type = 'l', bty = 'l', lwd = 1.5, col='blue',
     xlab = paste0("time (eta = ", eta, ")"), ylab = "reprod. number", ylim=c(0,2))
polygon(c(tset, rev(tset)), c(Rhatci[1,], rev(Rhatci[2,])), 
        col =  adjustcolor("dodgerblue", alpha.f = 0.50), border = NA)
polygon(c(tset, rev(tset)), c(Rhatci[3,], rev(Rhatci[4,])), 
        col =  adjustcolor("dodgerblue", alpha.f = 0.70), border = NA)
lines(tset, rep(1, length(tset)), lwd = 2, col = 'black', lty = 'dashed')
dev.off()

# Incidence predictions and confidence interval
Inexhat <- Ifilt[[1]]
Inexhatci <- Ifilt[[2]]
Iplt <- Iday[2:nday]

plot(tset, Inexhat, type = 'l', bty = 'l', lwd = 2, col='blue',
     xlab = paste0("time (eta = ", eta, ")"), ylab = "incidence", ylim = c(0, max(Iplt)+30))
polygon(c(tset, rev(tset)), c(Inexhatci[1,], rev(Inexhatci[2,])),
        col =  adjustcolor("dodgerblue", alpha.f = 0.50), border = NA)
polygon(c(tset, rev(tset)), c(Inexhatci[3,], rev(Inexhatci[4,])), 
        col =  adjustcolor("dodgerblue", alpha.f = 0.70), border = NA)
points(tset, Iplt, pch = 19, col = 'black', cex=0.1)




######################################################################
#  Try with estimate_R
######################################################################


window <- 7
t_start <- seq(2, nday-window) # starting at 2 as conditional on the past observations
t_end <- t_start + window # adding 7 to get 7-day windows as bounds included in window

Rs.weekly <- estimate_R(Iday, method="parametric_si",
                        config = make_config(list(t_start = t_start, t_end = t_end, 
                                                   mean_si = 5.2, std_si = 0.1)))

temp <- Rs.weekly$R %>% select( mean = "Mean(R)",
                                median = "Median(R)",
                                sd = "Std(R)",
                                lower.CI = "Quantile.0.025(R)",
                                upper.CI = "Quantile.0.975(R)")
temp$date <- dates[t_end]
temp <- temp %>% filter(mean > 0)
latest.R <- round(temp$median[nrow(temp)],2)

p1 <- ggplot(data=temp, aes(x=date)) +
  geom_ribbon(aes(ymin=lower.CI, ymax=upper.CI), color="grey70", fill="grey90") +
  geom_line(aes(y=median), color="black") + 
  theme + ylim(0, 3) + labs(x="Time", y=paste0("R (",window," d window)"), 
               title=paste0("Bangladesh, latest median R number = ",latest.R))
p1

###############################################################################
Rs.weekly <- estimate_R(Iday_14da, method="parametric_si",
                        config = make_config(list(t_start = t_start, t_end = t_end, 
                                                  mean_si = 5.2, std_si = 0.1)))
###############################################################################
###############################################################################
# TAKE AWAY BASELINE
window <- 7
t_start <- seq(2, 90-window) # starting at 2 as conditional on the past observations
t_end <- t_start + window # adding 7 to get 7-day windows as bounds included in window

Rs.weekly <- estimate_R(Iday_14da[(nday-90):nday]-280, method="parametric_si",
                        config = make_config(list(t_start = t_start, t_end = t_end, 
                                                  mean_si = 5.2, std_si = 0.1)))

temp <- Rs.weekly$R %>% select( mean = "Mean(R)",
                                median = "Median(R)",
                                sd = "Std(R)",
                                lower.CI = "Quantile.0.025(R)",
                                upper.CI = "Quantile.0.975(R)")



# Convert R to understand R0 of new variant
est_infections <- sum(Iday) * 10
dhaka_pop <- 13000000
est_infections/dhaka_pop
S <- 1-prop_immune
Rhat[70:89]/S

###############################################################################
# Filtered (causal) estimates as list [Rmed, Rhatci, Rmean, pR, pRup, pstate]
Rfilt = epiFilter(Rgrid, m, eta, pR0, days2, Lday, Ivoc, 0.025)

par(mfrow=c(2,1), mar=c(2,2,1,1))
plot(ds[-1], Rfilt[[1]], type="l", ylim=c(0,6))
lines(dates, rep(1,length(days)), col="grey")

# Causal predictions from filtered estimates [pred predci]
Ifilt = recursPredict(Rgrid, Rfilt[[4]], Lday, Rfilt[[3]], 0.025)

# Smoothed estimates as list of [Rmed, Rhatci, Rmean, qR]
Rsmooth = epiSmoother(Rgrid, m, Rfilt[[4]], Rfilt[[5]], days2, Rfilt[[6]], 0.025)
# Smoothed predictions from filtered estimates [pred predci]
Ismooth = recursPredict(Rgrid, Rsmooth[[4]], Lday, Rsmooth[[3]], 0.025)


Rhat <- Rfilt[[3]][2:days2]
Rhatci <- Rfilt[[2]][, 2:days2]
tset = 1:length(Rhat)

pdf(paste0(f.path, "voc_R.pdf"), 4, 5)
plot(tset, Rhat, type = 'l', bty = 'l', lwd = 1.5, col='blue',
     xlab = paste0("time (eta = ", eta, ")"), ylab = "reprod. number", 
     ylim=c(0,2), xlim=c(30,90))
polygon(c(tset, rev(tset)), c(Rhatci[1,], rev(Rhatci[2,])), 
        col =  adjustcolor("dodgerblue", alpha.f = 0.50), border = NA)
polygon(c(tset, rev(tset)), c(Rhatci[3,], rev(Rhatci[4,])), 
        col =  adjustcolor("dodgerblue", alpha.f = 0.70), border = NA)
lines(tset, rep(1, length(tset)), lwd = 2, col = 'black', lty = 'dashed')


Rhat <- Rsmooth[[3]][2:days2]
Rhatci <- Rsmooth[[2]][, 2:days2]
plot(tset, Rhat, type = 'l', bty = 'l', lwd = 1.5, col='blue',
     xlab = paste0("time (eta = ", eta, ")"), ylab = "reprod. number", ylim=c(0,2))
polygon(c(tset, rev(tset)), c(Rhatci[1,], rev(Rhatci[2,])), 
        col =  adjustcolor("dodgerblue", alpha.f = 0.50), border = NA)
polygon(c(tset, rev(tset)), c(Rhatci[3,], rev(Rhatci[4,])), 
        col =  adjustcolor("dodgerblue", alpha.f = 0.70), border = NA)
lines(tset, rep(1, length(tset)), lwd = 2, col = 'black', lty = 'dashed')
dev.off()

#' -----------------------------------------------------------------------------------------------------
#' * R curve for each district*

#windows <- c(6, 13) # for daily data
windows <- c(1, 3, 6) # 1 = 1 week, 5 = 5 weeks back check

for (window in windows) {
  
  #' -----------------------------------------------------------------------------------------------------
  #' * run R estimation for different moving window*
  
  nday <- nrow(data)
  t_start <- seq(2, nday-window) # starting at 2 as conditional on the past observations
  t_end <- t_start + window # adding 3 to get 3-day windows as bounds included in window
  
  Rs.weekly <- estimate_R(data$cases, 
                          method="parametric_si",
                          config = make_config(list( t_start = t_start,
                                                     t_end = t_end, 
                                                     mean_si = 5.2, 
                                                     std_si = 0.1)))
  
  temp <- Rs.weekly$R %>% select( mean = "Mean(R)",
                                  median = "Median(R)",
                                  sd = "Std(R)",
                                  lower.CI = "Quantile.0.025(R)",
                                  upper.CI = "Quantile.0.975(R)")
  temp$date <- data$date[t_end]
  temp <- temp %>% filter(mean > 0)
  latest.R <- round(temp$median[nrow(temp)],2)
  
  p1 <- ggplot(data=temp, aes(x=date)) +
    geom_ribbon(aes(ymin=lower.CI, ymax=upper.CI), color="grey70", fill="grey90") +
    geom_line(aes(y=median), color="black") +
    theme + labs(x="Time", y=paste0("R (",window," week window)"), 
                 title=paste0("Bangladesh, latest median R number = ",latest.R))
  p1
  
  ggsave(p1, file = paste0(f.path,"BGD/R_weekly_full_",window,".svg"),
         units = "cm", dpi = "retina", width =60, height = 30)
}



generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
reported_cases <- example_confirmed[1:90]
reporting_delay <- list(mean = convert_to_logmean(3, 1), 
     mean_sd = 0.1, sd = convert_to_logsd(3, 1), sd_sd = 0.1, max = 10)
estimates <- epinow(reported_cases = reported_cases, 
                    generation_time = generation_time,
                    delays = delay_opts(incubation_period, reporting_delay),
                    rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
                    stan = stan_opts(cores = 4))
names(estimates)
knitr::kable(summary(estimates))

head(summary(estimates, type = "parameters", params = "R"))

par(mfrow=c(2,1), mar=c(2,2,1,1))
plot(reported_cases$date, reported_cases$confirm, type = "l")
plot(summary(estimates, type = "parameters", params = "R")$date,
     summary(estimates, type = "parameters", params = "R")$median)




nday <- nrow(data)
t_start <- seq(2, nday-window) # starting at 2 as conditional on the past observations
t_end <- t_start + window # adding 3 to get 3-day windows as bounds included in window

Rs <- estimate_R(data$cases, 
                 method="parametric_si",
                 config = make_config(list( t_start = t_start,
                                            t_end = t_end, 
                                            mean_si = 5.2, 
                                            std_si = 0.1)))

temp <- Rs.weekly$R %>% select( mean = "Mean(R)",
                                median = "Median(R)",
                                sd = "Std(R)",
                                lower.CI = "Quantile.0.025(R)",
                                upper.CI = "Quantile.0.975(R)")
temp$date <- data$date[t_end]
temp <- temp %>% filter(mean > 0)
latest.R <- round(temp$median[nrow(temp)],2)


data("Flu2009")
t_start <- seq(2, nrow(Flu2009$incidence)-13)   
t_end <- t_start + 13                 
res <- estimate_R(incid = Flu2009$incidence, 
                  method = "non_parametric_si",
                  config = make_config(list(
                    si_distr = Flu2009$si_distr, 
                    t_start = t_start, 
                    t_end = t_end)))
plot(t_end, res$R$`Mean(R)`)

si_distr = dgamma(0:30, shape = 2.3669, scale = 2.7463)/sum(dgamma(0:30, shape = 2.3669, scale = 2.7463))
days = 71:440
t_start <-seq(2, length(days) -13)   
t_end <- t_start + 13                
res <- estimate_R(incid = BGD$case_10da[days], 
                  method = "non_parametric_si",
                  config = make_config(list(
                    si_distr = si_distr, 
                    t_start = t_start, 
                    t_end = t_end)))
plot(t_end, res$R$`Mean(R)`, type="l")
lines(t_end, res$R$`Quantile.0.025(R)`, type="l", col="grey")
lines(t_end, res$R$`Quantile.0.975(R)`, type="l", col="grey")
lines(t_end, rep(1, length(t_end)))
lines(t_end, rep(2, length(t_end)))

###############################################################
t_days = 15+14+14+31
t_window = (length(Iday)-t_days+1):length(Iday)
var_cases = round(Iday[t_window] *
                    c(seq(from = var_freq[1,1], to = var_freq[2,1], length.out= 15+14),
                      seq(from = var_freq[2,1], to = var_freq[3,1], length.out= 14+31)), 0) 
var_max = round(Iday[t_window] *
                  c(seq(from = var_freq[1,2], to = var_freq[2,1], length.out= 15+14),
                    seq(from = var_freq[2,1], to = var_freq[3,3], length.out= 14+31)), 0) 
var_min = round(Iday[t_window] *
                  c(seq(from = var_freq[1,3], to = var_freq[2,1], length.out= 15+14),
                    seq(from = var_freq[2,1], to = var_freq[3,2], length.out= 14+31)), 0)
dates2 = dates[days2]
plot(dates2,var_cases)

t_start <-seq(2, length(t_window)-7)   
t_end <- t_start + 7                
res <- estimate_R(incid = var_cases, 
                  method = "non_parametric_si",
                  config = make_config(list(
                    si_distr = si_distr, 
                    t_start = t_start, 
                    t_end = t_end)))

res_min <- estimate_R(incid = var_min, 
                  method = "non_parametric_si",
                  config = make_config(list(
                    si_distr = si_distr, 
                    t_start = t_start, 
                    t_end = t_end)))

res_max <- estimate_R(incid = var_max, 
                      method = "non_parametric_si",
                      config = make_config(list(
                        si_distr = si_distr, 
                        t_start = t_start, 
                        t_end = t_end)))

plot(t_window, var_cases, type="l")
lines(t_window, var_min, type="l", col="grey")
lines(t_window, var_max, type="l", col="grey")

plot(dates[t_window[9:74]], res$R$`Mean(R)`, type="l", ylim=c(0,3), 
     xlim=c(as.Date("2021-02-14"),as.Date("2021-4-01")))
lines(dates[t_window[9:74]], res_min$R$`Quantile.0.025(R)`, type="l", col="grey")
lines(dates[t_window[9:74]], res_max$R$`Quantile.0.975(R)`, type="l", col="grey")
lines(dates[t_window[9:74]], res$R$`Quantile.0.025(R)`, type="l", col="grey")
lines(dates[t_window[9:74]], res$R$`Quantile.0.975(R)`, type="l", col="grey")
lines(dates[t_window[9:74]], rep(1, length(9:74)))
lines(dates[t_window[9:74]], rep(2, length(9:74)))



