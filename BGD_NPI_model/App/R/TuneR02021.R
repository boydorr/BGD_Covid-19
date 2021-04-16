rm(list=ls())

require(deSolve)
require(data.table)
require(lubridate)

source("R/covid_model.R")
source("R/amalgamate_cats.R")
source("R/calc_fractions.R")
source("R/bangladesh_covid_data.R")
source("R/duration_household_infectious.R")

## Proportion cases in Dhaka (from the dashboard)
district_cases <- read.csv("data/district_cases_dashboard_19.11.csv")
propDhaka <- district_cases$X.[which(district_cases$Distirct=="Dhaka")]/sum(district_cases$X.)



## Load in baseline parameters and adjust as needed
#______________________

## Load in baseline parameters
source("R/pars_baseline.R")
parms_baseline["ld"] <- 0 # turn off lockdown

# End date for model
end_date <- as.Date("2021-04-01") # before new lockdown

# time periods of interest
dates <- as.Date(start_date_upa:end_date,origin=as.Date("1970-01-01"))
times <- seq(0, as.numeric(end_date-start_date_upa), 1)


# Select R0
R0<-3.5# target R0
parms_baseline["beta_s"] <- R0/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] + 
                                        (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] + 
                                                                          parms_baseline["dur_s"]))
parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]
dur_hh_vec <- duration_household_infection(parms_baseline,10000)
parms_baseline["dur_hh"] <- dur_hh_vec[1]
parms_baseline["dur_hha"] <- dur_hh_vec[2]
parms_baseline["dur_hhs"] <- dur_hh_vec[3]


# Select infectious at start of forecast
mean(BGD$dhaka_cases_2021[which(BGD$date>="2021-02-26"& BGD$date<="2021-03-01")]) # probably 10x this
# also add some more to account for infectious individuals still there from previous days (mean duration infectiousness ~8 days - let's multiply by half that)
infectious <- mean(BGD$dhaka_cases_2021[which(BGD$date>="2021-02-26"& BGD$date<="2021-03-01")])*
        10*4# 11451


# Select immune proportion at start of forecast
immune <-0.25



## Run model
#______________________

# Initial conditions
source("R/initial_conds.R")
y["R_n"] <- parms_baseline["population"]*immune*(1-((infectious+infectious*0.5)/(parms_baseline["population"]/parms_baseline["HHsize"])))
y["R_I"] <- parms_baseline["population"]*immune*(infectious/(parms_baseline["population"]/parms_baseline["HHsize"]))
y["R_E"] <- parms_baseline["population"]*immune*((infectious*0.5)/(parms_baseline["population"]/parms_baseline["HHsize"]))
y["Ia_f"] <- infectious*parms_baseline["fa"] # assume infectious individuals are the only infectious individual in their households and are at the beginning of their infectious period
y["Ip_f"] <- infectious*(1-parms_baseline["fa"])
y["E_f"]<- infectious*0.5# Assume same number incubating as infectious and evenly distribute among households with and without infecteds (assume no more than one per houshold)
y["E_sa"]<- infectious*0.5*parms_baseline["fa"]
y["E_ss"]<- infectious*0.5*(1-parms_baseline["fa"])
y["S_I"] <- (parms_baseline["HHsize"]-1)*(infectious*0.5) 
y["S_E"] <- (parms_baseline["HHsize"]-1)*(infectious*0.5) 
y["S_n"] <- as.numeric(parms_baseline["population"] - 
                               (y["R_n"]+y["R_I"]+y["R_E"]+y["Ia_f"]+y["Ip_f"]+y["E_f"]+
                                        y["E_sa"]+y["E_ss"]+y["S_I"]+y["S_E"]))
y["CumCases"] <- infectious


# Run model 
out <- amalgamate_cats(as.data.frame(lsoda(y, times, covid_model, parms=parms_baseline))) 





##Plot
#______________________


par(mfrow=c(1,2))

# Dates for plotting
date_ticks <- as.numeric(seq(start_date_upa,end_date,by="month") - start_date_upa)
date_labels <- paste(month.abb[month(seq(start_date_upa,end_date,by="month"))], (year(seq(start_date_upa,end_date,by="month")))-2000,sep="")

# Compare with data on cumulative deaths
xRange = c(0, end_date-start_date_upa)
ind <- which((BGD$date-start_date_upa)<=xRange[2] & (BGD$date-start_date_upa)>=xRange[1])
yRange = c(0, max(out$D,BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]]))
plot((BGD$date-start_date_upa)[ind], BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]],
     col=1, type = "l", lty=2, ylim=yRange, xlim=xRange, ylab="",bty="l",axes=F,lwd=1,xlab="")
graphics::box(bty="l")
title(ylab="Count", line=1.6)
title(xlab="Date", line=1.6)
axis(2,cex.axis=0.8)
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.8)
lines(out$time[(xRange[1]+1):(xRange[2]+1)], out$D[(xRange[1]+1):(xRange[2]+1)], col=1, lwd=1)

legend(1,yRange[2], c("Modelled total deaths", "Reported total deaths"),
       col=1, lty=c(1,2), lwd=c(1,1), box.col="white",cex=0.9)


# Compare with data on daily deaths
xRange = c(0, end_date-start_date_upa)
ind <- which((BGD$date-start_date_upa)<=xRange[2] & (BGD$date-start_date_upa)>=xRange[1])
yRange = c(0, max(diff(c(0,out$D)),BGD$dhaka_deaths_2021[ind]))
plot((BGD$date-start_date_upa)[ind], BGD$dhaka_deaths_2021[ind],
     col=1, type = "l", lty=2, ylim=yRange, xlim=xRange, ylab="",bty="l",axes=F,lwd=1,xlab="")
graphics::box(bty="l")
title(ylab="Count", line=1.6)
title(xlab="Date", line=1.6)
axis(2,cex.axis=0.8)
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.8)
lines(out$time[(xRange[1]+1):(xRange[2]+1)], diff(c(0,out$D[(xRange[1]+1):(xRange[2]+1)])), col=1, lwd=1)


legend(1,yRange[2], c("Modelled daily deaths", "Reported daily deaths"),
       col=1, lty=c(1,2), lwd=c(1,1), box.col="white",cex=0.9)



out$D[which(dates==as.Date("2021-04-01"))]-(BGD$dhaka_cum_deaths_2021[which(BGD$date==as.Date("2021-04-01"))]-BGD$dhaka_cum_deaths_2021[which(BGD$date==as.Date("2021-03-01"))])
