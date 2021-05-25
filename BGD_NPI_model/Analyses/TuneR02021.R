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

# Dates for model
start_date <- as.Date("2021-03-01")
end_date <- as.Date("2021-04-07") # before new lockdown

# time periods of interest
dates <- as.Date(start_date:end_date,origin=as.Date("1970-01-01"))
times <- seq(0, as.numeric(end_date-start_date), 1)


# Select R0
R0<-3.3# target R0
parms_baseline["beta_s"] <- R0/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] + 
                                        (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] + 
                                                                          parms_baseline["dur_s"]))
parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]


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

# Difference between cumulative cases from data and model at end date
out$D[which(dates==end_date)]-(BGD$dhaka_cum_deaths_2021[which(BGD$date==end_date)]-BGD$dhaka_cum_deaths_2021[which(BGD$date==start_date)])




## Plot
#____________

R_ests <- data.frame("infectious"=rep(mean(BGD$dhaka_cases_2021[which(BGD$date>="2021-02-26"& BGD$date<="2021-03-01")])*
                                              10*4*c(0.5,1,1.5),3),
                     "immune"=rep(c(0,0.25,0.5),each=3),
                     "R0"= c(3.0,2.5,2.2,4.0,3.3,2.8,5.9,4.8,4.2))
par(mfrow=c(1,1))
xRange = c(0, end_date-start_date)
ind <- which((BGD$date-start_date)<=xRange[2] & (BGD$date-start_date)>=xRange[1])
yRange = c(0, max(BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]]))
plot((BGD$date-start_date)[ind], BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]],
     col=1, type = "l", lty=2, ylim=yRange, xlim=xRange, ylab="",bty="l",axes=F,lwd=1,xlab="")
graphics::box(bty="l")
title(ylab="Count", line=1.6)
title(xlab="Date", line=1.6)
axis(2,cex.axis=0.8)
date_ticks <- as.numeric(seq(start_date,end_date,by="month") - start_date)
date_labels <- paste(month.abb[month(seq(start_date,end_date,by="month"))], (year(seq(start_date,end_date,by="month")))-2000,sep="")
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.8)

# legend(1,yRange[2], c("Modelled total deaths", "Reported total deaths"),
#        col=1, lty=c(1,2), lwd=c(1,1), box.col="white",cex=0.9)

for(i in 1:nrow(R_ests)){
        
        R0<-R_ests$R0[i] # target R0
        parms_baseline["beta_s"] <- R0/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] + 
                                                (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] + 
                                                                                  parms_baseline["dur_s"]))
        parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
        parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]
        
        infectious <- R_ests$infectious[i]
        immune <- R_ests$immune[i]
        
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
        
        lines(out$time[(xRange[1]+1):(xRange[2]+1)], out$D[(xRange[1]+1):(xRange[2]+1)], col=c("red","orange","dodgerblue")[(i-1)%%3+1], lty=ceiling(i/3)+1)
        
}
lines((BGD$date-start_date)[ind], BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]],
      col=1, type = "l", lty=1,lwd=2)




