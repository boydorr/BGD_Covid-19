rm(list=ls())

require(deSolve)
require(data.table)
require(lubridate)

source("R/covid_model.R")
source("R/amalgamate_cats.R")
source("R/calc_fractions.R")
source("R/bangladesh_covid_data.R")


# Values of initial infectious to test
mean(BGD$dhaka_cases_2021[which(BGD$date>="2021-02-26"& BGD$date<="2021-03-01")]) # based on initial 2020 tuning probably only 6% of actual cases
# also add some more to account for infectious individuals still there from previous days (mean duration infectiousness ~8 days - let's multiply by half that)
infectious_vec <- (mean(BGD$dhaka_cases_2021[which(BGD$date>="2021-02-26"& BGD$date<="2021-03-01")])/0.06)*
        4*c(0.5,1,1.5,2)# 19085

# Values of initial immunity to test
immunity_vec <- c(0,0.25,0.5,0.75)

# Set up dataframe to hold results
R_ests <- data.frame("infectious"=rep(infectious_vec,length(immunity_vec)),
                     "immune"=rep(immunity_vec,each=length(infectious_vec)),
                     "R0_deaths"=NA,
                     "convergence_deaths"=NA,
                     "diff_deaths"=NA,
                     "prop_deaths_detected_may20_deaths"=NA,
                     "diff_peak_date_deaths"=NA,
                     "diff_peak_symptomatic_deaths"=NA,
                     "R0_peak"=NA,
                     "convergence_peak"=NA,
                     "diff_peak"=NA,
                     "prop_deaths_detected_may20_peak"=NA,
                     "diff_peak_symptomatic_peak"=NA)


## Load in baseline parameters
source("R/pars_baseline.R")
start_date <- as.Date("2021-03-01")
parms_baseline["ld"]<-1; 
parms_baseline["ld_start"]<-as.Date("2021-04-05")-start_date; parms_baseline["ld_end"]<- as.Date("2021-06-01")-start_date
parms_baseline["ld_improve"] <- 9
parms_baseline["mask"]<-1
parms_baseline["mask_start"]<-as.Date("2021-04-05")-start_date; parms_baseline["mask_end"]<- as.Date("2021-06-01")-start_date
parms_baseline["mask_improve"] <- 9
parms_baseline["mask_compliance"]<-0.3
parms_baseline["syndromic"]<-1
parms_baseline["syn_start"]<-as.Date("2021-04-05")-start_date; parms_baseline["syn_end"]<- as.Date("2021-06-01")-start_date
parms_baseline["community"]<-0.3
parms_baseline["syn_improve"] <- 9

## Initial conditions function
inits <- function(parms,infectious,immune){
        source("R/initial_conds.R")
        y["R_n"] <- parms["population"]*immune*(1-((infectious+infectious*0.5)/(parms["population"]/parms["HHsize"])))
        y["R_I"] <- parms["population"]*immune*(infectious/(parms["population"]/parms["HHsize"]))
        y["R_E"] <- parms["population"]*immune*((infectious*0.5)/(parms["population"]/parms["HHsize"]))
        y["Ia_f"] <- infectious*parms["fa"] # assume infectious individuals are the only infectious individual in their households and are at the beginning of their infectious period
        y["Ip_f"] <- infectious*(1-parms["fa"])
        y["E_f"]<- infectious*0.5# Assume same number incubating as infectious and evenly distribute among households with and without infecteds (assume no more than one per houshold)
        y["E_sa"]<- infectious*0.5*parms["fa"]
        y["E_ss"]<- infectious*0.5*(1-parms["fa"])
        y["S_I"] <- (parms["HHsize"]-1)*(infectious*0.5) 
        y["S_E"] <- (parms["HHsize"]-1)*(infectious*0.5) 
        y["S_n"] <- as.numeric(parms["population"] - 
                                       (y["R_n"]+y["R_I"]+y["R_E"]+y["Ia_f"]+y["Ip_f"]+y["E_f"]+
                                                y["E_sa"]+y["E_ss"]+y["S_I"]+y["S_E"]))
        y["CumCases"] <- infectious
        
        return(y)
}




## Tune R0 based on matching cumulative deaths
#_______________


tune_deaths <- function(par,model_parms,y,times,infectious,immune,BGD,end_date,start_date,dates){
        
        # Adjust transmission rates for R0
        parms <- model_parms
        parms["R0"] <- par
        parms["beta_s"] <- parms["R0"]/(parms["fa"]*(parms["asympTrans"]*(parms["propPresympTrans"]/(1-parms["propPresympTrans"]) + 1))*parms["dur_s"] + 
                                                (1-parms["fa"])*((parms["propPresympTrans"]/(1-parms["propPresympTrans"]))*parms["dur_s"] + 
                                                                                  parms["dur_s"]))
        parms["beta_p"] <- ((parms["propPresympTrans"]/(1-parms["propPresympTrans"]))*parms["beta_s"]*parms["dur_s"])/parms["dur_p"]
        parms["beta_a"] <- ((parms["asympTrans"]*(parms["propPresympTrans"]/(1-parms["propPresympTrans"]) + 1))*parms["beta_s"]*parms["dur_s"])/parms["dur_a"]

        # Run model 
        out <- amalgamate_cats(as.data.frame(lsoda(y, times, covid_model, parms=parms))) 
        
        # Difference between cumulative deaths from data and model at end date
        modelled <- out$D
        data <- BGD$dhaka_cum_deaths_2021[which(BGD$date>=start_date & BGD$date<=end_date)]-BGD$dhaka_cum_deaths_2021[which(BGD$date==start_date)]
        return(sum((modelled-data)^2))
        

        # # Difference between cumulative deaths from data and model at end date
        # return(abs(out$D[which(dates==end_date)]-(BGD$dhaka_cum_deaths_2021[which(BGD$date==end_date)]-BGD$dhaka_cum_deaths_2021[which(BGD$date==start_date)])))
}


for(i in 1:nrow(R_ests)){

        infectious <- R_ests$infectious[i]
        immune <- R_ests$immune[i]
        
        # Dates for model
        start_date <- as.Date("2021-03-01")
        end_date <- as.Date("2021-04-05") # before new lockdown
        
        # time periods of interest
        dates <- as.Date(start_date:end_date,origin=as.Date("1970-01-01"))
        times <- seq(0, as.numeric(end_date-start_date), 1)

        # Initial conditions
        y<-inits(parms = parms_baseline, infectious = infectious, immune = immune)
        
        # Optimise
        opt <- optim(par=3,fn=tune_deaths,model_parms=parms_baseline,y=y,times=times,method="Brent",
                     infectious=infectious,immune=immune,BGD=BGD,lower=0,upper=30,
                     end_date=end_date,start_date=start_date,dates=dates)

        # Run with optimised value
        end_date <- as.Date("2021-05-30") 
        dates <- as.Date(start_date:end_date,origin=as.Date("1970-01-01"))
        times <- seq(0, as.numeric(end_date-start_date), 1)
        parms_baseline["R0"] <- opt$par
        parms_baseline["beta_s"] <- parms_baseline["R0"]/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] + 
                                                                  (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] + 
                                                                                                    parms_baseline["dur_s"]))
        parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
        parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]
        out <- amalgamate_cats(as.data.frame(lsoda(y, times, covid_model, parms=parms_baseline))) 
        
        # Cumulative deaths on 20th May for model and data
        D_may20 <- out$D[which(dates=="2021-05-20")]
        D_may20_data <- BGD$dhaka_cum_deaths_2021[which(BGD$date=="2021-05-20")]-BGD$dhaka_cum_deaths_2021[which(BGD$date==start_date)]

        # Get difference in timing of peak daily new cases
        peak_data<-BGD$date[which(BGD$date>=start_date&BGD$date<=end_date)][which.max(BGD$dhaka_cases_2021[which(BGD$date>=start_date&BGD$date<=end_date)])]
        peak_model<-dates[which.max(diff(out$CumSymp))]
        peak_symp_model<-max(diff(out$CumSymp))
        peak_symp_data <- max(BGD$dhaka_cases_2021[which(BGD$date>=start_date&BGD$date<=end_date)])
        
        # Save info
        R_ests$R0_deaths[i]<-opt$par
        R_ests$diff_deaths[i]<-opt$value
        R_ests$convergence_deaths[i]<-opt$convergence
        R_ests$prop_deaths_detected_may20_deaths[i] <- D_may20_data/D_may20
        R_ests$diff_peak_date_deaths[i] <- peak_data-peak_model
        R_ests$diff_peak_symptomatic_deaths[i] <- peak_symp_data/peak_symp_model
}



## Tune R0 based on matching peak timing
#_______________

# Dates for model
start_date <- as.Date("2021-03-01")
end_date <- as.Date("2021-05-30") 

# time periods of interest
dates <- as.Date(start_date:end_date,origin=as.Date("1970-01-01"))
times <- seq(0, as.numeric(end_date-start_date), 1)

tune_peak <- function(par,model_parms,y,times,infectious,immune,BGD,end_date,start_date,dates){
        
        # Adjust transmission rates for R0
        parms <- model_parms
        parms["R0"] <- par^2+1
        parms["beta_s"] <- parms["R0"]/(parms["fa"]*(parms["asympTrans"]*(parms["propPresympTrans"]/(1-parms["propPresympTrans"]) + 1))*parms["dur_s"] + 
                                                (1-parms["fa"])*((parms["propPresympTrans"]/(1-parms["propPresympTrans"]))*parms["dur_s"] + 
                                                                         parms["dur_s"]))
        parms["beta_p"] <- ((parms["propPresympTrans"]/(1-parms["propPresympTrans"]))*parms["beta_s"]*parms["dur_s"])/parms["dur_p"]
        parms["beta_a"] <- ((parms["asympTrans"]*(parms["propPresympTrans"]/(1-parms["propPresympTrans"]) + 1))*parms["beta_s"]*parms["dur_s"])/parms["dur_a"]
        
        # Run model 
        out <- amalgamate_cats(as.data.frame(lsoda(y, times, covid_model, parms=parms))) 
        
        # Cumulative deaths on 20th May for model and data
        D_may20 <- out$D[which(dates=="2021-05-20")]
        D_may20_data <- BGD$dhaka_cum_deaths_2021[which(BGD$date=="2021-05-20")]-BGD$dhaka_cum_deaths_2021[which(BGD$date==start_date)]
        
        
        # Difference between peak symptomatic times from data and model
        peak_data<-BGD$date[which(BGD$date>=start_date&BGD$date<=end_date)][which.max(BGD$dhaka_cases_2021[which(BGD$date>=start_date&BGD$date<=end_date)])]
        peak_model<-dates[which.max(diff(out$CumSymp))]
        return(ifelse(D_may20<D_may20_data,Inf,abs(peak_data-peak_model)))
}

set.seed(0)
for(i in 1:nrow(R_ests)){
        
        print(paste("i =",i))
        infectious <- R_ests$infectious[i]
        immune <- R_ests$immune[i]
        
        # Initial conditions
        y<-inits(parms = parms_baseline, infectious = infectious, immune = immune)
        
        # Optimise
        opt <- optim(par=sqrt(3),fn=tune_peak,model_parms=parms_baseline,y=y,times=times,method="SANN",
                     infectious=infectious,immune=immune,BGD=BGD,control=list(maxit=400,trace=1,REPORT=20),
                     end_date=end_date,start_date=start_date,dates=dates)

        # Run with optimised value
        parms_baseline["R0"] <- opt$par^2+1
        parms_baseline["beta_s"] <- parms_baseline["R0"]/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] + 
                                                (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] + 
                                                                         parms_baseline["dur_s"]))
        parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
        parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]
        out <- amalgamate_cats(as.data.frame(lsoda(y, times, covid_model, parms=parms_baseline))) 

        # Cumulative deaths on 20th May for model and data
        D_may20 <- out$D[which(dates=="2021-05-20")]
        D_may20_data <- BGD$dhaka_cum_deaths_2021[which(BGD$date=="2021-05-20")]-BGD$dhaka_cum_deaths_2021[which(BGD$date==start_date)]

        #Peak in daily symptomatics
        peak_symp_model<-max(diff(out$CumSymp))
        peak_symp_data <- max(BGD$dhaka_cases_2021[which(BGD$date>=start_date&BGD$date<=end_date)])
        
                
        # Save info
        R_ests$R0_peak[i]<-opt$par^2+1
        R_ests$diff_peak[i]<-opt$value
        R_ests$convergence_peak[i]<-opt$convergence
        R_ests$prop_deaths_detected_may20_peak[i] <- D_may20_data/D_may20
        R_ests$diff_peak_symptomatic_peak[i] <- peak_symp_data/peak_symp_model
}




## Save finished data frame
#______________________

write.csv(R_ests,"Output/R0ests_2021.csv",row.names=F)



##Plot
#______________________

R_ests <- read.csv("Output/R0ests_2021.csv")

tiff(filename="Figs/R0_2021.tiff",width=160,height=190,units="mm",pointsize=12,res=250)
par(mar=c(3,2.7,0.8,1.7))

# Cumulative deaths in tuning period
#------------------

par(mfrow=c(3,2))
par(mar=c(2.5,2.5,1,1))
end_date=as.Date("2021-04-05")
dates <- as.Date(start_date:end_date,origin=as.Date("1970-01-01"))
times <- seq(0, as.numeric(end_date-start_date), 1)
xRange = c(0, end_date-start_date)
ind <- which((BGD$date-start_date)<=xRange[2] & (BGD$date-start_date)>=xRange[1])
yRange = c(0, max(BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]]))
plot((BGD$date-start_date)[ind], BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]],
     col=1, type = "l", lty=2, ylim=yRange, xlim=xRange, ylab="",bty="l",axes=F,lwd=1,xlab="")
graphics::box(bty="l")
title(ylab="Cumulative deaths", line=1.6)
title(xlab="Date", line=1.6)
axis(2,cex.axis=0.8)
date_ticks <- as.numeric(seq(start_date,end_date,by="month") - start_date)
date_labels <- paste(month.abb[month(seq(start_date,end_date,by="month"))], (year(seq(start_date,end_date,by="month")))-2000,sep="")
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.8)

legend("topleft", c(round(infectious_vec),"data"),title="Initial infectious",
       col=c("blue2","red","orange","dodgerblue","black"), lwd=c(rep(1,length(infectious_vec)),2),cex=0.9,bty="n")

for(i in 1:nrow(R_ests)){
        
        parms_baseline["R0"]<-R_ests$R0_deaths[i] # target R0
        parms_baseline["beta_s"] <- parms_baseline["R0"]/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] + 
                                                                  (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] + 
                                                                                                    parms_baseline["dur_s"]))
        parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
        parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]
        
        infectious <- R_ests$infectious[i]
        immune <- R_ests$immune[i]
        
        # Initial conditions
        y<-inits(parms = parms_baseline, infectious = infectious, immune = immune)
        
        # Run model 
        out <- amalgamate_cats(as.data.frame(lsoda(y, times, covid_model, parms=parms_baseline))) 
        
        lines(out$time[(xRange[1]+1):(xRange[2]+1)], out$D[(xRange[1]+1):(xRange[2]+1)], col=c("blue2","red","orange","dodgerblue")[(i-1)%%4+1], lty=ceiling(i/4)+1)
        
}
lines((BGD$date-start_date)[ind], BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]],
      col=1, type = "l", lty=1,lwd=2)
legend("bottomright","A",bty="n",text.font = 2)



xRange = c(0, end_date-start_date)
ind <- which((BGD$date-start_date)<=xRange[2] & (BGD$date-start_date)>=xRange[1])
yRange = c(0, max(BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]]))
plot((BGD$date-start_date)[ind], BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]],
     col=1, type = "l", lty=2, ylim=yRange, xlim=xRange, ylab="",bty="l",axes=F,lwd=1,xlab="")
graphics::box(bty="l")
title(ylab="Cumulative deaths", line=1.6)
title(xlab="Date", line=1.6)
axis(2,cex.axis=0.8)
date_ticks <- as.numeric(seq(start_date,end_date,by="month") - start_date)
date_labels <- paste(month.abb[month(seq(start_date,end_date,by="month"))], (year(seq(start_date,end_date,by="month")))-2000,sep="")
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.8)
legend("topleft", c(immunity_vec,"data"),title="Initial proportion immune",bty="n",
       lty=c(2:5,1), lwd=c(rep(1,length(immunity_vec)),2),cex=0.9)


# legend(1,yRange[2], c("Modelled total deaths", "Reported total deaths"),
#        col=1, lty=c(1,2), lwd=c(1,1), box.col="white",cex=0.9)

for(i in 1:nrow(R_ests)){
        
        parms_baseline["R0"]<-R_ests$R0_peak[i] # target R0
        parms_baseline["beta_s"] <- parms_baseline["R0"]/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] + 
                                                                  (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] + 
                                                                                                    parms_baseline["dur_s"]))
        parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
        parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]
        
        infectious <- R_ests$infectious[i]
        immune <- R_ests$immune[i]
        
        # Initial conditions
        y<-inits(parms = parms_baseline, infectious = infectious, immune = immune)
        
        # Run model 
        out <- amalgamate_cats(as.data.frame(lsoda(y, times, covid_model, parms=parms_baseline))) 
        
        lines(out$time[(xRange[1]+1):(xRange[2]+1)], out$D[(xRange[1]+1):(xRange[2]+1)], col=c("blue2","red","orange","dodgerblue")[(i-1)%%4+1], lty=ceiling(i/4)+1)
        
}
lines((BGD$date-start_date)[ind], BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]],
      col=1, type = "l", lty=1,lwd=2)
legend("bottomright","B",bty="n",text.font = 2)



# Daily deaths up until September
#------------------

end_date=as.Date("2021-09-01")
dates <- as.Date(start_date:end_date,origin=as.Date("1970-01-01"))
times <- seq(0, as.numeric(end_date-start_date), 1)
xRange = c(0, end_date-start_date-1)
ind <- which((BGD$date-start_date)<=xRange[2] & (BGD$date-start_date)>=xRange[1])
yRange = c(0, max(BGD$dhaka_deaths_2021[ind])*3)
plot((BGD$date-start_date)[ind], BGD$dhaka_deaths_2021[ind],
     col=1, type = "l", lty=2, ylim=yRange, xlim=xRange, ylab="",bty="l",axes=F,lwd=1,xlab="")
graphics::box(bty="l")
title(ylab="Daily deaths", line=1.6)
title(xlab="Date", line=1.6)
axis(2,cex.axis=0.8)
date_ticks <- as.numeric(seq(start_date,end_date,by="month") - start_date)
date_labels <- paste(month.abb[month(seq(start_date,end_date,by="month"))], (year(seq(start_date,end_date,by="month")))-2000,sep="")
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.8)

for(i in 1:nrow(R_ests)){
        
        parms_baseline["R0"]<-R_ests$R0_deaths[i] # target R0
        parms_baseline["beta_s"] <- parms_baseline["R0"]/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] + 
                                                                  (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] + 
                                                                                                    parms_baseline["dur_s"]))
        parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
        parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]
        
        infectious <- R_ests$infectious[i]
        immune <- R_ests$immune[i]
        
        # Initial conditions
        y<-inits(parms = parms_baseline, infectious = infectious, immune = immune)
        
        # Run model 
        out <- amalgamate_cats(as.data.frame(lsoda(y, times, covid_model, parms=parms_baseline))) 
        
        lines(out$time[(xRange[1]+1):(xRange[2])], diff(out$D[(xRange[1]+1):(xRange[2]+1)]), col=c("blue2","red","orange","dodgerblue")[(i-1)%%4+1], lty=ceiling(i/4)+1)
        
}
lines((BGD$date-start_date)[ind], BGD$dhaka_deaths_2021[ind],col=1, type = "l", lty=1,lwd=2)
legend("bottomright","C",bty="n",text.font = 2)


end_date=as.Date("2021-09-01")
dates <- as.Date(start_date:end_date,origin=as.Date("1970-01-01"))
times <- seq(0, as.numeric(end_date-start_date), 1)
xRange = c(0, end_date-start_date-1)
ind <- which((BGD$date-start_date)<=xRange[2] & (BGD$date-start_date)>=xRange[1])
yRange = c(0, max(BGD$dhaka_deaths_2021[ind])*3)
plot((BGD$date-start_date)[ind], BGD$dhaka_deaths_2021[ind],
     col=1, type = "l", lty=2, ylim=yRange, xlim=xRange, ylab="",bty="l",axes=F,lwd=1,xlab="")
graphics::box(bty="l")
title(ylab="Daily deaths", line=1.6)
title(xlab="Date", line=1.6)
axis(2,cex.axis=0.8)
date_ticks <- as.numeric(seq(start_date,end_date,by="month") - start_date)
date_labels <- paste(month.abb[month(seq(start_date,end_date,by="month"))], (year(seq(start_date,end_date,by="month")))-2000,sep="")
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.8)

for(i in 1:nrow(R_ests)){
        
        parms_baseline["R0"]<-R_ests$R0_peak[i] # target R0
        parms_baseline["beta_s"] <- parms_baseline["R0"]/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] + 
                                                                  (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] + 
                                                                                                    parms_baseline["dur_s"]))
        parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
        parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]
        
        infectious <- R_ests$infectious[i]
        immune <- R_ests$immune[i]
        
        # Initial conditions
        y<-inits(parms = parms_baseline, infectious = infectious, immune = immune)
        
        # Run model 
        out <- amalgamate_cats(as.data.frame(lsoda(y, times, covid_model, parms=parms_baseline))) 
        
        lines(out$time[(xRange[1]+1):(xRange[2])], diff(out$D[(xRange[1]+1):(xRange[2]+1)]), col=c("blue2","red","orange","dodgerblue")[(i-1)%%4+1], lty=ceiling(i/4)+1)
        
}
lines((BGD$date-start_date)[ind], BGD$dhaka_deaths_2021[ind],col=1, type = "l", lty=1,lwd=2)
legend("bottomright","D",bty="n",text.font = 2)



# Daily cases up until July
#------------------

end_date=as.Date("2021-07-01")
dates <- as.Date(start_date:end_date,origin=as.Date("1970-01-01"))
times <- seq(0, as.numeric(end_date-start_date), 1)
xRange = c(0, end_date-start_date-1)
ind <- which((BGD$date-start_date)<=xRange[2] & (BGD$date-start_date)>=xRange[1])
yRange = c(0, max(BGD$dhaka_cases_2021[ind])*15)
plot((BGD$date-start_date)[ind], BGD$dhaka_cases_2021[ind]*10,
     col=1, type = "l", lty=2, ylim=yRange, xlim=xRange, ylab="",bty="l",axes=F,lwd=1,xlab="")
graphics::box(bty="l")
title(ylab="Daily cases", line=1.6)
title(xlab="Date", line=1.6)
axis(2,cex.axis=0.8)
date_ticks <- as.numeric(seq(start_date,end_date,by="month") - start_date)
date_labels <- paste(month.abb[month(seq(start_date,end_date,by="month"))], (year(seq(start_date,end_date,by="month")))-2000,sep="")
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.8)

for(i in 1:nrow(R_ests)){

        parms_baseline["R0"]<-R_ests$R0_deaths[i] # target R0
        parms_baseline["beta_s"] <- parms_baseline["R0"]/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] +
                                                                  (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] +
                                                                                                    parms_baseline["dur_s"]))
        parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
        parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]

        infectious <- R_ests$infectious[i]
        immune <- R_ests$immune[i]

        # Initial conditions
        y<-inits(parms = parms_baseline, infectious = infectious, immune = immune)

        # Run model
        out <- amalgamate_cats(as.data.frame(lsoda(y, times, covid_model, parms=parms_baseline)))

        lines(out$time[(xRange[1]+1):(xRange[2])], diff(out$CumSymp[(xRange[1]+1):(xRange[2]+1)]), col=c("blue2","red","orange","dodgerblue")[(i-1)%%4+1], lty=ceiling(i/4)+1)

}
lines((BGD$date-start_date)[ind], BGD$dhaka_cases_2021[ind]*10,col=1, type = "l", lty=1,lwd=2)
legend("bottomright","E",bty="n",text.font = 2)


end_date=as.Date("2021-07-01")
dates <- as.Date(start_date:end_date,origin=as.Date("1970-01-01"))
times <- seq(0, as.numeric(end_date-start_date), 1)
xRange = c(0, end_date-start_date-1)
ind <- which((BGD$date-start_date)<=xRange[2] & (BGD$date-start_date)>=xRange[1])
yRange = c(0, max(BGD$dhaka_cases_2021[ind])*15)
plot((BGD$date-start_date)[ind], BGD$dhaka_cases_2021[ind]*10,
     col=1, type = "l", lty=2, ylim=yRange, xlim=xRange, ylab="",bty="l",axes=F,lwd=1,xlab="")
graphics::box(bty="l")
title(ylab="Daily cases", line=1.6)
title(xlab="Date", line=1.6)
axis(2,cex.axis=0.8)
date_ticks <- as.numeric(seq(start_date,end_date,by="month") - start_date)
date_labels <- paste(month.abb[month(seq(start_date,end_date,by="month"))], (year(seq(start_date,end_date,by="month")))-2000,sep="")
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.8)

for(i in 1:nrow(R_ests)){

        parms_baseline["R0"]<-R_ests$R0_peak[i] # target R0
        parms_baseline["beta_s"] <- parms_baseline["R0"]/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] +
                                                                  (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] +
                                                                                                    parms_baseline["dur_s"]))
        parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
        parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]

        infectious <- R_ests$infectious[i]
        immune <- R_ests$immune[i]

        # Initial conditions
        y<-inits(parms = parms_baseline, infectious = infectious, immune = immune)

        # Run model
        out <- amalgamate_cats(as.data.frame(lsoda(y, times, covid_model, parms=parms_baseline)))

        lines(out$time[(xRange[1]+1):(xRange[2])], diff(out$CumSymp[(xRange[1]+1):(xRange[2]+1)]), col=c("blue2","red","orange","dodgerblue")[(i-1)%%4+1], lty=ceiling(i/4)+1)

}
lines((BGD$date-start_date)[ind], BGD$dhaka_cases_2021[ind]*10,col=1, type = "l", lty=1,lwd=2)
legend("bottomright","F",bty="n",text.font = 2)

dev.off()




##Plot for SI
#______________________

R_ests <- read.csv("Output/R0ests_2021.csv")
R_ests_sub <- R_ests[which(R_ests$immune<0.75 & R_ests$infectious<30000),]

tiff(filename="Figs/R0_2021_SI.tiff",width=160,height=160,units="mm",pointsize=12,res=250)
par(mar=c(3.5,3.5,0.8,1))

# Cumulative deaths in tuning period
#------------------

par(mfrow=c(2,2))
end_date=as.Date("2021-04-05")
dates <- as.Date(start_date:end_date,origin=as.Date("1970-01-01"))
times <- seq(0, as.numeric(end_date-start_date), 1)
date_ticks <- as.numeric(seq(start_date,end_date,by="month") - start_date)
date_labels <- paste(month.abb[month(seq(start_date,end_date,by="month"))], (year(seq(start_date,end_date,by="month")))-2000,sep="")
xRange = c(0, end_date-start_date)
ind <- which((BGD$date-start_date)<=xRange[2] & (BGD$date-start_date)>=xRange[1])
yRange = c(0, max(BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]])*1.3)
plot((BGD$date-start_date)[ind], BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]],
     col=1, type = "l", lty=2, ylim=yRange, xlim=xRange, ylab="",bty="l",axes=F,lwd=1,xlab="")
graphics::box(bty="l")
title(ylab="Cumulative deaths", line=2,cex.lab=1.2)
title(xlab="Date", line=2,cex.lab=1.2)
axis(2,cex.axis=0.9,padj=0.8)
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.9,padj=-0.8)

legend("topleft", c(round(infectious_vec[1:(length(infectious_vec)-1)]),"data"),title="Initial infectious:",
       col=c("red","orange","blue2","black"), lwd=c(rep(1,length(infectious_vec)-1),2),cex=1,bty="n")

for(i in 1:nrow(R_ests_sub)){
        
        parms_baseline["R0"]<-R_ests_sub$R0_deaths[i] # target R0
        parms_baseline["beta_s"] <- parms_baseline["R0"]/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] + 
                                                                  (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] + 
                                                                                                    parms_baseline["dur_s"]))
        parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
        parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]
        
        infectious <- R_ests_sub$infectious[i]
        immune <- R_ests_sub$immune[i]
        
        # Initial conditions
        y<-inits(parms = parms_baseline, infectious = infectious, immune = immune)
        
        # Run model 
        out <- amalgamate_cats(as.data.frame(lsoda(y, times, covid_model, parms=parms_baseline))) 
        
        lines(out$time[(xRange[1]+1):(xRange[2]+1)], out$D[(xRange[1]+1):(xRange[2]+1)], col=c("red","orange","blue2")[(i-1)%%3+1], lty=ceiling(i/3)+1)
        
}
lines((BGD$date-start_date)[ind], BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]],
      col=1, type = "l", lty=1,lwd=2)
legend("topright","A",bty="n",text.font = 2,cex=1.2)



xRange = c(0, end_date-start_date)
ind <- which((BGD$date-start_date)<=xRange[2] & (BGD$date-start_date)>=xRange[1])
yRange = c(0, max(BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]])*1.2)
plot((BGD$date-start_date)[ind], BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]],
     col=1, type = "l", lty=2, ylim=yRange, xlim=xRange, ylab="",bty="l",axes=F,lwd=1,xlab="")
graphics::box(bty="l")
title(ylab="Cumulative deaths", line=2,cex.lab=1.2)
title(xlab="Date", line=2,cex.lab=1.2)
axis(2,cex.axis=0.9,padj=0.8)
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.9,padj=-0.8)
legend("topleft", c(immunity_vec[1:(length(immunity_vec)-1)],"data"),title="Initial immune:",bty="n",
       lty=c(2:4,1), lwd=c(rep(1,length(immunity_vec)-1),2),cex=1)


# legend(1,yRange[2], c("Modelled total deaths", "Reported total deaths"),
#        col=1, lty=c(1,2), lwd=c(1,1), box.col="white",cex=0.9)

for(i in 1:nrow(R_ests_sub)){
        
        parms_baseline["R0"]<-R_ests_sub$R0_peak[i] # target R0
        parms_baseline["beta_s"] <- parms_baseline["R0"]/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] + 
                                                                  (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] + 
                                                                                                    parms_baseline["dur_s"]))
        parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
        parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]
        
        infectious <- R_ests_sub$infectious[i]
        immune <- R_ests_sub$immune[i]
        
        # Initial conditions
        y<-inits(parms = parms_baseline, infectious = infectious, immune = immune)
        
        # Run model 
        out <- amalgamate_cats(as.data.frame(lsoda(y, times, covid_model, parms=parms_baseline))) 
        
        lines(out$time[(xRange[1]+1):(xRange[2]+1)], out$D[(xRange[1]+1):(xRange[2]+1)], col=c("red","orange","blue2")[(i-1)%%3+1], lty=ceiling(i/3)+1)
        
}
lines((BGD$date-start_date)[ind], BGD$dhaka_cum_deaths_2021[ind]-BGD$dhaka_cum_deaths_2021[ind[1]],
      col=1, type = "l", lty=1,lwd=2)
legend("topright","B",bty="n",text.font = 2,cex=1.2)



# Daily deaths up until September
#------------------

end_date=as.Date("2021-09-01")
dates <- as.Date(start_date:end_date,origin=as.Date("1970-01-01"))
times <- seq(0, as.numeric(end_date-start_date), 1)
xRange = c(0, end_date-start_date-1)
ind <- which((BGD$date-start_date)<=xRange[2] & (BGD$date-start_date)>=xRange[1])
yRange = c(0, max(BGD$dhaka_deaths_2021[ind])*6)
plot((BGD$date-start_date)[ind], BGD$dhaka_deaths_2021[ind],
     col=1, type = "l", lty=2, ylim=yRange, xlim=xRange, ylab="",bty="l",axes=F,lwd=1,xlab="")
graphics::box(bty="l")
date_ticks <- as.numeric(seq(start_date,end_date,by="2 month") - start_date)
date_labels <- paste(month.abb[month(seq(start_date,end_date,by="2 month"))], (year(seq(start_date,end_date,by="2 month")))-2000,sep="")
title(ylab="Daily deaths", line=2,cex.lab=1.2)
title(xlab="Date", line=2,cex.lab=1.2)
axis(2,cex.axis=0.9,padj=0.8)
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.9,padj=-0.8)

for(i in 1:nrow(R_ests_sub)){
        
        parms_baseline["R0"]<-R_ests_sub$R0_deaths[i] # target R0
        parms_baseline["beta_s"] <- parms_baseline["R0"]/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] + 
                                                                  (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] + 
                                                                                                    parms_baseline["dur_s"]))
        parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
        parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]
        
        infectious <- R_ests_sub$infectious[i]
        immune <- R_ests_sub$immune[i]
        
        # Initial conditions
        y<-inits(parms = parms_baseline, infectious = infectious, immune = immune)
        
        # Run model 
        out <- amalgamate_cats(as.data.frame(lsoda(y, times, covid_model, parms=parms_baseline))) 
        
        lines(out$time[(xRange[1]+1):(xRange[2])], diff(out$D[(xRange[1]+1):(xRange[2]+1)]), col=c("red","orange","blue2")[(i-1)%%3+1], lty=ceiling(i/3)+1)
        
}
lines((BGD$date-start_date)[ind], BGD$dhaka_deaths_2021[ind],col=1, type = "l", lty=1,lwd=2)
legend("topright","C",bty="n",text.font = 2,cex=1.2)


end_date=as.Date("2021-09-01")
dates <- as.Date(start_date:end_date,origin=as.Date("1970-01-01"))
times <- seq(0, as.numeric(end_date-start_date), 1)
xRange = c(0, end_date-start_date-1)
ind <- which((BGD$date-start_date)<=xRange[2] & (BGD$date-start_date)>=xRange[1])
yRange = c(0, max(BGD$dhaka_deaths_2021[ind])*6)
plot((BGD$date-start_date)[ind], BGD$dhaka_deaths_2021[ind],
     col=1, type = "l", lty=2, ylim=yRange, xlim=xRange, ylab="",bty="l",axes=F,lwd=1,xlab="")
graphics::box(bty="l")
title(ylab="Daily deaths", line=2,cex.lab=1.2)
title(xlab="Date", line=2,cex.lab=1.2)
axis(2,cex.axis=0.9,padj=0.8)
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.9,padj=-0.8)

for(i in 1:nrow(R_ests_sub)){
        
        parms_baseline["R0"]<-R_ests_sub$R0_peak[i] # target R0
        parms_baseline["beta_s"] <- parms_baseline["R0"]/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] + 
                                                                  (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] + 
                                                                                                    parms_baseline["dur_s"]))
        parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
        parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]
        
        infectious <- R_ests_sub$infectious[i]
        immune <- R_ests_sub$immune[i]
        
        # Initial conditions
        y<-inits(parms = parms_baseline, infectious = infectious, immune = immune)
        
        # Run model 
        out <- amalgamate_cats(as.data.frame(lsoda(y, times, covid_model, parms=parms_baseline))) 
        
        lines(out$time[(xRange[1]+1):(xRange[2])], diff(out$D[(xRange[1]+1):(xRange[2]+1)]), col=c("red","orange","blue2")[(i-1)%%3+1], lty=ceiling(i/3)+1)
        
}
lines((BGD$date-start_date)[ind], BGD$dhaka_deaths_2021[ind],col=1, type = "l", lty=1,lwd=2)
legend("topright","D",bty="n",text.font = 2,cex=1.2)



dev.off()
