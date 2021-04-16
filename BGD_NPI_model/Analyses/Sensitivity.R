rm(list=ls())

require(deSolve)
require(data.table)
require(lubridate)
require(dplyr)

source("R/covid_model.R")
source("R/worker_days_lost.R")
source("R/amalgamate_cats.R")
source("R/calc_fractions.R")
source("R/duration_household_infectious.R")

set.seed(0)


## Parameter ranges
par_ranges <- read.csv("data/ParameterRanges.csv",stringsAsFactors = F)

# Should the baseline scenario include a lockdown?
lockdown<-F

# Should community surveillance be included?
community_surveillance <- F

# Should mask wearing be included?
masks <- F

# Parameters for baseline scenario (with no interventions)
source("R/pars_baseline.R")
if(lockdown==F){parms_baseline["ld"]<-F}
if(masks==T){parms_baseline["mask"]<-T}
if(community_surveillance==T){parms_baseline["syndromic"]<-T}

# Initial conditions
source("R/initial_conds.R")


# Set up variables to save outputs
out_stats <- rep(list(data.frame(Parameter=par_ranges$Parameter,min_deaths=NA,max_deaths=NA,
                        min_hosp=NA,max_hosp=NA,min_cases=NA,max_cases=NA,
                        min_wdl=NA,max_wdl=NA,min_excess_beds=NA,max_excess_beds=NA)),2)
names(out_stats) <- c("stat_values","par_values")


# Run sensitivity analysis
system.time({
        for(par in 1:nrow(par_ranges)){
                
                intro_date_now <- intro_date
                times_model_now <- c(as.numeric(intro_date_now-start_date):as.numeric(end_date-start_date))
                
                R0_now<-R0
                parms<-parms_baseline
                
                if(par_ranges$Parameter[par]!="Intro"){
                        par_values<-seq(par_ranges$min[par],par_ranges$max[par],length.out = 100)
                }else{
                        par_values<-as.Date((start_date+par_ranges$min[par]):(start_date+par_ranges$max[par]),origin=as.Date("1970-01-01"))
                }
                par_stats <- data.frame(value=par_values,deaths=NA,hosp=NA,cases=NA,wdl=NA,excess_beds=NA) 
                
                for(value in 1:length(par_values)){
                        
                        #Alter parameter
                        if(par_ranges$Parameter[par]=="Intro"){
                                intro_date_now <- par_values[value]
                                times_model_now <- c(as.numeric(intro_date_now-start_date):as.numeric(end_date-start_date))
                                
                        }else if(par_ranges$Parameter[par]=="R0"){
                                R0_now<-par_values[value]   
                                
                        }else if(par_ranges$Parameter[par]=="delay_hosp"){
                                parms["delay_hosp"]<-par_values[value]->parms["delay_ICU"]
                                
                        }else{parms[par_ranges$Parameter[par]] <- par_values[value]}
                        
                        
                        # Recalculate betas
                        parms["beta_s"] <- R0_now/(parms["fa"]*(parms["asympTrans"]*(parms["propPresympTrans"]/(1-parms["propPresympTrans"]) + 1))*parms["dur_s"] + 
                                                           (1-parms["fa"])*((parms["propPresympTrans"]/(1-parms["propPresympTrans"]))*parms["dur_s"] + 
                                                                                    parms["dur_s"]))
                        parms["beta_p"] <- ((parms["propPresympTrans"]/(1-parms["propPresympTrans"]))*parms["beta_s"]*parms["dur_s"])/parms["dur_p"]
                        parms["beta_a"] <- ((parms["asympTrans"]*(parms["propPresympTrans"]/(1-parms["propPresympTrans"]) + 1))*parms["beta_s"]*parms["dur_s"])/parms["dur_a"]
                        
                        # Recalculate dur_hh
                        dur_hh_vec <- duration_household_infection(parms,5000)
                        parms["dur_hh"] <- dur_hh_vec[1]
                        parms["dur_hha"] <- dur_hh_vec[2]
                        parms["dur_hhs"] <- dur_hh_vec[3]
                        
                        
                        # Run model 
                        preIntro <- data.frame(time=0:(min(times_model_now)-1))
                        preIntro <- cbind(preIntro, matrix(0, ncol=length(y), nrow=nrow(preIntro), dimnames = list(NULL,names(y))))
                        preIntro$S_n <- parms_baseline["population"]
                        out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model_now, covid_model, parms=parms)))) 
                        
                        # Working days lost
                        wdl<-worker_days_lost(out,parms)
                        wdl<-(sum(wdl)/length(wdl))
                        
                        
                        # Store statistic
                        par_stats$value[value]<-par_values[value]
                        par_stats$deaths[value]<-max(out$D)
                        par_stats$cases[value]<-max(out$CumCases)
                        par_stats$hosp[value]<-max(out$CumSevere)
                        par_stats$wdl[value]<-wdl
                        par_stats$excess_beds[value]<- sum(pmax(0,(out$Hosp+out$ICU)-beds))

                }
                
                
                # Save min and max statistics
                out_stats$stat_values$min_deaths[par] <- min(par_stats$death)
                out_stats$stat_values$max_deaths[par] <- max(par_stats$death)
                out_stats$stat_values$min_cases[par] <- min(par_stats$cases)
                out_stats$stat_values$max_cases[par] <- max(par_stats$cases)
                out_stats$stat_values$min_hosp[par] <- min(par_stats$hosp)
                out_stats$stat_values$max_hosp[par] <- max(par_stats$hosp)
                out_stats$stat_values$min_wdl[par] <- min(par_stats$wdl)
                out_stats$stat_values$max_wdl[par] <- max(par_stats$wdl)
                out_stats$stat_values$min_excess_beds[par] <- min(par_stats$excess_beds)
                out_stats$stat_values$max_excess_beds[par] <- max(par_stats$excess_beds)

                out_stats$par_values$min_deaths[par] <- par_stats$value[which.min(par_stats$death)]
                out_stats$par_values$max_deaths[par] <- par_stats$value[which.max(par_stats$death)]
                out_stats$par_values$min_cases[par] <- par_stats$value[which.min(par_stats$cases)]
                out_stats$par_values$max_cases[par] <- par_stats$value[which.max(par_stats$cases)]
                out_stats$par_values$min_hosp[par] <- par_stats$value[which.min(par_stats$hosp)]
                out_stats$par_values$max_hosp[par] <- par_stats$value[which.max(par_stats$hosp)]
                out_stats$par_values$min_wdl[par] <- par_stats$value[which.min(par_stats$wdl)]
                out_stats$par_values$max_wdl[par] <- par_stats$value[which.max(par_stats$wdl)]
                out_stats$par_values$min_excess_beds[par] <- par_stats$value[which.min(par_stats$excess_beds)]
                out_stats$par_values$max_excess_beds[par] <- par_stats$value[which.max(par_stats$excess_beds)]

        } 
})

        
        
        
        
# Run baseline model 
preIntro <- data.frame(time=0:(min(times_model)-1))
preIntro <- cbind(preIntro, matrix(0, ncol=length(y), nrow=nrow(preIntro), dimnames = list(NULL,names(y))))
preIntro$S_n<-parms_baseline["population"]
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms_baseline)))) 

# Baseline working days lost
wdl<-worker_days_lost(out,parms)
wdl<-(sum(wdl)/length(wdl))

# Calc baseline stats
out_stats$baseline_stats <- c("deaths"=max(out$D),"cases"=max(out$CumCases),"hosp"=max(out$CumSevere),
                              "wdl"=wdl, "excess_beds"=sum(pmax(0,(out$Hosp+out$ICU)-beds))) 



# Adjust sensitivity analysis outputs to the baseline
out_stats$perc_change$parameter <- par_ranges$Parameter
out_stats$perc_change$low_deaths=round(((out_stats$stat_values$min_deaths-out_stats$baseline_stats["deaths"])/out_stats$baseline_stats["deaths"])*100,2)
out_stats$perc_change$high_deaths=round(((out_stats$stat_values$max_deaths-out_stats$baseline_stats["deaths"])/out_stats$baseline_stats["deaths"])*100,2)
out_stats$perc_change$low_cases=round(((out_stats$stat_values$min_cases-out_stats$baseline_stats["cases"])/out_stats$baseline_stats["cases"])*100,2)
out_stats$perc_change$high_cases=round(((out_stats$stat_values$max_cases-out_stats$baseline_stats["cases"])/out_stats$baseline_stats["cases"])*100,2)
out_stats$perc_change$low_hosp=round(((out_stats$stat_values$min_hosp-out_stats$baseline_stats["hosp"])/out_stats$baseline_stats["hosp"])*100,2)
out_stats$perc_change$high_hosp=round(((out_stats$stat_values$max_hosp-out_stats$baseline_stats["hosp"])/out_stats$baseline_stats["hosp"])*100,2)
out_stats$perc_change$low_wdl=round(((out_stats$stat_values$min_wdl-out_stats$baseline_stats["wdl"])/out_stats$baseline_stats["wdl"])*100,2)
out_stats$perc_change$high_wdl=round(((out_stats$stat_values$max_wdl-out_stats$baseline_stats["wdl"])/out_stats$baseline_stats["wdl"])*100,2)
out_stats$perc_change$low_excess_beds=round(((out_stats$stat_values$min_excess_beds-out_stats$baseline_stats["excess_beds"])/out_stats$baseline_stats["excess_beds"])*100,2)
out_stats$perc_change$high_excess_beds=round(((out_stats$stat_values$max_excess_beds-out_stats$baseline_stats["excess_beds"])/out_stats$baseline_stats["excess_beds"])*100,2)



# Save outputs
save(out_stats,file=paste("Output/Sensitivity_lockdown",lockdown,
                          ifelse(community_surveillance==T,"_community",""),
                          ifelse(masks==T,"_masks",""),
                          ".Rdata",sep=""))






