library(lubridate)
library(viridis)

# Load default parameters
source("R/calc_fractions.R")
source("R/pars_baseline.R")

# Load Google mobility data
# mobility <- rbind(read.csv("data/2020_BD_Region_Mobility_Report.csv"),read.csv("data/2021_BD_Region_Mobility_Report.csv"))
mobility <- read.csv("data/2020_BD_Region_Mobility_Report.csv")
mobility$date <- as.Date(mobility$date)


## Fit lockdown compliance parameters
#-------------------

# Fit neg exponential with scale-up
tune_compliance_pars_nexp <- function(par,ld_improve,ld_min_compliance,data,parms_baseline){
  
  par<-par^2
  
  # Get modelled % reduction in workers
  compliance_t <-rep(NA,as.numeric(as.Date("2020-06-01")-as.Date("2020-03-26")))
  for(t in 0:(length(compliance_t)-1)){
    if(ld_improve>0){ld_improve_stage <- min(1,t/ld_improve)
    }else if(ld_improve==0){ld_improve_stage <- 1} # implement max effect straight away
    compliance_t[t+1] <-1- min(1,par["fNC"] + (1-ld_improve_stage)*(1-par["fNC"]) +
                                 ifelse(ld_improve_stage<1,0,1-par["fNC"]-(exp(-par["ld_decline"]*(t-ld_improve))*(1-par["fNC"]-ld_min_compliance)+ld_min_compliance)))
  }
  workers_off <- (1-parms_baseline["fEW"]/parms_baseline["propWorkers"])*compliance_t
  modelled_reduction <- -workers_off*100
  
  # Identify days to fit to (not Friday/Saturday or Eid week)
  weekdays <- which(!weekdays(data$date)%in%c("Friday","Saturday"))
  eid_week <- which(data$date>=(as.Date("2020-05-25")-3) & data$date<=(as.Date("2020-05-25")+3))
  to_fit <- weekdays[!weekdays%in%eid_week]
  modelled_reduction <- modelled_reduction[to_fit]
  data_reduction <- data$workplaces_percent_change_from_baseline[to_fit]
  
  # sum of squared differences between modelled and data
  return(sum((modelled_reduction-data_reduction)^2))
}

# Optimise
opt_nexp <- matrix(nrow=15,ncol=5,dimnames = list(NULL,c("ld_improve","fNC","ld_decline","ssd","convergence")))
for(scale_up in 0:14){
  opt_nexp_su <- optim(par=c("fNC"=sqrt(0.2),"ld_decline"=sqrt(0.01)),
                       ld_improve = scale_up,
                       ld_min_compliance = 0.3,
                       fn=tune_compliance_pars_nexp,
                       data=mobility[which(mobility$date>=as.Date("2020-03-26") & mobility$date<as.Date("2020-06-01")),c("date","workplaces_percent_change_from_baseline")],
                       parms_baseline=parms_baseline,
                       method="BFGS")  
  opt_nexp[scale_up+1,] <- c(scale_up,opt_nexp_su$par^2,opt_nexp_su$value,opt_nexp_su$convergence)
}
opt_nexp <- opt_nexp[which.min(opt_nexp[,"ssd"]),]



# Fit sigmoid
tune_compliance_pars_sigmoid <- function(par,ld_improve,ld_min_compliance,data,parms_baseline){
  
  par<-par^2
  
  # Get modelled % reduction in workers
  compliance_t <-rep(NA,as.numeric(as.Date("2020-06-01")-as.Date("2020-03-26")))
  for(t in 0:(length(compliance_t)-1)){
    if(t>=ld_improve){compliance_t[t+1] <-((1-par["fNC"])-ld_min_compliance)/(1+exp((t-ld_improve-par["sigmoid_mid"])*par["ld_decline"])) + ld_min_compliance}
    if(t<ld_improve){compliance_t[t+1]<-(t/ld_improve)*((1-par["fNC"])-ld_min_compliance)/(1+exp((-par["sigmoid_mid"])*par["ld_decline"])) + ld_min_compliance}
  }
  workers_off <- (1-parms_baseline["fEW"]/parms_baseline["propWorkers"])*compliance_t
  modelled_reduction <- -workers_off*100
  
  # Identify days to fit to (not Friday/Saturday or Eid week)
  weekdays <- which(!weekdays(data$date)%in%c("Friday","Saturday"))
  eid_week <- which(data$date>=(as.Date("2020-05-25")-3) & data$date<=(as.Date("2020-05-25")+3))
  to_fit <- weekdays[!weekdays%in%eid_week]
  modelled_reduction <- modelled_reduction[to_fit]
  data_reduction <- data$workplaces_percent_change_from_baseline[to_fit]
  
  # sum of squared differences between modelled and data
  ssd <- sum((modelled_reduction-data_reduction)^2)
  
  # # Force first compliance value to be close to (1-fNC) - so that the
  # # designation of this as the maximum compliance experienced is maintained
  # if(((1-par["fNC"]) - compliance_t[1])>0.005){ssd <- 1000000}
  
  return(ssd)
  
}

# Optimise
opt_sigmoid <- matrix(nrow=15,ncol=6,dimnames = list(NULL,c("ld_improve","fNC","ld_decline","sigmoid_mid","ssd","convergence")))
for(scale_up in 0:14){
  opt_sigmoid_su <- optim(par=c("fNC"=sqrt(0.2),"ld_decline"=sqrt(0.1),"sigmoid_mid"=sqrt(70)),
                       ld_improve = scale_up,
                       ld_min_compliance = 0.3,
                       fn=tune_compliance_pars_sigmoid,
                       data=mobility[which(mobility$date>=as.Date("2020-03-26") & mobility$date<as.Date("2020-06-01")),c("date","workplaces_percent_change_from_baseline")],
                       parms_baseline=parms_baseline,
                       method="BFGS")  
  opt_sigmoid[scale_up+1,] <- c(scale_up,opt_sigmoid_su$par^2,opt_sigmoid_su$value,opt_sigmoid_su$convergence)
}
opt_sigmoid <- opt_sigmoid[which.min(opt_sigmoid[,"ssd"]),]



#Check AIC
data=mobility[which(mobility$date>=as.Date("2020-03-26") & mobility$date<as.Date("2020-06-01")),c("date","workplaces_percent_change_from_baseline")]
weekdays <- which(!weekdays(data$date)%in%c("Friday","Saturday"))
eid_week <- which(data$date>=(as.Date("2020-05-25")-3) & data$date<=(as.Date("2020-05-25")+3))
to_fit <- weekdays[!weekdays%in%eid_week]
n<-length(to_fit)
n*log(opt_sigmoid["ssd"]/n) + 2*4
n*log(opt_nexp["ssd"]/n) + 2*3
# sigmoid gives better fit

# Sigmoid gives a minimum non-compliance of 
ld_min_compliance <- 0.3
(minNC <-1-(((1-opt_sigmoid["fNC"])-ld_min_compliance)/(1+exp((-opt_sigmoid["sigmoid_mid"])*opt_sigmoid["ld_decline"])) + ld_min_compliance))
#0.06673928 

# the max of the sigmoid is 
((1-minNC)-ld_min_compliance)*(1+exp((-opt_sigmoid["sigmoid_mid"])*opt_sigmoid["ld_decline"])) + ld_min_compliance
(1-opt_sigmoid["fNC"]) # calc seems fine!



## Plot
#-------------------

# Set up colours
cols <- viridis(6)

tiff(filename="Figs/mobility.tiff",width=160,height=200,units="mm",pointsize=12,res=250)

par(mfrow=c(2,1))

par(mar=c(4.5,4.5,1,1))
start_date <- min(mobility$date)
end_date <- max(mobility$date)
date_ticks <- seq(start_date-14,end_date-14,by="2 month")
date_labels <- paste(month.abb[month(date_ticks)], (year(date_ticks))-2000,sep="")
plot(mobility$date,mobility$residential_percent_change_from_baseline,type="l",col=cols[1],lwd=2,
     xlab="",ylab="",ylim=c(-110,40),bty="l",axes=F)
box(bty="l")
title(ylab="% Change from baseline",xlab="Date", line=2.2)
axis(2,cex.axis=0.9)
axis(1,at=date_ticks,labels = date_labels,cex.axis=0.9)
lines(mobility$date,mobility$workplaces_percent_change_from_baseline,col=cols[2],lwd=2)#,lty=2)
lines(mobility$date,mobility$retail_and_recreation_percent_change_from_baseline,col=cols[3],lwd=2)#,lty=3)
lines(mobility$date,mobility$grocery_and_pharmacy_percent_change_from_baseline,col=cols[4],lwd=2)#,lty=4)
lines(mobility$date,mobility$parks_percent_change_from_baseline,col=cols[5],lwd=2)#,lty=5)
lines(mobility$date,mobility$transit_stations_percent_change_from_baseline,col=cols[6],lwd=2)#,lty=6)

lines(rep(as.Date("2020-03-26"),2),c(-200,100),col="grey45",lty=2)
# text(as.Date("2020-03-27"), -22, col="grey45", "Lockdown start",srt=90,cex=0.8)
lines(rep(as.Date("2020-06-01"),2),c(-200,100),col="grey45",lty=2)
# text(as.Date("2020-06-02"), -20, col="grey45", "Lockdown end",srt=90,cex=0.8)
lines(c(start_date-60,end_date),c(0,0),col="grey45",lty=2)
# text(as.Date("2020-04-25"), 4, col="grey45", "baseline",cex=0.8)

legend("bottomright",c("home","workplace","retail/recreation","grocery/pharmacy","parks","transit"),
       col=cols,lty=1,lwd=2,bty="n",ncol=2)


start_date_ld <- as.Date("2020-03-21")
end_date_ld <- as.Date("2020-06-05")
date_ticks <- seq(start_date_ld-14,end_date_ld,by="2 week")
date_labels <- paste(day(date_ticks),month.abb[month(date_ticks)], (year(date_ticks))-2000,sep="")
compliance_t_nexp <- compliance_t_sigmoid <-rep(0,length(start_date_ld:end_date_ld))
ld_start <- which(seq(start_date_ld,end_date_ld,"day")=="2020-03-26")
ld_end <- which(seq(start_date_ld,end_date_ld,"day")=="2020-06-01")
for(t in ld_start:ld_end){
  
  # what proportion of the way through the improvement stage are we (for neg exponentials)
  if(opt_nexp["ld_improve"]>0){ld_improve_stage_nexp <- min(1,(t-ld_start)/opt_nexp["ld_improve"])
  }else if(opt_nexp["ld_improve"]==0){ld_improve_stage_nexp <- 1} # implement max effect straight away
  if(opt_sigmoid["ld_improve"]>0){ld_improve_stage_sigmoid <- min(1,(t-ld_start)/opt_sigmoid["ld_improve"])
  }else if(opt_sigmoid["ld_improve"]==0){ld_improve_stage_sigmoid <- 1} # implement max effect straight away
  
  # what is the current level of compliance?
  # compliance increases during the improvement stage and then decreases
  compliance_t_nexp[t] <-1- min(1,opt_nexp["fNC"] +
                                  (1-ld_improve_stage_nexp)*(1-opt_nexp["fNC"]) +
                                  ifelse(ld_improve_stage_nexp<1,0,1-opt_nexp["fNC"]-(exp(-opt_nexp["ld_decline"]*(t-ld_start-opt_nexp["ld_improve"]))*(1-opt_nexp["fNC"]-parms_baseline["ld_min_compliance"])+parms_baseline["ld_min_compliance"])))
  if(ld_improve_stage_sigmoid==1){compliance_t_sigmoid[t] <-((1-opt_sigmoid["fNC"])-ld_min_compliance)/(1+exp((t-ld_start-opt_sigmoid["ld_improve"]-opt_sigmoid["sigmoid_mid"])*opt_sigmoid["ld_decline"])) + ld_min_compliance}
  if(ld_improve_stage_sigmoid<1){compliance_t_sigmoid[t]<-ld_improve_stage_sigmoid*((1-opt_sigmoid["fNC"])-ld_min_compliance)/(1+exp((-opt_sigmoid["sigmoid_mid"])*opt_sigmoid["ld_decline"])) + ld_min_compliance}
  
  
}
workers_off_nexp <- (1-parms_baseline["fEW"]/parms_baseline["propWorkers"])*compliance_t_nexp # what proportion of workers are not working?
workers_off_sigmoid <- (1-parms_baseline["fEW"]/parms_baseline["propWorkers"])*compliance_t_sigmoid # what proportion of workers are not working?
modelled_reduction_nexp <- -workers_off_nexp*100
modelled_reduction_sigmoid <- -workers_off_sigmoid*100
par(mar=c(4.5,4.5,1,1))
plot(mobility$date,mobility$workplaces_percent_change_from_baseline,type="l",col=cols[1],lwd=2,
     xlab="",ylab="",ylim=c(-85,2),bty="l",axes=F,
     xlim=c(start_date_ld,end_date_ld))
lines(start_date_ld:end_date_ld,modelled_reduction_nexp,lwd=2,col=cols[4])
lines(start_date_ld:end_date_ld,modelled_reduction_sigmoid,lwd=2,col=cols[6])
title(ylab="% Change in workplace attendance\nfrom baseline",xlab="Date", line=2.2)
box(bty="l")
axis(2,cex.axis=0.9)
axis(1,at=date_ticks,labels = date_labels,cex.axis=0.9)
legend(as.Date("2020-04-25"),-60,c("Workplace data","Exponential model","Sigmoidal model"),
       col=cols[c(1,4,6)],lty=1,lwd=2,bty="n")
lines(rep(as.Date("2020-03-26"),2),c(-200,100),col="grey45",lty=2)
# text(as.Date("2020-03-27"), -22, col="grey45", "Lockdown start",srt=90,cex=0.8)
lines(rep(as.Date("2020-06-01"),2),c(-200,100),col="grey45",lty=2)
# text(as.Date("2020-06-02"), -20, col="grey45", "Lockdown end",srt=90,cex=0.8)
lines(rep(as.Date("2020-05-25"),2),c(-200,100),col="deepskyblue",lty=2)#eid
# lines(rep(as.Date("2020-04-14"),2),c(-200,100),col="grey45",lty=2)#bengali new year
# lines(rep(as.Date("2020-04-8"),2),c(-200,100),col="grey45",lty=2)#Mid Sha'ban
# lines(rep(as.Date("2020-05-01"),2),c(-200,100),col="grey45",lty=2)#labour day
# lines(rep(as.Date("2020-05-06"),2),c(-200,100),col="grey45",lty=2)#vesak
# lines(rep(as.Date("2020-05-20"),2),c(-200,100),col="grey45",lty=2)#laylat al-Qadr
# lines(rep(as.Date("2020-05-21"),2),c(-200,100),col="grey45",lty=2)#Jumu'atul-Wida
# text(as.Date("2020-06-02"), -20, col="grey45", "Lockdown end",srt=90,cex=0.8)
lines(c(start_date_ld,end_date_ld),c(0,0),col="grey45",lty=2)
# text(as.Date("2020-04-16"), 2, col="grey45", "baseline",cex=0.8)

dev.off()





