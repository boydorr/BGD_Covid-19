rm(list=ls())

require(deSolve)
require(data.table)
require(lubridate)

source("R/covid_model.R")
source("R/amalgamate_cats.R")
source("R/calc_fractions.R")
source("R/bangladesh_covid_data.R")

## Proportion cases in Dhaka (from the dashboard)
district_cases <- read.csv("data/district_cases_dashboard_19.11.csv")
propDhaka <- district_cases$X.[which(district_cases$Distirct=="Dhaka")]/sum(district_cases$X.)



## Load in baseline parameters and initial conditions and adjust as needed
#______________________

## Load in baseline parameters
source("R/pars_baseline.R")

# End date for model
end_date <- as.Date("2020-06-01")+5 # lockdown started on 26th March and ended in June
end_date_early <- as.Date("2020-04-01") # lockdown started on 26th March

# time periods of interest
dates <- as.Date(start_date:end_date,origin=as.Date("1970-01-01"))
times <- seq(0, as.numeric(end_date-start_date), 1)
times_model <- c(as.numeric(intro_date-start_date):as.numeric(end_date-start_date))

# Initial conditions
source("R/initial_conds.R")




## Tune R0
#_______________________

tune_R0 <- function(par,model_parms,y,times,bangladesh,end_date,start_date,dates){
        
        # Adjust transmission rates for R0
        print(par)
        parms <- model_parms
        parms["R0"] <- par
        parms["beta_s"] <- parms["R0"]/(parms["fa"]*(parms["asympTrans"]*(parms["propPresympTrans"]/(1-parms["propPresympTrans"]) + 1))*parms["dur_s"] + 
                                                (1-parms["fa"])*((parms["propPresympTrans"]/(1-parms["propPresympTrans"]))*parms["dur_s"] + 
                                                                         parms["dur_s"]))
        parms["beta_p"] <- ((parms["propPresympTrans"]/(1-parms["propPresympTrans"]))*parms["beta_s"]*parms["dur_s"])/parms["dur_p"]
        parms["beta_a"] <- ((parms["asympTrans"]*(parms["propPresympTrans"]/(1-parms["propPresympTrans"]) + 1))*parms["beta_s"]*parms["dur_s"])/parms["dur_a"]
        
        # Run model 
        preIntro <- data.frame(time=0:(min(times_model)-1))
        preIntro <- cbind(preIntro, matrix(0, ncol=length(y), nrow=nrow(preIntro), dimnames = list(NULL,names(y))))
        preIntro$S_n<-parms_baseline["population"]
        out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
        
        # Difference between cumulative deaths from data and model at end date
        return(abs(out$D[which(dates==as.Date("2020-03-26"))]-bangladesh$cumulative_death[which(bangladesh$date==as.Date("2020-03-26"))]*propDhaka))
}

# Optimise
opt <- optim(par=3,fn=tune_R0,model_parms=parms_baseline,y=y,times=times,method="Brent",
             bangladesh=bangladesh,lower=0,upper=30,
             end_date=end_date,start_date=start_date,dates=dates)

opt$par
opt$value
opt$convergence

# Select R0
parms_baseline["R0"] <- round(opt$par,2)
parms_baseline["beta_s"] <- parms_baseline["R0"]/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] + 
                                                          (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] + 
                                                                                            parms_baseline["dur_s"]))
parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]





## Tune lockdown effect
#_______________________

tune_ld_effect <- function(par,model_parms,y,times,bangladesh,end_date,start_date,dates){
        
        # Adjust transmission rates for R0
        parms <- model_parms
        parms["ld_effect"] <- par
        
        # Run model 
        preIntro <- data.frame(time=0:(min(times_model)-1))
        preIntro <- cbind(preIntro, matrix(0, ncol=length(y), nrow=nrow(preIntro), dimnames = list(NULL,names(y))))
        preIntro$S_n<-parms_baseline["population"]
        out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
        
        # Difference between cumulative deaths from data and model at end date
        return(abs(out$D[which(dates==as.Date("2020-06-01"))]-bangladesh$cumulative_death[which(bangladesh$date==as.Date("2020-06-01"))]*propDhaka))
}

# Optimise
opt <- optim(par=0.5,fn=tune_ld_effect,model_parms=parms_baseline,y=y,times=times,method="Brent",
             bangladesh=bangladesh,lower=0,upper=1,
             end_date=end_date,start_date=start_date,dates=dates)

opt$par
opt$value
opt$convergence


# Select ld_effect
parms_baseline["ld_effect"] <- round(opt$par,2)





## Run model
#______________________

preIntro <- data.frame(time=0:(min(times_model)-1))
preIntro <- cbind(preIntro, matrix(0, ncol=length(y), nrow=nrow(preIntro), dimnames = list(NULL,names(y))))
preIntro$S_n<-parms_baseline["population"]
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 

# Check difference between data and model at beginning of lockdown
out$D[which(dates==as.Date("2020-03-26"))]-bangladesh$cumulative_death[which(bangladesh$date==as.Date("2020-03-26"))]*propDhaka

# Check difference between data and model at end of lockdown
out$D[which(dates==as.Date("2020-06-01"))]-bangladesh$cumulative_death[which(bangladesh$date==as.Date("2020-06-01"))]*propDhaka

# Modelled and data cases at the end of lockdown
modelled_cases <- out$CumCases[which(dates==as.Date("2020-06-01"))]
data_cases <- bangladesh$cumulative_cases[which(bangladesh$date==as.Date("2020-06-01"))]*propDhaka
data_cases/modelled_cases



##Figure
#______________________

# Open figure
tiff(filename="Figs/ModelTuning.tiff",width=160,height=150,units="mm",pointsize=12,res=200)
par(mfrow=c(2,2))
par(mar=c(3,2.7,1,1.7))



#Plot 1
#-----------

# Dates for plotting
date_ticks <- as.numeric(seq(start_date,end_date,by="month") - start_date)
date_labels <- paste(month.abb[month(seq(start_date,end_date,by="month"))], (year(seq(start_date,end_date,by="month")))-2000,sep="")

# Compare with data in early-stage of epidemic
yRange = c(0, 5)#max(out$D[60:(end_date_early-start_date)])*5)
xRange = c(60, end_date_early-start_date)
plot((bangladesh$date-start_date)[which((bangladesh$date-start_date)<=xRange[2])], bangladesh$cumulative_death[which((bangladesh$date-start_date)<=xRange[2])]*propDhaka,
     col=1, type = "l", lty=2, ylim=yRange, xlim=xRange, ylab="",bty="l",axes=F,lwd=1,xlab="")
graphics::box(bty="l")
title(ylab="Count", line=1.6)
title(xlab="Date", line=1.6)

axis(2,cex.axis=0.8)
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.8)
# lines(out$time[(xRange[1]+1):(xRange[2]+1)], c(0,diff(out$CumCases))[(xRange[1]+1):(xRange[2]+1)], col="red", lwd=1)
# lines(out$time[(xRange[1]+1):(xRange[2]+1)], c(0,diff(out$CumSymp))[(xRange[1]+1):(xRange[2]+1)], col="orange", lwd=1)
lines(out$time[(xRange[1]+1):(xRange[2]+1)], out$D[(xRange[1]+1):(xRange[2]+1)], col=1, lwd=1)
# lines(bangladesh$date-start_date, bangladesh$cases*10*propDhaka, col="red", type = "l", lwd=1, lty=3)
# lines((bangladesh$date-start_date)[which((bangladesh$date-start_date)<=xRange[2])], bangladesh$cases[which((bangladesh$date-start_date)<=xRange[2])]*propDhaka, col=2, type = "l", lwd=1, lty=2)

# # First detection
# lines(rep(bangladesh$date[1]-start_date, 100), (0:99)*yRange[2]/100, col="#a39999", type = "l", lwd=1,lty=2) # First detected case
# text(bangladesh$date[1]-start_date+1, yRange[2]*0.6, col="#a39999", "First detection",srt=90,cex=0.8) # First detected case

# Lockdown date
lines(rep(as.Date("2020-03-26")-start_date, 100), (0:99)*yRange[2]/100, col="grey45", type = "l", lwd=1,lty=2)
text(as.Date("2020-03-26")-start_date+1, yRange[2]*0.6, col="grey45", "Lockdown start",srt=90,cex=0.8)

# legend("topright",
#        c("Daily new cases","Daily new symptomatic","Total fatal", "Reported daily new cases", "Reported total deaths"),
#        col=c("red","orange","black", "red","black"),
#        lty=c(1,1,1,2,2), lwd=c(2,2,2,1,1), box.col="white",cex=0.9)

legend(xRange[1],yRange[2]*1.04,
       c("Modelled total deaths", "Reported total deaths"),
       col=c("black","black"),box.col="white",
       lty=c(1,2), lwd=1,cex=0.8)
legend("topright","A",text.font=2,box.col = "white")



#Plot 2
#-----------

# Compare with data to end of lockdown
# yRange = c(0, max(diff(out$CumCases)))
yRange = c(0, 320)
xRange = c(60, end_date-start_date)
plot((bangladesh$date-start_date)[which((bangladesh$date-start_date)<=xRange[2])], bangladesh$cumulative_death[which((bangladesh$date-start_date)<=xRange[2])]*propDhaka, col=1, type = "l", lwd=1, lty=2,
     ylim=yRange, xlim=xRange, ylab="",bty="l",axes=F,xlab="")
title(ylab="Count", line=1.6)
title(xlab="Date", line=1.6)
graphics::box(bty="l")
axis(2,cex.axis=0.8,padj=1)
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.8,padj=-1)

# lines(out$time, c(0,diff(out$CumCases)), col="red", lwd=1)
# lines(out$time, c(0,diff(out$CumSymp)), col="orange", lwd=1)
lines(out$time[(xRange[1]+1):(xRange[2]+1)], out$D[(xRange[1]+1):(xRange[2]+1)], col=1, lwd=1)
# lines(bangladesh$date-start_date, bangladesh$cases*10*propDhaka, col="red", type = "l", lwd=1, lty=3)
# lines((bangladesh$date-start_date)[which((bangladesh$date-start_date)<=xRange[2])], bangladesh$cases[which((bangladesh$date-start_date)<=xRange[2])]*propDhaka, col="red", type = "l", lwd=1, lty=2)

# # First detection
# lines(rep(bangladesh$date[1]-start_date, 100), (0:99)*yRange[2]/100, col="#a39999", type = "l", lwd=1,lty=2) # First detected case
# text(bangladesh$date[1]-start_date+2, yRange[2]*0.6, col="#a39999", "First detection",srt=90,cex=0.8) # First detected case

# Lockdown
lines(rep(as.Date("2020-03-26")-start_date, 100), (0:99)*yRange[2]/100, col="grey45", type = "l", lwd=1,lty=2)
text(as.Date("2020-03-26")-start_date+2, yRange[2]*0.6, col="grey45", "Lockdown start",srt=90,cex=0.8)
lines(rep(as.Date("2020-06-01")-start_date, 100), (0:99)*yRange[2]/100, col="grey45", type = "l", lwd=1,lty=2)
text(as.Date("2020-06-01")-start_date+2, yRange[2]*0.6, col="grey45", "Lockdown end",srt=90,cex=0.8)

legend(xRange[1]*0.96,yRange[2]*1.04,
       c("Modelled total deaths", "Reported total deaths"),
       col=c("black","black"),
       lty=c(1,2), lwd=1, box.col="white",cex=0.8)
legend("topright","B",text.font=2,box.col = "white")




#Plot 3
#-----------

# yRange = c(0, max(diff(out$CumCases)))
yRange = c(0, 300000)
xRange = c(60, end_date-start_date)
plot((bangladesh$date-start_date)[which((bangladesh$date-start_date)<=xRange[2])], bangladesh$cumulative_cases[which((bangladesh$date-start_date)<=xRange[2])]*propDhaka, col=1, type = "l", lwd=1, lty=2,
     ylim=yRange, xlim=xRange, ylab="",bty="l",axes=F,xlab="")
title(ylab="Count", line=1.6)
title(xlab="Date", line=1.6)
graphics::box(bty="l")
axis(2,cex.axis=0.8,padj=0.5,at=c(0,100000,200000,300000),labels=c(0,expression("1x10"^5),expression("2x10"^5),expression("3x10"^5)))
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.8,padj=-1)

lines(out$time, out$CumCases, col=1, lwd=1)
lines(out$time, out$CumSymp, col=1, lwd=1,lty=3)
# lines(out$time[(xRange[1]+1):(xRange[2]+1)], out$D[(xRange[1]+1):(xRange[2]+1)], col=1, lwd=1)
# lines(bangladesh$date-start_date, bangladesh$cases*10*propDhaka, col="red", type = "l", lwd=1, lty=3)
# lines((bangladesh$date-start_date)[which((bangladesh$date-start_date)<=xRange[2])], bangladesh$cumulative_death[which((bangladesh$date-start_date)<=xRange[2])]*propDhaka, col="black", type = "l", lwd=1, lty=2)

# First detection
lines(rep((bangladesh$date[1]-start_date), 100), (0:99)*yRange[2]/100, col="grey45", type = "l", lwd=1,lty=2)
text(bangladesh$date[1]-start_date+2, yRange[2]-yRange[2]/2, col="grey45", "First detection",srt=90,cex=0.8) # First detected case

# Lockdown
lines(rep(as.Date("2020-03-26")-start_date, 100), (0:99)*yRange[2]/100, col="grey45", type = "l", lwd=1,lty=2)
text(as.Date("2020-03-26")-start_date+2, yRange[2]-yRange[2]/2, col="grey45", "Lockdown start",srt=90,cex=0.8)
lines(rep(as.Date("2020-06-01")-start_date, 100), (0:99)*yRange[2]/100, col="grey45", type = "l", lwd=1,lty=2)
text(as.Date("2020-06-01")-start_date+2, yRange[2]-yRange[2]/2, col="grey45", "Lockdown end",srt=90,cex=0.8)

legend("topright","C",text.font=2,box.col = "white")
legend(xRange[1]*0.96,yRange[2]*1.04,
       c("Modelled total cases","Modelled total symptomatic cases", "Reported total cases"),
       col=c(1,1,1),box.col="white",
       lty=c(1,3,2), lwd=1,cex=0.8)


#Plot 4
#-----------

# Dates for plotting
date_ticks <- as.numeric(seq(start_date,as.Date("2021-01-01"),by="month") - start_date)
date_labels <- paste(month.abb[month(seq(start_date,as.Date("2021-01-01"),by="month"))], (year(seq(start_date,as.Date("2021-01-01"),by="month")))-2000,sep="")
date_ticks <- date_ticks[seq(1,length(date_ticks),2)]
date_labels <- date_labels[seq(1,length(date_labels),2)]

## Plot lockdown compliance over time
compliance_t<-rep(0,as.Date("2021-01-01")-start_date)
for(t in parms_baseline["ld_start"]:(as.Date("2021-01-01")-start_date-1)){

        # what proportion of the way through the improvement stage are we?
        if(parms_baseline["ld_improve"]>0){ld_improve_stage <- min(1,(t-parms_baseline["ld_start"])/parms_baseline["ld_improve"])
        }else if(ld_improve==0){ld_improve_stage <- 1} # implement max effect straight away

        # what is the current level of non-compliance?
        # compliance increases during the improvement stage and then decreases
        compliance_t[t] <-1- min(1,parms_baseline["fNC"] +
                             (1-ld_improve_stage)*(1-parms_baseline["fNC"]) +
                             ifelse(ld_improve_stage<1,0,1-parms_baseline["fNC"]-(exp(-parms_baseline["ld_decline"]*(t-parms_baseline["ld_start"]-parms_baseline["ld_improve"]))*(1-parms_baseline["fNC"]-parms_baseline["ld_min_compliance"])+parms_baseline["ld_min_compliance"])))

}
plot(compliance_t*100,ylim=c(0,100),
     ylab="", xlab="",
     type="l",bty="l",col=1,lwd=1,axes=F,lty=2)
title(ylab="% population compliant with lockdown", line=1.6)
title(xlab="Date", line=1.6)
compliance_t_short <- compliance_t
compliance_t_short[parms_baseline["ld_end"]:length(compliance_t)]<-0
lines(compliance_t_short*100,lwd=1)
box(bty="l")
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.8,padj=-1)
axis(2,cex.axis=0.8,padj=1)
legend(0,100*1.04,
       c("Implemented lockdown end date","Extended lockdown"),lty=1:2,bty="n",cex=0.8)
legend("topright","D",text.font=2,box.col = "white")


# Close figure
dev.off()

