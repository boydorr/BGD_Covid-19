rm(list=ls())

require(deSolve)
require(data.table)
require(lubridate)

source("R/covid_model.R")
source("R/worker_days_lost.R")
source("R/amalgamate_cats.R")
source("R/calc_fractions.R")


## Parameter ranges
par_ranges <- read.csv("data/ParameterRanges.csv",stringsAsFactors = F)

# Parameters for baseline scenario (with no interventions)
end_date <- as.Date("2020-12-31") 
source("R/pars_baseline.R")

# Initial conditions
source("R/initial_conds.R")

# # Which timing variable to investigate? (improvement or start_date)
# vary <- "start_date"



# Set up list to hold output stats
# _______________________

# Scenario descriptions
scenarios <- c("Lockdown + Quarantine",
               "Lockdown + Masks",
               "Lockdown + Quarantine + Masks")

# values to test
values_sd<--30:30
values_improve<-0:60

# Set up data frame
stats_sd <- rep(list(data.frame(value=values_sd,
                                deaths=NA,
                                cases=NA,
                                hosp=NA,
                                wdl=NA,
                                excess_beds=NA)),
                length(scenarios))
stats_improve <- rep(list(data.frame(value=values_improve,
                                     deaths=NA,
                                     cases=NA,
                                     hosp=NA,
                                     wdl=NA,
                                     excess_beds=NA)),
                     length(scenarios))
names(stats_sd)<-scenarios
names(stats_improve)<-scenarios




# Gather statistics for different scenarios
# _______________________

# Function to calculate statistics for given intervention scenario
get_stats <- function(parms){
  
  # Run model 
  preIntro <- data.frame(time=0:(min(times_model)-1))
  preIntro <- cbind(preIntro, matrix(0, ncol=length(y), nrow=nrow(preIntro), dimnames = list(NULL,names(y))))
  preIntro$S_n <- parms_baseline["population"]
  out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
  
  # Working days lost
  wdl<-worker_days_lost(out,parms)
  wdl<-(sum(wdl)/length(wdl))
  
  # Statistics
  c(deaths=max(out$D),
    cases=max(out$CumCases),
    hosp=max(out$CumSevere),
    wdl=wdl,
    excess_beds=sum(pmax(0,(out$Hosp+out$ICU)-beds))/sum(out$Hosp+out$ICU))
  
}


# Scenario 1 (quarantine)
for(value in 1:length(values_sd)){
  parms<-parms_baseline
  parms["syndromic"] <- T
  parms["syn_start"] <- parms["ld_end"] + values_sd[value] 
  stats_sd[[1]][value,2:ncol(stats_sd[[1]])] <- get_stats(parms)
}
for(value in 1:length(values_improve)){
  parms<-parms_baseline
  parms["syndromic"] <- T
  parms["syn_improve"] <- values_improve[value] 
  stats_improve[[1]][value,2:ncol(stats_improve[[1]])] <- get_stats(parms)
}


# Scenario 2 (masks - medium quality)
for(value in 1:length(values_sd)){
  parms<-parms_baseline
  parms["mask_effect_outward"] <- 0.5
  parms["f_mask_effect_inward"] <- 0.5
  parms["mask"] <- T
  parms["mask_start"] <- parms["ld_end"] + values_sd[value]  
  stats_sd[[2]][value,2:ncol(stats_sd[[2]])] <- get_stats(parms)
}
for(value in 1:length(values_improve)){
  parms<-parms_baseline
  parms["mask_effect_outward"] <- 0.5
  parms["f_mask_effect_inward"] <- 0.5
  parms["mask"] <- T
  parms["mask_improve"] <- values_improve[value] 
  stats_improve[[2]][value,2:ncol(stats_improve[[2]])] <- get_stats(parms)
}


# Scenario 3 (quarantine + masks - medium quality)
for(value in 1:length(values_sd)){
  parms<-parms_baseline
  
  parms["syndromic"] <- T
  parms["syn_start"] <- parms["ld_end"] + values_sd[value] 
  
  parms["mask_effect_outward"] <- 0.5
  parms["f_mask_effect_inward"] <- 0.5
  parms["mask"] <- T
  parms["mask_start"] <- parms["ld_end"] + values_sd[value] 
  stats_sd[[3]][value,2:ncol(stats_sd[[3]])] <- get_stats(parms)
}
for(value in 1:length(values_improve)){
  parms<-parms_baseline
  
  parms["syndromic"] <- T
  parms["syn_improve"] <- values_improve[value] 
  
  parms["mask_effect_outward"] <- 0.5
  parms["f_mask_effect_inward"] <- 0.5
  parms["mask"] <- T
  parms["mask_improve"] <- values_improve[value] 
  stats_improve[[3]][value,2:ncol(stats_improve[[3]])] <- get_stats(parms)
}


# baseline 
baseline <- get_stats(parms_baseline)



# Figure
# _______________________

# Open figure
tiff(filename=ifelse(end_date=="2020-12-31",paste("Figs/Intervention_timing.tiff",sep=""),paste("Figs/Intervention_timing_endDate",end_date,".tiff",sep="")),width=240,height=120,units="mm",pointsize=12,res=250)
par(mar=c(3,2.7,0.8,0.8))



#Plots 1&4: deaths
#-----------

# Set up plot axes
par(fig=c(0,0.25,0.5,1))
yRange = c(0, max(baseline["deaths"],stats_sd[[1]]$deaths,stats_sd[[2]]$deaths,stats_sd[[3]]$deaths)/1000)
xRange = c(start_date+values_sd[1]+parms_baseline["ld_end"], start_date+values_sd[length(values_sd)]+parms_baseline["ld_end"])

plot(NA,xlim=xRange,ylim=yRange,xlab="",ylab="",bty="l",axes=F)
title(ylab="Deaths (thousands)", line=1.6,cex.lab=0.9)
title(xlab="New intervention start date", line=1.6,cex.lab=0.9)
graphics::box(bty="l")
axis(2,cex.axis=0.7,padj=1,at=c(0,10,20,30))
axis(1,at=c(as.Date("2020-05-02"),as.Date("2020-06-01"),as.Date("2020-07-01")),
     labels=format(c(as.Date("2020-05-02"),as.Date("2020-06-01"),as.Date("2020-07-01")), format = "%d/%m"),
     cex.axis=0.7,padj=-1)

# Quarantine
lines(x=xRange[1]:xRange[2],y=stats_sd[[1]]$deaths/1000,lty=2,col=2,lwd=1.5)

# Masks
lines(x=xRange[1]:xRange[2],y=stats_sd[[2]]$deaths/1000,lty=3,col="dodgerblue",lwd=1.5)

# Masks + Quarantine
lines(x=xRange[1]:xRange[2],y=stats_sd[[3]]$deaths/1000,lty=4,col="darkorange",lwd=1.5)

# Lockdown only
lines(x=xRange[1]:xRange[2],y=rep(baseline["deaths"],length(xRange[1]:xRange[2]))/1000,lty=1,col="navy",lwd=1.5)


lines(rep(as.Date("2020-06-01"), 100), (0:99)*yRange[2]/100, col="grey45", type = "l", lwd=1,lty=2)
text(as.Date("2020-06-01")+2, yRange[2]-yRange[2]/2, col="grey45", "Lockdown end",srt=90,cex=0.8)


legend("topright","A",text.font=2,bty="n",cex=1)



# Set up plot axes
par(fig=c(0,0.25,0,0.5),new=T)
yRange = c(0, max(baseline["deaths"],stats_improve[[1]]$deaths,stats_improve[[2]]$deaths,stats_improve[[3]]$deaths)/1000)
xRange=c(min(values_improve),max(values_improve))

plot(NA,xlim=xRange,ylim=yRange,xlab="",ylab="",bty="l",axes=F)
title(ylab="Deaths (thousands)", line=1.6,cex.lab=0.9)
title(xlab="Days to scale up", line=1.6,cex.lab=0.9)
graphics::box(bty="l")
axis(2,cex.axis=0.7,padj=1,at=c(0,10,20,30))
axis(1,pretty(xRange),labels=format(pretty(xRange), format = "%d/%m"),cex.axis=0.7,padj=-1)

# Quarantine
lines(x=xRange[1]:xRange[2],y=stats_improve[[1]]$deaths/1000,lty=2,col=2,lwd=1.5)

# Masks
lines(x=xRange[1]:xRange[2],y=stats_improve[[2]]$deaths/1000,lty=3,col="dodgerblue",lwd=1.5)

# Masks + Quarantine
lines(x=xRange[1]:xRange[2],y=stats_improve[[3]]$deaths/1000,lty=4,col="darkorange",lwd=1.5)

# Lockdown only
lines(x=xRange[1]:xRange[2],y=rep(baseline["deaths"],length(xRange[1]:xRange[2]))/1000,lty=1,col="navy",lwd=1.5)


legend(1,16,legend=c("Lockdown only","Quarantine","Masks","Quarantine+Masks"),
       col=c("navy",2,"dodgerblue","darkorange"),lty=1:4,bty="n",cex=0.8,lwd=1.5)


legend("topright","E",text.font=2,bty="n",cex=1)






#Plot 2&5: Hospitalisations
#-----------

# Set up plot axes
par(fig=c(0.25,0.5,0.5,1),new=T)
yRange = c(0, max(baseline["hosp"],stats_sd[[1]]$hosp,stats_sd[[2]]$hosp,stats_sd[[3]]$hosp)/1000)
xRange = c(start_date+values_sd[1]+parms_baseline["ld_end"], start_date+values_sd[length(values_sd)]+parms_baseline["ld_end"])
plot(NA,xlim=xRange,ylim=yRange,xlab="",ylab="",bty="l",axes=F)
title(ylab="Hospitalisations (thousands)", line=1.6,cex.lab=0.9)
title(xlab="New intervention start date", line=1.6,cex.lab=0.9)
graphics::box(bty="l")
axis(2,cex.axis=0.7,padj=1,at=c(0,100,200,300))
axis(1,at=c(as.Date("2020-05-02"),as.Date("2020-06-01"),as.Date("2020-07-01")),
     labels=format(c(as.Date("2020-05-02"),as.Date("2020-06-01"),as.Date("2020-07-01")), format = "%d/%m"),
     cex.axis=0.7,padj=-1)

# Quarantine
lines(x=xRange[1]:xRange[2],y=stats_sd[[1]]$hosp/1000,lty=2,col=2,lwd=1.5)

# Masks
lines(x=xRange[1]:xRange[2],y=stats_sd[[2]]$hosp/1000,lty=3,col="dodgerblue",lwd=1.5)

# Masks + Quarantine
lines(x=xRange[1]:xRange[2],y=stats_sd[[3]]$hosp/1000,lty=4,col="darkorange",lwd=1.5)

# Lockdown only
lines(x=xRange[1]:xRange[2],y=rep(baseline["hosp"],length(xRange[1]:xRange[2]))/1000,lty=1,col="navy",lwd=1.5)

lines(rep(as.Date("2020-06-01"), 100), (0:99)*yRange[2]/100, col="grey45", type = "l", lwd=1,lty=2)
text(as.Date("2020-06-01")+2, yRange[2]-yRange[2]/2, col="grey45", "Lockdown end",srt=90,cex=0.8)


legend("topright","B",text.font=2,bty="n",cex=1)


# Set up plot axes
par(fig=c(0.25,0.5,0,0.5),new=T)
yRange = c(0, max(baseline["hosp"],stats_improve[[1]]$hosp,stats_improve[[2]]$hosp,stats_improve[[3]]$hosp)/1000)
xRange=c(min(values_improve),max(values_improve))
plot(NA,xlim=xRange,ylim=yRange,xlab="",ylab="",bty="l",axes=F)
title(ylab="Hospitalisations (thousands)", line=1.6,cex.lab=0.9)
title(xlab="Days to scale up", line=1.6,cex.lab=0.9)
graphics::box(bty="l")
axis(2,cex.axis=0.7,padj=1,at=c(0,100,200,300))
axis(1,pretty(xRange),labels=format(pretty(xRange), format = "%d/%m"),cex.axis=0.7,padj=-1)

# Quarantine
lines(x=xRange[1]:xRange[2],y=stats_improve[[1]]$hosp/1000,lty=2,col=2,lwd=1.5)

# Masks
lines(x=xRange[1]:xRange[2],y=stats_improve[[2]]$hosp/1000,lty=3,col="dodgerblue",lwd=1.5)

# Masks + Quarantine
lines(x=xRange[1]:xRange[2],y=stats_improve[[3]]$hosp/1000,lty=4,col="darkorange",lwd=1.5)

# Lockdown only
lines(x=xRange[1]:xRange[2],y=rep(baseline["hosp"],length(xRange[1]:xRange[2]))/1000,lty=1,col="navy",lwd=1.5)


legend("topright","F",text.font=2,bty="n",cex=1)





#Plot 3&7: Working days lost
#-----------

# Set up plot axes
par(fig=c(0.5,0.75,0.5,1),new=T)
yRange = c(0, max(baseline["wdl"],stats_sd[[1]]$wdl,stats_sd[[2]]$wdl,stats_sd[[3]]$wdl)*100)
xRange = c(start_date+values_sd[1]+parms_baseline["ld_end"], start_date+values_sd[length(values_sd)]+parms_baseline["ld_end"])
plot(NA,xlim=xRange,ylim=yRange,xlab="",ylab="",bty="l",axes=F)
title(ylab="% working days lost", line=1.6,cex.lab=0.9)
title(xlab="New intervention start date", line=1.6,cex.lab=0.9)
graphics::box(bty="l")
axis(2,cex.axis=0.7,padj=1)
axis(1,at=c(as.Date("2020-05-02"),as.Date("2020-06-01"),as.Date("2020-07-01")),
     labels=format(c(as.Date("2020-05-02"),as.Date("2020-06-01"),as.Date("2020-07-01")), format = "%d/%m"),
     cex.axis=0.7,padj=-1)

# Quarantine
lines(x=xRange[1]:xRange[2],y=stats_sd[[1]]$wdl*100,lty=2,col=2,lwd=1.5)

# Masks
lines(x=xRange[1]:xRange[2],y=stats_sd[[2]]$wdl*100,lty=3,col="dodgerblue",lwd=1.5)

# Masks + Quarantine
lines(x=xRange[1]:xRange[2],y=stats_sd[[3]]$wdl*100,lty=4,col="darkorange",lwd=1.5)

# Lockdown only
lines(x=xRange[1]:xRange[2],y=rep(baseline["wdl"],length(xRange[1]:xRange[2]))*100,lty=1,col="navy",lwd=1.5)

lines(rep(as.Date("2020-06-01"), 100), (0:99)*yRange[2]/100, col="grey45", type = "l", lwd=1,lty=2)
text(as.Date("2020-06-01")+2, yRange[2]-yRange[2]/2, col="grey45", "Lockdown end",srt=90,cex=0.8)

legend("topright","C",text.font=2,bty="n",cex=1)




# Set up plot axes
par(fig=c(0.5,0.75,0,0.5),new=T)
yRange = c(0, max(baseline["wdl"],stats_improve[[1]]$wdl,stats_improve[[2]]$wdl,stats_improve[[3]]$wdl)*100)
xRange=c(min(values_improve),max(values_improve))
plot(NA,xlim=xRange,ylim=yRange,xlab="",ylab="",bty="l",axes=F)
title(ylab="% working days lost", line=1.6,cex.lab=0.9)
title(xlab="Days to scale up", line=1.6,cex.lab=0.9)
graphics::box(bty="l")
axis(2,cex.axis=0.7,padj=1)
axis(1,pretty(xRange),labels=format(pretty(xRange), format = "%d/%m"),cex.axis=0.7,padj=-1)

# Quarantine
lines(x=xRange[1]:xRange[2],y=stats_improve[[1]]$wdl*100,lty=2,col=2,lwd=1.5)

# Masks
lines(x=xRange[1]:xRange[2],y=stats_improve[[2]]$wdl*100,lty=3,col="dodgerblue",lwd=1.5)

# Masks + Quarantine
lines(x=xRange[1]:xRange[2],y=stats_improve[[3]]$wdl*100,lty=4,col="darkorange",lwd=1.5)

# Lockdown only
lines(x=xRange[1]:xRange[2],y=rep(baseline["wdl"],length(xRange[1]:xRange[2]))*100,lty=1,col="navy",lwd=1.5)

legend("topright","G",text.font=2,bty="n",cex=1)

# legend(-2,5,legend=c("Lockdown only","Quarantine","Masks","Quarantine+Masks"),
#        col=c(1,2,"navy","darkorange"),lty=1:4,bty="n",cex=0.8,lwd=1.5)



#Plot 4&8: % patient days without bed capacity
#-----------


# Set up plot axes
par(fig=c(0.75,1,0.5,1),new=T)
yRange = c(0, max(baseline["excess_beds"],stats_sd[[1]]$excess_beds,stats_sd[[2]]$excess_beds,stats_sd[[3]]$excess_beds)*100)
xRange = c(start_date+values_sd[1]+parms_baseline["ld_end"], start_date+values_sd[length(values_sd)]+parms_baseline["ld_end"])
plot(NA,xlim=xRange,ylim=yRange,xlab="",ylab="",bty="l",axes=F)
title(ylab="% patient days lacking beds", line=1.6,cex.lab=0.9)
title(xlab="New intervention start date", line=1.6,cex.lab=0.9)
graphics::box(bty="l")
axis(2,cex.axis=0.7,padj=1)
axis(1,at=c(as.Date("2020-05-02"),as.Date("2020-06-01"),as.Date("2020-07-01")),
     labels=format(c(as.Date("2020-05-02"),as.Date("2020-06-01"),as.Date("2020-07-01")), format = "%d/%m"),
     cex.axis=0.7,padj=-1)

# Quarantine
lines(x=xRange[1]:xRange[2],y=stats_sd[[1]]$excess_beds*100,lty=2,col=2,lwd=1.5)

# Masks
lines(x=xRange[1]:xRange[2],y=stats_sd[[2]]$excess_beds*100,lty=3,col="dodgerblue",lwd=1.5)

# Masks + Quarantine
lines(x=xRange[1]:xRange[2],y=stats_sd[[3]]$excess_beds*100,lty=4,col="darkorange",lwd=1.5)

# Lockdown only
lines(x=xRange[1]:xRange[2],y=rep(baseline["excess_beds"],length(xRange[1]:xRange[2]))*100,lty=1,col="navy",lwd=1.5)


lines(rep(as.Date("2020-06-01"), 100), (0:99)*yRange[2]/100, col="grey45", type = "l", lwd=1,lty=2)
text(as.Date("2020-06-01")+2, yRange[2]-yRange[2]/2, col="grey45", "Lockdown end",srt=90,cex=0.8)


legend("topright","D",text.font=2,bty="n",cex=1)


# Set up plot axes
par(fig=c(0.75,1,0,0.5),new=T)
yRange = c(0, max(baseline["excess_beds"],stats_improve[[1]]$excess_beds,stats_improve[[2]]$excess_beds,stats_improve[[3]]$excess_beds)*100)
xRange=c(min(values_improve),max(values_improve))
plot(NA,xlim=xRange,ylim=yRange,xlab="",ylab="",bty="l",axes=F)
title(ylab="% patient days lacking beds", line=1.6,cex.lab=0.9)
title(xlab="Days to scale up", line=1.6,cex.lab=0.9)
graphics::box(bty="l")
axis(2,cex.axis=0.7,padj=1)
axis(1,pretty(xRange),labels=format(pretty(xRange), format = "%d/%m"),cex.axis=0.7,padj=-1)

# Quarantine
lines(x=xRange[1]:xRange[2],y=stats_improve[[1]]$excess_beds*100,lty=2,col=2,lwd=1.5)

# Masks
lines(x=xRange[1]:xRange[2],y=stats_improve[[2]]$excess_beds*100,lty=3,col="dodgerblue",lwd=1.5)

# Masks + Quarantine
lines(x=xRange[1]:xRange[2],y=stats_improve[[3]]$excess_beds*100,lty=4,col="darkorange",lwd=1.5)

# Lockdown only
lines(x=xRange[1]:xRange[2],y=rep(baseline["excess_beds"],length(xRange[1]:xRange[2]))*100,lty=1,col="navy",lwd=1.5)


legend("topright","H",text.font=2,bty="n",cex=1)



## Close figure
dev.off()


