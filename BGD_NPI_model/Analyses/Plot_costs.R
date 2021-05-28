rm(list=ls())

require(deSolve)
require(data.table)
require(lubridate)

source("R/covid_model.R")
source("R/worker_days_lost.R")
source("R/amalgamate_cats.R")
source("R/calc_fractions.R")
source("R/Costs.R")


## Parameter ranges
par_ranges <- read.csv("data/ParameterRanges.csv",stringsAsFactors = F)

# Parameters for baseline scenario (with no interventions)
source("R/pars_baseline.R")

# Initial conditions
source("R/initial_conds.R")



# Gather statistics for different scenarios
# _______________________

# Scenario descriptions
scenarios <- c("No interventions",
               "Lockdown",
               "Lockdown +1month",
               "Lockdown +2months",
               "Lockdown +3months",
               "Lockdown + Quarantine",
               paste("Lockdown + Masks ",1:9,sep=""),
               paste("Lockdown + Quarantine + Masks ",1:9,sep=""))


# Set up data frame
stats <- data.frame(scenarios=scenarios,
                    deaths=NA,
                    cases=NA,
                    hosp=NA,
                    wdl=NA,
                    excess_beds=NA,
                    total_cost=NA,
                    int_cost=NA)


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
  
  # Costs
  costs <- costs(out,parms)
  
  # Statistics
  c(deaths=max(out$D),
    cases=max(out$CumCases),
    hosp=max(out$CumSevere),
    wdl=wdl,
    excess_beds=sum(pmax(0,(out$Hosp+out$ICU)-beds))/sum(out$Hosp+out$ICU),
    total_cost=costs$total,
    int_cost=sum(costs[c("lockdown_advertising","CST","mask")]))
  
}


# Run model with no interventions
parms<-parms_baseline
parms["ld"] <- F
stats[1,2:ncol(stats)] <- get_stats(parms)

# Run model with lockdown as implemented
parms<-parms_baseline
stats[2,2:ncol(stats)] <- get_stats(parms)

# Run model with lockdown extended by a month
parms<-parms_baseline
parms["ld_end"] <- parms["ld_end"]+30
stats[3,2:ncol(stats)] <- get_stats(parms)

# Run model with lockdown extended by two months
parms<-parms_baseline
parms["ld_end"] <- parms["ld_end"]+61
stats[4,2:ncol(stats)] <- get_stats(parms)

# Run model with lockdown extended by three months
parms<-parms_baseline
parms["ld_end"] <- parms["ld_end"]+92
stats[5,2:ncol(stats)] <- get_stats(parms)

# Run model with community surveillance
parms<-parms_baseline
parms["syndromic"] <- T
stats[6,2:ncol(stats)] <- get_stats(parms)

# Run model with low quality, poorly protective masks
parms<-parms_baseline
parms["mask"]<-T
parms["mask_effect_outward"] <- 0.2
parms["f_mask_effect_inward"] <- 0
stats[7,2:ncol(stats)] <- get_stats(parms)

# Run model with low quality, moderately protective masks
parms["mask_effect_outward"] <- 0.2
parms["f_mask_effect_inward"] <- 0.5
stats[8,2:ncol(stats)] <- get_stats(parms)

# Run model with low quality, highly protective masks
parms["mask_effect_outward"] <- 0.2
parms["f_mask_effect_inward"] <- 1
stats[9,2:ncol(stats)] <- get_stats(parms)

# Run model with medium quality, poorly protective masks
parms["mask_effect_outward"] <- 0.5
parms["f_mask_effect_inward"] <- 0
stats[10,2:ncol(stats)] <- get_stats(parms)

# Run model with medium quality, moderately protective masks
parms["mask_effect_outward"] <- 0.5
parms["f_mask_effect_inward"] <- 0.5
stats[11,2:ncol(stats)] <- get_stats(parms)

# Run model with medium quality, highly protective masks
parms["mask_effect_outward"] <- 0.5
parms["f_mask_effect_inward"] <- 1
stats[12,2:ncol(stats)] <- get_stats(parms)

# Run model with high quality, poorly protective masks
parms["mask_effect_outward"] <- 0.8
parms["f_mask_effect_inward"] <- 0
stats[13,2:ncol(stats)] <- get_stats(parms)

# Run model with high quality, moderately protective masks
parms["mask_effect_outward"] <- 0.8
parms["f_mask_effect_inward"] <- 0.5
stats[14,2:ncol(stats)] <- get_stats(parms)

# Run model with high quality, highly protective masks
parms["mask_effect_outward"] <- 0.8
parms["f_mask_effect_inward"] <- 1
stats[15,2:ncol(stats)] <- get_stats(parms)

# Run model with community surveillance & low quality, poorly protective masks
parms<-parms_baseline
parms["mask"]<-T
parms["syndromic"]<-T
parms["mask_effect_outward"] <- 0.2
parms["f_mask_effect_inward"] <- 0
stats[16,2:ncol(stats)] <- get_stats(parms)

# Run model with community surveillance & low quality, moderately protective masks
parms["mask_effect_outward"] <- 0.2
parms["f_mask_effect_inward"] <- 0.5
stats[17,2:ncol(stats)] <- get_stats(parms)

# Run model with community surveillance & low quality, highly protective masks
parms["mask_effect_outward"] <- 0.2
parms["f_mask_effect_inward"] <- 1
stats[18,2:ncol(stats)] <- get_stats(parms)

# Run model with community surveillance & medium quality, poorly protective masks
parms["mask_effect_outward"] <- 0.5
parms["f_mask_effect_inward"] <- 0
stats[19,2:ncol(stats)] <- get_stats(parms)

# Run model with community surveillance & medium quality, moderately protective masks
parms["mask_effect_outward"] <- 0.5
parms["f_mask_effect_inward"] <- 0.5
stats[20,2:ncol(stats)] <- get_stats(parms)

# Run model with community surveillance & medium quality, highly protective masks
parms["mask_effect_outward"] <- 0.5
parms["f_mask_effect_inward"] <- 1
stats[21,2:ncol(stats)] <- get_stats(parms)

# Run model with community surveillance & high quality, poorly protective masks
parms["mask_effect_outward"] <- 0.8
parms["f_mask_effect_inward"] <- 0
stats[22,2:ncol(stats)] <- get_stats(parms)

# Run model with community surveillance & high quality, moderately protective masks
parms["mask_effect_outward"] <- 0.8
parms["f_mask_effect_inward"] <- 0.5
stats[23,2:ncol(stats)] <- get_stats(parms)

# Run model with community surveillance & high quality, highly protective masks
parms["mask_effect_outward"] <- 0.8
parms["f_mask_effect_inward"] <- 1
stats[24,2:ncol(stats)] <- get_stats(parms)

# Cost/death averted
deaths_averted <- stats$deaths[1] - stats$deaths
cost_diff <- stats$total_cost - stats$total_cost[1]
stats$cost_death_averted <- cost_diff/deaths_averted

# % ROI
stats$ROI <- -100*cost_diff/stats$int_cost




# Figure
# _______________________

# Open figure
tiff(filename="Figs/Costs.tiff",width=160,height=85,units="mm",pointsize=12,res=200)
par(mar=c(1.8,1.7,0.4,1))



#Plot 1: Total costs (high cost scenario)
#-----------

par(fig=c(0,0.33,0.3,1))
yRange = c(0, max(stats$total_cost)/1000000)
xRange = c(1, 5)
plot(NA,xlim=xRange,ylim=yRange,xlab="",ylab="",bty="l",axes=F)
title(ylab="Total cost ($1,000,000)", line=0.9,cex.lab=1)
graphics::box(bty="l")
axis(2,cex.axis=0.7,padj=1.9,tcl=-0.25)
axis(1,at=c(1:5),labels=c("NI","L","L+Q","L+M","L+Q+M"),cex.axis=0.64,padj=-2,tcl=-0.25)

points(x=c(1,rep(2,4),3,rep(4,9),rep(5,9)),y=c(stats$total_cost)/1000000,
       pch=c(16,1:4,1,rep(1:3,3),rep(1:3,3)),col=c(1,colorRampPalette(c(1,4,"white"))(6)[2:5],"red3",rep(c(rep("dodgerblue",3),rep("darkorange",3),rep(2,3)),2)))

legend(xRange[2]-1,yRange[2]*1.09,"A",text.font=2,bty="n",cex=1)



#Plot 2: Cost/death averted (high cost scenario)
#-----------

par(fig=c(0.33,0.66,0.3,1),new=T)
yRange = c(min(stats$cost_death_averted[2:nrow(stats)]), max(stats$cost_death_averted[2:nrow(stats)]))/1000
xRange = c(1, 4)
plot(NA,xlim=xRange,ylim=yRange,xlab="",ylab="",bty="l",axes=F)
title(ylab="Cost/death averted ($1,000)", line=0.9,cex.lab=0.9)
graphics::box(bty="l")
axis(2,cex.axis=0.7,padj=1.9,tcl=-0.25)
axis(1,at=c(1:4),labels=c("L","L+Q","L+M","L+Q+M"),cex.axis=0.64,padj=-2,tcl=-0.25)

points(x=c(rep(1,4),2,rep(3,9),rep(4,9)),y=c(stats$cost_death_averted[2:nrow(stats)])/1000,
       pch=c(1:4,1,rep(1:3,3),rep(1:3,3)),col=c(colorRampPalette(c(1,4,"white"))(6)[2:5],"red3",rep(c(rep("dodgerblue",3),rep("darkorange",3),rep(2,3)),2)))

legend(xRange[2]-0.8,yRange[2]*1.11,"B",text.font=2,bty="n",cex=1)



#Plot 3: % Return on investment
#-----------

par(fig=c(0.66,1,0.3,1),new=T)
yRange = c(min(stats$ROI[2:nrow(stats)]), max(stats$ROI[2:nrow(stats)]))
xRange = c(1, 4)
plot(NA,xlim=xRange,ylim=yRange,xlab="",ylab="",bty="l",axes=F)
title(ylab="% Return on investment", line=0.9,cex.lab=0.9)
graphics::box(bty="l")
axis(2,cex.axis=0.7,padj=1.9,at=c(0,5000,10000,15000),tcl=-0.25)
axis(1,at=c(1:4),labels=c("L","L+Q","L+M","L+Q+M"),cex.axis=0.64,padj=-2,tcl=-0.25)

points(x=c(rep(1,4),2,rep(3,9),rep(4,9)),y=stats$ROI[2:nrow(stats)],
       pch=c(1:4,1,rep(1:3,3),rep(1:3,3)),col=c(colorRampPalette(c(1,4,"white"))(6)[2:5],"red3",rep(c(rep("dodgerblue",3),rep("darkorange",3),rep(2,3)),2)))

legend(xRange[2]-0.8,yRange[2]*1.1,"C",text.font=2,bty="n",cex=1)





# Legends
#-------------

par(fig=c(0,1,0,0.3),new=T)
par(mar=c(0,0,0,0))
plot(NA,axes=F,xlab="",ylab="",xlim=c(0,1),ylim=c(0,1))


legend(0.18,1.06,
       legend=c("as implemented","+1month","+2months","+3months"),
       col=colorRampPalette(c(1,4,"white"))(6)[2:5],pch=1:4,bty="n",title="Lockdown options (L)",title.adj = 0,cex=0.8,pt.cex = 1)
legend(0.45,1.06,
       legend=as.expression(lapply(c(0.2,0.5,0.8), function(d) {bquote(italic(epsilon^m)==.(d))} )),
       col=c("dodgerblue","darkorange",2),lty=1,bty="n",title="Mask options (L+M/L+Q+M)",title.adj = 5,cex=0.8,pt.cex = 1)
legend(0.72,1.06,
       legend=as.expression(lapply(c(0,0.5,1), function(d) {bquote(italic(rho^m)==.(d))} )),
       pch=1:3,bty="n",xjust=0,title=" ",cex=0.8,pt.cex = 1)



## Close figure
dev.off()
