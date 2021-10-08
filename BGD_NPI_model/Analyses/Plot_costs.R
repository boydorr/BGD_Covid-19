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
                    int_cost=NA,
                    hosp_cost=NA)


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
    int_cost=sum(costs[c("lockdown_advertising","CST","mask")]),
    hosp_cost=costs["healthcare"])
  
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
stats$deaths_averted <- stats$deaths[1] - stats$deaths
stats$cost_diff <- stats$total_cost - stats$total_cost[1]
stats$cost_death_averted <- stats$cost_diff/stats$deaths_averted

# % ROI
stats$ROI <- -100*stats$cost_diff/stats$int_cost




# Figure
# _______________________

# Open figure
tiff(filename="Figs/Costs.tiff",width=160,height=140,units="mm",pointsize=12,res=350)
par(mar=c(1.8,1.7,0.5,1))



#Plot 1: Intervention costs
#-----------

par(fig=c(0,0.5,0.5,1))
yRange = c(0, max(stats$int_cost)/1000000)
xRange = c(0.9, 5.1)
plot(NA,xlim=xRange,ylim=yRange,xlab="",ylab="",bty="l",axes=F)
title(ylab="Implementation cost ($1,000,000)", line=0.9,cex.lab=1)
graphics::box(bty="l")
axis(2,cex.axis=0.7,padj=1.9,tcl=-0.25)
axis(1,at=c(1:5),labels=c("NI","L","L+Q","L+M","L+Q+M"),cex.axis=0.64,padj=-2,tcl=-0.25)

points(x=c(1,3,4,5),y=stats$int_cost[c(1,6,7,16)]/1000000,lwd=1.5,cex=0.8,
       pch=c(16,16),col=1)
arrows(c(2), stats$int_cost[c(2)]/1000000, 
       c(2), stats$int_cost[c(5)]/1000000, 
       length=0.03, angle=90, code=3,col=c(1))


# points(x=c(1,rep(2,4),3,rep(4,9),rep(5,9)),y=c(stats$total_cost)/1000000,
#        pch=c(16,1:4,1,rep(1:3,3),rep(1:3,3)),col=c(1,colorRampPalette(c(1,4,"white"))(6)[2:5],"red3",rep(c(rep("dodgerblue",3),rep("darkorange",3),rep(2,3)),2)))

legend(xRange[2]-1.1,yRange[2]*1.09,"A",text.font=2,bty="n",cex=1)



#Plot 2: Total costs 
#-----------

par(fig=c(0.5,1,0.5,1),new=T)
yRange = c(0, max(stats$total_cost)/1000000)
xRange = c(0.9, 5.1)
plot(NA,xlim=xRange,ylim=yRange,xlab="",ylab="",bty="l",axes=F)
title(ylab="Total cost ($1,000,000)", line=0.9,cex.lab=1)
graphics::box(bty="l")
axis(2,cex.axis=0.7,padj=1.9,tcl=-0.25)
axis(1,at=c(1:5),labels=c("NI","L","L+Q","L+M","L+Q+M"),cex.axis=0.64,padj=-2,tcl=-0.25)

points(x=c(1,3),y=stats$total_cost[c(1,6)]/1000000,lwd=1.5,cex=0.8,
       pch=c(16,16),col=1)
arrows(c(2,rep(4,3)+c(-0.15,0,0.15),rep(5,3)+c(-0.15,0,0.15)), stats$total_cost[c(2,7,10,13,16,19,22)]/1000000, 
       c(2,rep(4,3)+c(-0.15,0,0.15),rep(5,3)+c(-0.15,0,0.15)), stats$total_cost[c(5,9,12,15,18,21,24)]/1000000, 
       length=0.03, angle=90, code=3,col=c(1,rep(c("dodgerblue","darkorange","red"),2)))


# points(x=c(1,rep(2,4),3,rep(4,9),rep(5,9)),y=c(stats$total_cost)/1000000,
#        pch=c(16,1:4,1,rep(1:3,3),rep(1:3,3)),col=c(1,colorRampPalette(c(1,4,"white"))(6)[2:5],"red3",rep(c(rep("dodgerblue",3),rep("darkorange",3),rep(2,3)),2)))

legend(xRange[2]-1.1,yRange[2]*1.09,"B",text.font=2,bty="n",cex=1)



#Plot 2: Cost/death averted 
#-----------

par(fig=c(0,0.5,0,0.5),new=T)
yRange = c(min(stats$cost_death_averted[2:nrow(stats)]), max(stats$cost_death_averted[2:nrow(stats)]))/1000
xRange = c(0.9, 4.1)
plot(NA,xlim=xRange,ylim=yRange,xlab="",ylab="",bty="l",axes=F)
title(ylab="Cost/death averted ($1,000)", line=0.9,cex.lab=0.9)
graphics::box(bty="l")
axis(2,cex.axis=0.7,padj=1.9,tcl=-0.25)
axis(1,at=c(1:4),labels=c("L","L+Q","L+M","L+Q+M"),cex.axis=0.64,padj=-2,tcl=-0.25)

points(x=c(2),y=stats$cost_death_averted[c(6)]/1000,lwd=1.5,cex=0.8,
       pch=c(16,16),col=1)
arrows(c(1,rep(3,3)+c(-0.15,0,0.15),rep(4,3)+c(-0.15,0,0.15)), stats$cost_death_averted[c(2,7,10,13,16,19,22)]/1000, 
       c(1,rep(3,3)+c(-0.15,0,0.15),rep(4,3)+c(-0.15,0,0.15)), stats$cost_death_averted[c(5,9,12,15,18,21,24)]/1000, 
       length=0.03, angle=90, code=3,col=c(1,rep(c("dodgerblue","darkorange","red"),2)))


# points(x=c(rep(1,4),2,rep(3,9),rep(4,9)),y=c(stats$cost_death_averted[2:nrow(stats)])/1000,
#        pch=c(1:4,1,rep(1:3,3),rep(1:3,3)),col=c(colorRampPalette(c(1,4,"white"))(6)[2:5],"red3",rep(c(rep("dodgerblue",3),rep("darkorange",3),rep(2,3)),2)))

legend(xRange[2]-0.8,yRange[2]*1.11,"C",text.font=2,bty="n",cex=1)



#Plot 3: % Return on investment
#-----------

par(fig=c(0.5,1,0,0.5),new=T)
yRange = c(min(stats$ROI[2:nrow(stats)]), max(stats$ROI[2:nrow(stats)]))
xRange = c(0.9, 4.1)
plot(NA,xlim=xRange,ylim=yRange,xlab="",ylab="",bty="l",axes=F)
title(ylab="% Return on investment", line=0.9,cex.lab=0.9)
graphics::box(bty="l")
axis(2,cex.axis=0.7,padj=1.9,at=c(0,1000,2000,3000),tcl=-0.25)
axis(1,at=c(1:4),labels=c("L","L+Q","L+M","L+Q+M"),cex.axis=0.64,padj=-2,tcl=-0.25)

points(x=c(2),y=stats$ROI[c(6)],lwd=1.5,cex=0.8,
       pch=c(16,16),col=1)
arrows(c(1,rep(3,3)+c(-0.15,0,0.15),rep(4,3)+c(-0.15,0,0.15)), stats$ROI[c(2,7,10,13,16,19,22)], 
       c(1,rep(3,3)+c(-0.15,0,0.15),rep(4,3)+c(-0.15,0,0.15)), stats$ROI[c(5,9,12,15,18,21,24)], 
       length=0.03, angle=90, code=3,col=c(1,rep(c("dodgerblue","darkorange","red"),2)))


# points(x=c(rep(1,4),2,rep(3,9),rep(4,9)),y=stats$ROI[2:nrow(stats)],
#        pch=c(1:4,1,rep(1:3,3),rep(1:3,3)),col=c(colorRampPalette(c(1,4,"white"))(6)[2:5],"red3",rep(c(rep("dodgerblue",3),rep("darkorange",3),rep(2,3)),2)))

legend(xRange[2]-0.8,yRange[2]*1.1,"D",text.font=2,bty="n",cex=1)
legend(1.03,2000,
       legend=as.expression(mapply(function(c,d) {bquote(.(c)~ (italic(epsilon^m)==.(d)))},c("low","medium","high"),c(0.2,0.5,0.8) )),
       col=c("dodgerblue","darkorange",2),lty=1,bty="n",title="Mask (M) filtration efficiency:",
       cex=0.7,lwd=1.5,pt.cex = 1)





## Close figure
dev.off()
