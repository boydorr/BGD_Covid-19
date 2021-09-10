rm(list=ls())

require(deSolve)
require(data.table)
require(lubridate)

source("R/covid_model.R")
source("R/worker_days_lost.R")
source("R/amalgamate_cats.R")
source("R/calc_fractions.R")
source("R/bangladesh_covid_data.R")


# Parameters for baseline scenario 
source("R/pars_baseline.R")

# Initial conditions
source("R/initial_conds.R")

# What to plot? (Patients, Daily new cases or Total deaths)
y_axis <- "Daily new cases"
if(y_axis=="Patients"){
        plot_func <- function(out){out$Hosp+out$ICU}
        ylim <- 46000
}else if(y_axis=="Daily new cases"){
        plot_func <- function(out){c(0,diff(out$CumCases))}
        ylim <- 470000
}else if(y_axis=="Total deaths"){
        plot_func <- function(out){out$D}
        ylim <- 38000
}

## Proportion cases in Dhaka (from the dashboard)
district_cases <- read.csv("data/district_cases_dashboard_19.11.csv")
propDhaka <- district_cases$X.[which(district_cases$Distirct=="Dhaka")]/sum(district_cases$X.)





# Time series 
# _______________________

# Open figure
tiff(filename=paste("Figs/",y_axis,"_ts.tiff",sep=""),
     width=160,height=200,units="mm",pointsize=12,res=250)
par(mar=c(4,4,1,1.1))

# Dates for plotting
date_ticks <- as.numeric(seq(start_date,end_date+1,by="month") - start_date)
date_labels <- paste(month.abb[month(seq(start_date,end_date+1,by="month"))], (year(seq(start_date,end_date+1,by="month")))-2000,sep="")

# Pre-introduction matrix 
preIntro <- data.frame(time=0:(min(times_model)-1))
preIntro <- cbind(preIntro, matrix(0, ncol=length(y), nrow=nrow(preIntro), dimnames = list(NULL,names(y))))
preIntro$S_n<-parms_baseline["population"]




# Plot 1: No intervention, lockdowns of different lengths, community surveillance
#---------------

# Set up plot
par(mar=c(0,3.3,1,0.25))
par(fig=c(0,1,0.695,1))
plot(NA, xlim=c(0,max(times)),ylim=c(0,ylim),axes=F,xlab="",ylab="")
graphics::box(bty="l")
title(ylab=y_axis, line=2.3, cex.lab=1)
# title(xlab="Date", line=2.3,cex.lab=1.1)
if(y_axis!="Daily new cases"){
        y_ticks <- pretty(c(0,ylim))
}else{
        y_ticks <- c(0,expression("1x10"^5),expression("2x10"^5),expression("3x10"^5),expression("4x10"^5),expression("5x10"^5))
}
axis(2,cex.axis=0.8,at=pretty(c(0,ylim)),labels=y_ticks)
# axis(1,at=date_ticks,labels=date_labels,cex.axis=0.8)
for(i in 1:length(date_labels)){
        if(i%%2==1){
                polygon(c(date_ticks[i],date_ticks[i+1],date_ticks[i+1],date_ticks[i]),
                        c(0,0,ylim*1.2,ylim*1.2),
                        col="grey95",border="grey95")
        }
}


if(y_axis=="Patients"){
        # Add horizontal lines and text for beds
        abline(h=beds,lty=2,col="grey45")
        # abline(h=iso_beds,lty=2,col="grey45")
        text(0, beds+1500, col="grey45", "Hospital beds",cex=0.9,adj=0)
        # text(0, iso_beds+1500, col="grey45", "Hospital isolation beds",cex=0.9,adj=0)
}


# Vertical lines for lockdown
lines(rep(as.Date("2020-03-26")-start_date, 100), (0:99)*ylim/100, col="grey45", type = "l", lwd=1,lty=2)
text(as.Date("2020-03-26")-start_date+5, ifelse(y_axis=="Patients",ylim*0.65,ylim*0.72), col="grey45", "Lockdown start",srt=90,cex=0.9)
lines(rep(as.Date("2020-06-01")-start_date, 100), (0:99)*ylim/100, col="grey45", type = "l", lwd=1,lty=2)
text(as.Date("2020-06-01")-start_date+5, ifelse(y_axis=="Patients",ylim*0.65,ylim*0.72), col="grey45", "Lockdown end",srt=90,cex=0.9)


# Colours
cols <- colorRampPalette(c(1,4,"white"))(6)


# Run model with no interventions
parms<-parms_baseline
parms["ld"] <- F
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[1],lty=1,lwd=1.5)

# Run model with lockdown as implemented
parms<-parms_baseline
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[2],lty=2,lwd=1.5)

# Run model with lockdown extended by a month
parms<-parms_baseline
parms["ld_end"] <- parms["ld_end"]+30
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[3],lty=3,lwd=1.5)

# Run model with lockdown extended by two months
parms<-parms_baseline
parms["ld_end"] <- parms["ld_end"]+61
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[4],lty=4,lwd=1.5)

# Run model with lockdown extended by three months
parms<-parms_baseline
parms["ld_end"] <- parms["ld_end"]+92
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[5],lty=5,lwd=1.5)

# Run model with community surveillance
parms<-parms_baseline
parms["syndromic"] <- T
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col="red3",lty=6,lwd=1.5)

# legend
if(y_axis!="Total deaths"){
        legend_y <- 1.035*ylim;legend_x <- 235
        legend(legend_x,legend_y,
               legend=c("No intervention","Lockdown","Lockdown +1month","Lockdown +2months","Lockdown +3months",
                        "Lockdown + Quarantine"),
               col=c(cols[1:5],"red3"),lty=c(1:6),bty="n",cex=0.85,xjust=0,lwd=rep(1.5,6))
}else{
        legend_y<- 0.63*ylim;legend_x<--17
        legend(legend_x,legend_y,
               legend=c("No intervention","Lockdown","Lockdown +1month","Lockdown +2months","Lockdown +3months",
                        "Lockdown + Quarantine","Data"),
               col=c(cols[1:5],"red3","grey50"),lty=c(1:6),bty="n",cex=0.85,xjust=0,lwd=c(rep(1.5,6),2))
}
legend("topleft","A",text.font=2,bty="n",cex=1.1)

# Data cases is too low to bother adding...
# if(y_axis=="Daily new cases"){
#         lines(bangladesh$date-start_date,bangladesh$cases*propDhaka,col="grey50",lty=1,lwd=1.5)
# }
if(y_axis=="Total deaths"){
        lines(bangladesh$date-start_date,bangladesh$cumulative_death*propDhaka,col="grey50",lty=1,lwd=2)
}




# Plot 2: Different mask effectivenesses
#---------------


# Set up plot
par(mar=c(0,3.3,1,0.25))
par(fig=c(0,1,0.39,0.695),new=T)
plot(NA, xlim=c(0,max(times)),ylim=c(0,ylim),axes=F,xlab="",ylab="")
graphics::box(bty="l")
title(ylab=y_axis, line=2.3,cex.lab=1)
# title(xlab="Date", line=2.3,cex.lab=1.1)
axis(2,cex.axis=0.8,at=pretty(c(0,ylim)),labels=y_ticks)
# axis(1,at=date_ticks,labels=date_labels,cex.axis=0.8)
for(i in 1:length(date_labels)){
        if(i%%2==1){
                polygon(c(date_ticks[i],date_ticks[i+1],date_ticks[i+1],date_ticks[i]),
                        c(0,0,ylim*1.2,ylim*1.2),
                        col="grey95",border="grey95")
        }
}


if(y_axis=="Patients"){
        # Add horizontal lines and text for beds
        abline(h=beds,lty=2,col="grey45")
        # abline(h=iso_beds,lty=2,col="grey45")
        text(0, beds+1500, col="grey45", "Hospital beds",cex=0.9,adj=0)
        # text(0, iso_beds+1500, col="grey45", "Hospital isolation beds",cex=0.9,adj=0)
}


# Vertical lines for lockdown
lines(rep(as.Date("2020-03-26")-start_date, 100), (0:99)*ylim/100, col="grey45", type = "l", lwd=1,lty=2)
text(as.Date("2020-03-26")-start_date+5, ifelse(y_axis=="Patients",ylim*0.65,ylim*0.72), col="grey45", "Lockdown start",srt=90,cex=0.9)
lines(rep(as.Date("2020-06-01")-start_date, 100), (0:99)*ylim/100, col="grey45", type = "l", lwd=1,lty=2)
text(as.Date("2020-06-01")-start_date+5, ifelse(y_axis=="Patients",ylim*0.65,ylim*0.72), col="grey45", "Lockdown end",srt=90,cex=0.9)


# Colours
cols <- colorRampPalette(c(1,4,"white"))(6)


# Run model with no interventions
parms<-parms_baseline
parms["ld"] <- F
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[1],lty=1,lwd=1.5)

# Run model with lockdown as implemented
parms<-parms_baseline
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[2],lty=2,lwd=1.5)



# Colours
cols <- c(colorRampPalette(c("navy","white"))(4)[1:3],
          colorRampPalette(c("darkorange","white"))(4)[1:3],
          colorRampPalette(c("red","white"))(4)[1:3])
cols <- c("dodgerblue","darkorange","red")

# Run model with low quality, poorly protective masks
parms<-parms_baseline
parms["mask"]<-T
parms["mask_effect_outward"] <- 0.2
parms["f_mask_effect_inward"] <- 0
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[1],lty=1,lwd=1.5)

# Run model with low quality, moderately protective masks
parms["mask_effect_outward"] <- 0.2
parms["f_mask_effect_inward"] <- 0.5
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[1],lty=2,lwd=1.5)

# Run model with low quality, highly protective masks
parms["mask_effect_outward"] <- 0.2
parms["f_mask_effect_inward"] <- 1
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[1],lty=3,lwd=1.5)

# Run model with medium quality, poorly protective masks
parms["mask_effect_outward"] <- 0.5
parms["f_mask_effect_inward"] <- 0
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[2],lty=1,lwd=1.5)

# Run model with medium quality, moderately protective masks
parms["mask_effect_outward"] <- 0.5
parms["f_mask_effect_inward"] <- 0.5
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[2],lty=2,lwd=1.5)

# Run model with high quality, poorly protective masks
parms["mask_effect_outward"] <- 0.8
parms["f_mask_effect_inward"] <- 0
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[3],lty=1,lwd=1.5)

# Run model with medium quality, highly protective masks
parms["mask_effect_outward"] <- 0.5
parms["f_mask_effect_inward"] <- 1
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[2],lty=3,lwd=1.5)

# Run model with high quality, moderately protective masks
parms["mask_effect_outward"] <- 0.8
parms["f_mask_effect_inward"] <- 0.5
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[3],lty=2,lwd=1.5)

# Run model with high quality, highly protective masks
parms["mask_effect_outward"] <- 0.8
parms["f_mask_effect_inward"] <- 1
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[3],lty=3,lwd=1.5)



# legend
if(y_axis!="Total deaths"){legend_y <- ylim;legend_x <- 210
}else{legend_y<- 0.4*ylim;legend_x<--20}
legend(legend_x,legend_y,
       legend=as.expression(lapply(c(0.2,0.5,0.8), function(d) {bquote(italic(epsilon^m)==.(d))} )),
       col=cols[1:3],lty=1,bty="n",xjust=0,title="Lockdown + Masks",title.adj = 3,cex=0.85,lwd=1.5)
legend(legend_x+85,legend_y,
       legend=as.expression(lapply(c(0,0.5,1), function(d) {bquote(italic(rho^m)==.(d))} )),
       col=1,lty=1:3,bty="n",xjust=0,title=" ",cex=0.85,lwd=1.5)
legend("topleft","B",text.font=2,bty="n",cex=1.1)

# if(y_axis=="Daily new cases"){
#         lines(bangladesh$date-start_date,bangladesh$cases*propDhaka,col="grey50",lty=1,lwd=1.5)
# }
if(y_axis=="Total deaths"){
        lines(bangladesh$date-start_date,bangladesh$cumulative_death*propDhaka,col="grey50",lty=1,lwd=2)
}



# Plot 3: Combined community surveillance and masks
#---------------

# Set up plot
par(mar=c(3.5,3.3,1,0.25))
par(fig=c(0,1,0,0.39),new=T)
plot(NA, xlim=c(0,max(times)),ylim=c(0,ylim),axes=F,xlab="",ylab="")
graphics::box(bty="l")
title(ylab=y_axis, line=2.3,cex.lab=1)
title(xlab="Date", line=2.3,cex.lab=1)
axis(2,cex.axis=0.8,at=pretty(c(0,ylim)),labels=y_ticks)
axis(1,at=date_ticks,labels=date_labels,cex.axis=0.8)
for(i in 1:length(date_labels)){
        if(i%%2==1){
                polygon(c(date_ticks[i],date_ticks[i+1],date_ticks[i+1],date_ticks[i]),
                        c(0,0,ylim*1.2,ylim*1.2),
                        col="grey95",border="grey95")
        }
}


if(y_axis=="Patients"){
        # Add horizontal lines and text for beds
        abline(h=beds,lty=2,col="grey45")
        # abline(h=iso_beds,lty=2,col="grey45")
        text(0, beds+1500, col="grey45", "Hospital beds",cex=0.9,adj=0)
        # text(0, iso_beds+750, col="grey45", "Hospital isolation beds",cex=0.9,adj=0)
}


# Vertical lines for lockdown
lines(rep(as.Date("2020-03-26")-start_date, 100), (0:99)*ylim/100, col="grey45", type = "l", lwd=1,lty=2)
text(as.Date("2020-03-26")-start_date+5, ifelse(y_axis=="Patients",ylim*0.65,ylim*0.72), col="grey45", "Lockdown start",srt=90,cex=0.9)
lines(rep(as.Date("2020-06-01")-start_date, 100), (0:99)*ylim/100, col="grey45", type = "l", lwd=1,lty=2)
text(as.Date("2020-06-01")-start_date+5, ifelse(y_axis=="Patients",ylim*0.65,ylim*0.72), col="grey45", "Lockdown end",srt=90,cex=0.9)



# Colours
cols <- colorRampPalette(c(1,4,"white"))(6)


# Run model with no interventions
parms<-parms_baseline
parms["ld"] <- F
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[1],lty=1,lwd=1.5)

# Run model with lockdown as implemented
parms<-parms_baseline
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[2],lty=2,lwd=1.5)


# Colours
cols <- c(colorRampPalette(c("navy","white"))(4)[1:3],
          colorRampPalette(c("darkorange","white"))(4)[1:3],
          colorRampPalette(c("red","white"))(4)[1:3])
cols <- c("dodgerblue","darkorange","red")

# Run model with low quality, poorly protective masks
parms<-parms_baseline
parms["syndromic"] <- T
parms["mask"] <- T
parms["mask_effect_outward"] <- 0.2
parms["f_mask_effect_inward"] <- 0
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[1],lty=1,lwd=1.5)

# Run model with low quality, moderately protective masks
parms["mask_effect_outward"] <- 0.2
parms["f_mask_effect_inward"] <- 0.5
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[1],lty=2,lwd=1.5)

# Run model with low quality, highly protective masks
parms["mask_effect_outward"] <- 0.2
parms["f_mask_effect_inward"] <- 1
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[1],lty=3,lwd=1.5)

# Run model with medium quality, poorly protective masks
parms["mask_effect_outward"] <- 0.5
parms["f_mask_effect_inward"] <- 0
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[2],lty=1,lwd=1.5)

# Run model with medium quality, moderately protective masks
parms["mask_effect_outward"] <- 0.5
parms["f_mask_effect_inward"] <- 0.5
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[2],lty=2)


# Run model with high quality, poorly protective masks
parms["mask_effect_outward"] <- 0.8
parms["f_mask_effect_inward"] <- 0
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[3],lty=1,lwd=1.5)

# Run model with medium quality, highly protective masks
parms["mask_effect_outward"] <- 0.5
parms["f_mask_effect_inward"] <- 1
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[2],lty=3,lwd=1.5)


# Run model with high quality, moderately protective masks
parms["mask_effect_outward"] <- 0.8
parms["f_mask_effect_inward"] <- 0.5
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[3],lty=2,lwd=1.5)

# Run model with high quality, highly protective masks
parms["mask_effect_outward"] <- 0.8
parms["f_mask_effect_inward"] <- 1
out <- amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms)))) 
lines(c(plot_func(out))~times,col=cols[3],lty=3,lwd=1.5)



# legend
if(y_axis!="Total deaths"){legend_y <- ylim;legend_x <- 210
}else{legend_y<- 0.2*ylim;legend_x<--25}
legend(legend_x,legend_y,
       legend="Lockdown + Quarantine + Masks",cex=0.85,bty="n")
# legend(205,22000,
#        legend=as.expression(lapply(c(0.2,0.5,0.8), function(d) {bquote(italic(epsilon^m)==.(d))} )),
#        col=cols[1:3],lty=1,bty="n",xjust=0,title="Lockdown + Quarantine + Masks",title.adj = 10,cex=1.1)
# legend(305,22000,
#        legend=as.expression(lapply(c(0,0.5,1), function(d) {bquote(italic(rho^m)==.(d))} )),
#        col=1,lty=1:3,bty="n",xjust=0,title=" ",cex=1.1)
legend("topleft","C",text.font=2,bty="n",cex=1.1)

# if(y_axis=="Daily new cases"){
#         lines(bangladesh$date-start_date,bangladesh$cases*propDhaka,col="grey50",lty=1,lwd=1.5)
# }
if(y_axis=="Total deaths"){
        lines(bangladesh$date-start_date,bangladesh$cumulative_death*propDhaka,col="grey50",lty=1,lwd=2)
}


# End Fig
dev.off()

