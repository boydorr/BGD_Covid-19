#
# Plot tornado plots using sensitivity.R outputs
#___________________________

rm(list=ls())

# Which baseline scenario to use?
lockdown <- F
community_surveillance <- F
masks <- F

# Load ouputs
load(paste("Output/Sensitivity_lockdown",lockdown,
           ifelse(community_surveillance==T,"_community",""),
           ifelse(masks==T,"_masks",""),
           ".Rdata",sep=""))

# Source plotting function
source("R/tornado_plot.R")


# Open figure
tiff(filename=paste("Figs/Sensitivity_lockdown",lockdown,
                    ifelse(community_surveillance==T,"_community",""),
                    ifelse(masks==T,"_masks",""),".tiff",sep=""),
     width=160,height=230,units="mm",pointsize=12,res=200)
par(mfrow=c(3,2))
par(mar=c(4,4,1,1.1))


# Parameter labels
par_names <- c(expression(italic(R)[0]),
               expression(italic(t^intro)),
               expression(italic(d^E)),
               expression(italic(d^a)),
               expression(italic(d^p)),
               expression(italic(d^s)),
               expression(italic(d^H[wait])),
               expression(italic(d^D[wait])),
               expression(italic(d^H)),
               expression(italic(d^ICU)),
               expression(italic(f^pt)),
               expression(italic(f^at)),
               expression(italic(sigma)))

# Deaths
tornado_plot(out_stats$perc_change$high_deaths,out_stats$perc_change$low_deaths,
             xlab = "% change in deaths",par_names = par_names)
legend("bottomright","A",text.font=2,bty="n",cex=1.4)

# Hospitalisations
tornado_plot(out_stats$perc_change$high_hosp,out_stats$perc_change$low_hosp,
             xlab = "% change in hospitalisations",par_names = par_names)
legend("bottomright","B",text.font=2,bty="n",cex=1.4)

# Cases
tornado_plot(out_stats$perc_change$high_cases,out_stats$perc_change$low_cases,
             xlab = "% change in cases",par_names = par_names)
legend("bottomright","C",text.font=2,bty="n",cex=1.4)

# Working days lost
tornado_plot(out_stats$perc_change$high_wdl,out_stats$perc_change$low_wdl,
             xlab = "% change in working days lost",par_names = par_names)
legend("bottomright","D",text.font=2,bty="n",cex=1.4)

# Excess beds required
tornado_plot(out_stats$perc_change$high_excess_beds,out_stats$perc_change$low_excess_beds,
             xlab = "% change in patients lacking beds",par_names = par_names)
legend("bottomright","E",text.font=2,bty="n",cex=1.4)

dev.off()
