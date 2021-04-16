#
# Creates a tornado plot
#______________

# Inputs
#  - max_perc_change = the maximum percentage change in the statistic of interest for each parameter
#  - min_perc_change = the minimum percentage change in the statistic of interest for each parameter
#  - xlab = the x-axis label
#  - par_names = the parameter names to be displayed on the y-axis 
#  - col = colours (or colour) to fill the bars
#  - max_perc_change_pars = the parameter values associated with the max

# Deaths
tornado_plot <- function(max_perc_change,min_perc_change,xlab,par_names,col=c("dodgerblue","gold")){
  order<-(order(max_perc_change-min_perc_change))
  xlim <- c(-max(max_perc_change,-min_perc_change),max(max_perc_change,-min_perc_change)) 
  barplot(max_perc_change[order], horiz = T, las=1, xlim = xlim, xaxt='n', ylab = '',names=par_names[order],
          beside=T, col=col[1],xlab="",cex.names=1.2)
  title(xlab=xlab, line=2.4,cex.lab=1.3)
  barplot(min_perc_change[order], horiz = T, las=1, xlim = xlim, xaxt='n', ylab = '',
          beside=T, col=col[length(col)], add = TRUE)
  axis(1, at=pretty(xlim), las=TRUE)
  
}
