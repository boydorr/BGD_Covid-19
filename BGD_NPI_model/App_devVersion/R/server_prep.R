#
## Code to create non-reactive objects required by the server
#_________________________________


# Pre-introduction dataframe to bind to model outputs
preIntro <- data.frame(time=0:(min(times_model)-1))
preIntro <- cbind(preIntro, matrix(0, ncol=length(y), nrow=nrow(preIntro), dimnames = list(NULL,names(y))))
preIntro$S_n<-parms_baseline["population"]

# Dates for plotting
date_ticks <- as.numeric(seq(start_date,end_date+1,by="month") - start_date)
date_labels <- paste(month.abb[month(seq(start_date,end_date+1,by="month"))], (year(seq(start_date,end_date,by="month")))-2000,sep="")

# Alternative start dates for March (for April, replace 61 with 91)
# date_ticks <- as.numeric(seq((start_date+61),end_date,by="month") - start_date)
# date_labels <- paste(month.abb[month(seq((start_date+61),end_date,by="month"))], (year(seq(start_date+61,end_date,by="month")))-2000,sep="")

date_ticks2 <- as.numeric(seq(start_date+90,end_date+1,by="month") - start_date)
date_labels2 <- paste(month.abb[month(seq(start_date+90,end_date+1,by="month"))], (year(seq(start_date,end_date,by="month")))-2000,sep="")

