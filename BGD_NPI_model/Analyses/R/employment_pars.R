

##Bangladesh
bdeshpop2011 <- 144043697
working <- 40995005 
employed <- (working - 1733030)/bdeshpop2011
household <- 1733030/bdeshpop2011

##Dhaka District
dhaka_pop_by_age <- read.csv("data/dhaka_dist_pop_by_age_census2011_10yearBands.csv")
employed_dhaka <- 5140759/sum(dhaka_pop_by_age$pop[2:nrow(dhaka_pop_by_age)]) # proportion employed
household_dhaka <- 2274927/sum(dhaka_pop_by_age$pop[2:nrow(dhaka_pop_by_age)]) # prop working in HH
