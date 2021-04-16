#
## Convert Bangladesh data to 10 year age bands

## 5 year age bands
pop_by_age_5years <- read.csv("data/bangladesh_pop_by_age_census2011.csv")

## Load age dependent disease stages
age_dep_pars <- read.csv("data/age_dep_CFR_hosp_asymp.csv")

## Convert 5 year band demog data to 10 year bands
pop_by_age_10years <- data.frame(age_group=age_dep_pars$age_group,pop=rowsum(pop_by_age_5years$pop,gl(nrow(pop_by_age_5years),2,nrow(pop_by_age_5years))))

## Get proportion population in each age group
pop_by_age_10years$prop <- pop_by_age_10years$pop/sum(pop_by_age_10years$pop)

## Write new file
write.table(pop_by_age_10years,"data/bangladesh_pop_by_age_census2011_10yearBands.csv",row.names = F,sep=",")



## Convert Dhaka data to 10 year age bands

## 5 year age bands
pop_by_age_5years <- read.csv("data/dhaka_dist_pop_by_age_census2011.csv")

## Load age dependent disease stages
age_dep_pars <- read.csv("data/age_dep_CFR_hosp_asymp.csv")

## Convert 5 year band demog data to 10 year bands
pop_by_age_10years <- data.frame(age_group=age_dep_pars$age_group,pop=rowsum(pop_by_age_5years$pop,gl(nrow(pop_by_age_5years),2,nrow(pop_by_age_5years))))

## Get proportion population in each age group
pop_by_age_10years$prop <- pop_by_age_10years$pop/sum(pop_by_age_10years$pop)

## Write new file
write.table(pop_by_age_10years,"data/dhaka_dist_pop_by_age_census2011_10yearBands.csv",row.names = F,sep=",")
