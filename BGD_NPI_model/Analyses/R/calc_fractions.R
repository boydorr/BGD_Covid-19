#
## Function to calculate fraction of infectiouns that go through each disease state
#__________________________________________

# Arguments
#   age_dep_pars = percentage of individuals in each 10 year age band that are 
#                  hospitalised (hospitalised) and die (CFR). And proportion of
#                  cases in each age group that are symptomatic. From Davies et al.
#   demog = describes proportion of the population in each 10-year age band 

# Output
#   named vector of fractions required by model

calc_fractions <- function(age_dep_pars,demog){
  fa <- sum((1-age_dep_pars$prop_symptomatic)*demog$prop) # fraction of infections that are asymptomatic
  fd <- sum((age_dep_pars$CFR/100)*demog$prop)/(1-fa) # fraction of symptomatics that die
  fHosp <- sum((age_dep_pars$hospitalised/100)*demog$prop)/(1-fa) # fraction of symptomatics hospitalised

  return(c("fa"=fa,
           "fd"=fd,
           "fHosp"=fHosp))
}



