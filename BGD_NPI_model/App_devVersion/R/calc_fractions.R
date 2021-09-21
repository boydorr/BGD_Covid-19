#
## Function to calculate fraction of infections that go through each disease state
#__________________________________________

# Arguments
#   age_dep_pars = percentage of individuals in each 10 year age band that are 
#                  hospitalised (hospitalised) and die (CFR). And proportion of
#                  cases in each age group that are symptomatic. From Davies et al.
#   demog = describes proportion of the population in each 10-year age band 

# Output
#   named vector of fractions required by model

calc_fractions <- function(age_dep_pars,demog,Vax1=NULL,Vax2=NULL,model_parms,prevVax=NULL){
  
  if(is.null(Vax1)){
    fa <- sum((1-age_dep_pars$prop_symptomatic)*demog$prop) # fraction of infections that are asymptomatic
    fd <- sum((age_dep_pars$CFR/100)*demog$prop)/(1-fa) # fraction of symptomatics that die
    fHosp <- sum((age_dep_pars$hospitalised/100)*demog$prop)/(1-fa) # fraction of symptomatics hospitalised
  
  }else{
    
    if(!is.null(prevVax)){
      if(prevVax["vax_order"]==2){
        vax2_pops <- min(Vax2,prevVax["Vax1"])*demog$prop
        Vax2_rem <- Vax2-sum(vax2_pops)
        vax1_pops <- prevVax["Vax1"]*demog$prop
        Vax1_rem <- Vax1-sum(vax1_pops)
      }else{
        pops_to_vax <- demog$prop*model_parms["population"]*prevVax["vax_compliance"]
        vax2_pops <- pmin(pops_to_vax,pmax(0,min(Vax2,prevVax["Vax1"])-rev(cumsum(c(0,rev(pops_to_vax)[1:(length(pops_to_vax)-1)])))))
        vax1_pops <- pmin(pops_to_vax,pmax(0,prevVax["Vax1"]-rev(cumsum(c(0,rev(pops_to_vax)[1:(length(pops_to_vax)-1)]))))) 
        Vax2_rem <- Vax2-sum(vax2_pops)
        Vax1_rem <- Vax1-sum(vax1_pops)
      }
      
    }else{
      vax2_pops <- vax1_pops <- rep(0,nrow(demog))
      Vax2_rem <- Vax2
      Vax1_rem <- Vax1
    }
    
    if(model_parms["vax_order"]==1){
      pops_to_vax <- demog$prop*model_parms["population"]*model_parms["vax_compliance"]
      vax2_pops <- vax2_pops + pmin(pops_to_vax-vax2_pops,pmax(0,Vax2_rem-rev(cumsum(c(0,rev(pops_to_vax-vax2_pops)[1:(length(pops_to_vax)-1)])))))
      vax1_pops <- vax1_pops + pmin(pops_to_vax-vax1_pops,pmax(0,Vax1_rem-rev(cumsum(c(0,rev(pops_to_vax-vax1_pops)[1:(length(pops_to_vax)-1)]))))) - vax2_pops
      
    }else{
      pops_to_vax <- demog$prop*model_parms["population"]*model_parms["vax_compliance"]
      vax2_pops <- vax2_pops + Vax2_rem*(pops_to_vax-vax2_pops)/sum(pops_to_vax-vax2_pops)
      vax1_pops <- vax1_pops + Vax1_rem*(pops_to_vax-vax1_pops)/sum(pops_to_vax-vax1_pops) - vax2_pops
    }
    
    # Proportion of infected people with one or two vaccination doses in each age category
    vax2_props <- vax2_pops/(demog$prop*model_parms["population"])
    vax1_props <- vax1_pops/(demog$prop*model_parms["population"])
    vax1_props_infected <- vax1_props*(1-model_parms["vax_transmission_effect_dose1"])/((1-vax1_props-vax2_props) + vax1_props*(1-model_parms["vax_transmission_effect_dose1"])+vax2_props*(1-model_parms["vax_transmission_effect_dose2"]))
    vax2_props_infected <- vax2_props*(1-model_parms["vax_transmission_effect_dose2"])/((1-vax1_props-vax2_props) + vax1_props*(1-model_parms["vax_transmission_effect_dose1"])+vax2_props*(1-model_parms["vax_transmission_effect_dose2"]))
    
    
    fa <- sum((1-age_dep_pars$prop_symptomatic)*demog$prop*(1-vax2_props_infected-vax1_props_infected) + 
                (1-age_dep_pars$prop_symptomatic*(1-model_parms["vax_severity_effect_dose1"]))*demog$prop*vax1_props_infected +
                (1-age_dep_pars$prop_symptomatic*(1-model_parms["vax_severity_effect_dose2"]))*demog$prop*vax2_props_infected) # fraction of infections that are asymptomatic
    fd <- sum((age_dep_pars$CFR/100)*demog$prop*(1-vax2_props_infected-vax1_props_infected) + 
                (age_dep_pars$CFR*(1-model_parms["vax_severity_effect_dose1"])/100)*demog$prop*vax1_props_infected +
                (age_dep_pars$CFR*(1-model_parms["vax_severity_effect_dose2"])/100)*demog$prop*vax2_props_infected)/(1-fa) # fraction of symptomatics that die
    fHosp <- sum((age_dep_pars$hospitalised/100)*demog$prop*(1-vax2_props_infected-vax1_props_infected) + 
                   (age_dep_pars$hospitalised*(1-model_parms["vax_severity_effect_dose1"])/100)*demog$prop*vax1_props_infected +
                   (age_dep_pars$hospitalised*(1-model_parms["vax_severity_effect_dose2"])/100)*demog$prop*vax2_props_infected)/(1-fa) # fraction of symptomatics hospitalised
    
  }
  
  return(c("fa"=fa,
           "fd"=fd,
           "fHosp"=fHosp))
}


