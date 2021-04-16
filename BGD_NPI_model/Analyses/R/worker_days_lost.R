#
## Function estimate worker days lost based on covid model output
#__________________________________________

# Arguments
#   modelOutput = output from simulating from the covid model: a dataframe describing the number of 
#                 individuals in each disease state through time
#   model_parms = vector of the model parameters used to create modelOutput

# Output
#   daily vector of proportion working days lost of length nrow(modelOutput)


## Calculate worker days (note: all these parameters are currently mostly complete guesses..)
worker_days_lost <- function(modelOutput,
                             model_parms){ # model parameters
                             
  
  # Total possible days worked
  total_wd <- model_parms["population"] * model_parms["propWorkers"] * 5/7 # assume 5 days working every week

  ## Population subsets
  N <- model_parms["population"] 
  N_q <- rowSums(modelOutput[,c("S_q","E_q","E_qE","Ip_q","Ip_qp","Ia_q","Ia_qa","Is_q","Is_qs","R_qR","R_Ia_qa","R_Is_qs","R_Ia_q","R_Is_q")])
  N_I <- rowSums(modelOutput[,c("S_I","E_b","E_ss","E_sa","E_t","Ip_f","Ip_b","Ip_s","Ip_sa","Ia_f","Ia_b","Ia_s","Ia_sa","Is_f","Is_b","Is_s","R_I","R_Ia_f","R_Is_f","R_I_b")])
    
  # days lost due to illness, death & quarantine
  wdl <-  model_parms["propWorkers"] * 5/7 *
    ((modelOutput$D + modelOutput$Recup)*(N-N_q)/N + modelOutput$Is + # days lost being ill/deaad 
       (N_q-modelOutput$Is_q-modelOutput$Is_qs)) # days lost in quarantine
  
  # days lost due to lockdown
  if(model_parms["ld"]==T){
    
    # proportion not complying with lockdown through time
    ld_improve_stage <- pmax(0,pmin(1,(times-model_parms["ld_start"])/model_parms["ld_improve"])) 
    ld_improve_stage[which(is.nan(ld_improve_stage))] <- 1 # can happen if t==ld_start and ld_improve==0
    ld_improve_stage[which(times>=model_parms["ld_end"])] <- 0
    NC_ld <- pmin(1,model_parms["fNC"] + 
                    (1-ld_improve_stage)*(1-model_parms["fNC"]) + 
                    sapply(1:length(times),function(x){ifelse(ld_improve_stage[x]<1,0,1-model_parms["fNC"]-(exp(-model_parms["ld_decline"]*(times[x]-model_parms["ld_start"]-model_parms["ld_improve"]))*(1-model_parms["fNC"]-model_parms["ld_min_compliance"])+model_parms["ld_min_compliance"]))}))
    
    # update working days lost
    wdl <- wdl + ((modelOutput$S-modelOutput$S_q) + (modelOutput$E-modelOutput$E_q-modelOutput$E_qE) + # everyone not already off sick/on quarantine
                    (modelOutput$Ip-modelOutput$Ip_q-modelOutput$Ip_qp) + (modelOutput$Ia - modelOutput$Ia_q-modelOutput$Ia_qa) + 
                    (modelOutput$R - modelOutput$R_qR - modelOutput$R_Ia_qa - modelOutput$R_Is_qs - modelOutput$R_Ia_q - modelOutput$R_Is_q)*ifelse(modelOutput$R>0,(1-(modelOutput$D+modelOutput$Recup)/modelOutput$R),0)) * 
      (model_parms["propWorkers"]-model_parms["fEW"])*(1-NC_ld) * 5/7
  }else{NC_ld<-rep(1,length(times))}
  
  # days lost due to second lockdown
  if(model_parms["ld2"]==T){

    # proportion not complying with lockdown through time
    ld2_improve_stage <- pmax(0,pmin(1,(times-model_parms["ld2_start"])/model_parms["ld2_improve"]))
    ld2_improve_stage[which(is.nan(ld2_improve_stage))] <- 1 # can happen if t==ld_start and ld_improve==0
    ld2_improve_stage[which(times>=model_parms["ld2_end"])] <- 0
    NC_ld2 <- pmin(1,model_parms["fNC2"] +
                    (1-ld2_improve_stage)*(1-model_parms["fNC2"]) +
                    sapply(1:length(times),function(x){ifelse(ld2_improve_stage[x]<1,0,1-model_parms["fNC2"]-(exp(-model_parms["ld_decline"]*(times[x]-model_parms["ld2_start"]-model_parms["ld2_improve"]))*(1-model_parms["fNC2"]-model_parms["ld_min_compliance"])+model_parms["ld_min_compliance"]))}))

    wdl <- wdl + ((modelOutput$S-modelOutput$S_q) + (modelOutput$E-modelOutput$E_q-modelOutput$E_qE) + # everyone not already off sick/on quarantine
                    (modelOutput$Ip-modelOutput$Ip_q-modelOutput$Ip_qp) + (modelOutput$Ia - modelOutput$Ia_q - modelOutput$Ia_qa) +
                    (modelOutput$R - modelOutput$R_qR - modelOutput$R_Ia_qa - modelOutput$R_Is_qs - modelOutput$R_Ia_q - modelOutput$R_Is_q)*ifelse(modelOutput$R>0,(1-(modelOutput$D+modelOutput$Recup)/modelOutput$R),0)) *
      (model_parms["propWorkers"]-model_parms["fEW2"])*(1-NC_ld2) * 5/7

  }else{NC_ld2<-rep(1,length(times))}
  
  # days lost due to family member having to take over duties
  wdl <- wdl + ((modelOutput$D + modelOutput$Recup)*(N-N_q)/N + 
                  modelOutput$Is*ifelse((N_I + N_q)>0,(N_I/(N_I + N_q)),0))*
    pmin(((model_parms["propWorkers"]-model_parms["fEW"])*NC_ld + model_parms["fEW"]),((model_parms["propWorkers"]-model_parms["fEW2"])*NC_ld2 + model_parms["fEW2"]))*
    model_parms["propCarers"] * 5/7 
  
  # days lost grieving people who died
  wdl <- wdl + c(rep(0,7),diff(modelOutput$D,7))*(model_parms["HHsize"]-1-model_parms["propCarers"])*
    pmin(((model_parms["propWorkers"]-model_parms["fEW"])*NC_ld + model_parms["fEW"]),((model_parms["propWorkers"]-model_parms["fEW2"])*NC_ld2 + model_parms["fEW2"]))*
    (1-(modelOutput$D+modelOutput$Recup)/N) * 5/7 
  
  
  # days lost due to those in households with non-covid ILIs quarantining
  if(model_parms["syndromic"]==T){
    
    # proportion complying with syndromic surveillance
    syn_improve_stage <- pmax(0,pmin(1,(times-model_parms["syn_start"])/model_parms["syn_improve"])) 
    syn_improve_stage[which(is.nan(syn_improve_stage))] <- 1 # can happen if t==ld_start and ld_improve==0
    syn_improve_stage[which(times>=model_parms["syn_end"])] <- 0
    NC_syn <- (1-model_parms["community"]) + model_parms["community"]*(1-syn_improve_stage)
    
    # New ILI cases in non-Covid pop
    ILIs <- (model_parms["mild_nonCovid"]/365)*(N-N_q)
    
    # New households with ILIs
    ILI_HH <- ILIs*(1-model_parms["nonCovidHH"])
    
    # Extra people under quarantine 
    ILI_quarantine <- ILI_HH*(1-NC_syn)
    
    # Remove people who would have been off due to lockdown, illness or death anyway
    ILI_quarantine <- 
      ILI_quarantine*pmin(((model_parms["propWorkers"]-model_parms["fEW"])*NC_ld + model_parms["fEW"]),((model_parms["propWorkers"]-model_parms["fEW2"])*NC_ld2 + model_parms["fEW2"]))*
      (1-((modelOutput$Is_s+modelOutput$Is_f+modelOutput$Is_b)/(N-N_q))-(modelOutput$D+modelOutput$Recup)/N)
    

    
    # Update days lost
    wdl <- wdl + ILI_quarantine
    
  }
  
  
  prop_wdl <- pmin(1,wdl/total_wd) # make sure doesn't exceed 1
  
  #
  
  return(prop_wdl)
  
}

