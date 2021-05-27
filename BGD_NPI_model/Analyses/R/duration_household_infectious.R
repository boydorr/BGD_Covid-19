duration_household_infection <- function(parms_baseline,nsims){
  
  # Parameters
  parms_hh <- c(dur_E=as.numeric(parms_baseline["dur_inc"]-parms_baseline["dur_p"]),
                parms_baseline["dur_p"],
                parms_baseline["dur_s"],
                parms_baseline["dur_a"],
                parms_baseline["fa"],
                parms_baseline["beta_a"]*(parms_baseline["probHHtrans"]*(parms_baseline["HHsize"]-1))/parms_baseline["R0"],
                parms_baseline["beta_p"]*(parms_baseline["probHHtrans"]*(parms_baseline["HHsize"]-1))/parms_baseline["R0"],
                parms_baseline["beta_s"]*(parms_baseline["probHHtrans"]*(parms_baseline["HHsize"]-1))/parms_baseline["R0"])
  
  #Propensity matrix
  v  <- matrix(c(-1,+1,0,0,0,0, #S->E 
                 0,-1,+1,0,0,0, #E->Ip
                 0,-1,0,0,+1,0, #E->Ia
                 0,0,-1,+1,0,0, #Ip->Is
                 0,0,0,-1,0,+1, #Is->R
                 0,0,0,0,-1,+1),#Ia->R
               nrow=6)
  
  
  #Propensity function
  pfun <- function(t, X, params){
    
    #Value to return
    matreturn  <- matrix(NA, nrow = length(t), ncol = 6)
    
    #Estimate values
    matreturn[,1] <- (parms_hh["beta_p"]*X[,3]+parms_hh["beta_s"]*X[,4]+parms_hh["beta_a"]*X[,5])*X[,1]/(X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6])
    matreturn[,2] <- (1-parms_hh["fa"])*X[,2]/params["dur_E"]
    matreturn[,3] <- parms_hh["fa"]*X[,2]/params["dur_E"]
    matreturn[,4] <- X[,3]/params["dur_p"]
    matreturn[,5] <- X[,4]/params["dur_s"]
    matreturn[,6] <- X[,5]/params["dur_a"]
    
    #Return
    return(matreturn)
    
  }
  
  
  # starting values
  y_hh <- matrix(c(S=3, E=1, Ip=0, Is=0, Ia=0, R=0),ncol=6)
  
  
  # run simulations and extract end times
  out <- ssa(y_hh, pfun, v, params=parms_hh, tmin=0, tmax=365, nsim=nsims,   
             print.time=F,plot.sim = F) # no intervention
  out<-filter(out,!is.infinite(Time)&Time>0)
  symp_first <- filter(out, Var3==1 & Iteration==0)$Simulation

    
  end_times<-out%>%group_by(Simulation)%>%summarise(max(Time))
  start_times<-out%>%group_by(Simulation)%>%summarise(min(Time))
  
  
  # estimate mean time household is infectious
  return(c(mean(end_times$`max(Time)`-start_times$`min(Time)`),
           mean(end_times$`max(Time)`[-symp_first] - start_times$`min(Time)`[-symp_first]),
           mean(end_times$`max(Time)`[symp_first] - start_times$`min(Time)`[symp_first])))
  
}



