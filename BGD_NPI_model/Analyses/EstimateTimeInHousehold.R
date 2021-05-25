rm(list=ls())

library(ssar)

set.seed(0)



# Parameters for baseline scenario (with no interventions)
source("R/calc_fractions.R")
source("R/pars_baseline.R")

# Parameters
parms <- c(dur_E=as.numeric(parms_baseline["dur_inc"]-parms_baseline["dur_p"]),
           parms_baseline["dur_p"],
           parms_baseline["dur_s"],
           parms_baseline["dur_a"],
           parms_baseline["fa"])

parms <- c(parms,
           "beta_s"=as.numeric((parms_baseline["probHHtrans"]*(parms_baseline["HHsize"]-1))/
                                 (parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] + 
                                    (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] + 
                                                                parms_baseline["dur_s"])))
)
parms <- c(parms,
           "beta_p"=as.numeric(((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"])
)
parms <- c(parms,
           "beta_a"=as.numeric(((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"])
)



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
  matreturn[,1] <- (parms["beta_p"]*X[,3]+parms["beta_s"]*X[,4]+parms["beta_a"]*X[,5])*X[,1]/(X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6])
  matreturn[,2] <- (1-parms["fa"])*X[,2]/params["dur_E"]
  matreturn[,3] <- parms["fa"]*X[,2]/params["dur_E"]
  matreturn[,4] <- X[,3]/params["dur_p"]
  matreturn[,5] <- X[,4]/params["dur_s"]
  matreturn[,6] <- X[,5]/params["dur_a"]
  
  #Return
  return(matreturn)
  
}


# starting values
y <- matrix(c(S=3, E=1, Ip=0, Is=0, Ia=0, R=0),ncol=6)

# run epidemic
nsims_vec <- c(100,1000,5000)
reps<-20
means<-means_a<-means_s<-matrix(NA,ncol=length(nsims_vec),nrow=reps)
for(nsims_i in 1:length(nsims_vec)){
  nsims <- nsims_vec[nsims_i]
  for(rep in 1:reps){
    out <- ssa(y, pfun, v, params=parms, tmin=0, tmax=365, nsim=nsims,
               print.time=F,plot.sim = F) # no intervention
    out<-filter(out,!is.infinite(Time)&Time>0)
    symp_first <- filter(out, Var3==1 & Iteration==0)$Simulation
    end_times<-out%>%group_by(Simulation)%>%summarise(max(Time))
    start_times<-out%>%group_by(Simulation)%>%summarise(min(Time))
    means[rep,nsims_i] <- mean(end_times$`max(Time)` - start_times$`min(Time)`)
    means_a[rep,nsims_i] <- mean(end_times$`max(Time)`[-symp_first] - start_times$`min(Time)`[-symp_first])
    means_s[rep,nsims_i] <- mean(end_times$`max(Time)`[symp_first] - start_times$`min(Time)`[symp_first])
  }
}

par(mfrow=c(2,2))
plot(c(means)~rep(1:length(nsims_vec),each=reps))
plot(c(means_a)~rep(1:length(nsims_vec),each=reps))
plot(c(means_s)~rep(1:length(nsims_vec),each=reps))
range(means[,ncol(means)])
range(means_a[,ncol(means)])
range(means_s[,ncol(means)])
max(means[,ncol(means)])-min(means[,ncol(means)])
max(means_a[,ncol(means)])-min(means_a[,ncol(means)])
max(means_s[,ncol(means)])-min(means_s[,ncol(means)])
## 5000 sims seems do-able for speed when estimated on the fly (in model
## calibration and sensitivity analysis) using R/duration_household_infectious.R
## Will use 50,000 to get more accurate for analyses that only use the baseline pars (see below)



nsims<-50000
set.seed(0)
system.time({
  out_original <- ssa(y, pfun, v, params=parms, tmin=0, tmax=365, nsim=nsims,
             print.time=F,plot.sim = F) # no intervention
})
out<-out_original%>%filter(!is.infinite(Time)&Time>0)
symp_first <- filter(out, Var3==1 & Iteration==0)$Simulation

end_times<-out%>%group_by(Simulation)%>%summarise(max(Time))
start_times<-out%>%group_by(Simulation)%>%summarise(min(Time))

(dur_hh <- mean(end_times$`max(Time)`-start_times$`min(Time)`)) # 10.56
(dur_hha <- mean(end_times$`max(Time)`[-symp_first]-start_times$`min(Time)`[-symp_first])) # 9.87
(dur_hhs <- mean(end_times$`max(Time)`[symp_first]-start_times$`min(Time)`[symp_first])) # 12.16
hist(end_times$`max(Time)`-start_times$`min(Time)`)
max(end_times$`max(Time)`-start_times$`min(Time)`)

out<-out_original[order(out_original$Simulation),]
out<-out%>%filter(!is.infinite(Time))
out_symp <- out %>% group_by(Simulation) %>% filter(sum(Var4)>0)
nsymp <- out_symp %>% group_by(Simulation) %>% mutate(diff_symp=c(0,diff(Var3)),new_symp=pmax(0,diff_symp)) %>% summarise(sum(new_symp))
sum(nsymp$`sum(new_symp)`-1)/sum(nsymp$`sum(new_symp)`) #13% in same household as another symptomatic 


