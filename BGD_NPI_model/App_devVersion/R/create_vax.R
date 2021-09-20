create_vax <- function(model_parms,times,vax_init=NA){
  with(as.list(model_parms),{
    if(is.na(vax_init)){Vax1<-Vax2<-dailyVax1<-dailyVax2<-rep(0,length(times))
    }else{
      Vax1<-c(vax_init$Vax1,rep(0,length(times)-1));Vax2<-c(vax_init$Vax2,rep(0,length(times)-1))
      dailyVax1<-c(vax_init$dailyVax1,rep(0,length(times)-1));dailyVax2<-c(vax_init$dailyVax2,rep(0,length(times)-1))}
    for(t in times){
      if(vax==1 & t>vax_start){
        if(vax_2_doses==T){
          if(t>(vax_start+t_between_doses)){
            Vax2RateToday <- dailyVax1[t-t_between_doses]
          }else{
            Vax2RateToday <- 0
          }
        }else{Vax2RateToday <- 0}
        
        dailyVax2[t+1] <- ifelse(t<=vax_end,min(Vax2RateToday,vax_rate,max(0,maxVax*population-Vax2[t])),0)
        Vax2[t+1] <- Vax2[t] + dailyVax2[t+1]
        dailyVax1[t+1] <- ifelse(t<=vax_end,min(vax_rate - dailyVax2[t+1],max(0,maxVax*population - Vax1[t])),0)
        Vax1[t+1] <- Vax1[t] + dailyVax1[t+1]
      }else{
        Vax2[t+1] <- Vax2[t] 
        Vax1[t+1] <- Vax1[t] 
        
      }
    }

    
    vax <- data.frame("dailyVax1"=dailyVax1,"Vax1"=Vax1,
                      "dailyVax2",dailyVax2,"Vax2"=Vax2,
                      "VaxDosesUsed"=Vax1+Vax2)
    
    return(vax)
  })
  
}