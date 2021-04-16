#
## Function estimate rates of change in categories
#__________________________________________

# Arguments
#   t = time
#   y = number of individuals in each category at t
#   parms = vector of model parameters

# Output
#   list containing vector of rates of change of same length as y



covid_model <- function(t, y, parms) {
  with(as.list(c(y, parms)), {


    ##Calculate time-dependent rate parameters for disease transmission
    #________________________

    
    # Household betas (not affected by any of the inerventions)
    #---------------
    
    # proportion of beta_p, beta_a, beta_m that occurs in the household (in order to have propHHtrans*R0 in HH)
    beta_a_t_hh <- beta_a*(probHHtrans*(HHsize-1))/R0
    beta_p_t_hh <- beta_p*(probHHtrans*(HHsize-1))/R0
    beta_s_t_hh <- beta_s*(probHHtrans*(HHsize-1))/R0
    
    
    # Baseline between household transmission
    #---------------
    
    beta_a_t_bHH <- beta_a*(1-(probHHtrans*(HHsize-1))/R0)
    beta_p_t_bHH <- beta_p*(1-(probHHtrans*(HHsize-1))/R0)
    beta_s_t_bHH <- beta_s*(1-(probHHtrans*(HHsize-1))/R0)
    
    
    # Impact of lockdown on between household transmission
    #---------------
    
    if(t<ld_start|t>=ld_end|ld==F){ # No lockdown, no change
      NC_ld <- 1 # everyone is non-compliant with lockdown
      
    }else if(t>=ld_start & t<ld_end){ 
      
      # what proportion of the way through the improvement stage are we?
      if(ld_improve>0){ld_improve_stage <- min(1,(t-ld_start)/ld_improve) 
      }else if(ld_improve==0){ld_improve_stage <- 1} # implement max effect straight away
      
      # what is the current level of non-compliance?
      # compliance increases during the improvement stage and then decreases 
      NC_ld <- min(1,fNC + 
                     (1-ld_improve_stage)*(1-fNC) + 
                     ifelse(ld_improve_stage<1,0,1-fNC-(exp(-ld_decline*(t-ld_start-ld_improve))*(1-fNC-ld_min_compliance)+ld_min_compliance)))
      
      # edit betas  
      beta_a_t_bHH <- beta_a_t_bHH*(fEW + (1-fEW)*(NC_ld + (1-ld_effect)*(1-NC_ld)))
      beta_p_t_bHH <- beta_p_t_bHH*(fEW + (1-fEW)*(NC_ld + (1-ld_effect)*(1-NC_ld)))
      beta_s_t_bHH <- beta_s_t_bHH*(NC_ld + (1-ld_effect)*(1-NC_ld)) # assume that mildly symptomatic essential workers can no longer work
      
    }
    
    
    if(ld2==T & t>=ld2_start & t<ld2_end){ 
      
      # what proportion of the way through the improvement stage are we?
      if(ld2_improve>0){ld2_improve_stage <- min(1,(t-ld2_start)/ld2_improve) 
      }else if(ld2_improve==0){ld2_improve_stage <- 1} # implement max effect straight away
      
      # what is the current level of non-compliance?
      # compliance increases during the improvement stage and then decreases 
      NC_ld2 <- min(1,fNC2 + 
                      (1-ld2_improve_stage)*(1-fNC2) + 
                      ifelse(ld2_improve_stage<1,0,1-fNC2-(exp(-ld_decline*(t-ld2_start-ld2_improve))*(1-fNC2-ld_min_compliance)+ld_min_compliance)))
      
      # edit betas  
      beta_a_t_bHH <- beta_a_t_bHH*(fEW2 + (1-fEW2)*(NC_ld2 + (1-ld_effect)*(1-NC_ld2)))
      beta_p_t_bHH <- beta_p_t_bHH*(fEW2 + (1-fEW2)*(NC_ld2 + (1-ld_effect)*(1-NC_ld2)))
      beta_s_t_bHH <- beta_s_t_bHH*(NC_ld2 + (1-ld_effect)*(1-NC_ld2)) # assume that mildly symptomatic essential workers can no longer work
      
    }else{NC_ld2<-1}
    
    
    
    
    # Compliance with syndromic surveillance 
    #---------------
    if(syndromic==T & t>=syn_start & t<syn_end){
      
      # what proportion of the way through the improvement stage are we?
      if(syn_improve==0 & t==syn_start){syn_improve_stage <- 1
      }else{syn_improve_stage <- max(0,min(1,(t-syn_start)/syn_improve))}
      
      
      # Look at non-compliers and improvement stages for each of lockdown and syndromic surveillance
      NC_syn <- (1-community) + community*(1-syn_improve_stage)
      
    }else{NC_syn<-1}
    
    
    
    # Impact of mask wearing
    #---------------
    # Assume that groups that don't comply with masks and lockdown/syndromic surveillance are not correlated 
    if(mask==T & t>=mask_start & t<mask_end){
      
      # what proportion of the way through the improvement stage are we?
      if(mask_improve>0){mask_improve_stage <- min(1,(t-mask_start)/mask_improve)
      }else if(mask_improve==0){mask_improve_stage <- 1} # implement max effect straight away
      
      # what is the current level of non-compliance?
      # compliance increases during the improvement stage
      NC_mask <- (1-mask_compliance) + (1-mask_improve_stage)*mask_compliance
      
      # edit betas
      overall_mask_effect <- ((1-(1-NC_mask)*mask_effect_outward)*(1-(1-NC_mask)*f_mask_effect_inward*mask_effect_outward))
      beta_a_t_bHH <- beta_a_t_bHH*overall_mask_effect
      beta_p_t_bHH <- beta_p_t_bHH*overall_mask_effect
      beta_s_t_bHH <- beta_s_t_bHH*overall_mask_effect
      
    }
    



    ##Calculate rates of change in compartments
    #________________________
    
    # Subpopulations
    N <- population
    N_I <- S_I + E_ss + E_sa + E_t + Ia_f + Ia_s + Ia_sa + Ip_f + Ip_s + Ip_sa + Is_f + Is_s + R_I + R_Ia_f + R_Is_f + R_I_b + E_b + Ip_b + Ia_b +Is_b 
    N_q <- S_q + E_q + E_qE + Ia_q + Ia_qa + Ip_q +Ip_qp + Is_q + Is_qs + R_qR + R_Ia_qa + R_Is_qs + R_Ia_q + R_Is_q 
    N_fq <- S_I + E_ss + E_b + Ip_b + Ia_b + R_I
    N_sq <- S_I + E_sa + E_b + Ip_sa + Ip_b + Ia_f + Ia_sa + Ia_b + R_I + R_Ia_f
    
    
    # Forces of infection
    lambda_b <- (beta_a_t_bHH*(Ia_f + Ia_s + Ia_sa + Ia_b) + beta_p_t_bHH*(Ip_f + Ip_s + Ip_sa + Ip_b) + beta_s_t_bHH*(Is_f + Is_s + Is_b))/N
    lambda_t <- (beta_a_t_hh*(Ia_s + Ia_sa + Ia_b) + beta_p_t_hh*(Ip_s + Ip_sa + Ip_b) + beta_s_t_hh*(Is_s + Is_b))/N_I
    lambda_sa <- (beta_a_t_hh*Ia_f)/N_I
    lambda_ss <- (beta_p_t_hh*Ip_f + beta_s_t_hh*Is_f)/N_I
    lambda_q <- ifelse(N_q>0,(beta_a_t_hh*(Ia_q + Ia_qa) + beta_p_t_hh*(Ip_q + Ip_qp) + beta_s_t_hh*(Is_q + Is_qs))/N_q,0)
    
    # dur_E 
    dur_E <- (dur_inc-dur_p)
    
    # Susceptibles 
    dS_n <- -lambda_b*S_n*(1+(HHsize-1)*S_n/(S_n+R_n)) + S_I/dur_hh + S_q/dur_q 
    dS_E <- -lambda_b*S_E + (HHsize-1)*lambda_b*(S_n^2)/(S_n+R_n) - S_E/dur_E
    dS_I <- S_E/dur_E - (lambda_sa + lambda_ss + lambda_t + lambda_b)*S_I - (HHsize-1)*(1-NC_syn)*((Ip_f/dur_p)*(S_I/N_fq) + (Ip_sa/dur_p)*(S_I/N_sq)) - S_I/dur_hh
    dS_q <- ifelse(N_q>0,-lambda_q*S_q,0) + (HHsize-1)*(1-NC_syn)*((Ip_f/dur_p)*(S_I/N_fq) + (Ip_sa/dur_p)*(S_I/N_sq)) - S_q/dur_q
    
    # Latent 
    dE_f <- lambda_b*S_n - E_f/dur_E 
    dE_b <- lambda_b*(S_E+S_I)- E_b/dur_E - (HHsize-1)*(1-NC_syn)*((Ip_f/dur_p)*(E_b/N_fq)+(Ip_sa/dur_p)*(E_b/N_sq))
    dE_ss <- lambda_ss*S_I - E_ss/dur_E - (HHsize-1)*(1-NC_syn)*(Ip_f/dur_p)*(E_ss/N_fq)
    dE_sa <- lambda_sa*S_I - E_sa/dur_E - (HHsize-1)*(1-NC_syn)*(Ip_sa/dur_p)*(E_sa/N_sq)
    dE_t <- lambda_t*S_I - E_t/dur_E 
    dE_q <- ifelse(N_q>0,lambda_q*S_q,0) - E_q/dur_E 
    dE_qE <- (HHsize-1)*(1-NC_syn)*((Ip_f/dur_p)*((E_ss+E_b)/N_fq) + (Ip_sa/dur_p)*((E_sa+E_b)/N_sq)) - 2*E_qE/dur_E
    
    # Asymptomatic 
    dIa_f <- fa*E_f/dur_E  - Ia_f/dur_a - (HHsize-1)*(1-NC_syn)*(Ip_sa/dur_p)*(Ia_f/N_sq)
    dIa_b <- fa*E_b/dur_E - (HHsize-1)*(1-NC_syn)*((Ip_f/dur_p)*(Ia_b/N_fq)+(Ip_sa/dur_p)*(Ia_b/N_sq))- Ia_b/dur_a
    dIa_s <- fa*(E_ss + E_t)/dur_E  - Ia_s/dur_a
    dIa_sa <- fa*E_sa/dur_E  - Ia_sa/dur_a - (HHsize-1)*(1-NC_syn)*(Ip_sa/dur_p)*(Ia_sa/N_sq)
    dIa_q <- fa*(E_q + 2*E_qE)/dur_E  - Ia_q/dur_a
    dIa_qa <- (HHsize-1)*(1-NC_syn)*((Ip_f/dur_p)*(Ia_b/N_fq) + (Ip_sa/dur_p)*((Ia_f+Ia_sa+Ia_b)/N_sq)) - 2*Ia_qa/dur_a
    
    # Pre-symptomatic 
    dIp_f <- (1-fa)*E_f/dur_E - Ip_f/dur_p
    dIp_s <- (1-fa)*(E_ss+E_t)/dur_E - Ip_s/dur_p
    dIp_b <- (1-fa)*(E_b)/dur_E - Ip_b/dur_p - (HHsize-1)*(1-NC_syn)*((Ip_f/dur_p)*(Ip_b/N_fq)+(Ip_sa/dur_p)*(Ip_b/N_sq))
    dIp_sa <- (1-fa)*E_sa/dur_E - Ip_sa/dur_p - (HHsize-1)*(1-NC_syn)*(Ip_sa/dur_p)*(Ip_sa/N_sq)
    dIp_q <- (1-fa)*(E_q + 2*E_qE)/dur_E - Ip_q/dur_p
    dIp_qp <- (HHsize-1)*(1-NC_syn)*((Ip_f/dur_p)*(Ip_b/N_fq) + (Ip_sa/dur_p)*((Ip_sa+Ip_b)/N_sq)) - 2*Ip_qp/dur_p
    
    # Symptomatic
    dIs_f <- NC_syn*Ip_f/dur_p - Is_f/dur_s 
    dIs_s <- (NC_syn*Ip_sa + Ip_s)/dur_p - Is_s/dur_s
    dIs_b <- Ip_b/dur_p - Is_b/dur_s
    dIs_q <- (Ip_q + 2*Ip_qp)/dur_p - Is_q/dur_s
    dIs_qs <- (1-NC_syn)*(Ip_f + Ip_sa)/dur_p - Is_qs/dur_s
    
    # Removed
    dR_n <- (Ia_s + Ia_sa)/dur_a + Is_s/dur_s + R_I/dur_hh + R_qR/dur_q + R_Ia_f/(dur_hha-dur_a) + R_Is_f/(dur_hhs-dur_s-dur_p) +
      2*R_I_b/(dur_hh-(fa*dur_a+(1-fa)*(dur_p+dur_s))) + ifelse(dur_s<dur_q,(R_Is_qs + 2*R_Is_q)/(dur_q-dur_s),(Is_q+Is_qs)/dur_s) + R_Ia_qa/(dur_q-dur_a/2) + 2*R_Ia_q/(dur_q-dur_a) - 
      (HHsize-1)*lambda_b*S_n*(R_n/(S_n+R_n))
    dR_E <- (HHsize-1)*lambda_b*S_n*(R_n/(S_n+R_n)) - R_E/dur_E
    dR_I <- R_E/dur_E - R_I/dur_hh - (HHsize-1)*(1-NC_syn)*((Ip_f/dur_p)*(R_I/N_fq) + (Ip_sa/dur_p)*(R_I/N_sq))
    dR_Ia_f <- Ia_f/dur_a - R_Ia_f/(dur_hha-dur_a) - (HHsize-1)*(1-NC_syn)*(Ip_sa/dur_p)*(R_Ia_f/N_sq)
    dR_Is_f <- Is_f/dur_s - R_Is_f/(dur_hhs-dur_s-dur_p)
    dR_I_b <- Ia_b/dur_a + Is_b/dur_s - 2*R_I_b/(dur_hh-(fa*dur_a+(1-fa)*(dur_p+dur_s)))
    dR_qR <- (HHsize-1)*(1-NC_syn)*((Ip_f/dur_p)*(R_I/N_fq) + (Ip_sa/dur_p)*((R_I+R_Ia_f)/N_sq)) - R_qR/dur_q
    dR_Is_qs <- ifelse(dur_s<dur_q,Is_qs/dur_s - R_Is_qs/(dur_q-dur_s),0)
    dR_Ia_qa <- 2*Ia_qa/dur_a - R_Ia_qa/(dur_q-dur_a/2)
    dR_Is_q <- ifelse(dur_s<dur_q,Is_q/dur_s - 2*R_Is_q/(dur_q-dur_s),0)
    dR_Ia_q <- Ia_q/dur_a - 2*R_Ia_q/(dur_q-dur_a)
    
    
    # Deaths
    dD_wait <- fd*(Ip_f + Ip_s + Ip_sa + Ip_q + Ip_b + 2*Ip_qp)/dur_p - D_wait/delay_death
    dD <- D_wait/delay_death
    
    # General hospital beds
    dHosp_wait <- fHosp*(1-probICU)*(Ip_f + Ip_s + Ip_sa + Ip_q + Ip_b + 2*Ip_qp)/dur_p - Hosp_wait/delay_hosp
    dHosp <- Hosp_wait/delay_hosp - Hosp/dur_hosp
    
    # ICU beds
    dICU_wait <- fHosp*probICU*(Ip_f + Ip_s + Ip_sa + Ip_q + Ip_b + 2*Ip_qp)/dur_p - ICU_wait/delay_ICU
    dICU <- ICU_wait/delay_ICU - ICU/dur_ICU
    
    # Recuperating individuals
    dRecup <-  (Hosp/dur_hosp + ICU/dur_ICU)*(1-fd/fHosp) - Recup/recuperation # assumes that all deaths occur in hospital, so likely to be an underestimate of recuperating individuals
    
    
    
    
    # Cases detected by rapid tests (targeted at those with mild symptoms)
    if(syndromic == T & t>=syn_start & t<syn_end){
      targets_nonCovid_mild <- (mild_nonCovid/365)*((1-NC_syn)*(S_n+S_E+S_I + E_f+E_b+E_ss+E_sa+E_t + R_n+R_E+R_I+R_Is_f+R_Ia_f+R_I_b) + (S_q + E_q+E_qE + R_qR+R_Is_qs+R_Ia_qa+R_Is_q+R_Ia_q)) 
      targets_covid_mild <- ((Ip_f+Ip_sa)*(1-NC_syn)+Ip_q+2*Ip_qp)/dur_p 
      prop_covid <- targets_covid_mild/(targets_covid_mild+targets_nonCovid_mild)
      
      # Total positive rapid tests 
      dRapidTotal <- (targets_covid_mild - #total targets with covid that are amenable to testing
                        max(0,(targets_nonCovid_mild+targets_covid_mild) - capacity_rapid * syn_improve_stage) * prop_covid) * # number of those cases we don't have capacity to test (accounting for non-covid respiratory infections)
        (1-rapid_fneg) # fraction of tests that are not false negatives
      
      # Total rapid tests used
      dRapidUsed <-  min(targets_covid_mild+targets_nonCovid_mild, capacity_rapid * syn_improve_stage) 
      
    }else{
      dRapidTotal <- 0
      dRapidUsed <-  0
    }
    # Current individuals that have had a positive rapid test
    dRapidCurrent <- dRapidTotal - RapidCurrent/dur_s
    dRapidHospWait <- fHosp*(1-probICU)*dRapidTotal - RapidHospWait/delay_hosp
    dRapidICUWait <- fHosp*probICU*dRapidTotal - RapidICUWait/delay_ICU
  
    
    
    
    # Cases detected by lab tests
    if(lab==T & t>=lab_start & t<lab_end){ # Assume that lab testing is targeted towards severe/critical cases i.e. hospitalized only
      
      # Targets of community testing and the proportion that actually have COVID-19
      targets_nonCovid_severe <- (severe_nonCovid/365)*((S_n+S_E+S_I + E_f+E_b+E_ss+E_sa+E_t + R_n+R_E+R_I+R_Is_f+R_Ia_f+R_I_b) + (S_q + E_q+E_qE + R_qR+R_Is_qs+R_Ia_qa+R_Is_q+R_Ia_q))
      targets_covid_severe <- Hosp_wait/dur_hosp + ICU_wait/dur_ICU
      prop_covid <- targets_covid_severe/(targets_covid_severe+targets_nonCovid_severe)
      
      # what proportion of the way through the improvement stage are we?
      if(lab_improve>0){lab_improve_stage <- min(1,(t-lab_start)/lab_improve)
      }else if(lab_improve==0){lab_improve_stage <- 1}
      
      # change in total number of positive lab tests
      dLabTotal <-  (targets_covid_severe - #total new severe covid cases 
                       max(0,targets_covid_severe + targets_nonCovid_severe - capacity_lab * lab_improve_stage) * prop_covid) * # number of those cases we don't have capacity to test (accounting for non-covid respiratory infections)
        (1-lab_fneg) # fraction of tests that are not false negatives
      
      # change in current number of positive lab tests for individuals not previously detected by rapid tests
      propTargetsHosp<-ifelse((Hosp_wait+ICU_wait)>0,(Hosp_wait/delay_hosp)/(Hosp_wait/delay_hosp + ICU_wait/delay_ICU),0)
      dLabTotalNew <- ifelse(Hosp_wait>0,dLabTotal*propTargetsHosp*(1-(RapidHospWait/delay_hosp)/(Hosp_wait/delay_hosp)),0) + 
        ifelse(ICU_wait>0,dLabTotal*(1-propTargetsHosp)*(1-(RapidICUWait/delay_ICU)/(ICU_wait/delay_ICU)),0)# no double counting if lab tested after already rapid tested - still doesn't account for potential multiple tests while in hospital however
      
      # Total lab tests used
      dLabUsed <-  min(targets_covid_severe+targets_nonCovid_severe, capacity_lab * lab_improve_stage) 
      
      
    }else{
      dLabTotal <- 0
      dLabTotalNew <- 0
      dLabUsed <- 0
    }
    
    
    # Total tested positive
    dTested <- dRapidTotal + dLabTotalNew
   
    
    

    # Cumulative cases
    dCumCases <- (E_f+E_b+E_ss+E_sa+E_t+E_q+2*E_qE)/dur_E # counts individuals moving out of E
    
    # Cumulative symptomatic cases
    dCumSymp <- (Ip_f+Ip_b+Ip_s+Ip_sa+Ip_q+2*Ip_qp)/dur_p
    
    # Cumulative hospital cases (general + ICU)
    dCumSevere <- Hosp_wait/delay_hosp + ICU_wait/delay_ICU
    
    # Cumulative ICU cases
    dCumICU <- ICU_wait/delay_ICU
    
    
    
    ##Output
    #________________________
    list(c(dS_n,dS_E,dS_I,dS_q,
           dE_f,dE_b,dE_ss,dE_sa,dE_t,dE_q,dE_qE,
           dIp_f,dIp_b,dIp_s,dIp_sa,dIp_q,dIp_qp,
           dIa_f,dIa_b,dIa_s,dIa_sa,dIa_q,dIa_qa,
           dIs_f,dIs_b,dIs_s,dIs_q,dIs_qs,
           dR_n,dR_E,dR_I,dR_Ia_f,dR_Is_f,dR_I_b,dR_qR,dR_Ia_qa,dR_Is_qs,dR_Ia_q,dR_Is_q,
           Recup,dD_wait,dD,dHosp_wait,dHosp,dICU_wait,dICU,
           dLabTotal,dLabTotalNew,dLabUsed,dRapidTotal,dRapidCurrent,dRapidHospWait,dRapidICUWait,dRapidUsed,dTested,
           dCumCases,dCumSymp,dCumSevere,dCumICU))
    
  })
}


