initial_conds <- function(model_parms,initial_infectious,initial_immune){
        
        inits <- c(S_n=0,
                   S_E=as.numeric((model_parms["HHsize"]-1)*(initial_infectious*0.5)),
                   S_I=as.numeric((model_parms["HHsize"]-1)*(initial_infectious*0.5)),
                   S_q=0,
                   E_f=initial_infectious*0.5,# Assume same number incubating as infectious and evenly distribute among households with and without infecteds (assume no more than one per houshold),
                   E_b=0,
                   E_ss=0,
                   E_sa=as.numeric(initial_infectious*0.5*model_parms["fa"]),
                   E_t=0,
                   E_q=0,
                   E_qE=as.numeric(initial_infectious*0.5*(1-model_parms["fa"])),
                   Ip_f=as.numeric(initial_infectious*(1-model_parms["fa"])),
                   Ip_b=0,
                   Ip_s=0,
                   Ip_sa=0,
                   Ip_q=0,
                   Ip_qp=0,
                   Ia_f=as.numeric(initial_infectious*model_parms["fa"]), # assume infectious individuals are the only infectious individual in their households and are at the beginning of their infectious period
                   Ia_b=0,
                   Ia_s=0,
                   Ia_sa=0,
                   Ia_q=0,
                   Ia_qa=0,
                   Is_f=0,
                   Is_b=0,
                   Is_s=0,
                   Is_q=0,
                   Is_qs=0,
                   R_n=as.numeric(model_parms["population"]*(initial_immune/100)*(1-((initial_infectious+initial_infectious*0.5)/(model_parms["population"]/model_parms["HHsize"])))),
                   R_E=as.numeric(model_parms["population"]*(initial_immune/100)*((initial_infectious*0.5)/(model_parms["population"]/model_parms["HHsize"]))),
                   R_I=as.numeric(model_parms["population"]*(initial_immune/100)*(initial_infectious/(model_parms["population"]/model_parms["HHsize"]))),
                   R_Ia_f=0,
                   R_Is_f=0,
                   R_I_b=0,
                   R_qR=0,
                   R_Ia_qa=0,
                   R_Is_qs=0,
                   R_Ia_q=0,
                   R_Is_q=0,
                   Recup=0,
                   D_wait=0,
                   D=0,
                   Hosp_wait=0,
                   Hosp=0,
                   ICU_wait=0,
                   ICU=0,
                   Detected=0,
                   TestsUsed=0,
                   CumCases=initial_infectious,
                   CumSymp=0,
                   CumAsymp=initial_infectious*model_parms["fa"],
                   CumSevere=0,
                   CumICU=0)
        
        inits["S_n"]<-as.numeric(model_parms["population"] -
                                         (inits["R_n"]+inits["R_I"]+inits["R_E"]+inits["Ia_f"]+inits["Ip_f"]+inits["E_f"]+
                                                  inits["E_sa"]+inits["E_ss"]+inits["S_I"]+inits["S_E"]))
        
        return(inits)
        
}
