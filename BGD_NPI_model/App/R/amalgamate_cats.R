#
## Function to add new categories to model output by summing other categories 
#__________________________________________

# Arguments
#   modelOutput = output from simulating from the covid model: a dataframe describing the number of 
#                 individuals in each disease state through time

# Output
#   daily vector of proportion working days lost of length nrow(modelOutput)



#
## Function to add new categories to model output by summing other categories 
#__________________________________________

# Arguments
#   modelOutput = output from simulating from the covid model: a dataframe describing the number of 
#                 individuals in each disease state through time

# Output
#   daily vector of proportion working days lost of length nrow(modelOutput)



amalgamate_cats <- function(modelOutput){ 
  
  modelOutput$S <- modelOutput$S_n + modelOutput$S_E + modelOutput$S_I + modelOutput$S_q
  modelOutput$E <- modelOutput$E_f + modelOutput$E_b + modelOutput$E_ss + modelOutput$E_sa + modelOutput$E_t + modelOutput$E_q + modelOutput$E_qE
  modelOutput$Ip <- modelOutput$Ip_f + modelOutput$Ip_b + modelOutput$Ip_s + modelOutput$Ip_sa + modelOutput$Ip_q + modelOutput$Ip_qp
  modelOutput$Ia <- modelOutput$Ia_f + modelOutput$Ia_b + modelOutput$Ia_s + modelOutput$Ia_sa + modelOutput$Ia_q + modelOutput$Ia_qa
  modelOutput$Is <- modelOutput$Is_f + modelOutput$Is_b + modelOutput$Is_s + modelOutput$Is_q + modelOutput$Is_qs
  modelOutput$R <- modelOutput$R_n + modelOutput$R_E + modelOutput$R_I + modelOutput$R_Ia_f + modelOutput$R_Is_f + modelOutput$R_I_b + modelOutput$R_qR + modelOutput$R_Ia_qa + modelOutput$R_Is_qs + modelOutput$R_Ia_q + modelOutput$R_Is_q
  
  return(modelOutput)
  
}

