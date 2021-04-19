
# Costs of each strategy (defaults to high cost)
costs = function(out,parms, 
                 lockdown_advertising=150000, 
                 CST_training=200,nCST=2000, CST_monthly=240, mask_per_person=1.2*2, mask_advertising=150000, bed_cost=200,
                 rapid_test=5,lab_test=10){
  
  lockdown_advertising_cost <- parms["ld"]*lockdown_advertising 
  
  mask_distribution_cost <- parms["mask"]*(mask_per_person*parms["population"])
  mask_advertising_cost <- parms["mask"]*mask_advertising
  mask_total_cost <- mask_distribution_cost + mask_advertising_cost
    
  # nCST <- (parms["community"]*(parms["population"]/parms["HHsize"]))/20
  CST_training_cost <- parms["syndromic"]*CST_training*nCST
  CST_running_cost <- parms["syndromic"]*(CST_monthly/(365/12))*(parms_baseline["syn_end"]-parms_baseline["syn_start"])*nCST
  CST_total_cost <- CST_training_cost + CST_running_cost
  
  deaths <- max(out$D)
  
  testing_cost <- max(out$LabUsed)*lab_test+max(out$RapidUsed)*rapid_test
  
  healthcare_cost <- sum(out$Hosp + out$ICU)*bed_cost

  data.frame(deaths = deaths, 
             lockdown_advertising = lockdown_advertising_cost,
             healthcare = healthcare_cost, 
             CST_training = CST_training_cost,
             CST_running = CST_running_cost,
             CST = CST_total_cost, 
             mask_distribution = mask_distribution_cost,
             mask_advertising = mask_advertising_cost,
             mask = mask_total_cost,
             testing = testing_cost,
             total = lockdown_advertising_cost + healthcare_cost + mask_total_cost + CST_total_cost + testing_cost)
}



