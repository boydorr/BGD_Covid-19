
# Costs of each strategy (defaults to high cost)
costs = function(out,parms, advertising_monthly=4832+24780/6, 
                 CST_training=200,nCST=2000, CST_monthly=240, 
                 mask_per_household=1*5, 
                 bed_cost_general=266/7, bed_cost_icu=649/7){
  
  lockdown_advertising_cost <- max(1,round((parms["ld_end"]-parms["ld_start"])/(365/12)))*advertising_monthly 
  
  mask_distribution_cost <- parms["mask"]*(mask_per_household*(parms["population"]/parms["HHsize"]))
  mask_advertising_cost <- max(1,round((parms["mask_end"]-parms["mask_start"])/(365/12)))*advertising_monthly 
  mask_total_cost <- mask_distribution_cost + mask_advertising_cost
  
  # nCST <- (parms["community"]*(parms["population"]/parms["HHsize"]))/20
  CST_training_cost <- parms["syndromic"]*CST_training*nCST
  CST_running_cost <- parms["syndromic"]*(CST_monthly/(365/12))*(parms_baseline["syn_end"]-parms_baseline["syn_start"])*nCST
  CST_advertising_cost <- max(1,round((parms["syn_end"]-parms["syn_start"])/(365/12)))*advertising_monthly 
  CST_total_cost <- CST_training_cost + CST_running_cost + CST_advertising_cost
  
  deaths <- max(out$D)
  
  healthcare_cost <- sum(out$Hosp*bed_cost_general + out$ICU*bed_cost_icu)
  
  data.frame(deaths = deaths, 
             lockdown_advertising = lockdown_advertising_cost,
             healthcare = healthcare_cost, 
             CST_training = CST_training_cost,
             CST_running = CST_running_cost,
             CST_advertising =  CST_advertising_cost,
             CST = CST_total_cost, 
             mask_distribution = mask_distribution_cost,
             mask_advertising = mask_advertising_cost,
             mask = mask_total_cost,
             total = lockdown_advertising_cost + healthcare_cost + mask_total_cost + CST_total_cost)
}



