# Vector for each column of interest
int_parameter = c("Lockdown period",
                  "% population permitted to work under lockdown",
                  "Maximum lockdown compliance",
                  "Lockdown impact",
                  "Lockdown scale-up days",
                  "Rate of decline in lockdown compliance following scaling up period",
                  "Compulsory mask wearing period",
                  "Days to scale up mask wearing",
                  "Mask compliance",
                  "Mask impact on emissions from wearer",
                  "Mask impact on transmission to wearer",
                  "Lab testing period",
                  "Days to scale up lab testing",
                  "Lab testing capacity",
                  "Lab test false Negative probability",
                  "Cost of a lab test",
                  "Community surveillance and rapid testing period",
                  "Days to scale up community surveillance",
                  "Quarantine adherence",
                  "Rapid testing capacity",
                  "Quarantine duration (days)",
                  "Rapid test false Negative probability",
                  "Cost of a rapid test",
                  "Non-COVID-19 respiratory illness")

int_value = c(paste(start_date + parms_baseline["ld_start"],"-<br/>", start_date + parms_baseline["ld_end"]),
              parms_baseline["fEW"]*100,
              round((1-parms_baseline["fNC"])*100),
              parms_baseline["ld_effect"]*100,
              parms_baseline["ld_improve"],
              paste(HTML("&mu;="),round(((1-parms_baseline["fNC"])-parms_baseline["ld_min_compliance"])*(1+exp((-parms_baseline["ld_sigmoid_mid"])*parms_baseline["ld_decline"])) + parms_baseline["ld_min_compliance"],2),"<br/>",
                    HTML("&nu;="),parms_baseline["ld_min_compliance"],"<br/>",
                    HTML("&eta;="),round(parms_baseline["ld_sigmoid_mid"]),"<br/>",
                    HTML("&rho;="),round(parms_baseline["ld_decline"],2)),
              paste(start_date + parms_baseline["mask_start"],"-<br/>", start_date + parms_baseline["mask_end"]),
              parms_baseline["mask_improve"],
              parms_baseline["mask_compliance"]*100,
              parms_baseline["mask_effect_outward"],
              parms_baseline["f_mask_effect_inward"],
              paste(start_date + parms_baseline["lab_start"],"-<br/>",start_date + parms_baseline["lab_end"]),
              parms_baseline["lab_improve"],
              parms_baseline["capacity_lab"],
              parms_baseline["lab_fneg"],
              parms_baseline["lab_cost"],
              paste(start_date + parms_baseline["syn_start"],"-<br/>",start_date + parms_baseline["syn_end"]),
              parms_baseline["syn_improve"],
              parms_baseline["community"],
              parms_baseline["capacity_rapid"],
              parms_baseline["quarantine"],
              parms_baseline["rapid_fneg"],
              parms_baseline["rapid_cost"],
              paste(parms_baseline["mild_nonCovid"],"<sub>(mild)</sub><br>", parms_baseline["severe_nonCovid"],"<sub>(hospitalised)</sub>"))

int_description = c("Start and end dates of the lockdown period, which can be edited using the slider. A second slider is provided for the dates of the (optional) second non-overlapping lockdown period",
                    
                    "% of the population that continues to work throughout the lockdown, unless symptomatic. This can be adjusted using the sliders for both the initial and the optional secondary lockdown periods.",
                    
                    "Peak % of the population that complies with the lockdown. This can be adjusted using the sliders for both the initial and the optional secondary lockdown periods.",
                    
                    "% by which between-household transmission drops during lockdown for people who are compliant with the lockdown.",
                    
                    "Days until full effect of lockdown is reached. The impact of the lockdown on transmission increases linearly during this period. This can be adjusted for both the initial and the optional secondary lockdown periods.",
                    
                    "When (lockdown start day + days to full effectiveness)&le;t&ltlockdown end day, lockdown compliance declines according to a sigmoidal function:
                    <br>% compliance(t) = (&mu;-&nu;)/(1+e<sup>(-&rho;*(t - lockdown start day - scale-up days - &eta;))</sup>) +  &nu;
                    <br>&mu;, &rho;, and &eta; were estimated from <a class='table_a' href=https://www.google.com/covid19/mobility/>Google community mobility data</a>.  
                    &nu;, the minimum that the proportion of people that comply with lockdown can fall to, is assumed.  
                    The maximum lockdown compliance (see above) is achieved when t=lockdown start day + scale-up days.",

                    "Start and end dates of the compulsory mask wearing period, which can be edited using the slider.",
                    
                    "Days until full effect of mask wearing is reached. The impact of the mask wearing on transmission increases linearly during this period. This can be adjusted in the sidebar.",
                    
                    "% of the population that complies with mask wearing following the scale-up period.  This can be adjusted using the slider",
                    
                    "Proportion by which masks lower the wearer's virus emissions outside of the household.",
                    "The proportion by which masks lower the exposure of wearers to the virus outside of the household is taken to be this value multiplied by the impact of masks in preventing emissions from the wearer (above).",
                    
                    "Start and end dates for laboratory testing, which can be edited using the slider.",
                    
                    "Days until full lab testing capacity is reached (can be adjusted using the slider). The capacity increases linearly during this period.",

                    "Number of lab tests that can be analysed per day (can be adjusted using the slider)",
                    
                    "Probability of obtaining a false negative in laboratory testing.",
                    
                    "",

                    "Start and end dates for community surveillance, which can be edited using the sliders.",

                    "Days until full syndromic surveillance and rapid testing capacities are reached (can be adjusted using the slider). Both the rapid testing capacity and the proportion of people with mild symptoms that are reached by and comply with messaging from community health workers increases linearly during this period.",
                    
                    "Proportion of households in which a symptomatic individual occurs that are reached by and comply with messaging from community health workers to quarantine their households.",
                    
                    "Number of rapid tests that can be analysed per day (can be adjusted using the slider)",
                    
                    "Days for which households detected by community surveillance must quarantine",

                    "Probability of obtaining a false negative in rapid testing.",
                    
                    "",
                    
                    "The proportions of people expected to have mild and severe (hospitalised) non-COVID-19 respiratory illnesses over the course of a year.  Since the symptoms of individuals with these illnesses are consistent with those of COVID-19, we assume that tests for COVID-19 are also targeted at this section of the population. Under community surveillance these households are assumed to also be instructed to quarantine, affecting working days lost."
)

# For links, make sure you have class='table_a' within the <a ... > code to match the link styling
int_references = c("", 
                   "", 
                   "", 
                   "", 
                   "", 
                   "", 
                   "", 
                   "", 
                   "", 
                   "", 
                   "", 
                   "", 
                   "", 
                   "", 
                   "", 
                   "", 
                   "", 
                   "", 
                   "", 
                   "", 
                   "",
                   "",
                   "",
                   "") 




