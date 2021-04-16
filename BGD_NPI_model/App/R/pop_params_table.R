# Vector for each column of interest
pop_parameter = c("Population",
                  "Proportion employed",
                  "Proportion at-home carers",
                  "Average household size",
                  "Hospital beds")

pop_value = c(parms_baseline["population"], 
              parms_baseline["propWorkers"],
              parms_baseline["propCarers"], 
              parms_baseline["HHsize"], 
              parms_baseline["beds"]
              )

pop_description = c("The population size of Dhaka District",

                    "The proportion of the population that is employed is used to estimate working days lost by people who are affected by infection or the interventions.
                      Workers who die are assumed to lose all future working days, while those who are in symptomatic infectious categories lose the working days that they are in those categories.
                      Individuals who are hospitalised also lose an extra two weeks of work while they recuperate.",
                    
                    "The proportion of the population that is unemployed, but works at home (e.g. with caring responsibilities) is used in the estimation of working days lost.
                      When an individual in this category is hospitalised, is recuperating following release from hospital, or dies, it is assumed that an individual in the working population may have to stop working to take over caring responsibilities.",
                    
                    "Average household size is used in the calculation of worker days lost to determine how many people need to quarantine (losing any working days) alongside an individual that develops symptoms during the community surveillance period.
                      It is also used to determine the number of people who lose a week's working days to grieving when a household member dies of COVID-19.",
                    
                    "The number of hospital beds in Dhaka is used to inform the plot of hospital demand."
                    
                    
)

# For links, make sure you have class='table_a' within the <a ... > code to match the link styling
pop_references = c("<a class='table_a' href=https://population.un.org/wpp/Download/Standard/Population/>UN,</a>
                   <a class='table_a' href='http://203.112.218.65:8008/Census.aspx?MenuKey=43'>Bangladesh Bureau of Statistics</a>", # Population
               "<a class='table_a' href='http://203.112.218.65:8008/Census.aspx?MenuKey=43'>Bangladesh Bureau of Statistics</a>", # Proportion employed
               "<a class='table_a' href='http://203.112.218.65:8008/Census.aspx?MenuKey=43'>Bangladesh Bureau of Statistics</a>", # Proportion at-home carers
               "<a class='table_a' href='http://bbs.portal.gov.bd/sites/default/files/files/bbs.portal.gov.bd/page/b343a8b4_956b_45ca_872f_4cf9b2f1a6e0/Comparative Matrix HIES_fnl.pdf'>Bangladesh Bureau of Statistics, Household Income and Expenditure Survey (HIES) 2016</a>", # Average household size
               "<a class='table_a' href='http://www.who.int/data/gho/data/indicators/indicator-details/GHO/hospital-beds-(per-10-000-population)'>World Health Organisation</a>") # Hospital beds
# # Describe lockdown situation
# # reduces symptomatic transmission to ZERO, for proportion of nonlocked down persons
# # In bangladesh as of May 5th, R = 1.2: https://epiforecasts.io/covid/posts/national/bangladesh/
# trans_types <- pc_trans(parms_baseline) # pre-symptomatic transmission, asymptomatic, symptomatic
# R0 <- calc_R0(parms_baseline)
# lockdown_pc <- (1-0.05)*(1-0.2)
# R_lockdown <- (trans_types[1] + trans_types[2])*R0  + (1-lockdown_pc)*trans_types[3] # R_lockdown
# 





