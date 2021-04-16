# Vector for each column of interest
Parameter = c("Introduction Date",
              
              "R<sub>0</sub>",
              "Daily transmission rate of asymptomatics",
              "Daily transmission rate of presymptomatics",
              "Daily transmission rate of symptomatics",
              
              "Mean duration of incubation period (days)",
              "Mean duration of asymptomatic infection (days)",
              "Mean duration of presymptomatic infection (days)",
              "Mean duration of symptomatic infection (days)",
              
              "Mean delay from symptoms onset to hospitalisation (general/ICU) (days)",
              "Mean delay from symptoms onset to death (days)",
              "Mean duration of stay in general hospital bed (days)",
              "Mean duration of stay in ICU bed (days)",
              "Mean duration of recuperation period (days)",
              
              "Household secondary attack rate",
              "Mean duration of household infection",
              "Mean duration of household infection following asymptomatic initial case",
              "Mean duration of household infection following symptomatic initial case",
              
              "% of transmission by symptomatics that occurs in the presymptomatic period",
              "% of symptomatic transmission achieved by asymptomatic cases in the absence of intervention",
              
              "% cases that are asymptomatic",
              "% symptomatic cases that are hospitalised",
              "% hospitalised cases that require ICU",
              "% symptomatic cases that die")

Value = c(as.character(intro_date),
          R0,
          round(parms_baseline["beta_a"],2),
          round(parms_baseline["beta_p"],2),
          round(parms_baseline["beta_s"],2),
          parms_baseline["dur_inc"],
          parms_baseline["dur_a"],
          parms_baseline["dur_p"],
          parms_baseline["dur_s"],
          parms_baseline["delay_hosp"],
          parms_baseline["delay_death"],
          parms_baseline["dur_hosp"],
          parms_baseline["dur_ICU"],
          parms_baseline["recuperation"],
          parms_baseline["propHHtrans"],
          parms_baseline["dur_hh"],
          parms_baseline["dur_hha"],
          parms_baseline["dur_hhs"],
          parms_baseline["propPresympTrans"]*100,
          parms_baseline["asympTrans"]*100,
          parms_baseline["fa"]*100,
          parms_baseline["fHosp"]*100,
          parms_baseline["probICU"]*100,
          parms_baseline["fd"]*100
          
 )

Description = c("",
                
                "The slider for R<sub>0</sub> allows uncertainty in this value to be explored.",
                
                "","","",
                
                "Comprises both latent period and presymptomatic period","","","",
                
                "","","","","",
                
                "Probability with which an infected individual transmits to each of their household contacts",
                "Mean duration for which households remain infected following introduction of the first infectious case","","",
                
                "","",
                
                "","","",""
                
)

References = c(paste("The first cases of COVID-19 in Bangladesh were confirmed on 08/03/2020, but it is likely the disease had started circulating undetected prior to this. Estimates from genomic data indicate an introduction in mid-February, with at least 8 introductions occurring before the ban on international travel (March 21st) <a class='table_a' href=http://medrxiv.org/content/early/2021/01/13/2021.01.05.21249196.abstract>(Cowley et al., 2021)</a>; we therefore initialise with 8 cases on 15/02/2020."), # Introduction Date
               
               "Tuned to match <a class='table_a' href=https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide>ECDC</a> death data for Bangladesh", #R0
               
               # Transmission rates
               "Estimated based on R<sub>0</sub>, the durations in each infectious category, 
                the % of transmission by symptomatics that occurs in the presymptomatic period, and 
                the % of symptomatic transmission achieved by asymptomatic cases in the absence of intervention",
               "as above",
               "as above",
               
               # Durations
               "<a class='table_a' href=https://doi.org/10.1101/2020.04.24.20073957>McAloon et al. (2020)</a>",#incubation
               "<a class='table_a' href=https://www.medrxiv.org/content/10.1101/2020.04.25.20079889v1>Byrne et al. (2020), </a>
               <a class='table_a' href=http://link.springer.com/10.1007/s11427-020-1661-4>Hu et al. (2020), </a>",#asymp
               "<a class='table_a' href=https://www.medrxiv.org/content/10.1101/2020.04.25.20079889v1>Byrne et al. (2020)</a>",#presymp
               "<a class='table_a' href=https://www.medrxiv.org/content/10.1101/2020.04.25.20079889v1>Byrne et al. (2020)</a>",#symp
               
               "<a class='table_a' href=https://www.mdpi.com/2077-0383/9/2/538>Linton et al. (2020), </a>
               <a class='table_a' href=https://dx.doi.org/10.1183%2F13993003.00562-2020>Liang et al. (2020), </a>",#delay_hosp
               "<a class='table_a' href=https://www.mdpi.com/2077-0383/9/2/538>Linton et al. (2020),</a>
               <a class='table_a' href=https://doi.org/10.1016/S1473-3099(20)30243-7>Verity et al. (2020), </a>",#delay_death
               "<a class='table_a' href=https://dx.doi.org/10.1186%2Fs12916-020-01726-3>Rees et al. (2020), </a>",#dur_hosp
               "<a class='table_a' href=https://dx.doi.org/10.1186%2Fs12916-020-01726-3>Rees et al. (2020), </a>",#dur_ICU
               "<a class='table_a' href=https://doi.org/10.1002/jmv.26368>Halpin et al. (2020), </a>",#recuperation
               
               "<a class='table_a' href=https://doi.org/10.1001/jamanetworkopen.2020.31756>Madewell et al. (2020), </a>",#secondary attack rate
               "A stochastic version of the SEIR dynamics was run 100,000 times within a household of the mean size in Bangladesh, using within-household transmission rates estimated from the secondary attack rate.  The mean time at which no infection remained in the household was calculated.",
               "Estimated using the appropriate subset of the household simulations described above",
               "Estimated using the appropriate subset of the household simulations described above",
               
               "<a class='table_a' href=https://wellcomeopenresearch.org/articles/5-58/v1>Liu et al. (2020)</a>", # % of transmission by symptomatics that occurs in the presymptomatic period
               "<a class='table_a' href=http://rs.yiigle.com/yufabiao/1186223.htm>Yi et al. (2020)</a>",# % of symptomatic transmission achieved by asymptomatic cases in the absence of intervention
               
               "Population average calculated from the age-dependent values below, and the age distribution of the Dhaka District population (see Forecast Upazilas tab defaults)",# % cases that are asymptomatic
               "Population average calculated from age-dependent values below, and the age distribution of the Dhaka District population (see Forecast Upazilas tab defaults)",# % symptomatic cases that are hospitalised
               "<a class='table_a' href=https://www.who.int/docs/default-source/coronaviruse/who-china-joint-mission-on-covid-19-final-report.pdf>World Health Organisation (2020), </a>",# % hospitalised cases that require ICU
               "Population average calculated from the age-dependent values below, and the age distribution of the Dhaka District population (see Forecast Upazilas tab defaults)"# % symptomatic cases that die
          )
               





