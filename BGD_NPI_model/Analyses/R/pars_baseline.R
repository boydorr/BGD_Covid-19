

# Start date for all outputs
start_date <- as.Date("2020-01-01")

# End date for model
if(!is.element("end_date",ls())){end_date <- as.Date("2020-12-31")}

# time periods of interest
times <- seq(0, as.numeric(end_date-start_date), 1)
intro_date <- as.Date("2020-02-15")
times_model <- c(as.numeric(intro_date-start_date):as.numeric(end_date-start_date))


## Load age dependent disease stage probabilities
age_dep_pars <- read.csv("data/age_dep_CFR_hosp_asymp.csv")

## Demographic data
# bangladesh_pop_by_age <- read.csv("data/bangladesh_pop_by_age_census2011_10yearBands.csv")
dhaka_pop_by_age <- read.csv("data/dhaka_dist_pop_by_age_census2011_10yearBands.csv")



# Estimate 2020 Dhaka population
dhakapop2011 <- 12043977
bdeshpop2011 <- 144043697
bdeshpop2020 <- 164689000 # UN estimate https://population.un.org/wpp/Download/Standard/Population/
propDhakaPop <- dhakapop2011/bdeshpop2011
dhakapop2020 <- round(bdeshpop2020*propDhakaPop)


# Parameters for baseline scenario (with just standard lockdown)
parms_baseline <- c(R0=3.515,
                    dur_inc=5.8, dur_p=2, dur_s=7, dur_a=7.7, # duration of incubation & residence in each infectious class (days)
                    dur_ICU=7, dur_hosp=5,delay_ICU=7,delay_hosp=7,delay_death=20.2, 
                    dur_hh=10.56,dur_hha=9.87,dur_hhs=12.16,
                    ld=T, # Is there a lockdown period?
                    ld_effect=0.759, # By what proportion do non-household transmissions drop during lockdown for pre/asymptomatic compliant non-workers
                    ld_start=85, ld_end=85+35+32, # When does the lockdown start (26 Mar) and end (1 Jun)
                    ld_improve=0,# How many days does it take for the full effect of the lockdown to be reached?
                    fEW=0.52*0.326, # What proportion of the population is essential workers?
                    fNC=0.06673928, # What is the initial (max) proportion of people that are non-compliant to lockdown?
                    ld_decline=0.08072955, # At what rate does compliance drop?
                    ld_min_compliance=0.3, # what is the minimum compliance to which the lockdown can drop during the declining phase?
                    ld_sigmoid_mid = 47.87292115, # location (in days after ld_start) of the lockdown compliance function's inflection point
                    ld2=F, # Is there a second lockdown period?
                    ld2_start=85+35+32, ld2_end=85+35+32+60, # When does the lockdown start and end
                    fEW2=0.52*0.326, # What proportion of people are essential workers?
                    fNC2=0.3, # What is the initial (max) proportion of people that are non-compliant to lockdown 2?
                    ld2_improve=0, # How many days does it take for the full effect of the lockdown to be reached?
                    lab = F, syndromic = F, # Testing is introduced - laboratory (severe/critical cases) and syndromic (mild cases)
                    capacity_lab = 100, capacity_rapid=1000, community = 0.8, # Testing capacity - for lab tests and for community HWs supporting isolation
                    lab_start=90, lab_end=max(times_model)+1, # When does lab testing start and end
                    syn_start=152-7, syn_end=max(times_model)+1, # When does syndromic testing start and end
                    lab_improve=7, # How many days does it take for the full effect of the lab testing to be reached?
                    syn_improve=7, # How many days does it take for the full effect of the syndromic testing to be reached?
                    lab_fneg=0.05, # false negative probability for lab test
                    rapid_fneg=0.05, # false negative probability for rapid tests
                    severe_nonCovid = 0.035, # proportion of people with severe respiratory symptoms not caused by covid-19 over a year
                    mild_nonCovid = 0.35, # proportion of people with mild respiratory symptoms not caused by covid-19 over a year
                    nonCovidHH = 0.23, # proportion of non-covid ILI cases that occur in same household as another ILI case
                    lab_cost=10, # Temporary number for cost of lab test
                    rapid_cost=2, # Temporary number of cost of rapid test
                    mask = F, # Are masks used?
                    mask_start=152-7, mask_end=max(times_model)+1, # When does mask wearing start and end
                    mask_effect_outward=0.5, # By what proportion does mask wearing reduce viral emissions  from infectious individuals
                    f_mask_effect_inward=0.5, # What proportion of mask_effect_outward is the impact of masks in protecting the wearer from infection
                    mask_compliance=0.8, # What proportion of people comply with mask wearing
                    mask_improve=7, # How many days does it take for the full effect of mask wearing to be reached?
                    probHHtrans=0.166, # probability of infecting each other household member
                    propPresympTrans=0.23, # proportion of symptomatic transmission that occurs in the presymptomatic period
                    asympTrans=0.65, # Multiplier of symptomatic transmission achieved by asymptomatic cases in the absence of intervention
                    HHsize=4, # average household size in Bangladesh
                    propWorkers = 0.52, # proportion of the population that is in work
                    gd = 7, # grieving days for other household members of each person that died
                    recuperation = 21, # recuperation days for those recovering from sever/critical illness 
                    propCarers = 0.23, # what of people are at home workers/carers who will need to be replaced by a worker if ill/dead
                    dur_q = 14, # length of household quarantine
                    population = dhakapop2020, # Dhaka metropolitan in 2020 
                    probICU = 0.31 # probability that a hospitalised case requires ICU
)




# Fraction of cases reaching each disease state
parms_baseline <- c(parms_baseline, calc_fractions(age_dep_pars,dhaka_pop_by_age))


# Estimate transmission rates based on baseline parameters
#---------------------------
## Given
## R0 <- 
##   fa*beta_a*dur_a + # asymptomatic transmission
##   (1-fa)*(beta_p*dur_p + beta_s*dur_s) # symptomatic transmission

## If propPresympTrans of the total transmission by a symptomatic individual is in the presymptomatic period, then
## beta_p*dur_p=(propPresympTrans/(1-propPresympTrans))*beta_s*dur_s

## and given the above result and that asymptomatic cases produce on average asympTrans of the cases asymptomatics do:
## beta_a*dur_a = asympTrans*(propPresympTrans/(1-propPresympTrans) + 1)*beta_s*dur_s

## R0 <- fa*asympTrans*(propPresympTrans/(1-propPresympTrans) + 1)*beta_s*dur_s + 
##   (1-fa)*((propPresympTrans/(1-propPresympTrans))*beta_s*dur_s + beta_s*dur_s)

parms_baseline["beta_s"] <- parms_baseline["R0"]/(parms_baseline["fa"]*(parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["dur_s"] + 
                                  (1-parms_baseline["fa"])*((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["dur_s"] + 
                                                              parms_baseline["dur_s"]))
parms_baseline["beta_p"] <- ((parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_p"]
parms_baseline["beta_a"] <- ((parms_baseline["asympTrans"]*(parms_baseline["propPresympTrans"]/(1-parms_baseline["propPresympTrans"]) + 1))*parms_baseline["beta_s"]*parms_baseline["dur_s"])/parms_baseline["dur_a"]

beds <- 10947 



