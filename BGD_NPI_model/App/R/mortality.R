# Function to calculate excess mortality in a given population
rel_mortality = function(LE, pop, deaths_ts){

  # Set up estimate of daily deaths
  death_rate = 1/LE
  annual_deaths = death_rate * pop
  deaths_per_day = annual_deaths/365

  # calculate daily covid deaths from model output
  daily_covid_deaths = c(0, diff(deaths_ts))

  # print excess daily deaths from covid
  daily_covid_deaths/deaths_per_day
  }

# LE = 72.05 # Life expectancy in Bangladesh
# pop = Now 21,000,000 # 8900000 # Dhaka population!
# deaths_ts = out_intervention$D # modelled deaths
# xs_deaths = rel_mortality(LE, pop, deaths_ts)
# plot(xs_deaths)



