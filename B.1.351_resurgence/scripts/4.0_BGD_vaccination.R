# Examine vaccination coverage data for BGD
# Katie Hampson- 9 April 2021
library(tidyverse)
library(zoo)
# remotes::install_github("joachim-gassen/tidycovid19")
library(tidycovid19)

# see https://joachim-gassen.github.io/2021/01/vaccination-data-an-outlier-tale/
##########################################################
df <- download_merged_data(cached = TRUE, silent = TRUE)

BGD <- df %>%
  filter(iso3c == "BGD") %>%
  mutate(
    new_cases = confirmed - lag(confirmed),
    ave_new_cases = rollmean(new_cases, 7, na.pad=TRUE, align="right"))
names(BGD)

BGD %>% 
  filter(!is.na(new_cases), !is.na(ave_new_cases)) %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = new_cases), stat = "identity", fill = "lightblue") +
  geom_line(aes(y = ave_new_cases), color ="red") +
  theme_minimal()

BGD %>% 
  filter(!is.na(total_vaccinations)) %>%
  ggplot(aes(x = date, y = total_vaccinations)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "", y = "Number", x = "Month") +
  xlim(as.Date("2021-01-31"), as.Date("2021-04-01")) +
  theme_minimal()

##########################################################
# Summarize numbers and % vaccinated - consider mostly administered to Dhaka population
dhaka = 18305671 # https://en.wikipedia.org/wiki/Dhaka_District

summary_BGD <- BGD %>% 
  filter(date>"2021-01-01" & date<"2021-04-01") %>% 
  mutate(
    vacc_1e5pop = 1e5*(total_vaccinations/population),
    cases_1e5pop = 1e5*(confirmed/population),
    deaths_1e5pop = 1e5*(deaths/population)
  ) %>%
  summarise(
    population = max(population, na.rm = TRUE),
    vax = max(total_vaccinations, na.rm = TRUE),
    cases = max(confirmed, na.rm = TRUE),
    mortality = max(deaths, na.rm = TRUE),
    vacc_pc_1dose  = max(total_vaccinations/population, na.rm = TRUE)*100,
    vacc_pc_2dose  = max((total_vaccinations/2)/population, na.rm = TRUE)*100,
    vacc_dhaka_1dose  = max(total_vaccinations/dhaka, na.rm = TRUE)*100,
    vacc_dhaka_2dose  = max((total_vaccinations/2)/dhaka, na.rm = TRUE)*100,
    vacc_1e5pop  = max(vacc_1e5pop, na.rm = TRUE),
    cases_1e5pop = max(cases_1e5pop, na.rm = TRUE),
    deaths_1e5pop = max(deaths_1e5pop, na.rm = TRUE),
    .groups = "drop"
  ) %>% na.omit() 
summary_BGD

# Dates of vaccinations
range(BGD$date[which(BGD$total_vaccinations>0)]) # 27 Jan - 7 April

