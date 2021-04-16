library(dplyr)

ECDCweekly <- read.csv("data/ECDCweekly05_01_2021.csv")

(final2020deaths <- ECDCweekly %>% 
  filter(year_week=="2020-52" & indicator=="deaths" & grepl("(total)",country)==F) %>%
  summarise(sum(cumulative_count)))

(final2020deaths <- ECDCweekly %>% 
    filter(year_week=="2020-52" & indicator=="cases" & grepl("(total)",country)==F) %>%
    summarise(sum(cumulative_count)))

