#########################################################################
# Use pre-prepared A2i data and develop time-series
#########################################################################
# Key learning outcomes are how to plot timeseries 
# and how to use loops to look at plots per district
# and pull out some specific statistics (overall and for specific districts)
#########################################################################

## Load libraries 
library(sf)
library(tidyverse)
library(lubridate)
library(viridis)
library(ggthemes)
library(zoo)
library(forcats)

# use a theme for pretty plotting
theme <- theme_clean() + theme(axis.title=element_text(size=15), axis.text=element_text(size=12), 
                               legend.text=element_text(size=14), legend.title=element_text(size=15), 
                               plot.background = element_rect(color = "white"), 
                               strip.text=element_text(size=15))

# data
dhaka <-  readRDS(file = "data/dhaka_12_09_2021.rda"); names(dhaka)
pop.district <- read.csv("data/corrected_pop_district.csv")
pop <- subset(pop.district, district == "Dhaka")$population
cst <- read.csv("data/cst_testing.csv")
cst$date <- as.Date(cst$date, "%d/%m/%Y")
cst %>% select(date, RDTs, PCRs, CST_identified, comments) %>% 
  summarise(tests = RDTs + PCRs,
            CST = RDTs/CST_identified,
            MT = PCRs/CST_identified)

cst_collected <- sum(cst$RDTs)
mt_collected <- sum(cst$PCRs)
cst_collected/(cst_collected + mt_collected)

cst_long <- gather(cst, key = "type", value = "tests", 
                   RDTs,PCRs, -date, -comments, -CST_identified) %>%
  mutate(type = factor(type, levels = c("RDTs", "PCRs")))

#'  * Examine CST testing data*
p <-ggplot(cst_long, aes(x = date, y = tests)) +
  geom_col(aes(fill=type)) +
  labs(title = "", y = "Number", x = "Date") +
  scale_fill_viridis(discrete = T) +
  #scale_fill_manual(values = c("MTs" = "red", "CSTs" = "orange")) +
  theme_bw(base_size = 8) +  theme(legend.position = c(0.1,0.8), legend.key.size = unit(.5, 'cm')) # + scale_x_date(date_labels = "%b")
p
ggsave(p, file = "figs/RDT_testing.pdf", units = "cm", dpi = "retina", width = 15, height = 8)

# Read in the extended data
cst_all <- read.csv("data/cst_data.csv"); dim(cst_all)
cst_all$date <- as.Date(cst_all$Date,"%d/%m/%Y"); head(cst_all); names(cst_all)
cst_all_long$

cst_all_long <- gather(cst_all, key = "type", value = "samples", 
                    RDT,CST,IEDCR, -date, -Comments) %>%
  mutate(type = recode(type, IEDCR = "MT"),
         type = factor(type), 
         type = fct_relevel(type, "CST", "RDT", "MT"),
         samples[which(is.na(samples))] <- 0)  %>% 
  arrange(desc(type))

#'  * % of samples collected by CST*
p <- cst_all_long %>% 
  filter(date >= "2021-05-01" & date <= "2021-09-01") %>% 
  ggplot(aes(x = date, y = samples)) +
  geom_col(aes(fill=type)) +
  annotate("segment", x = ymd("2021-05-19"), xend = ymd("2021-07-10"), y = 100, yend = 100, colour = "black", alpha = 0.8,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) + 
  labs(title = "", y = "Number", x = "Date") +
  scale_fill_viridis(discrete = T) + #scale_fill_manual(values = c("PCRs" = "red", "MTs" = "orange")) + 
  theme_bw(base_size = 8) +  theme(legend.position = c(0.1,0.8)) + scale_x_date(date_labels = "%b")
p
ggsave(p, file = "figs/daily_testing_dhaka.pdf", units = "cm", dpi = "retina", width = 15, height = 10)


# dhaka overall trends
data.daily <- dhaka %>% 
  group_by(date) %>% 
  summarise(cases=sum(cases),
            tests=sum(tests)) %>% 
  ungroup %>% arrange(date) %>%
  mutate(cum_cases = cumsum(cases),
         cum_tests = cumsum(tests),
         cases_07 = floor(rollmean(cases, k = 7, fill = NA, align="right")),
         tests_07 = floor(rollmean(tests, k = 7, fill = NA, align="right")),
         tests.100k = tests*100000/pop,
         cases.100k = cases*100000/pop) %>%
  mutate(positivity = round(cases/tests, 2),
         positivity_07 = round(cases_07/tests_07, 2))
dhaka_subset <- subset(data.daily, date > "2021-03-01" & date < "2021-10-01")

p <- ggplot(dhaka_subset, aes(x=date)) + 
  geom_line(aes(y = cases, color="Cases"), alpha=0.4) +
  #geom_line(aes(y = tests, color="Tests"), alpha=0.4) +
  theme +
  scale_x_date(date_breaks="1 month", date_labels = "%b") + 
  scale_y_continuous(sec.axis = sec_axis(~ .*10, name = "Tests")) + 
  labs(x="2020 - 2021", y="Cases", color="Legend") +
  theme(legend.position = c(0.2, 0.8), legend.title = element_blank(), legend.background = element_blank())
p


#'----------------------------------------------------------------
#'  * Examine daily cases and daily tests*
p <- ggplot(data.daily, aes(x=date)) + 
      geom_line(aes(y = cases, color="Cases"), alpha=0.4) +
      geom_line(aes(y = tests, color="Tests"), alpha=0.4) +
      theme +
      scale_x_date(date_breaks="1 month", date_labels = "%b") + 
      scale_y_continuous(sec.axis = sec_axis(~ .*10, name = "Tests")) + 
      labs(x="2020 - 2021", y="Cases", color="Legend") +
  theme(legend.position = c(0.2, 0.8), legend.title = element_blank(), legend.background = element_blank())
p
ggsave(p, file = "figs/daily_cases_dhaka.pdf", units = "cm", dpi = "retina", width = 20, height = 15)

# Daily cases and daily tests per 100,000
p <- ggplot(data.daily, aes(x=date)) + 
        geom_line(aes(y = cases.100k,color="cases.100k"), alpha=0.5) +
        geom_smooth(aes(y = cases.100k, color="cases.100k"), span=0.15) +
        geom_line(aes(y = tests.100k,color="tests.100k"), alpha=0.5) +
        geom_smooth(aes(y = tests.100k, color="tests.100k"), span=0.15) +
        theme +
        scale_x_date(date_breaks="1 month", date_labels = "%b") + 
        labs(x="", y="Cases & Tests per 100,000", color="Legend") +
  theme(legend.position = c(0.2, 0.8), legend.title = element_blank(), legend.background = element_blank())
p
ggsave(p, file = "figs/daily_cases_tests_100k_BGD_trend.svg", units = "cm", dpi = "retina", width = 15, height = 12)

# Daily test positivity
p <- ggplot(data.daily, aes(x=date)) + 
        geom_line(aes(y = positivity), color="forestgreen", alpha=0.5) +
        geom_smooth(aes(y = positivity), color="forestgreen", span=0.15) +
        theme +
        scale_x_date(date_breaks="1 month", date_labels = "%b") + 
        labs(x="", y="Positivity (cases/tests)", color="Legend")
p
ggsave(p, file = "figs/daily_positivity_BGD_trend.svg", units = "cm", dpi = "retina", width = 15, height = 12)

