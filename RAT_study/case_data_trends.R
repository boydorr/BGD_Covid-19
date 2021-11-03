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

# Make sure within correct folder of BGD_Covid-19 repo
# setwd("RAT_study/")
# use a theme for pretty plotting
theme <- theme_clean() + theme(axis.title=element_text(size=15), axis.text=element_text(size=12), 
                               legend.text=element_text(size=14), legend.title=element_text(size=15), 
                               plot.background = element_rect(color = "white"), 
                               strip.text=element_text(size=15))

# data
dhaka <-  readRDS(file = "data/dhaka_12_09_2021.rda"); names(dhaka)
pop.district <- read.csv("data/corrected_pop_district.csv")
pop <- subset(pop.district, district == "Dhaka")$population

# RAT validation phase of study data ONLY
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
p <- ggplot(cst_long, aes(x = date, y = tests)) +
  geom_col(aes(fill=type)) +
  labs(title = "", y = "Number", x = "Date") +
  scale_fill_viridis(discrete = T) +
  #scale_fill_manual(values = c("MTs" = "red", "CSTs" = "orange")) +
  theme_bw(base_size = 8) +  theme(legend.position = c(0.1,0.8), legend.key.size = unit(.5, 'cm')) # + scale_x_date(date_labels = "%b")
p
ggsave(p, file = "figs/RDT_testing.pdf", units = "cm", dpi = "retina", width = 15, height = 8)

# Extended data
cst_all <- read.csv("data/cst_data.csv"); dim(cst_all)
cst_all$date <- as.Date(cst_all$Date,"%d/%m/%Y"); head(cst_all); names(cst_all)

# Sample collection over periods
samples <- cst_all %>% 
  filter(date >= "2021-07-30" & date <= "2021-09-01") %>% 
  summarise(RDTs = sum(RDT, na.rm=TRUE),
            CSTs = sum(CST, na.rm=TRUE),
            MTs = sum(IEDCR, na.rm=TRUE))
samples$CSTs/sum(samples)


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

# Try weekly
cst_wkly <- cst_all %>% 
  mutate(wk = floor_date(date, "week")) %>% 
  arrange(wk) %>% 
  group_by(wk) %>%
  summarize(MT = sum(IEDCR),
            CST = sum(CST),
            RDT = sum(RDT),
            tested = sum(TOTAL))
head(cst_wkly)
  
cst_wkly_long <- gather(cst_wkly, key = "type", value = "samples", 
                       RDT,CST,MT, -wk) %>%
  mutate(type = factor(type), 
         type = fct_relevel(type, "CST", "RDT", "MT")) %>% 
  arrange(desc(type))
cst_wkly_long$samples[which(is.na(cst_wkly_long$samples))] <- 0

p<- cst_wkly_long %>% 
  filter(wk >= "2021-05-01" & wk <= "2021-08-25") %>% 
  ggplot(aes(x = wk, y = samples)) +
  geom_col(aes(fill=type)) +
  annotate("segment", x = ymd("2021-05-19"), xend = ymd("2021-07-10"), y = 500, yend = 500, colour = "black", alpha = 0.8,
           arrow = arrow(ends = "both", angle = 90, length = unit(.1,"cm"))) + 
  labs(title = "", y = "Samples collected", x = "Date") +
  #scale_fill_viridis(discrete = T) + 
  scale_fill_manual(values = c("MT" = "dark grey", "RDT" = "red", "CST" = "orange")) + 
  annotate("text", label = "rapid antigen testing", x=as.Date("2021-06-14"), y=530, size = 3) +
  annotate("segment", x = as.Date("2021-07-18"), xend = as.Date("2021-07-18"), y = 600, yend = 250,
           colour = "dark gray", size = 0.5, arrow = arrow(length = unit(.2,"cm"))) +
  annotate("text", label = "festival", x=as.Date("2021-07-18"), y=630, size = 3, color = "dark grey") +
  theme_bw(base_size = 8) +  scale_x_date(date_labels = "%b") +
  theme_clean() + theme(axis.title=element_text(size=15), axis.text=element_text(size=12), 
                        legend.text=element_text(size=12), legend.title=element_text(size=12), 
                        plot.background = element_rect(color = "white"), legend.background = element_rect(color = NA),
                        strip.text=element_text(size=15), legend.position = c(0.1,0.8))
p
ggsave(p, file = "figs/weekly_testing_dhaka.pdf", units = "cm", dpi = "retina", width = 16, height = 9)

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

################################################################
# Probability of a false negative RAT result broken down by day from self-reported symptom onset for primary contacts
# Data read in
metadat <- read.csv("data/metadata-phaseII.csv") 
RAT <- read.csv("data/RAT_phase2_w_PCR.csv")

# Match up IDs
RAT$id <- RAT$sample_id
dat <- inner_join(metadat, RAT) %>% 
  select(vvf_nasal, PCR_result, 
         Symptom_Onset, Specimen_Collected_Date)
# Select rows with full data
dat <- dat[complete.cases(dat),]

dat$vvf_nasal <- dat$vvf_nasal %>% recode(negative = 0,
                                          positive = 1,
                                          "NaN" = NaN)

# Calculate days between offset and testing
dat$Symptom_Onset <- mdy(dat$Symptom_Onset) %>% yday()
dat$Specimen_Collected_Date <- mdy(dat$Specimen_Collected_Date) %>% yday()
dat$days_since_offset <- dat$Specimen_Collected_Date - dat$Symptom_Onset 
dat <- dat[complete.cases(dat),] %>% 
  filter(days_since_offset > 0) %>% # Remove impossible negative values
  filter(days_since_offset <7) %>% # Remove improbably large values
  mutate(classification = vvf_nasal - PCR_result)

dat$classification[dat$classification == 0 & dat$PCR_result == 1] <- "TruePos"
dat$classification[dat$classification == 0 & dat$PCR_result == 0] <- "TrueNeg"
dat$classification[dat$classification == -1] <- "FalseNeg"
dat$classification[dat$classification == 1] <-  "FalsePos" 

# Calculate FNR counts by days since offset
dat_summ <- dat %>% 
  group_by(days_since_offset) %>%
  summarise(FalseNegSum = sum(classification == "FalseNeg"),
            FalsePosSum = sum(classification == "FalsePos"),
            TrueNegSum = sum(classification == "TrueNeg"),
            TruePosSum = sum(classification == "TruePos"),
            N = n())
# Convert to percentages
class_perc <- data.frame("days_since" = dat_summ$days_since_offset,
                         "FN" = dat_summ$FalseNegSum,
                         "FNPerc" = dat_summ$FalseNegSum/dat_summ$N,
                         "FPPerc" = dat_summ$FalsePosSum/dat_summ$N,
                         "TNPerc" = dat_summ$TrueNegSum/dat_summ$N,
                         "TPPerc" = dat_summ$TruePosSum/dat_summ$N,
                         "N" = dat_summ$N)

# Plot
p <- ggplot(class_perc, aes(x = factor(days_since), y = FNPerc)) +
  geom_col() +
  xlab("Days Since Symptom Onset") +
  ylab("False Negatives (%)") +
  theme_clean() + theme(axis.title=element_text(size=15), axis.text=element_text(size=12), 
                        legend.text=element_text(size=14), legend.title=element_text(size=15), 
                        plot.background = element_rect(color = "white"), 
                        strip.text=element_text(size=15))

p
ggsave(p, file = "figs/false_negs_days_since_offset.svg", units = "cm", dpi = "retina", width = 15, height = 12)
ggsave(p, file = "figs/false_negs_days_since_offset.pdf", units = "cm", dpi = "retina", width = 15, height = 12)

sum(class_perc$FN)
sum(class_perc$N)

############################################
# Summary of test comparion results
sum(class_perc$FN)
sum(class_perc$N)
fn <- sum(class_perc$FN)/sum(class_perc$N)
sens <- 0.68 # (adjusts up to 80-100% sensitivity!)
RDT <- 5
PCR <- 30
budget <- 1000
prev <- seq(0:100)/100

testing <- function(budget, prevalence, test1, test2, sensitivity1, sensitivity2){
  tested1 = budget/test1
  tested2 = budget/test2
  detected1 = tested1 * prevalence * sensitivity1
  missed1 = tested1 * prevalence * (1-sensitivity1)
  detected2 = tested2 * prevalence * sensitivity2
  missed2 = tested2 * prevalence * (1-sensitivity2)
  data.frame(RDTpos = detected1, PCRpos = detected2, RDTfalseneg = missed1, PCRfalseneg = missed2)
}

test_res <- sapply(prev, testing, 
                   budget=10000, test1 = 5, test2 = 30, 
                   sensitivity1 = 0.68, sensitivity2 = 0.99)

plot(prev, test_res[1,])
lines(prev, test_res[2,])
lines(prev, test_res[2,])

# taking results at face value
low_prev = testing(0.05, budget=10000, test1 = 5, test2 = 30, 
        sensitivity1 = 0.68, sensitivity2 = 0.99)
cost_per_RATpos = budget/low_prev$RDTpos; cost_per_RATpos
cost_per_PCRpos = budget/low_prev$PCRpos; cost_per_PCRpos
PCR_missed = low_prev$RDTpos - low_prev$PCRpos; PCR_missed

high_prev = testing(0.4, budget=10000, test1 = 5, test2 = 30, 
        sensitivity1 = 0.68, sensitivity2 = 0.99)
cost_per_RATpos = budget/high_prev$RDTpos; cost_per_RATpos
cost_per_PCRpos = budget/high_prev$PCRpos; cost_per_PCRpos
PCR_missed = high_prev$RDTpos - high_prev$PCRpos; PCR_missed

df <- rbind(low_prev, high_prev)
df_long <- gather(df, key = "type", value = "tests", 
                  RDTpos, PCRpos, RDTfalseneg, PCRfalseneg)
df_long$res <- c(rep("TP",4),rep("FN",4)) 
df_long$prev <- rep(c("low","high"),4) 
df_long$type <- c("RDT","RDT","PCR","PCR","RDT","RDT","PCR","PCR")

p <- ggplot(data=df_long, aes(x=type, y=tests, fill = res)) +
  geom_bar(stat="identity") +
  facet_wrap(~prev) +
  theme_clean() + theme(axis.title=element_text(size=15), axis.text=element_text(size=12), 
                        legend.text=element_text(size=12), legend.title=element_text(size=12), 
                        plot.background = element_rect(color = "white"), legend.background = element_rect(color = NA),
                        strip.text=element_text(size=15), legend.position = c(0.8,0.8)) + labs(fill = "Results") + scale_fill_brewer(palette="Paired")
p
ggsave(p, file = "figs/testing_comparison.pdf", units = "cm", dpi = "retina", width = 10, height = 15)



high_prev = testing(0.4, budget=10000, test1 = 5, test2 = 30, 
                    sensitivity1 = 0.8, sensitivity2 = 0.99)


