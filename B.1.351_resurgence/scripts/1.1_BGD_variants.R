# Examine variant frequency from nextstrain
# Katie Hampson- 12 April 2021
git.path <- "insert your git folder here/"
git.path <- "D:/GITHUB/"
git.path <- "/Users/katiehampson/Github/"

library(dplyr)
library(tidyr)
library(tidyverse)
library(zoo)
library(ggthemes)
library(svglite)
library(Hmisc)

# Import LATEST sequence metadata - 19 April 2021 - 
seq1 <- read_tsv(paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/data/nextstrain_community_CHRF-Genomics_ncovBangladesh@main_metadata_20210407.tsv")) 
seq2 <- read_tsv(paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/data/nextstrain_community_CHRF-Genomics_ncovBangladesh@main_metadata_20210419.tsv"))

# theme
theme <- theme_clean() + theme(axis.title=element_text(size=15), axis.text=element_text(size=13), 
                               legend.text=element_text(size=15), legend.title=element_text(size=15), 
                               plot.background = element_rect(color = "white"), legend.background = element_rect(color = NA),
                               strip.text=element_text(size=15))

# Summarize clades by year and month
var_y <- seq2 %>%
  mutate(year = year(`Collection Data`), 
         month = factor(floor_date(`Collection Data`, "month"))) %>% 
  group_by(year, Clade) %>%
  summarise(clade_n = n())

# lineage/ clade info
clades <- unique(seq2$Clade)
variants <- c("19B","20A","20C","20H/501Y.V2 (B.1.351)","20B","20I/501Y.V1 (B.1.1.7)")
clade_col <-c("grey43", "dark grey", "light grey", "red", "grey", "orange")
factor(clades, levels=clades[1:6])

var_m <- seq2 %>%
  mutate(year = year(`Collection Data`), 
         month = factor(floor_date(`Collection Data`, "month")),
         Clade = factor(Clade, levels = clades[c(1:3,5,6,4)])) %>% 
  group_by(month, Clade) %>%
  summarise(clade_n = n()) %>% 
  mutate(freq = clade_n/sum(clade_n)) 

# date of first and last detection
range(subset(seq2, Clade == "20H/501Y.V2")$"Collection Data")
range(subset(seq2, Clade == "20I/501Y.V1")$"Collection Data")

# Stacked barplot of lineages by month
p1 <- var_m %>% ggplot(aes(x = month, y = clade_n)) +
  geom_col(aes(fill=Clade)) +
  labs(title = "", y = "Sequences", x = "", fill="Lineages") +
  scale_fill_manual(values = clade_col[c(1:3,5,6,4)]) +
  theme_bw(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
p1
ggsave(p1, file = paste0(git.path,"BGD_Covid-19/B.1.351_resurgence/output/variant_freq_19April2021.pdf"), units = "cm", dpi = "retina", width = 12, height = 8)
ggsave(p1, file = paste0(git.path,"BGD_Covid-19/B.1.351_resurgence/output/variant_freq_19April2021.png"), units = "cm", dpi = "retina", width = 12, height = 8)

# Spread variants out so they each have a column
var_m_df <- var_m <- seq2 %>%
  mutate(year = year(`Collection Data`), 
         month = factor(floor_date(`Collection Data`, "month"))) %>%  
  group_by(month, Clade) %>%
  summarise(clade_n = n()) %>% 
  spread(Clade, clade_n, fill = 0)
# n sequences per month
seq_m <- seq2 %>%
  mutate(year = year(`Collection Data`), 
         month = factor(floor_date(`Collection Data`, "month")),
         Clade = factor(Clade, levels = clades)) %>% 
  group_by(month) %>%
  summarise(n = n())

# write table of variant frequency
var_m_df$voc1_freq <- var_m_df$`20H/501Y.V2`/seq_m$n
var_m_df$voc2_freq <- var_m_df$`20I/501Y.V1`/seq_m$n
var_m_df$freq <- seq_m$n
write.csv(var_m_df, paste0(git.path,"BGD_Covid-19/B.1.351_resurgence/output/variants_19April2021.csv"), row.names = FALSE)

B.1.1.7 = var_m_df$`20I/501Y.V1` 
B.1.351 = var_m_df$`20H/501Y.V2` 
var_n = var_m_df$freq 
as.data.frame(binconf(x=B.1.1.7, n=var_n))
n2021 = sum(var_n[10:14])
voc2021 = sum(var_m_df$`20I/501Y.V1`[10:14] + var_m_df$`20H/501Y.V2`[10:14])

var_freq = as.data.frame(binconf(x=B.1.351, n=var_n))
var_freq$month <- as.Date(var_m_df$month)
var_freq$voc <- "B.1.351"
var_freq$n <- var_m_df$freq 
write.csv(var_freq, paste0(git.path,"BGD_Covid-19/B.1.351_resurgence/output/var_freq_20210418.csv"), row.names = FALSE)

# Plot frequency of variants over time
p1 <- ggplot(var_freq[10:14,], aes(x=month, y=PointEst, group = voc, colour="voc")) + # DEC ONWARDS ONLY
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=2, colour = "black") +
  geom_line(colour = "black") +
  geom_point(colour = "black") +
  labs(x="", y="B.1.351 frequency") + theme 
theme
p1
ggsave(p1, file = paste0(git.path,"BGD_Covid-19/B.1.351_resurgence/output/B.1.351_freq_19April2021.pdf"), units = "cm", dpi = "retina", width = 8, height = 6)

intros = data.frame(
  n = c(4,4,4,4),
  date = as.Date(c("2020-12-17", "2021-01-04", "2021-02-07", "2021-01-02")),
  min_date = as.Date(c("2020-12-03", "2020-12-11", "2021-01-03", "2020-12-10")),
  max_date = as.Date(c("2021-01-06", "2021-02-05", "2021-03-14", "2021-01-09")),
  cases = c(159, 6, 1, 3))

mean(intros$date); range(intros$date)
intros$min_date; intros$max_date
range(c(intros$date, intros$min_date, intros$max_date))

SAvoc <- subset(seq2, Clade == "20H/501Y.V2"); dim(SAvoc)
UKvoc <- subset(seq2, Clade == "20I/501Y.V1"); dim(UKvoc)
table(SAvoc$`Admin Division`)

SAvoc[which(SAvoc$`Admin Division` !="Dhaka"),
      c("Collection Data", "Admin Division", "Division of exposure")]


