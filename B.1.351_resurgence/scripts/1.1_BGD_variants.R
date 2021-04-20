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
seq3 <- read_tsv(paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/data/nextstrain_community_CHRF-Genomics_ncovBangladesh@main_metadata_subset.tsv"))
seq2021t1 <- read_tsv(paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/data/nextstrain_community_CHRF-Genomics_ncovBangladesh@main_metadata_20210101_20210415.tsv"))
seq2020t3 <- read_tsv(paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/data/nextstrain_community_CHRF-Genomics_ncovBangladesh@main_metadata_20200901_20210101.tsv"))
seq2020t2 <- read_tsv(paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/data/nextstrain_community_CHRF-Genomics_ncovBangladesh@main_metadata_20200430_20200831.tsv"))
seq2020t1 <- read_tsv(paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/data/nextstrain_community_CHRF-Genomics_ncovBangladesh@main_metadata_20210101_20210415.tsv"))
dim(seq1); table(seq1$Country) # 877 from first download
dim(seq2); # 860 samples now downloaded
dim(seq3);
dim(seq2021t1) # 310 samples since 2021
dim(seq2020t3) # 158 samples in last Q of 2020
dim(seq2020t2) # 307 samples in last Q of 2020

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

B.1.351 = var_m_df$`20H/501Y.V2`[10:14] # Jan - March (first detected on 24th Jan!)
sequences = var_m_df$freq[10:14] 
var_freq = as.data.frame(binconf(x=B.1.351, n=sequences))
var_freq$month <- as.Date(var_m_df$month[10:14])
var_freq$voc <- "B.1.351"
  
# Plot frequency of variants over time
P1 <- ggplot(var_freq, aes(x=month, y=PointEst, group = voc, colour="voc")) + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=2, colour = "black") +
  geom_line(colour = "black") +
  geom_point(colour = "black")
P1
ggsave(p1, file = paste0(git.path,"BGD_Covid-19/B.1.351_resurgence/output/B.1.351_freq_19April2021.pdf"), units = "cm", dpi = "retina", width = 12, height = 8)








