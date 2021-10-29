# Plot tdistrict-level maps covid-19 confirmed cases, tests and test positivity (rolling average)
git.path <- "/Users/katiehampson/Github/"

library(sf); library(tidyverse); library(lubridate); library(ggthemes); library(lubridate); library(viridis); library(ggpubr)

theme <- theme_clean() + theme(axis.title=element_blank(), axis.text=element_blank(), 
                               axis.ticks=element_blank(), axis.line=element_blank(), 
                               legend.text=element_text(size=8), legend.title=element_text(size=9), 
                               plot.background = element_rect(color = "white"), 
                               panel.grid = element_blank(), panel.border= element_blank(),
                               strip.text=element_text(size=10), legend.background = element_rect(color = NA),
                               panel.spacing=unit(0,'npc'))

# import a2i aggregated data
load(paste0(git.path, "BGD_Covid-19/Jul2021/data/data_weekly_a2i.rda"))

district.shp <- read_sf(paste0(git.path, "BGD_Covid-19/Jul2021/data/simplified_BGD_districts.shp")) %>%
        transmute(district = DISTNAME) %>% st_transform(crs=4326)

data.weekly <- st_sf(left_join(data.weekly, district.shp, by="district"))
unique(data.weekly$date)

# keep range of dates for figure
data.weekly <- data.weekly %>% filter(date==ymd("2021-01-28") | date==ymd("2021-04-01"))

p1 <- ggplot(data=data.weekly, aes(fill = cases)) +
        geom_sf(color="black", size = 0.1) +
        scale_fill_viridis(trans="log", option = "B",
                           breaks = c(1, 10, 100, 1000, data.weekly$cases[which.max(data.weekly$cases)]), 
                           labels = c(1, 10, 100, 1000, data.weekly$cases[which.max(data.weekly$cases)])) +
        facet_wrap(~date, nrow = 1 , ncol = 2) +
        theme + labs(x = "", y = "", fill="Cases") + 
        theme(plot.margin = unit(c(0,-2,0,-2), "cm"))

p2 <- ggplot(data=data.weekly, aes(fill = tests)) +
        geom_sf(color="black", size = 0.1) +
        scale_fill_viridis(trans="log", option = "D",
                           breaks = c(1, 10, 100, 1000, 10000, data.weekly$tests[which.max(data.weekly$tests)]), 
                           labels = c(1, 10, 100, 1000, 10000, data.weekly$tests[which.max(data.weekly$tests)])) +
        facet_wrap(~date, nrow = 1 , ncol = 2) +
        theme + labs(x = "", y = "", fill="Tests") + 
        theme(plot.margin = unit(c(-2.5,0,0,0), "cm"), strip.text.x = element_blank())

p3 <- ggplot(data=data.weekly, aes(fill = positivity)) +
        geom_sf(color="black", size = 0.1) +
        scale_fill_viridis(option = "C",
                           breaks = seq(0, data.weekly$positivity[which.max(data.weekly$positivity)], by=0.1), 
                           labels = seq(0, data.weekly$positivity[which.max(data.weekly$positivity)], by=0.1)) +
        facet_wrap(~date, nrow = 1 , ncol = 2) +
        theme + labs(x = "", y = "", fill="Positivity") + 
        theme(plot.margin = unit(c(-5.5,0,0,0), "cm"), strip.text.x = element_blank())

panel <- ggpubr::ggarrange(p1 + rremove("y.axis") + rremove("x.axis"), 
                   p2 + rremove("y.axis") + rremove("x.axis"), 
                   p3 + rremove("y.axis") + rremove("x.axis"),
                   #labels = c("A", "B", "C"),
                   #label.x = 0.95, label.y = 0.99,
                   font.label = list(size = 13, color ="grey80"),
                   ncol = 1, nrow = 3)

ggsave(panel, file = paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/output/Fig_2_panel_SA_appearance.svg"), units = "cm", dpi = "retina", width =10, height = 18)
ggsave(panel, file = paste0(git.path, "BGD_Covid-19/B.1.351_resurgence/output/Fig_2_panel_SA_appearance.png"), units = "cm", dpi = "retina", width =10, height = 18)

