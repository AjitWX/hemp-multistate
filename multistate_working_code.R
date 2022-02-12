rm(list=ls())

library(tidyverse)
library(dplyr)

#### Working Code ####

library(tidyverse)

std_yields <- function(f) { 
  data <- read.csv(f)
  avg_yields <- data %>%
    select(site, variety, grain.m, grain.a, straw.m, straw.a, poph.m, poph.a)%>%
    group_by(site,variety) %>%
    summarize(count = n(),    
              avg_grain.a = mean(grain.a, na.rm=T),
              avg_straw.a = mean(straw.a, na.rm=T),
              avg_poph.a = mean(poph.a,na.rm=T),
              se_grain.a = sd(grain.a, na.rm=T)/sqrt(n()),
              se_straw.a = sd(straw.a, na.rm=T)/sqrt(n()),
              se_poph.a = sd(poph.a, na.rm=T)/sqrt(n()))
  
  
  site_avg_yields <- avg_yields %>%
    group_by(site) %>%
    summarize(count = n(),    
              avg_grain.a = mean(avg_yields$avg_grain.a, na.rm=T),
              avg_straw.a = mean(avg_yields$avg_straw.a, na.rm=T),
              avg_poph.a = mean(avg_yields$avg_poph.a,na.rm=T),
              std_grain.a = sd(avg_yields$avg_grain.a, na.rm=T),
              std_straw.a = sd(avg_yields$avg_straw.a, na.rm=T),
              std_poph.a = sd(avg_yields$avg_poph.a, na.rm=T))
  
  site_std_yields <- avg_yields %>%
    rowwise() %>%
    mutate( std_grain = (avg_grain.a-site_avg_yields$avg_grain.a)/site_avg_yields$std_grain.a,
            std_straw = (avg_straw.a-site_avg_yields$avg_straw.a)/site_avg_yields$std_straw.a,
            std_poph = (avg_poph.a-site_avg_yields$avg_poph.a)/site_avg_yields$std_poph.a)
  
  std_yields <- site_std_yields %>%
    select(site, variety, avg_grain.a, avg_straw.a, avg_poph.a, 
           std_grain, std_straw, std_poph,
           se_grain.a, se_straw.a, se_poph.a) 
               
  return(std_yields)
}

std_list <- lapply((list.files(path="./data",pattern="multistate_2020_[0-9]{3}.csv",full.names=TRUE)),std_yields)

std_data <- bind_rows(std_list)

### Grain

grains <- std_data %>% 
  filter(!is.na(std_grain)) %>% 
  arrange(desc(std_grain))

grains$variety <- factor(grains$variety, levels=c(unique(grains$variety)))

ggplot(grains,aes(x=variety,y=std_grain))+
  geom_bar(stat="identity") +
  facet_wrap(vars(site)) +
  labs(x = "Variety", y = "Grain Yield Z") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle=90))
ggsave("output/grains.png", height=10, width=10)

std_grain_yield_variety <- grains %>% 
  group_by(variety) %>% 
  summarize(avg_std_grain = mean(std_grain),
            max_std_grain = max(std_grain),
            min_std_grain = min(std_grain),
            site_count = n()) %>% 
  arrange(desc(avg_std_grain))
write_csv(std_grain_yield_variety, "output/grain_rank.csv")

### Straw

ggplot(std_data,aes(x=variety,y=std_straw))+
  geom_bar(stat="identity")+
  facet_wrap(vars(site))+
  theme(axis.text.x = element_text(angle=90))

### Population

ggplot(std_data,aes(x=variety,y=std_poph))+
  geom_bar(stat="identity")+
  facet_wrap(vars(site))+
  theme(axis.text.x = element_text(angle=90))