##
rm(list=ls())

library(tidyverse)
library(dplyr)

variety.c<- c( "ALTAIR", "ANKA", "BIALOBRZESKIE", "CFX-1" ,"CFX-2", "GRANDI", 
               "H-51", "HENOLA", "HLESIA", "HLIANA", "LARA", "NWG 2730",
               "NWG 2463", "NWG 4000", "NWG 4113", "VEGA", "X-59")

avg_yields <- function(f) { 
  data <- read.csv(f)
  avg_yields <- data %>%
    select(site, variety, grain.m, grain.a, straw.m, straw.a)%>%
    filter(variety %in% c( "Altair", "Anka", "Bialobrzeskie", "CFX-1" ,"CFX-2", "Grandi", 
                           "H-51", "Henola", "Hlesia", "Hliana", "Lara", "NWG 2730",
                           "NWG 2463", "NWG 4000", "NWG 4113", "Vega", "X-59"))%>%
    mutate(site = toupper(site), variety = toupper(variety)) %>% 
    group_by(site,variety) %>%
    summarize(count = n(),    
              avg_grain.a = mean(grain.a, na.rm=T),
              avg_straw.a = mean(straw.a, na.rm=T),
              se_grain.a = sd(grain.a, na.rm=T)/sqrt(n()),
              se_straw.a = sd(straw.a, na.rm=T)/sqrt(n()))
  return(avg_yields)
}

var.z <- function(variety.c) {
  var <- avg_data %>% 
    filter( variety == variety.c)
  
  var.m <- var_avg_yields %>% 
    filter( variety == variety.c)

  var.z <- var %>% 
    select(site, variety, avg_grain.a, avg_straw.a) %>%
    mutate(z_grain = (avg_grain.a-var.m$avg_grain.A)/var.m$std_grain.A,
           z_straw = (avg_straw.a-var.m$avg_straw.A)/var.m$std_straw.A)
  return(var.z)
}

## Avg ####

avg_list <- lapply((list.files(path="./data",pattern="multistate_2021_[0-9]{3}.csv",full.names=TRUE)),avg_yields)

avg_data <- bind_rows(avg_list)

avg_data$variety <- factor(avg_data$variety, levels=c(unique(avg_data$variety)))

## Z ####

var_avg_yields <- avg_data %>% 
  group_by(variety) %>% 
  filter(!is.na(avg_grain.a)) %>%
  summarize(count = n(),
            avg_grain.A = mean(avg_grain.a, na.rm=T),
            avg_straw.A = mean(avg_straw.a, na.rm=T),
            std_grain.A = sd(avg_grain.a, na.rm=T),
            std_straw.A = sd(avg_straw.a, na.rm=T))

std_list <- lapply(variety.c,var.z)

std_data <- bind_rows(std_list)

std_avg_yields <- std_data %>% 
  group_by(variety) %>% 
  filter(!is.na(avg_grain.a)) %>%
  summarize(count = n(),
            z_grain.a = mean(z_grain, na.rm=T),
            z_straw.A = mean(z_straw, na.rm=T))
