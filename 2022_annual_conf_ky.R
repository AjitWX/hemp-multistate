rm(list=ls())

library(tidyverse)
library(dplyr)


#### Average Production Function ####

avg_yields <- function(f) { 
  data <- read.csv(f)
  avg_yields <- data %>%
    select(site, variety, grain.m, grain.a, straw.m, straw.a)%>%
    filter(variety %in% c( "Altair", "Anka", "Bialobrzeskie", "Canda", "CFX-1",
                           "CFX-2","CRS-1", "Grandi", "Fedora 17", "Felina 32", 
                           "Ferimon", "Helena","Hlukauskii51", "H-51", "Henola",
                           "Hlesia", "Hliana", "Joey", "Katani", "Lara", "NWG 2730",
                           "NWG 2463", "NWG 4000", "NWG 4113", "NWG 452", "Rigel", 
                           "Uso-31", "Vega", "X-59"))%>%
    mutate(site = toupper(site), variety = toupper(variety)) %>% 
    group_by(site,variety) %>%
    summarize(count = n(),    
              avg_grain.a = mean(grain.a, na.rm=T),
              avg_straw.a = mean(straw.a, na.rm=T),
              se_grain.a = sd(grain.a, na.rm=T)/sqrt(n()),
              se_straw.a = sd(straw.a, na.rm=T)/sqrt(n()))
  return(avg_yields)
}


#### Within Site Average & Z Score FUNCTION #### START

z_yields<- function(f) {
  data <- read.csv(f)
  data.avg<- data %>%
    select(site, variety, grain.m, grain.a, straw.m, straw.a)%>%
    filter(variety %in% c( "Altair", "Anka", "Bialobrzeskie", "Canda", "CFX-1",
                           "CFX-2","CRS-1", "Grandi", "Fedora 17", "Felina 32", 
                           "Ferimon", "Helena","Hlukauskii51", "H-51", "Henola",
                           "Hlesia", "Hliana", "Joey", "Katani", "Lara", "NWG 2730",
                           "NWG 2463", "NWG 4000", "NWG 4113", "NWG 452", "Rigel", 
                           "Uso-31", "Vega", "X-59"))%>%
    mutate(site = toupper(site), variety = toupper(variety)) %>% 
    group_by(site,variety)
  
  site_avg_yields.A<- data.avg %>%
    group_by(site)%>%
    summarize(count=n(),
              site_avg_grain.A = mean(grain.a, na.rm=T),
              site_avg_straw.A = mean(straw.a, na.rm=T),
              site_std_grain.A = sd(grain.a, na.rm=T),
              site_std_straw.A = sd(straw.a, na.rm=T))
  
  z_yields<- data.avg %>%
    select(site, variety, grain.a, straw.a) %>%
    mutate(z_grain = (grain.a-site_avg_yields.A$site_avg_grain.A)/site_avg_yields.A$site_std_grain.A,
           z_straw = (straw.a-site_avg_yields.A$site_avg_straw.A)/site_avg_yields.A$site_std_straw.A)
  
  return(z_yields)
}


## Factors and Constituents ####

variety.c<- c("ALTAIR", "ANKA", "BIALOBRZESKIE", "CANDA", "CFX-1",
              "CFX-2","CRS-1", "GRANDI", "FEDORA 17", "FELINA 32", 
              "FERIMON", "HELENA","HLUKAUSKII51", "H-51", "HENOLA",
              "HLESIA", "HLIANA", "JOEY", "KATANI", "LARA", "NWG 2730",
              "NWG 2463", "NWG 4000", "NWG 4113", "NWG 452", "RIGEL", 
              "USO-31", "VEGA", "X-59")

site.c <- c("CORNELL","KANSAS MANHATTAN", 
            "KANSAS WICHITA", "KENTUCKY LEXINGTON",
            "KENTUCKY QUICKSAND", "MARYLAND", 
            "MICHIGAN", "MONTANA","NORTH DAKOTA",
            "TENNESSEE", "VERMONT", "VIRGINIA",
            "WISCONSIN", "WISCONSIN ARLINGTON",
            "WISCONSIN CHIPPEWAFALLS")


#### Pull Data Set / Apply Functions ####

avg_z_list.19 <- lapply((list.files(path="./data",pattern="multistate_2019_[0-9]{3}.csv",
                               full.names=TRUE)),z_yields)
z_data.19<- bind_rows(avg_z_list.19)

avg_z_list.20 <- lapply((list.files(path="./data",pattern="multistate_2020_[0-9]{3}.csv",
                                    full.names=TRUE)),z_yields)
z_data.20<- bind_rows(avg_z_list.20)

avg_z_list.21 <- lapply((list.files(path="./data",pattern="multistate_2021_[0-9]{3}.csv",
                                    full.names=TRUE)),z_yields)
z_data.21<- bind_rows(avg_z_list.21)


#### Grain ####

grains <- avg_data %>% 
  group_by(variety) %>%
  filter(!is.na(avg_grain.a)) %>%
  arrange(desc(avg_grain.a)) 

grains$variety <- factor(grains$variety, levels=c(unique(grains$variety)))
grains$site <- factor(grains$site, levels = c("CORNELL","KANSAS MANHATTAN", 
                                              "KANSAS WICHITA", "KENTUCKY LEXINGTON",
                                              "KENTUCKY QUICKSAND", "MARYLAND", 
                                              "MICHIGAN", "MONTANA","NORTH DAKOTA",
                                              "TENNESSEE", "VERMONT", "VIRGINIA",
                                              "WISCONSIN", "WISCONSIN ARLINGTON",
                                              "WISCONSIN CHIPPEWAFALLS"))

grains.m <- grains %>%
  group_by(variety) %>%
  summarise( avg_grain.a = mean(avg_grain.a ))

grains.m$variety <- factor(grains.m$variety, levels=c(unique(grains.m$variety)))


#### Straw ####

straws <- avg_data %>%
  group_by(variety) %>%
  filter(!is.na(avg_straw.a)) %>%
  arrange(desc(avg_straw.a)) 

straws$variety <- factor(straws$variety, levels=c(unique(straws$variety)))
straws$site <- factor(straws$site, levels = c("CORNELL","KANSAS MANHATTAN", 
                                              "KANSAS WICHITA", "KENTUCKY LEXINGTON",
                                              "KENTUCKY QUICKSAND", "MARYLAND", 
                                              "MICHIGAN", "MONTANA","NORTH DAKOTA",
                                              "TENNESSEE", "VERMONT", "VIRGINIA",
                                              "WISCONSIN", "WISCONSIN ARLINGTON",
                                              "WISCONSIN CHIPPEWAFALLS"))


straws.m <- straws %>%
  group_by(variety) %>%
  summarise( avg_straw.a = mean(avg_straw.a ))

straws.m$variety <- factor(straws.m$variety, levels=c(unique(straws.m$variety)))

## Grain GGPlot ####

ggplot(grains, aes(x = variety, y = avg_grain.a, size = 5, color = site))+
  geom_point() +
  ylim(0,2300)+
  guides( size = "none")+
  guides( color = guide_legend(override.aes = list(size = 5)))+
  geom_point(data = grains.m, stat ="identity", shape = 23, size = 5, col="#a6dba0", 
             fill = "#1b7837")+
  scale_color_manual(name = "Site",
                     values = c("#4575b4", "#74add1", "#abd9e9", "#a1d99b", 
                                "#e5f5e0", "#fee090", "#fee8c8", "#fdae61", "#f46d43", 
                                "#d73027", "#a50026", 
                                "#99d8c9"))+
  labs(title = "2019 Grain Production", x = "Genotype", y = "Average Yield \n (lb/A) ", 
       color = "Site") +
  theme_bw(base_size = 12) +
  theme(plot.background = element_blank())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))+
  theme(legend.title = element_text(size =12))

##  Stalk GGPlot ####

ggplot(straws, aes(x = variety, y = avg_straw.a, size = 5, color = site))+
  geom_point() +
  ylim(0,4000)+
  guides( size = "none")+
  guides( color = guide_legend(override.aes = list(size = 5)))+
  geom_point(data = straws.m, stat ="identity", shape = 23, size = 5, col="#a6dba0", 
             fill = "#1b7837")+
  scale_color_manual(name = "Site",
                     values = c("#4575b4", "#74add1", "#abd9e9", "#a1d99b", 
                                "#e5f5e0", "#fee090", "#fee8c8", "#fdae61", "#f46d43", 
                                "#d73027", "#a50026", 
                                "#99d8c9"))+
  labs(title = "2019 Stalk Production", x = "Genotype", y = "Average Yield \n (lb/A) ", 
       color = "Site") +
  theme_bw(base_size = 12) +
  theme(plot.background = element_blank())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))+
  theme(legend.title = element_text(size =12))



#### Additional ####


## Avg ####

avg_list <- lapply((list.files(path="./data",pattern="multistate_2020_[0-9]{3}.csv",full.names=TRUE)),avg_yields)

avg_data <- bind_rows(avg_list)

avg_data$variety <- factor(avg_data$variety, levels=c(unique(avg_data$variety)))

## Z ####

var_avg_yields <- avg_data %>% 
  group_by(variety) %>% 
  filter(!is.na(grain.a)) %>%
  summarize(count = n(),
            avg_grain.A = mean(grain.a, na.rm=T),
            avg_straw.A = mean(straw.a, na.rm=T),
            std_grain.A = sd(grain.a, na.rm=T),
            std_straw.A = sd(straw.a, na.rm=T))

std_list <- lapply(variety.c,var.z)

std_data <- bind_rows(std_list)%>%
  na.omit(std_data)

std_avg_yields <- std_data %>% 
  group_by(variety) %>% 
  filter(!is.na(grain.a)) %>%
  summarize(count = n(),
            z_grain_a = mean(z_grain, na.rm=T),
            z_grain_max = max(z_grain, na.rm=T),
            z_grain_min = min(z_grain, na.rm=T),
            z_straw_a = mean(z_straw, na.rm=T),
            z_straw_max = max(z_straw, na.rm=T),
            z_straw_min = min(z_straw, na.rm=T))
#####