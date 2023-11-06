##
rm(list=ls())

library(tidyverse)
library(dplyr)


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

std_yields <- function(f){
  data <-read.csv(f)
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
  
  var_avg_yields <- avg_yields %>% 
    group_by(variety) %>% 
    summarize(count = n(),
              avg_grain.a = mean(avg_yields$avg_grain.a, na.rm=T),
              avg_straw.a = mean(avg_yields$avg_straw.a, na.rm=T),
              std_grain.a = sd(avg_yields$avg_grain.a, na.rm=T),
              std_straw.a = sd(avg_yields$avg_straw.a, na.rm=T))
  
  var_std_yields <- avg_yields %>% 
    rowwise() %>% 
    mutate(std_grain = (avg_grain.a-var_avg_yields$avg_grain.a)/var_avg_yields$std_grain.a,
           std_straw = (avg_straw.a-var_avg_yields$avg_straw.a)/var_avg_yields$std_straw.a)
  
  std_yields <- var_std_yields %>% 
    select(site, variety, avg_grain.a, avg_straw.a)
  
  return(std_yields)
}

            
avg_list <- lapply((list.files(path="./data",pattern="multistate_2021_[0-9]{3}.csv",full.names=TRUE)),avg_yields)

avg_data <- bind_rows(avg_list)

std_list <- lapply((list.files(path="./data",pattern="multistate_2021_[0-9]{3}.csv",full.names=TRUE)),std_yields)

# New working

# Grain 

grains <- avg_data %>% 
  group_by(variety) %>%
  filter(!is.na(avg_grain.a)) %>%
  arrange(desc(avg_grain.a)) 

grains$variety <- factor(grains$variety, levels=c(unique(grains$variety)))
grains$site <- factor(grains$site, levels = c("MONTANA","VERMONT","WISCONSIN",
                                              "MICHIGAN","CORNELL","KENTUCKY LEXINGTON","VIRGINIA"))

grains.m <- grains %>%
  group_by(variety) %>%
  summarise( avg_grain.a = mean(avg_grain.a ))

grains.m$variety <- factor(grains.m$variety, levels=c(unique(grains.m$variety)))


ggplot(grains, aes(x = variety, y = avg_grain.a, size = 5, color = site))+
  geom_point() +
  ylim(0,2400)+
  guides( size = "none")+
  guides( color = guide_legend(override.aes = list(size = 5)))+
  geom_point(data = grains.m, stat ="identity", shape = 23, size = 6, col="#a6dba0", 
             fill = "#1b7837")+
  scale_color_manual(name = "Latitude",
                     labels = c( "47°", "45°", "44°", "42.6°", "42.4°", "38°", "37°"),
                     values = c("#4575b4","#74add1", "#abd9e9", "#fee090", 
                                "#fdae61", "#f46d43", "#d73027"))+
  labs(title = "2021 Grain Production", x = "Genotype", y = "Average Yield \n (lb/A) ", 
       color = "Latitude") +
  theme_bw(base_size = 12) +
  theme(plot.background = element_blank())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))+
  theme(legend.title = element_text(size =12))
ggsave("output/grain_2021.png", height=1000, width=1000)
 

# Straw

straws <- avg_data %>% 
  group_by(variety) %>%
  filter(!is.na(avg_straw.a)) %>%
  arrange(desc(avg_straw.a)) 

straws$variety <- factor(straws$variety, levels=c(unique(straws$variety)))
straws$site <- factor(straws$site, levels = c("MONTANA","VERMONT","WISCONSIN",
                                              "MICHIGAN","CORNELL","KENTUCKY LEXINGTON",
                                              "VIRGINIA"))

straws.m <- straws %>%
  group_by(variety) %>%
  summarise( avg_straw.a = mean(avg_straw.a ))

straws.m$variety <- factor(straws.m$variety, levels=c(unique(straws.m$variety)))


ggplot(straws, aes(x = variety, y = avg_straw.a, size = 5, color = site))+
  geom_point() +
  ylim(0,13000)+
  guides( size = "none")+
  guides( color = guide_legend(override.aes = list(size = 5)))+
  geom_point(data = straws.m, stat ="identity", shape = 23, size = 6, col="#a6dba0", 
             fill = "#1b7837")+
  scale_color_manual(name = "Latitude",
                     labels = c( "47°", "45°", "44°", "42.6°", "42.4°", "38°", "37°"),
                     values = c("#4575b4","#74add1", "#abd9e9", "#fee090", 
                                "#fdae61", "#f46d43", "#d73027"))+
  labs(title = "2021 Straw Production", x = "Genotype", y = "Average Yield \n (lb/A) ", 
       color = "Latitude") +
  theme_bw(base_size = 12) +
  theme(plot.background = element_blank())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))+
  theme(legend.title = element_text(size =12))
ggsave("straw_2021.png", height = 10, width = 10)



## Grains ####
avg_data$site<- factor(avg_data$site, levels=c(unique(avg_data$site)))

grains <- avg_data %>% 
  group_by(variety) %>%
  filter(!is.na(avg_grain.a)) %>% 
  summarise(count =n(),
            site = ,
            avg_grain.l = avg_grain.a,
            avg_grain = mean(avg_grain.a))

grains1 <- grains %>%
  filter(!is.na(avg_grain)) %>% 
  arrange(desc(avg_grain)) 

grains1$variety <- factor(grains1$variety, levels=c(unique(grains1$variety)))

gm <- grains1 %>%
  group_by(variety) %>%
  summarise( avg_grain.l = mean(avg_grain))

gm$variety <- factor(gm$variety, levels=c(unique(gm$variety)))

ggplot(grains1,aes(x = variety,y = avg_grain.l))+
  geom_point() +
  geom_point(data = gm, stat ="identity", shape = 24, size = 3, col="orange", 
             fill = "blue")+
  labs(title = "2021 Grain Production", x = "Variety", y = "Yield ~ lb/A ") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle=90))

ggsave("output/grains.png", height=15, width=15)


## Straw ####

straws <- avg_data %>% 
  group_by(variety) %>%
  filter(!is.na(avg_straw.a)) %>% 
  summarise(count =n(),
            avg_straw.l = avg_straw.a,
            avg_straw = mean(avg_straw.a))

straws1 <- straws %>%
  filter(!is.na(avg_straw)) %>% 
  arrange(desc(avg_straw)) 

straws1$variety <- factor(straws1$variety, levels=c(unique(straws1$variety)))

sm <- straws1 %>%
  group_by(variety) %>%
  summarise( avg_straw.l = mean(avg_straw))

sm$variety <- factor(sm$variety, levels=c(unique(sm$variety)))

ggplot(straws1,aes(x = variety,y = avg_straw.l))+
  geom_point() +
  geom_point(data = sm, stat ="identity", shape = 24, size = 3, col="orange", 
             fill = "blue")+
  labs(title = "2021 Straw Production", x = "Variety", y = "Yield ~ lb/A ") +
  theme(plot.title = element_text(face="bold", size = 4, hjust = 0 ))+
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle=90))

  

data<-read.csv("data/multistate_2021_184.csv")

##
library(tidyverse)

std_yields <- function(f) { 
  data <- read.csv(f)
  
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
ggsave("output/grains.png", height=15, width=15)

grain_3 <- grains %>% 
  filter(site %in% c("CORNELL", "KANSAS LEXINGTON", "WISCONSIN" ))
grain_3$site <- factor(grain_3$site, levels=c("CORNELL", "KANSAS LEXINGTON", "WISCONSIN"))

ggplot(grain_3,aes(x=variety,y=std_grain))+
  geom_bar(stat="identity") +
  facet_wrap(vars(site)) +
  labs(x = "Variety", y = "Grain Yield Z") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle=90))
ggsave("output/grains_3.png", height=10, width=15)

std_grain_yield_variety <- grains %>% 
  group_by(variety) %>% 
  summarize(avg_std_grain = mean(std_grain),
            max_std_grain = max(std_grain),
            min_std_grain = min(std_grain),
            site_count = n()) %>% 
  arrange(desc(avg_std_grain))
write_csv(std_grain_yield_variety, "output/grain_rank.csv")

### Straw

straws <- std_data %>% 
  filter(!is.na(std_straw)) %>% 
  arrange(desc(std_straw))

straws$variety <- factor(straws$variety, levels=c(unique(straws$variety)))

ggplot(straws,aes(x=variety,y=std_straw))+
  geom_bar(stat="identity")+
  facet_wrap(vars(site))+
  labs(x = "Variety", y = "Straw Yield Z") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle=90))
ggsave("output/straws.png", height=10, width=10)

straw_3 <- straws %>% 
  filter(site %in% c( "CORNELL", "KANSAS LEXINGTON", "WISCONSIN"))
straw_3$site <- factor(straw_3$site, levels=c("CORNELL", "KANSAS LEXINGTON", "WISCONSIN"))

ggplot(straw_3,aes(x=variety,y=std_grain))+
  geom_bar(stat="identity") +
  facet_wrap(vars(site)) +
  labs(x = "Variety", y = "Straw Yield Z") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle=90))

ggsave("output/straw_3.png", height=10, width=15)

std_straw_yield_variety <- straws %>% 
  group_by(variety) %>% 
  summarize(avg_std_straw = mean(std_straw),
            max_std_straw = max(std_straw),
            min_std_straw = min(std_straw),
            site_count = n()) %>% 
  arrange(desc(avg_std_straw))
write_csv(std_straw_yield_variety, "output/straw_rank.csv")



