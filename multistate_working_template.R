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
  
  site_std_yields <- data %>%
    rowwise() %>%
    mutate( std_grain = (grain.a-site_avg_yields$avg_grain.a)/site_avg_yields$std_grain.a,
            std_straw = (straw.a-site_avg_yields$avg_straw.a)/site_avg_yields$std_straw.a,
            std_poph = (poph.a-site_avg_yields$avg_poph.a)/site_avg_yields$std_poph.a)
            
               
  return(site_std_yields)
}

std_list <- lapply((list.files(path=".",pattern="multistate_2020_[0-9]{3}.csv",full.names=TRUE)),std_yields)

std_data <- bind_rows(std_list)

ggplot(std_data,aes(x=variety,y=std_grain))+
  geom_bar(stat="identity")+
  facet_wrap(vars(site))+
  theme(axis.text.x = element_text(angle=90))

ggplot(std_data,aes(x=variety,y=std_straw))+
  geom_bar(stat="identity")+
  facet_wrap(vars(site))+
  theme(axis.text.x = element_text(angle=90))

ggplot(std_data,aes(x=variety,y=std_poph))+
  geom_bar(stat="identity")+
  facet_wrap(vars(site))+
  theme(axis.text.x = element_text(angle=90))

#### 

data <- read.csv("multistate_2020_fulldata.csv")
for(f in filenames){
  std_yields(f)
}  
   

# List of files

filenames<- list.files(path=".",pattern="multistate_2020_[0-9]{3}.csv",full.names=TRUE)
filenames<-filenames[1:13]
for(f in filenames){
  print(f)
}

# Vector 

variety <- pull(dat,variety)

variety <- list('altair','anka','bialobrzeskie','canda','cfx1','cfx2','crs1','enectoral','fedora17','felina32','ferimon','futura','grandi',
                'helena','henola','hlesia','joey','katani','nwg2730','nwg331','nwg452','picolo','rigel','uso31','vega','x59')

for ( x in variety){
  print(x)
}
# For Loop

for ( variety in c('altair','anka','bialobrzeskie','canda','cfx1','cfx2','crs1','enectoral','fedora17','felina32','ferimon','futura','grandi',
                   'helena','henola','hlesia','joey','katani','nwg2730','nwg331','nwg452','picolo','rigel','uso31','vega','x59')){
  
  standardizeZ<-function(variety) 
    variety.q<- dat$variety == variety
    variety.r<- which(variety.q)
    variety<-dat[variety.r,var]%>%mutate_at(c('grain.m','grain.a','straw.m','straw.a','poph.m','poph.a'),~(scale(.)%>% as.vector))
    return(variety)
  }

# Functions 

standardizeZ<-function(variety) {
  variety.q<- dat$variety == variety
  variety.r<- which(variety.q)
  variety<-dat[variety.r,var]%>%mutate_at(c('grain.m','grain.a','straw.m','straw.a','poph.m','poph.a'),~(scale(.)%>% as.vector))
  return(variety)
}
standardizeZ('anka')


#### Important ####

dat<-read.csv("multistate_2020_072.csv")

var<-c('variety','grain.m','grain.a','straw.m','straw.a','poph.m','poph.a')

altair.q<- dat$variety == "altair"

altair.r<-which(altair.q)
  
altair<-dat[altair.r,var] %>%mutate_at(c('grain.m','grain.a','straw.m','straw.a','poph.m','poph.a'),~(scale(.)%>% as.vector))


anka.q<- dat$variety == "anka"

anka.r<-which(anka.q)

anka<-dat[anka.r,var] %>%mutate_at(c('grain.m','grain.a','straw.m','straw.a','poph.m','poph.a'),~(scale(.)%>% as.vector))

standardized<- do.call("rbind",list(altair,anka))

## 
  
standardizeZ<-function(variety) {
  variety.q<- dat$variety == variety
  variety.r<- which(variety.q)
  variety<-dat[variety.r,var]%>%mutate_at(c('grain.m','grain.a','straw.m','straw.a','poph.m','poph.a'),~(scale(.)%>% as.vector))
  return(variety)
}
standardizeZ("anka")
standardizeZ("altair")

anka.r<-which(anka.q)

anka<-dat[anka.r,var] %>%mutate_at(c('grain.m','grain.a','straw.m','straw.a','poph.m','poph.a'),~(scale(.)%>% as.vector))


for ( variety in dat$variety){
  print(variety)
}


