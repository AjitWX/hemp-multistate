rm(list=ls())

library(tidyverse)
library(dplyr)

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


