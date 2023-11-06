rm(list=ls())

setwd("C:/Users/Ajit W/OneDrive - University of Florida/1. MS - Agroecology/s1084-hemp-multistate-trial/From SSD/s1084-hemp-multistate-trial")

##  install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))

library(tidyverse)
library(dplyr)
library(ggpubr)
library(broom)
library(AICcmodavg)
library(agricolae)
library(ggplot2)
library(ggthemes)
library(multcompView)


#S1084 Detailed Report

#### CREATE FULL LIST OF OBSERVATIONS ####

fdata= function(f) {  # function to read data
  data= read.csv(f)
  fdata= data %>%
    select(site, variety, grain.m, grain.a, straw.m, straw.a, pope.m, pope.a, 
           poph.m, poph.a, height, height.sd, stem, stem.sd)%>%
    mutate(site = toupper(site), variety = toupper(variety)) %>% 
    group_by(site,variety)
  j = c(as.numeric(3:14))
  fdata$site = as.factor(fdata$site)
  fdata$variety = as.factor(fdata$variety)
  fdata[,j] = apply(fdata[,j],2,
                    function(x) as.integer(as.character(x)))
  return(fdata)
}

fd.comb= function(fdata){ # function to combine data
  fd.list.19 = lapply((list.files(path="./data",pattern="multistate_2019_[0-9]{3}.csv",
                                  full.names=TRUE)),fdata)
  fd.19 = bind_rows(fd.list.19)
  fd.19$year = as.factor(2019)
  fd.list.20 = lapply((list.files(path="./data",pattern="multistate_2020_[0-9]{3}.csv",
                                  full.names=TRUE)),fdata)
  fd.20 = bind_rows(fd.list.20)
  fd.20$year = as.factor(2020)
  fd.list.21 = lapply((list.files(path="./data",pattern="multistate_2021_[0-9]{3}.csv",
                                  full.names=TRUE)),fdata)
  fd.21 = bind_rows(fd.list.21)
  fd.21$year = as.factor(2021)
  
  fd.all.list = list(fd.19,fd.20,fd.21)
  #### full data = fd.all ####
  fd.all = bind_rows(fd.all.list)
  
  return(fd.all)
}
fd.all=fd.comb(fdata)
fd.all = fd.all %>%   # convert from (lb/a to kg/ha) and (plant/acre to pant/hectare)
  select(year, site, variety, grain.m, grain.a, straw.m, straw.a, pope.m, pope.a, 
         poph.m, poph.a, height, height.sd, stem, stem.sd) %>%
  mutate( grain.a = as.integer(as.character(grain.a*1.121)),
          straw.a = as.integer(as.character(straw.a*1.121)),
          pope.a = as.integer(as.character(pope.a*2.471)),
          poph.a = as.integer(as.character(poph.a*2.471)))
# conversions

fd.all$variety=str_replace(fd.all$variety, "HLUKAUSKII51","H-51")
fd.all$variety=str_replace(fd.all$variety, "FERMION", "FERIMON")
fd.all$variety=str_replace(fd.all$variety, "FUTURE 75", "FUTURA 75")
fd.all$variety=str_replace(fd.all$variety, "GRANDI ", "GRANDI")
fd.all$variety=str_replace(fd.all$variety, "HENOLA ", "HENOLA")
fd.all$variety=str_replace(fd.all$variety, "HLUKHOVSKII51", "H-51")
fd.all$variety=str_replace(fd.all$variety, "HLUKAUSKII 51", "H-51")
fd.all$variety=str_replace(fd.all$variety, "KATANI ", "KATANI")
fd.all$variety=str_replace(fd.all$variety, "SANTHICA", "SANTHICA 70")
fd.all$variety=str_replace(fd.all$variety, "SONTHICA 70", "SANTHICA 70")
fd.all$variety=str_replace(fd.all$variety, "SSBETA", "SS BETA")
fd.all$site=as.character(fd.all$site) # convert sites 
fd.all = subset(fd.all, site != "")
fd.all$site=as.factor(fd.all$site)
fd.all = subset(fd.all, variety !="") # convert varieties
fd.all$variety=as.factor(fd.all$variety)

fd.var.all = fd.all
fd.var.all = fd.all %>%
  filter(year=="2019")

#### RAW comparison ####
fd.var.all.comp = fd.var.all%>%
  group_by(variety)%>%
  summarize(count=n())

#### LIST OF VARIETIES < 12 OBSERVATIONS
var.19= fd.all %>%
  select(year, site, variety)%>%
  filter(year=="2019")
var.19$variety=as.factor(as.character(var.19$variety))
var.19.list=var.19 %>%
  sapply(levels)
var.19.list=var.19.list[["variety"]]
var.19.comb = var.19 %>%
  group_by(year, site, variety)%>%
  summarize(count=n())

var.20= fd.all %>%
  select(year, site, variety)%>%
  filter(year=="2020")
var.20$variety=as.factor(as.character(var.20$variety))
var.20.list=var.20 %>%
  sapply(levels)
var.20.list=var.20.list[["variety"]]

var.21= fd.all %>%
  select(year, site, variety)%>%
  filter(year=="2021")
var.21$variety=as.factor(as.character(var.21$variety))
var.21.list=var.21 %>%
  sapply(levels)
var.21.list=var.21.list[["variety"]]
#### varieties ####
fd.var.all= 
  subset(fd.all, !(variety %in% c('AMAZE AUTO','CANDA','CS SEME','ELETTA CAMPANA',
                                  'ENECTAROL','FIBRANOVA','FIBROR 79','FERIMON',
                                  'FUTURA 83','GC-2','JOEY','HELENA','RIGEL',
                                  'JIN MA','MARTHA','SANTHICA 70','SS BETA' ,
                                  'TYGRA','VICTORIA','WI-M-H-19-00112')))

fd.var.all=fd.all%>%
  filter(variety %in% c("ANKA","CFX-1","H-51","HLESIA","HLIANA","KATANI",
                        "X-59"))
#fd.var.all$variety=str_replace(fd.var.all$variety, "CFX-1","CFX 1")
#fd.var.all$variety=str_replace(fd.var.all$variety, "H-51","H 51")
#fd.var.all$variety=str_replace(fd.var.all$variety, "X-59","X 59")
fd.var.all$variety=as.factor(as.character(fd.var.all$variety))

fd.var.all = fd.var.all %>%
  filter(year=="2021")
#### population comparison of all varieties ####

pope.0= fd.var.all %>%
  drop_na(pope.a)
pope.1=pope.0 %>%
  group_by(year,variety)%>%
  summarize(count=n(),
            avg_pope.a= mean(pope.a, na.rm=T))
pope.2= pope.0 %>%
  group_by(year,site)%>%
  summarize(count=n(),
            avg_pope.a= mean(pope.a, na.rm=T))
pope.3= pope.0 %>%
  group_by(year)%>%
  summarize(count=n(),
            avg_pope.a= mean(pope.a, na.rm=T))
poph.0= fd.var.all %>%
  drop_na(poph.a)
poph.1= poph.0 %>%
  group_by(year,variety)%>%
  summarize(count=n(),
            avg_poph.a= mean(poph.a, na.rm=T))
poph.1.1= poph.1 %>%
  group_by(variety)%>%
  summarize(count=n(),
            avg_poph.a= mean(avg_poph.a, na.rm=T))
poph.2= poph.0 %>%
  group_by(year,site)%>%
  summarize(count=n(),
            avg_poph.a= mean(poph.a, na.rm=T))
poph.2.1= poph.2 %>%
  group_by(site)%>%
  summarize(count=n(),
            avg_poph.a= mean(avg_poph.a, na.rm=T))
poph.3= poph.0 %>%
  group_by(year)%>%
  summarize(count=n(),
            avg_poph.a= mean(poph.a, na.rm=T))
poph.aov = aov(poph.a~variety+site+year, data = poph.0)
summary(poph.aov)
TukeyHSD(poph.aov)

poph.mean = mean(poph.0$poph.a)
poph.median = median(poph.0$poph.a)
poph.var = var(poph.0$poph.a)
poph.sd = sd(poph.0$poph.a)
# height and stem
height.0= fd.var.all%>%
  drop_na(height)
height.1= height.0 %>%
  group_by(year,variety)%>%
  summarize(count=n(),
            avg_height= mean(height, na.rm=T))
height.1.1= height.1 %>%
  group_by(variety)%>%
  summarize(count=n(),
            avg_height= mean(avg_height, na.rm=T))
height.2= height.0 %>%
  group_by(year,site)%>%
  summarize(count=n(),
            avg_height= mean(height, na.rm=T))
height.3= height.0 %>%
  group_by(year)%>%
  summarize(count=n(),
            avg_height= mean(height, na.rm=T))
stem.0= fd.var.all%>%
  drop_na(stem)
stem.1= stem.0 %>%
  group_by(year,variety)%>%
  summarize(count=n(),
            avg_stem= mean(stem, na.rm=T))
stem.1.1= stem.1 %>%
  group_by(variety)%>%
  summarize(count=n(),
            avg_stem= mean(avg_stem, na.rm=T))
stem.2= stem.0 %>%
  group_by(year,site)%>%
  summarize(count=n(),
            avg_stem= mean(stem, na.rm=T))
stem.2.1= stem.2 %>%
  group_by(site)%>%
  summarize(count=n(),
            avg_stem= mean(avg_stem, na.rm=T))
stem.3= stem.0 %>%
  group_by(year)%>%
  summarize(count=n(),
            avg_stem= mean(stem, na.rm=T))
stem.mean = mean(stem.0$stem)
stem.sd = sd(stem.0$stem)
stem.se = sd(stem.0$stem)/sqrt(length(stem.0$stem))
# biomass and grain
biomass.0= fd.var.all%>%
  drop_na(straw.a)
biomass.0.1 = biomass.0%>%
  group_by(site,variety,year)%>%
  summarize(count=n(),
            avg_biomass=mean(straw.a, na.rm=T))
biomass.1= biomass.0 %>%
  group_by(year,variety)%>%
  summarize(count=n(),
            avg_biomass= mean(straw.a, na.rm=T))
biomass.1.1= biomass.1 %>%
  group_by(variety)%>%
  summarize(count=n(),
            avg_biomass= mean(avg_biomass, na.rm=T))
biomass.2= biomass.0 %>%
  group_by(year,site)%>%
  summarize(count=n(),
            avg_biomass= mean(straw.a, na.rm=T))
biomass.2.1= biomass.2 %>%
  group_by(site)%>%
  summarize(count=n(),
            avg_biomass= mean(avg_biomass, na.rm=T))
biomass.3= biomass.0 %>%
  group_by(year)%>%
  summarize(count=n(),
            avg_biomass= mean(straw.a, na.rm=T))
biomass.mean = mean(biomass.0$straw.a,na.rm=T)
biomass.sd = sd(biomass.0$straw.a,na.rm=T)
biomass.var = var(biomass.0$straw.a,na.rm=T)

grain.0= fd.var.all%>%
  drop_na(grain.a)
grain.0.1 = grain.0%>%
  group_by(site,variety,year)%>%
  summarize(count=n(),
            avg_grain=mean(grain.a, na.rm=T))
grain.1= grain.0 %>%
  group_by(year,variety)%>%
  summarize(count=n(),
            avg_grain= mean(grain.a, na.rm=T))
grain.1.1= grain.1 %>%
  group_by(variety)%>%
  summarize(count=n(),
            avg_grain= mean(avg_grain, na.rm=T))
grain.2= grain.0 %>%
  group_by(year,site)%>%
  summarize(count=n(),
            avg_grain= mean(grain.a, na.rm=T))
grain.2.1= grain.2 %>%
  group_by(site)%>%
  summarize(count=n(),
            avg_grain= mean(avg_grain, na.rm=T))
grain.3= grain.0 %>%
  group_by(year)%>%
  summarize(count=n(),
            avg_grain= mean(grain.a, na.rm=T))
grain.mean = mean(grain.0$grain.a, na.rm=T)
grain.sd = sd(grain.0$grain.a)
grain.var = var(grain.0$grain.a)
#### plotting stuff ####
#### statistics? ####
lm(poph.a ~ as.factor(variety),
   data = poph.0) 
  tidy %>%
  kableExtra::kable()
## Standard errors? ###
#variety
se.h.1 = sd(height.1$avg_height)/sqrt(length(height.1$avg_height))
se.st.1 = sd(stem.1$avg_stem)/sqrt(length(stem.1$avg_stem))
#se.pope.1=sd(pope.1$avg_pope.a)/sqrt(length(pope.1$avg_pope.a))
se.poph.1=sd(poph.1$avg_poph.a)/sqrt(length(poph.1$avg_poph.a))
se.g.1=sd(grain.1$avg_grain)/sqrt(length(grain.1$avg_grain))
se.b.1=sd(biomass.1$avg_biomass)/sqrt(length(biomass.1$avg_biomass))

#site
se.poph.2=sd(poph.2$avg_poph.a)/sqrt(length(poph.2$avg_poph.a))
# height
tiff("Var_across_all_height_26var", units = "in", width =8, height =6, res=300)
ggplot(height.1, aes(y=avg_height, x=variety, fill=year, color="red"))+
  geom_col(width=0.5,position=position_dodge(0.5))+
  geom_errorbar(data=height.0, aes(x=variety, y =height),stat="summary", 
                fun.data="mean_se", fun.args = list(mult = 1.96),size = 0.75, width=0.5,
                position=position_dodge(0.5))+
  labs(x="\n Hemp varieties", y="Plant height (cm)", fill="Year \n")+
  scale_y_continuous(breaks=seq(0,180,30))+
  coord_cartesian(ylim=c(10,180))+
  scale_fill_manual(values=c("#4575b4","#cc4c02","#f0f0f0"))+
  scale_color_manual(values=c("black"))+
  guides(color="none")+
  theme_bw(base_size = 14) +
  theme_classic()+
  theme(panel.background = element_blank())+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.text.x = element_text(size = 12,angle=45, hjust = 1, vjust = 1),
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(face="bold", size = 14))
dev.off()                  
# stem
tiff("Var_across_all_stem_26var", units = "in", width =8, height =6, res=300)
ggplot(stem.1, aes(y=avg_stem, x=variety, fill=year, color="red"))+
  geom_col(width=0.5,position=position_dodge(0.5))+
  geom_errorbar(data=stem.0, aes(x=variety, y =stem),stat="summary", 
                fun.data="mean_se", fun.args = list(mult = 1.96),size = 0.75, width=0.5,
                position=position_dodge(0.5))+
  labs(x="\n Hemp varieties", y="Stem diameter (mm)", fill="Year \n")+
  scale_y_continuous(breaks=seq(0,12,1.5))+
  coord_cartesian(ylim=c(0.3,12))+
  scale_fill_manual(values=c("#4575b4","#cc4c02","#f0f0f0"))+
  scale_color_manual(values=c("black"))+
  guides(color="none")+
  theme_bw(base_size = 14) +
  theme_classic()+
  theme(panel.background = element_blank())+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.text.x = element_text(size = 12,angle=45, hjust = 1, vjust = 1),
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(face="bold", size = 12))
dev.off()                      
# population at harvest
tiff("Var_across_all_poph_alphab_1024", units = "in", width =8, height =6, res=300)
ggplot(poph.1, aes(y=avg_poph.a, x=variety, fill=year, color="red"))+
  geom_col(width=0.5,position=position_dodge(0.5))+
  geom_errorbar(data=poph.0, aes(x=variety, y =poph.a),stat="summary", 
                fun.data="mean_se", fun.args = list(mult = 1.96),size = 0.75, width=0.5,
                position=position_dodge(0.5))+
  labs(title= "Mean plant population at harvest", y="\n Plants per hectare",x=" Hemp varieties", fill="Year \n")+
  scale_y_continuous(breaks=seq(0,1000000,150000))+
  coord_cartesian(ylim=c(35000,1000000))+
  scale_fill_manual(values=c("#cc4c02","#f0f0f0"))+
  scale_color_manual(values=c("black"))+
  guides(color="none")+
  theme_bw(base_size = 14) +
  theme_classic()+
  theme(panel.background = element_blank())+
  theme(plot.title = element_text(face="bold", colour="black", size = 15, hjust = 0.5), 
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.text.x = element_text(size = 12,angle=45, hjust = 1, vjust = 1),
        legend.title = element_text(face="bold", size = 12))
dev.off()

# population at harvest
tiff("Site_across_all_poph", units = "in", width =8, height =6, res=300)
ggplot(poph.2, aes(y=avg_poph.a, x=site, fill=year, color="red"))+
  geom_col(width=0.5,position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=avg_poph.a-se.poph.2, ymax=avg_poph.a+se.poph.2), color="black",
                position=position_dodge(0.5), width=.25)+
  labs(x="\n Sites", y="Population at harvest ha", fill="Year \n")+
  scale_y_continuous(breaks=seq(0,1550000,300000))+
  coord_cartesian(ylim=c(100000,1550000))+
  scale_fill_manual(values=c("black","grey","white"))+
  scale_color_manual(values=c("black"))+
  guides(color="none")+
  theme_classic()+
  theme(panel.background = element_blank())+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        axis.text.x = element_text(angle=45, hjust = 1, vjust = 1),
        legend.title = element_text(face="bold", size = 12))
dev.off()

#### GGplot biomass and grain ####
#### 6.5 inch paper width max ##

mean(grain.0.1$avg_grain)

tiff("Grain_avg_comparison_26var21", units = "in", width =10, height =6, res=300)
ggplot(grain.0.1)+ 
  geom_point(data = grain.0.1, aes(x= reorder(variety, -avg_grain), y = avg_grain, shape = site, fill = site), 
             size = 7, alpha = 0.85) +
  ylim(5,2500)+
  guides( size = "none")+
  guides( color = guide_legend(override.aes = list(size = 5)))+
  scale_shape_manual(name = "Site",
                     breaks = c('NORTH DAKOTA','MONTANA','VERMONT','WISCONSIN CHIPPEWAFALLS',
                                'WISCONSIN ARLINGTON','MICHIGAN' ,'NEW YORK' ,'KANSAS MANHATTAN',
                                'MARYLAND','KENTUCKY LEXINGTON','KANSAS WICHITA',
                                'KENTUCKY QUICKSAND','VIRGINIA','TENNESSEE'),
                     values =c(21,22,23,21,22,23,21,22,23,21,22,23,21,22))+
  
  scale_fill_manual(name = "Site",
                    breaks = c('NORTH DAKOTA','MONTANA','VERMONT','WISCONSIN CHIPPEWAFALLS',
                               'WISCONSIN ARLINGTON','MICHIGAN' ,'NEW YORK' ,'KANSAS MANHATTAN',
                               'MARYLAND','KENTUCKY LEXINGTON','KANSAS WICHITA',
                               'KENTUCKY QUICKSAND','VIRGINIA','TENNESSEE'),
                    
                    values = c('#2166ac','#084081','#3690c0','#92c5de',
                               '#92c6df', '#e0f3f8','#e0f5fb','#ffeda0',
                               '#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c',
                               '#bd0026'))+
  labs(title = "Grain Production (2021)", x = "Hemp varieties", y = "Mean Grain Yield \n (kg/ha)", 
       color = "Site") +
  geom_point(data=grain.0.1, aes(x=variety, y=avg_grain), stat="summary", fun="mean",
             color= "black", size = 5, alpha=.95, na.rm=T)+
  geom_hline(yintercept=808, linetype='longdash', col = 'red', size = 1)+
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(size = 18, hjust = 0.5))+
  theme(plot.background = element_blank())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))+
  theme(legend.title = element_text(size =14))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

tiff("Drystrawyield_avg_comparison_26var21", units = "in", width =10, height =6, res=300)
ggplot(biomass.0.1)+ 
  geom_point(data = biomass.0.1, aes(x= reorder(variety,-avg_biomass), y = avg_biomass, shape = site, fill = site), 
             size = 7, alpha = 0.85) +
  ylim(5,13400)+
  guides( size = "none")+
  guides( color = guide_legend(override.aes = list(size = 5)))+
  scale_shape_manual(name = "Site",
                     breaks = c('NORTH DAKOTA','MONTANA','VERMONT','WISCONSIN CHIPPEWAFALLS',
                                'WISCONSIN ARLINGTON','MICHIGAN' ,'NEW YORK' ,'KANSAS MANHATTAN',
                                'MARYLAND','KENTUCKY LEXINGTON','KANSAS WICHITA',
                                'KENTUCKY QUICKSAND','VIRGINIA','TENNESSEE'),
                     values =c(21,22,23,21,22,23,21,22,23,21,22,23,21,22))+
  
  scale_fill_manual(name = "Site",
                    breaks = c('NORTH DAKOTA','MONTANA','VERMONT','WISCONSIN CHIPPEWAFALLS',
                               'WISCONSIN ARLINGTON','MICHIGAN' ,'NEW YORK' ,'KANSAS MANHATTAN',
                               'MARYLAND','KENTUCKY LEXINGTON','KANSAS WICHITA',
                               'KENTUCKY QUICKSAND','VIRGINIA','TENNESSEE'),
                    
                    values = c('#2166ac','#084081','#3690c0','#92c5de',
                               '#92c6df', '#e0f3f8','#e0f5fb','#ffeda0',
                               '#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c',
                               '#bd0026'))+
  labs(title = "Straw Production (2021)", x = "Hemp varieties", y = "Mean Dry Straw Yield \n (kg/ha)", 
       color = "Site") +
  geom_point(data=biomass.0.1, aes(x=variety, y=avg_biomass), stat="summary", fun="mean",
             color= "black", size = 5, alpha=.95, na.rm=T)+
  geom_hline(yintercept=3160, linetype='longdash', col = 'red', linewidth = 1)+
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(size = 18, hjust = 0.5))+
  theme(plot.background = element_blank())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))+
  theme(legend.title = element_text(size =14))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

#### geom_errorbar(data=biomass.0.1, aes(x=variety, y= avg_biomass),stat="summary", 
          #    fun.data="mean_se", fun.args = list(mult = 1.96),size = 1.25, width=0.4)+

#### Example stuff ###
compare_means(avg_biomass ~ variety,  data = biomass.0.1, method = "anova")
ggboxplot(biomass.0.1, x = "variety", y = "avg_biomass",
          color = "site", palette = "ggthemes::Classic Orange-Blue")+
  stat_compare_means()
paletteer_c("ggthemes::Classic Orange-Blue", 30) 
# Visualize the expression profile
ggboxplot(fd.all, x = "variety", y = "straw.a", color = "site", 
          add = "jitter", legend = "none") +
  rotate_x_text(angle = 45)+
  geom_hline(yintercept = mean(fd.all$straw.a), linetype = 2)+ # Add horizontal line at base mean
  stat_compare_means(method = "anova", label.y = 1600)+        # Add global annova p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.") 
##########################################################
# filter for varieties across all 3 years (2019-2021) ####
var.19= fd.all %>%
  select(year, site, variety)%>%
  filter(year=="2019")
var.19$variety=as.factor(as.character(var.19$variety))
var.19.list=var.19 %>%
  sapply(levels)
var.19.list=var.19.list[["variety"]]

var.20= fd.all %>%
  select(year, site, variety)%>%
  filter(year=="2020")
var.20$variety=as.factor(as.character(var.20$variety))
var.20.list=var.20 %>%
  sapply(levels)
var.20.list=var.20.list[["variety"]]

var.21= fd.all %>%
  select(year, site, variety)%>%
  filter(year=="2021")
var.21$variety=as.factor(as.character(var.21$variety))
var.21.list=var.21 %>%
  sapply(levels)
var.21.list=var.21.list[["variety"]]

# varieties across all 3 years #
# "ANKA","CFX-1","CFX-2","GRANDI","H-51","HLESIA","HLIANA","KATANI","PICOLO","X-59"

#### Testing new old code ####
#### Z WORK ####

# Variety Within Site Average & Z Score Grain and Straw ####

z_yields<- function(f) {
  data <- read.csv(f)
  data.avg<- data %>%
    select(site, variety, grain.m, grain.a, straw.m, straw.a)%>%
    mutate(site = toupper(site), variety = toupper(variety)) %>% 
    group_by(site,variety)
  
  data.avg$variety=str_replace(data.avg$variety, "HLUKAUSKII51","H-51")
  data.avg$variety=str_replace(data.avg$variety, "HLUKHOVSKII51", "H-51")
  data.avg$variety=str_replace(data.avg$variety, "HLUKAUSKII 51", "H-51")
  data.avg$variety=str_replace(data.avg$variety, "KATANI ", "KATANI")
  
  data.avg <- subset(data.avg, !(variety %in% c('AMAZE AUTO','CANDA','CS SEME','ELETTA CAMPANA',
                                    'ENECTAROL','FIBRANOVA','FIBROR 79','FERIMON',
                                    'FUTURA 83','GC-2','JOEY','HELENA','RIGEL',
                                    'JIN MA','MARTHA','SANTHICA 70','SS BETA' ,
                                    'TYGRA','VICTORIA','WI-M-H-19-00112')))
  
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
  j = c(as.numeric(3,4))
  z_yields$site = as.factor(z_yields$site)
  z_yields$variety = as.factor(z_yields$variety)
  z_yields$grain.a = as.integer(z_yields$grain.a)
  z_yields$straw.a = as.integer(z_yields$straw.a)
  z_yields$z_grain = as.numeric(z_yields$z_grain)
  z_yields$z_straw = as.numeric(z_yields$z_straw)
  return(z_yields)
}

# Combine average and Z score
zy.comb= function(z_yields){
  avg_z_list.19 <- lapply((list.files(path="./data",pattern="multistate_2019_[0-9]{3}.csv",
                                      full.names=TRUE)),z_yields)
  z_data.19<- bind_rows(avg_z_list.19)
  z_data.19$year = as.factor(2019)
  avg_z_list.20 <- lapply((list.files(path="./data",pattern="multistate_2020_[0-9]{3}.csv",
                                      full.names=TRUE)),z_yields)
  z_data.20<- bind_rows(avg_z_list.20)
  z_data.20$year = as.factor(2020)
  avg_z_list.21 <- lapply((list.files(path="./data",pattern="multistate_2021_[0-9]{3}.csv",
                                      full.names=TRUE)),z_yields)
  z_data.21<- bind_rows(avg_z_list.21)
  z_data.21$year = as.factor(2021)
  z_data_list = list(z_data.19,z_data.20,z_data.21)
  z.gs = bind_rows(z_data_list)
  
  return(z.gs)
}

### Call Z data ####

zy.all=zy.comb(z_yields)
zy.var.all = zy.all
#### varieties ####

zy.var.all$variety=as.factor(as.character(zy.var.all$variety))

z.biomass.0= zy.var.all%>%
  drop_na(z_straw)
z.biomass.1= z.biomass.0 %>%
  group_by(year,variety,site)%>%
  summarize(count=n(),
            z_biomass= mean(z_straw, na.rm=T))
z.biomass.1.0= z.biomass.1 %>%
  group_by(variety)%>%
  summarize(count=n(),
            z_biomass= mean(z_biomass, na.rm=T))
z.biomass.1.1= z.biomass.1 %>%
  group_by(year,variety)%>%
  summarize(count=n(),
            z_biomass= mean(z_biomass, na.rm=T))
z.biomass.1.2= z.biomass.1 %>%
  group_by(site,variety)%>%
  summarize(count=n(),
            z_biomass= mean(z_biomass, na.rm=T))
z.biomass.2= z.biomass.0 %>%
  group_by(variety,site)%>%
  summarize(count=n(),
            z_biomass= mean(z_straw, na.rm=T))
z.biomass.2.1= z.biomass.2 %>%
  group_by(site)%>%
  summarize(count=n(),
            z_biomass= mean(z_biomass, na.rm=T))
z.biomass.3= z.biomass.0 %>%
  group_by(year)%>%
  summarize(count=n(),
            avg_biomass= mean(straw.a, na.rm=T))
z.bio.sd = sd(z.biomass.1.0$z_biomass)
# grain
z.grain.0= zy.var.all%>%
  drop_na(z_grain)
z.grain.1= z.grain.0 %>%
  group_by(year,variety,site)%>%
  summarize(count=n(),
            z_grain= mean(z_grain, na.rm=T))
z.grain.1.1= z.grain.1 %>%
  group_by(year,variety)%>%
  summarize(count=n(),
            avg_grain= mean(z_grain, na.rm=T))
z.grain.1.2= z.grain.1 %>%
  group_by(variety)%>%
  summarize(count=n(),
            avg_grain= mean(z_grain, na.rm=T))
z.grain.2= z.grain.0 %>%
  group_by(variety,site)%>%
  summarize(count=n(),
            avg_grain= mean(z_grain, na.rm=T))
z.grain.2.1= z.grain.2 %>%
  group_by(site)%>%
  summarize(count=n(),
            avg_grain= mean(avg_grain, na.rm=T))
z.grain.3= z.grain.0 %>%
  group_by(year)%>%
  summarize(count=n(),
            avg_grain= mean(grain.a, na.rm=T))

library(ggpubr)

my_comparisons = list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )

ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons, label.y = c(29, 35, 40))+
  stat_compare_means(label.y = 45)

# Biomass
tiff("Drystraw_z_95_alphab", units = "in", width =10, height =6, res=300)
ggplot(z.biomass.1) + 
  geom_point(data=z.biomass.1, aes(x=variety, y=z_biomass, 
                                   shape=site, fill= site), size = 7, alpha=.80) +
  ylim(-1.75,1.75)+
  guides( size = "none")+
  guides( color = guide_legend(override.aes = list(size = 5)))+
  scale_shape_manual(name = "Site",
                     breaks = c('NORTH DAKOTA','MONTANA','VERMONT','WISCONSIN CHIPPEWAFALLS',
                                'WISCONSIN ARLINGTON','MICHIGAN' ,'NEW YORK' ,'KANSAS MANHATTAN',
                                'MARYLAND','KENTUCKY LEXINGTON','KANSAS WICHITA',
                                'KENTUCKY QUICKSAND','VIRGINIA','TENNESSEE'),
                     values =c(21,22,23,21,22,23,21,22,23,21,22,23,21,22))+
  
  scale_fill_manual(name = "Site",
                    breaks = c('NORTH DAKOTA','MONTANA','VERMONT','WISCONSIN CHIPPEWAFALLS',
                               'WISCONSIN ARLINGTON','MICHIGAN' ,'NEW YORK' ,'KANSAS MANHATTAN',
                               'MARYLAND','KENTUCKY LEXINGTON','KANSAS WICHITA',
                               'KENTUCKY QUICKSAND','VIRGINIA','TENNESSEE'),
                    
                    values = c('#2166ac','#084081','#3690c0','#92c5de',
                               '#92c6df', '#e0f3f8','#e0f5fb','#ffeda0',
                               '#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c',
                               '#bd0026'))+
  labs(title = "Dry Straw Performance", x = "Variety", y = "Z Score", 
       color = "Site") +
  geom_point(data=z.biomass.1, aes(x= variety, y=z_biomass), stat="summary", fun="mean",
             color= "black", size = 6, alpha=.95, na.rm=T)+
  geom_errorbar(data=z.biomass.1, aes(x= variety, y=z_biomass),stat="summary", 
                fun.data="mean_se", fun.args = list(mult = 1.96),size = 1.25, width=0.4)+
  geom_hline(yintercept=0, linetype='longdash', col = 'red', linewidth = 1)+
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(size = 18, hjust = 0.5))+
  theme(plot.background = element_blank())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))+
  theme(legend.title = element_text(size =15))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# Grain 
tiff("grain_z_95_alphab", units = "in", width =10, height =6, res=300)
ggplot(z.grain.1) + 
  geom_point(data=z.grain.1, aes(x= variety, y=z_grain, shape=site, fill= site), size = 7, alpha=.80) +
  ylim(-2.25,2.25)+
  guides( size = "none")+
  guides( color = guide_legend(override.aes = list(size =5)))+
  scale_shape_manual(name = "Site",
                     breaks = c('NORTH DAKOTA','MONTANA','VERMONT','WISCONSIN CHIPPEWAFALLS',
                                'WISCONSIN ARLINGTON','MICHIGAN' ,'NEW YORK' ,'KANSAS MANHATTAN',
                                'MARYLAND','KENTUCKY LEXINGTON','KANSAS WICHITA',
                                'KENTUCKY QUICKSAND','VIRGINIA','TENNESSEE'),
                     values =c(21,22,23,21,22,23,21,22,23,21,22,23,21,22))+
  
  scale_fill_manual(name = "Site",
                    breaks = c('NORTH DAKOTA','MONTANA','VERMONT','WISCONSIN CHIPPEWAFALLS',
                               'WISCONSIN ARLINGTON','MICHIGAN' ,'NEW YORK' ,'KANSAS MANHATTAN',
                               'MARYLAND','KENTUCKY LEXINGTON','KANSAS WICHITA',
                               'KENTUCKY QUICKSAND','VIRGINIA','TENNESSEE'),
                    
                    values = c('#2166ac','#084081','#3690c0','#92c5de',
                               '#92c6df', '#e0f3f8','#e0f5fb','#ffeda0',
                               '#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c',
                               '#bd0026'))+
  labs(title = "Grain Performance", x = "Variety", y = "Z Score", 
       color = "Site") +
  geom_point(data=z.grain.1, aes(x=variety, y=z_grain), stat="summary", fun="mean", 
             color= "black", size = 6, alpha=.95, na.rm=T)+
  geom_errorbar(data=z.grain.1, aes(x=variety, y =z_grain),stat="summary", 
                fun.data="mean_se", fun.args = list(mult = 1.96),size = 1.25, width=0.4)+
  geom_hline(yintercept=0, linetype='longdash', col = 'red', linewidth = .75)+
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(size = 18, hjust = 0.5))+
  theme(plot.background = element_blank())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))+
  theme(legend.title = element_text(size =15))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

#### Data manipulation 
var.19.el = fd.var.all %>%
  select(year, site, variety)%>%
  filter(year=="2019")
var.19.el$variety=as.factor(as.character(var.19.el$variety))
var.19.el.list=var.19.el %>%
  sapply(levels)
var.19.el.list=var.19.el.list[["variety"]]
var.19.el.comb = var.19.el %>%
  group_by(year, variety)%>%
  summarize(count=n())

var.20.el = fd.var.all %>%
  select(year, site, variety)%>%
  filter(year=="2020")
var.20.el$variety=as.factor(as.character(var.20.el$variety))
var.20.el.list=var.20.el %>%
  sapply(levels)
var.20.el.list=var.20.el.list[["variety"]]
var.20.el.comb = var.20.el %>%
  group_by(year, variety)%>%
  summarize(count=n())

var.21.el = fd.var.all %>%
  select(year, site, variety)%>%
  filter(year=="2021")
var.21.el$variety=as.factor(as.character(var.21.el$variety))
var.21.el.list=var.21.el %>%
  sapply(levels)
var.21.el.list=var.21.el.list[["variety"]]
var.21.el.comb = var.21.el %>%
  group_by(year, variety)%>%
  summarize(count=n())

#### GGplot stuff####
# loading the appropriate libraries
library(ggplot2)
library(ggthemes)
library(dplyr)
library(multcompView)
library(egg)


poph.0= fd.var.all %>%
  drop_na(poph.a)
poph.1= poph.0 %>%
  group_by(year,variety)%>%
  summarize(count=n(),
            avg_poph.a= mean(poph.a, na.rm=T))
poph.1.1= poph.1 %>%
  group_by(variety)%>%
  summarize(count=n(),
            avg_poph.a= mean(avg_poph.a, na.rm=T))
poph.2= poph.0 %>%
  group_by(year,site)%>%
  summarize(count=n(),
            avg_poph.a= mean(poph.a, na.rm=T))
poph.2.1= poph.2 %>%
  group_by(site)%>%
  summarize(count=n(),
            avg_poph.a= mean(avg_poph.a, na.rm=T))
poph.3= poph.0 %>%
  group_by(year)%>%
  summarize(count=n(),
            avg_poph.a= mean(poph.a, na.rm=T))
poph.aov = aov(poph.a~variety+site+year, data = poph.0)
summary(poph.aov)
poph.tukey=TukeyHSD(poph.aov)
poph.cld = multcompLetters4(poph.aov, poph.tukey)

poph.tidyfit = tidy(poph.aov)
# analysis of variance
poph.aov <- aov(poph.a ~ variety*year, data = poph.0)

# Tukey's test
poph.tukey <- TukeyHSD(poph.aov)

# compact letter display
poph.cld <- multcompLetters4(poph.aov, poph.tukey)

# table with factors and 3rd quantile
poph.dt <- group_by(poph.0, variety, year) %>%
  summarise(w=mean(poph.a), sd = sd(poph.a)) %>%
  arrange(desc(w))

# extracting the compact letter display and adding to the Tk table
poph.cld <- as.data.frame.list(poph.cld[["variety:year"]])
poph.dt$poph.cld <- poph.cld$Letters

print(poph.dt)

# population at harvest
tiff("Var_across_all_poph_0830", units = "in", width =8, height =6, res=300)
ggplot(poph.1, aes(x=avg_poph.a, y=variety, fill=year, color="red"))+
  geom_col(width=0.5,position=position_dodge(0.5))+
  geom_errorbar(data=poph.0, aes(y=variety, x =poph.a),stat="summary", 
                fun.data="mean_se", fun.args = list(mult = 1.96),size = 0.75, width=0.5,
                position=position_dodge(0.5))+
  labs(title= "Mean plant population at harvest", x="\n Plants per hectare", y=" Hemp varieties", fill="Year \n")+
  
  scale_x_continuous(breaks=seq(0,1000000,150000))+
  coord_cartesian(xlim=c(35000,1000000))+
  scale_fill_manual(values=c("#cc4c02","#f0f0f0"))+
  scale_color_manual(values=c("black"))+
  guides(color="none")+
  geom_text(aes(label = poph.cld))+
  theme_bw(base_size = 13) +
  theme_classic()+
  theme(panel.background = element_blank())+
  theme(plot.title = element_text(face="bold", colour="black", size = 15, hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 13),
        axis.title.y = element_text(face="bold", colour="black", size = 13),
        legend.title = element_text(face="bold", size = 12))
dev.off()

#### ANOVAS #####

# two-way ANOVA of raw grain and straw yield ####
grain.aov = aov(grain.a~variety+site+year+
                  variety*site+variety*year+
                  site*year+
                  variety*site*year, data = grain.0)
summary(grain.aov)
grain.tukey <- TukeyHSD(grain.aov)
summary(grain.aov.new)
biomass.aov = aov(straw.a~variety+site+year+
                  variety*site+variety*year+
                  site*year+
                  variety*site*year, data = biomass.0)
summary(biomass.aov)
plot(grain.aov)
plot(straw.aov)
# two-way ANOVA of raw population establishment/at harvest
pope.aov = aov(pope.a~variety+site+year, data = fd.all)
summary(pope.aov)
poph.aov = aov(poph.a~variety+site+year+
                   variety*site+variety*year+
                   site*year+
                   variety*site*year, data =poph.0)
summary(poph.aov)
plot(pope.aov)
plot(poph.aov)
# two-way ANOVA of raw height/stem
height.aov = aov(height~variety+site+year+
                 variety*site+variety*year+
                 site*year+
                 variety*site*year, data = height.0)
summary(height.aov)
stem.aov = aov(stem~variety+site+year+
                  variety*site+variety*year+
                  site*year+
                  variety*site*year, data = stem.0)
summary(stem.aov)
str(height.aov)
plot(height.aov)
plot(stem.aov)