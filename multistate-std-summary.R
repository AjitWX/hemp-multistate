library(tidyverse)

std_data <- function(site_name, yield_value, site_data){
  site_values <- filter(site_data, site_data$site == site_name) 
  std_yield <- (yield_value - site_values$site_avg_yield) / site_values$site_sd_yield
  return(std_yield)
}

convert_m_to_a <- function(yield_in_a, yield_in_m){
  if (exists(yield_in_a)){
    return(yield_in_a)
  } else {
    yeild_in_a_out <- 8.9217*yield_in_m
    return(yield_in_a_out)
  }
}

data<-read_csv("data/multistate_2020_fulldata.csv")

grain_yield <- data %>% 
  select(site, variety, grain.a, grain.m, straw.m, straw.a) %>% 
#  rowwise %>% 
#  mutate(grain.a = convert_m_to_a(grain.a, grain.m))


avg_grain_yield <- grain_yield %>%
  group_by(site, variety) %>% 
  summarize(count = n(),    
            avg_grain.a = mean(grain.a, na.rm=T),
            se_grain = sd(grain.a, na.rm=T)/sqrt(n()))

site_grain_yield <- avg_grain_yield %>% 
  group_by(site) %>% 
  summarize(site_avg_yield = mean(avg_grain.a),
            site_sd_yield = sd(avg_grain.a))

std_grain_yield <- avg_grain_yield %>%
  filter(!is.na(avg_grain.a)) %>% 
  rowwise %>%  
  mutate(std_grain = std_data(site, avg_grain.a, site_grain_yield)) %>% 
  arrange(site, desc(avg_grain.a))

ggplot(std_grain_yield, aes(x=variety, y=std_grain)) +
  geom_bar(stat="identity") +
  facet_wrap(vars(site)) + #, scale="free_x") +
  theme(axis.text.x = element_text(angle = 90)) 

ggsave("output/std_grain.png", width=10, height=10)

std_grain_yield_variety <- std_grain_yield %>% 
  group_by(variety) %>% 
  summarize(avg_std_grain = mean(std_grain),
            max_std_grain = max(std_grain),
            min_std_grain = min(std_grain),
            site_count = n()) %>% 
  arrange(desc(avg_std_grain))

for (site_name in std_grain_yield$site){
  
  site_data <- std_grain_yield %>% 
    filter(site == site_name) %>% 
    arrange(desc(avg_grain.a))
  
  ggplot(site_data, aes(x=variety, y=std_grain)) +
    geom_bar(stat="identity") +
    facet_wrap(vars(site)) +
    theme(axis.text.x = element_text(angle = 90))
  
  ggsave(paste0("output/std_grain_", site_name, ".png"), width=10, height=10)
}
