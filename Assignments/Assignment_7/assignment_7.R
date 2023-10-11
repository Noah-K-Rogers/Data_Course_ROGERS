library(tidyverse)
library(janitor)
#read in and clean names
df <- read.csv("./Utah_Religions_by_County.csv") %>% clean_names()
#tidy data with a simple pivot longer
df <- df %>% pivot_longer(-c(county,pop_2010,religious, non_religious), names_to = "religion", values_to = "Proportion") %>% 
mutate(county = as.factor(str_replace(county, " ", "_"))) %>% mutate(religion = as.factor(religion))
#Check a bunch of relations with ggpairs...
df %>% ggpairs(cardinality_threshold = 29)
#I dont know what im looking at here tbh. unsurprisingly theres a perfect correlation between religious and non religous.


#ploting each religion against the population to check if theres any correlation between those.
df %>% ggplot(aes(x=pop_2010,y=Proportion))+
  geom_point() +geom_smooth(method = "lm")+facet_wrap(df$religion)
#not really, some show small change depending on population but it could easily be just by chance as its such small scale for those ones
#testing non religous vs each religion 
df %>% ggplot(aes(x=non_religious, y= Proportion))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(df$religion)
#most of the proportions are small enough to not be affected but LDS proportion decreases as non_religous increases because
#it turns out you cant maintain 75+% lds when 50% of people arent religious

