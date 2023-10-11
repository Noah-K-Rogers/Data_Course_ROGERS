library(tidyverse)
library(GGally)
dat <- readRDS("./Code_Examples/cleaned_bp.rds")
dat %>% select(-c(pat_id,month_of_birth,day_birth,year_birth)) %>%  ggpairs()

dat %>% ggplot(aes(x=visit))+
  geom_point(aes(y = systolic),color = "black")+
  geom_point(aes(y = diastolic),color = "red")+
  stat_summary(aes(y=systolic), geom = "path", color = "black")+
  stat_summary(aes(y=diastolic), geom = "path", color = "red")+
  facet_wrap(~race)
