library(tidyverse)
library(readxl)
library(janitor)
source("./Code_Examples/pivot_multiple.R")
goal <- read.csv("./Data/cleaned_bird_data.csv") %>% clean_names()
df <- read_csv("./Data/Bird_Measurements.csv") %>% clean_names()
df <- df %>% select(-ends_with("_n"))
test <- pivot_longer_multiple(df,c("mass","wing","tarsus","tail","bill"),"egg_mass")
test <- test %>% mutate(gender = case_when(names == "m_" ~ "Male",
                                           names == "f_" ~ "Female",
                                           names == "unsexed_" ~ "Unsexed")) %>% 
  select(-names)


arrange(test,family) == arrange(goal,family)
