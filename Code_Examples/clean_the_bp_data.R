library(tidyverse)
library(janitor)
library(readxl)
source("./Code_Examples/pivot_multiple.R")
df <- read_xlsx("./Data/messy_bp.xlsx", skip=3) %>% clean_names()
visits <- read_xlsx("./Data/messy_bp.xlsx", skip = 2,n_max = 0) %>% select(-starts_with("...")) %>% names() 
test <- df %>% pivot_longer_multiple(c("bp","hr"),fix_names = TRUE) %>% rename(visit = names) %>% 
  mutate(visit = visits[as.numeric(visit)])
