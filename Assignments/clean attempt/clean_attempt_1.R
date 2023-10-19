library(tidyverse)
library(readxl)
library(janitor)
path <- "./Worst Data Storage Ever.xlsx"
df1 <- read_xlsx(path, n_max = 10)
df2 <- read_xlsx(path, skip = 10, n_max = 5)
df3 <- read_xlsx(path, skip = 15)
