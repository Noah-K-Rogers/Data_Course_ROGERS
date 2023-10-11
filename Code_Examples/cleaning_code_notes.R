#every variable gets its own column 
#every observation gets its own row
#rectangular
library(tidyverse)
table1 %>% 
  ggplot(aes(x=year,y=cases,color=country))+
  geom_path()
table2
table3
table2 %>% pivot_wider(names_from = type, values_from = count)
table3 %>% separate(rate,into=c("cases","population"))
table4a
table4b
?combine()
?join
full_join(table4b,table4a)
table4b %>% pivot_longer(-country, names_to = "year",values_to = "population") %>% 
full_join(table4a %>% pivot_longer(-country, names_to = "year",values_to = "cases")
          
table5 %>% mutate(year=paste0(century,year) %>% as.numeric()) %>% 
  select(-century) %>% separate(rate, into=c("cases","population"))
library(lubridate)
library(janitor)
library(readxl)
#read in data from excel
df1 <- read_xlsx("~/Downloads/popquiz data.xlsx") %>% clean_names()
#fix names col 1
names(df1)[1] <- "site"
dates <- janitor::excel_numeric_to_date(as.numeric(df1$site[1:3]))
#pull month abbr and uppercase
part1 <- month(dates, label = TRUE,  abbr=TRUE) %>% str_to_upper()
#pull days
part2 <- lubridate::day(dates)
#paste them together seperated by -
finalproduct <- paste(part1,part2,sep="-")
df1$site[1:3] <- finalproduct
df1 <- 
df1 %>% separate(site, into=c("location","site")) %>%
  pivot_longer(starts_with("week"),
               names_to = "week",
               values_to="rel_abund",
               names_prefix = "week_",
               names_transform=as.numeric
               )


df2 <- read_xlsx("~/Downloads/organized dataset.xlsx") %>% clean_names()
df2$site <- df2$site %>% str_replace(" Pool","Pool")
df2 <- 
df2 %>% separate(site,c("location","site")) %>% 
  pivot_longer(starts_with("week"),
                                                             names_to = "week",
                                                             values_to="rel_abund",
                                                             names_prefix = "week_",
                                                             names_transform=as.numeric
)
identical(df1,df2)

df2 <- df2 %>% mutate(location = case_when(location == "SewagePool"~"SEP",
                                        location == "Hatchery"~"HAT"))
print(df1, n=24)
print(df2,n=24)
?read_xlsx
