library(tidyverse)
library(janitor)
library(readxl)
library(gganimate)
#Read in data and clean names
df <- read_csv("../../Data/BioLog_Plate_Data.csv") %>% clean_names()

#clean data (turn columns of each hour into a column stating hour and a coulumn of absorbance data)
df <- df %>% pivot_longer(starts_with("hr"), values_to = "absorbance", names_to = "hour") %>% 
#remove the hr_ from in front of the time and chang eto numbers
  mutate(hour = str_remove(hour, "hr_") %>% as.numeric()) %>% 
#add new column of type to check if its from soil or water
  mutate(Type = case_when(sample_id == "Clear_Creek"|sample_id =="Waste_Water"~"water",str_detect(sample_id,"Soil")~"soil"))

#subset the data needed for first graph
dilute <- df %>% subset(df$dilution == .1)
#plot first graph
dilute %>% ggplot(aes(x=hour,y=absorbance,color = Type))+
  geom_smooth(se = FALSE) +
  facet_wrap(as.factor(dilute$substrate)) +
  theme_minimal()+
  labs(title = "Just dilution 0.1", x = "Time", y = "Absorbance")
#subset data for second graph
ita <- subset(df, df$substrate == "Itaconic Acid")

#get mean of data (this worked but im sure there's a better way)

ita_mean <- ita %>% subset(ita$rep == 1)
rep2 <- ita %>% subset(ita$rep == 2)
rep3 <- ita %>% subset(ita$rep == 3)
ita_mean %>% mutate(absorbance = (absorbance + rep2$absorbance + rep3$absorbance)/3) %>% 
#plot second graph
 ggplot(aes(x=hour,y=absorbance,color = sample_id)) +
  geom_line()+
  facet_wrap(ita_mean$dilution)+
  theme_minimal()+
  labs(x="Time",y="Mean_absorbance", color = "Sample ID")+
  transition_reveal(ita_mean$hour)





