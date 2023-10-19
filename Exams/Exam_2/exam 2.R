library(MASS)
library(tidyverse)
library(janitor)
library(easystats)
library(modelr)
#read and clean data####

#read in data
df <- read.csv("./unicef-u5mr.csv")
#tidy data. pretty simple pivot longer, remove the U5MR. and turn it to numbers 
df <- df %>% pivot_longer(starts_with("U5MR"), names_to = "Year",values_to = "U5MR",names_prefix = "U5MR.") %>% 
  mutate(Year = Year %>% as.numeric())

#plot 1####
#plot data... apparently you can just add more variables for it to group by but certain names, like color, have actual uses attached.
df %>%  ggplot(aes(x=Year,y=U5MR,randomvariablenamexDthisstillworksdontjudge = CountryName))+
  geom_line()+
  facet_wrap(~Continent)+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white"))
ggsave("ROGERS_Plot_1.png")
#plot 2 ####
df %>% group_by(Continent,Year) %>% summarize(Mean_U5MR = mean(U5MR,na.rm = TRUE)) %>% 
  ggplot(aes(x=Year,y=Mean_U5MR,color=Continent))+
  geom_line(size = 2)+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white"))
ggsave("ROGERS_Plot_2.png")

#Modeling####
mod1 <- glm(data = df, U5MR~Year)
mod2 <- glm(data = df, U5MR~Year+Continent)
mod3 <- glm(data = df, U5MR~Year*Continent)
#compare models
compare_performance(mod1,mod2,mod3) %>% plot
#mod3 seems to be the best for every category making it the best of these 3 models

#create dataset with models
#get just comlumns needed for prediction with current models
model_df <- df %>% select(Continent,Year) %>% 
  #remove unneeded duplicates
  distinct() %>% 
  #create columns for each prediction
  add_predictions(mod1,var="mod1") %>% 
  add_predictions(mod2,var="mod2") %>% 
  add_predictions(mod3,var="mod3") %>%
  #pivot longer the models together
  pivot_longer(starts_with("mod"),names_to = "mod",values_to = "predicted_U5MR")

#actually plot the data
model_df %>% ggplot(aes(x=Year,y=predicted_U5MR,color = Continent))+
  geom_line(size = 1)+
  facet_wrap(factor(model_df$mod))+
  theme_minimal()

#Extra Credit####
#i tried to do the cheat shortcut but my computer apparently really didnt like me trying to define the most
#complex glm and i had to restart Rstudio every time i ran the line
new_mod <- glm(U5MR~CountryName*Year, data = df)
summary(new_mod)
compare_performance(mod3,new_mod)
compare_performance(mod3,new_mod) %>% plot
#after some trial and error i found this model which has a significantly better AIC and R2 
test <- data.frame(Continent = "Americas",CountryName = "Ecuador",Year = 2020)
predict(mod3, test)
predict(new_mod, test)
#it did a worse job of predicting the outcome here though
dflog <- na.omit(df) %>% mutate(U5MRlog = log10(U5MR))
log_mod <- glm(U5MRlog~CountryName*Year, data = dflog)
log_mod2 <- glm(U5MRlog~CountryName+Year, data = dflog)
log_mod3 <- glm(U5MRlog~Continent*Year, data = dflog) 
compare_performance(log_mod,log_mod2,log_mod3) %>% plot
#this shows that log_mod is the "best" of these. I didnt realize you could get negative AIC 
10^predict(log_mod,test) 
10^predict(log_mod2,test) 
10^predict(log_mod3,test) 
#I find it interesting that the supposed worst of these three models is the closest to the actual value giving a predicted value of 12.
#also all the log models are significantly closer to predicting this than the previous models