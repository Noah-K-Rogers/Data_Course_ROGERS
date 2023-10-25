library(tidyverse)
library(janitor)
library(easystats)
library(modelr)
library(MASS)
#read in data
df <- read.csv("../../Data/mushroom_growth.csv")
#plot each variable against grwoth rate
df %>% ggplot(aes(x= Light, y= GrowthRate,color = Humidity))+
  geom_point()+
  facet_wrap(factor(df$Species))+
  geom_smooth(method = "lm")
df %>% ggplot(aes(x= Nitrogen, y= GrowthRate,color = Humidity))+
  geom_point()+
  facet_wrap(factor(df$Species))+
  geom_smooth(method = "lm")
df %>% ggplot(aes(x= Temperature, y= GrowthRate,color = Humidity))+
  geom_point()+
  facet_wrap(factor(df$Species))+
  geom_smooth(method = "lm")
#these show pretty heavy interactions between some of the variables so i'll include that in some models. 
#mostly simple model
mod1 <- glm(data=df, formula = GrowthRate~Species+Light+Nitrogen+Humidity+Temperature)
#most complex model
mod2 <- glm(data=df, formula = GrowthRate~Species*Light*Nitrogen*Humidity*Temperature)
#semi complex model
mod3 <- glm(data=df, formula = GrowthRate~ .^2)
#remove some complexity from complex model while retaining the important parts
modstep <- stepAIC(mod2)
mod4 <- glm(data = df, formula = modstep$formula)
#compare the models
compare_performance(mod1,mod2,mod3,mod4) %>% plot()
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)
mean(mod4$residuals^2)
#while mod2 is technically the lowest mean residual and R2, its much worse in other categories than mod4 so we'll be using that one
predictions<- add_predictions(df, mod4) %>% pivot_longer(cols = c(pred,GrowthRate),names_to = "model",values_to = "growth") %>% 
  mutate(model = case_when(model =="pred"~"model",
                           model =="GrowthRate"~"real"))
predictions %>% ggplot(aes(x= Light, y= growth,color = model))+
  geom_point()+
  facet_wrap(factor(predictions$Species))+
  geom_smooth(method = "lm")
predictions %>% ggplot(aes(x= Nitrogen, y= growth,color = model))+
  geom_point()+
  facet_wrap(factor(predictions$Species))+
  geom_smooth(method = "lm")
predictions %>% ggplot(aes(x= Temperature, y= growth,color = model))+
  geom_point()+
  facet_wrap(factor(predictions$Species))+
  geom_smooth(method = "lm")
#all these plots show the model is relatively close to the real data and the lines are nearly identical which makes sense
#given we are checking it on the data it was trained on.


#non linear
non_linear <- read_csv("../../Data/non_linear_relationship.csv")
non_linear %>% ggplot(aes(x=predictor^3,y=response))+
  geom_point()+
  geom_smooth()
model <- glm(response~predictor^3, data = non_linear)
