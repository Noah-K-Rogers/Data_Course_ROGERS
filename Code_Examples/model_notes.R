library(tidyverse)
library(palmerpenguins)
library(easystats)
library(MASS)

penguins %>% 
  ggplot(aes(y=bill_depth_mm,x=bill_length_mm,color=species))+
  geom_point()+
  geom_smooth(method="lm",aes(linetype = sex),se=FALSE)
  coord_cartesian(xlim = c(0,60))

#make linear regresssion of this plot

penguins %>% 
  ggplot(aes(y=bill_depth_mm,x=bill_length_mm))+
  stat_smooth(method = "lm")
  
aov(data = penguins,bill_depth_mm~bill_length_mm) %>% summary()
m1 <- glm(data = penguins,bill_depth_mm~bill_length_mm) 
m2 <- glm(data = penguins,bill_depth_mm~bill_length_mm+species)
m3 <- glm(data = penguins,bill_depth_mm~bill_length_mm*species)


m1$aic
m2$aic
m3$aic
compare_performance(m1,m2,m3)
names(penguins)
m4 <- glm(data = penguins,formula = bill_depth_mm~bill_length_mm+species+sex+islands)


formula()
x <- data.frame(bill_length_mm = 5000,
           species = "Chinstrap",
           sex = "male",
           island = "Dream")
mpg

predict(m4, newdata = x)



mpg %>% ggplot(aes(x=displ,y=hwy))+
  geom_point()+
  geom_smooth(method = "lm")
  


y <- data.frame(displ=.1)  
m5 <- glm(data = mpg, formula = hwy~log(displ))
10^predict(m5,y)



#logistic regression
Titanic %>% as.data.frame()
grad<- read_csv("./Data/GradSchool_Admissions.csv")
#logistic -> family=binomial
grad <- grad %>% mutate(admit = as.logical(admit))
m7 <- glm(data=grad,formula=admit~gre*gpa+rank,family = "binomial")
summary(m6)
library(modelr)
compare_performance(m6,m7,step) %>% plot
add_predictions(grad,m6,type = "response") %>%
  ggplot(aes(x=gre,y=pred,color = factor(rank)))+
  geom_smooth()

step<- stepAIC(m6)
step$formula

mod_best <- glm(data=grad,family = "binomial", formula = step$formula)



names(mod_best)
names(step)

penguins %>% names
peng_mod <- glm(data=train, formula = bill_length_mm ~ .^2 )

peng_step <- stepAIC(peng_mod,)

peng_mod_best <- glm(data=train,formula = peng_step$formula)
peng_mod_best$formula
names(peng_step)
names(peng_mod_best)
peng_step$anova
#train on some data
#test on other data

rbinom(nrow(penguins),1,.8)
penguins <- 
  penguins %>% 
  mutate(newcol = rbinom(nrow(penguins),1,.8))
test <- penguins %>% filter(newcol==0)
train <- penguins %>% filter(newcol==1) 

#test model on test set
predictions <- 
  add_predictions(test, peng_mod_best)

predictions <- 
predictions %>% mutate(resid= abs(pred-bill_length_mm))
mean_error <- mean(predictions$resid,na.rm = TRUE)















