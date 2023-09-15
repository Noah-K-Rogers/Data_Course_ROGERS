#I. Read the cleaned_covid_data.csv file into an R data frame. (20 pts)####
#simple read.csv command to read in data
covid_data <-read.csv("cleaned_covid_data.csv") 
#check to make sure it worked properly and meets requirements
head(covid_data)
tail(covid_data)
summary(covid_data)
class(covid_data)
#head/tail/summary look good and class is data.frame

#II. Subset the data set to just show states that begin with “A” and save this as an object called A_states. (20 pts)####
# Use the tidyverse suite of packages
library(tidyverse)
# Selecting rows where the state starts with “A” is tricky (you can use the grepl() function or just a vector of those states if you prefer)
#assign A_states as only the rows where grepl says the state column starts with A
A_states <- covid_data[grepl("^A",covid_data[,1]),]
#verify it worked properly
head(A_states)
tail(A_states)
summary(A_states)
class(A_states)
levels(as.factor(A_states$Province_State))
#factor shows only 4 levels being the states that start with A meaning the data is only from those states
#change last update to Date data type to make plotting easier
A_states$Last_Update <- as.Date(A_states$Last_Update)
#III. Create a plot of that subset showing Deaths over time, with a separate facet for each state. (20 pts)####
ggplot(A_states,aes(y=Deaths,x=Last_Update))+
#Create a scatterplot
geom_point()+

#Add loess curves WITHOUT standard error shading
geom_smooth(method = "loess", se = FALSE)+
#Keep scales “free” in each facet
facet_wrap(~ Province_State,scales = "free")


#IV. (Back to the full dataset) Find the “peak” of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate. (20 pts)####

#I’m looking for a new data frame with 2 columns:
#“Province_State”
#“Maximum_Fatality_Ratio”
max_rates <- covid_data %>% 
  group_by(Province_State)%>%
  summarise(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE))
#Arrange the new data frame in descending order by Maximum_Fatality_Ratio
max_rates <- max_rates[order(max_rates$Maximum_Fatality_Ratio, decreasing = TRUE),]
#This might take a few steps. Be careful about how you deal with missing values!
#checking data to make sure it looks right
head(max_rates)
tail(max_rates)
class(max_rates)

#V. Use that new data frame from task IV to create another plot. (20 pts)####
#X-axis is Province_State
#Y-axis is Maximum_Fatality_Ratio
ggplot(max_rates, aes(x= reorder(Province_State, -Maximum_Fatality_Ratio), y=Maximum_Fatality_Ratio))+
#bar plot
geom_bar(stat = "identity")+
#x-axis arranged in descending order, just like the data frame (make it a factor to accomplish this)
#X-axis labels turned to 90 deg to be readable
theme(axis.text.x =element_text(angle = 90) )
#Even with this partial data set (not current), you should be able to see that (within these dates), different states had very different fatality ratios.
#VI. (BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the entire US over time####

#You’ll need to read ahead a bit and use the dplyr package functions group_by() and summarize() to accomplish this.
death_over_time <- covid_data %>%
  group_by(Last_Update)%>%
  summarise(US_deaths = sum(Deaths))
ggplot(death_over_time,aes(x=Last_Update,y = US_deaths))+
  geom_point()
