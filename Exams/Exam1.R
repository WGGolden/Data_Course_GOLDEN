# Read the cleaned_covid_data.csv file into an R data frame. (20 pts)

library(tidyverse)
library(dplyr)

df <- read.csv("cleaned_covid_data.csv")

# Subset the data set to just show states that begin with "A" and 
# save this as an object called A_states. (20 pts)

A_states <- df[grep("^A", df$Province_State), ]

# Create a plot _of that subset_ showing Deaths over time, with a 
# separate facet for each state. (20 pts)

Time <- as.Date(A_states$Last_Update)

ggplot(A_states,mapping = aes(y=Deaths,x=Time))+
  geom_point(alpha=0.5,size=0.1)+
  facet_wrap(~Province_State, scales = "free")+
  geom_smooth(method = "loess", se = FALSE, color = "blue", size = 0.4)

# Find the "peak" of Case_Fatality_Ratio for each state and save 
# this as a new data frame object called state_max_fatality_rate. (20 pts)

state_max_fatality_rate <- df %>% 
  group_by(Province_State) %>% 
  summarise(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>% 
  arrange(desc(Maximum_Fatality_Ratio))

# Use that new data frame from task IV to create another plot. (20 pts)

state_max_fatality_rate %>% 
  mutate(ordered_rate = factor(Province_State, levels = state_max_fatality_rate$Province_State)) %>% 
  ggplot(state_max_fatality_rate, mapping = aes(x = ordered_rate, y = Maximum_Fatality_Ratio)) +
  geom_col()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "States", y = "Maximum Fatality Rato")
  

#Bonus Points 

as.Date(df$Last_Update)

cdeath <- df %>% 
  group_by(Last_Update) %>% 
  summarise(Last_Update, cudeath = cumsum(Deaths)) %>% 
  arrange(Last_Update)

library (plyr)

cumdeath <- ddply(cdeath,"Last_Update",numcolwise(sum)) %>% 
  arrange(cudeath)

newtime <- as.Date(cumdeath$Last_Update)

ggplot(cumdeath, aes(x = newtime, y = cudeath))+
  geom_point(size = 0.3)+
  geom_smooth(method = "lm", color = "red")+
  labs(x = "Time", y = "Cumulative death in the US")+
  theme_bw()





