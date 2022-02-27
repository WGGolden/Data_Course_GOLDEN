library(tidyverse)
library(dplyr)
library(patchwork)
library(easystats)
library(modelr)

# Import 4 data sets 

janflight <- read.csv("jan_flights.csv")
jansnow <- read.csv("Jan_snowfall.csv")
ports <- read.csv("airports.csv")
lines <- read.csv("airlines.csv")

# Combine the data sets appropriately to investigate 
# whether departure delay was correlated with snowfall amount
#You will need to think carefully about column names

#I tidied the year,month,day columns into a date column and made 
#both janflight and jansnow have matching Date columns labeled "Time"

newflight <- janflight %>% 
  unite(col = "date",
        sep = "-",
        YEAR,MONTH,DAY)

newflight$Time = as.POSIXct(newflight$date, format = '%Y-%m-%d')

jansnow$Time = as.POSIXct(jansnow$Date, format = '%Y-%m-%d')

# To join the data sets better, I'm matching origin airport from janflight
# with "iata" from jansnow

newflight$iata <- newflight$ORIGIN_AIRPORT

# joining the data sets

df <- full_join(newflight,jansnow)

# I created a side-by-side view of the flight delays and the 
# amount of snow in January

p1 <- df %>% ggplot(aes(x = Time, y = WEATHER_DELAY)) +
  geom_smooth()

p2 <- df %>% ggplot(aes(x= Time, y = snow_precip_cm))+
  geom_smooth()

p1+p2

# It looks like there may be correlation between snow amount and 
# delays when looking at the times of Jan 05 - Jan 12  and Jan 26 
# on both graphs

# Plot average departure delays by state over time

ports$iata <- ports$IATA_CODE

df2 <- full_join(df,ports)

df2 %>% group_by(STATE,Time) %>% 
  summarise(delayavg = mean(WEATHER_DELAY, na.rm = TRUE)) %>% 
  ggplot(aes(x = Time, y = delayavg)) +
  geom_smooth()+
  facet_wrap(~STATE) +
  theme(axis.text.x = (element_text(angle = 60, hjust = 1)))

# Plot average departure delays by airline over time

df2 %>% group_by(AIRLINE,Time) %>% 
  summarise(delayavg = mean(WEATHER_DELAY, na.rm = TRUE)) %>% 
  ggplot(aes(x = Time, y = delayavg)) +
  geom_smooth()+
  facet_wrap(~AIRLINE)

# Plot effect of snowfall on departure and arrival delays

# I'll make a model set with the variables of snow precip on departure/arrival
# delays

mod1 <- glm(data= df2,
            formula = snow_precip_cm~ ARRIVAL_DELAY + DEPARTURE_DELAY)

mod2 <- glm(data= df2,
            formula = snow_precip_cm~ ARRIVAL_DELAY * DEPARTURE_DELAY)

compare_performance(mod1,mod2) %>% plot()

# The models do not look very good, let's plot anyways

df2 %>% 
  gather_predictions(mod1,mod2) %>% 
  ggplot(aes(x=pred))+
  geom_smooth(aes(y = ARRIVAL_DELAY))+
  geom_smooth(aes(y = DEPARTURE_DELAY), color = "BLACK")+
  labs(x="Snowfall cm", y="Delays")

# From this graph, it looks like snowfall and delays have a inverse 
# correlation. That doesn't seem logical. 
