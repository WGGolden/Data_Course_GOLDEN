library(tidyverse)
library(dplyr)
library(easystats)
library(modelr)

#1. Read in the unicef data (10 pts) 

df <- read.csv("unicef-u5mr.csv")

#2. Get it into tidy format (10 pts) 

names(df) = gsub(pattern = "U5MR.", replacement = "", x = names(df))

cleandf <- df %>% 
  pivot_longer(-c(Continent,Region,CountryName),
               names_to = "year",
               values_to = "U5MR")

#3. Plot each country’s U5MR over time (20 points)

cleandf$year <- as.numeric(cleandf$year)

cleandf %>% 
  ggplot(aes(x= year, y = U5MR, group = CountryName))+
  geom_line()+
  facet_wrap(~Continent)

#4. Save this plot as LASTNAME_Plot_1.png (5 pts)

ggsave("GOLDEN_Plot_1.png")

#5. Create another plot that shows the mean U5MR for all the countries 
# within a given continent at each year (20 pts)

cleandf %>% 
  group_by(year,Continent) %>% 
  summarise(meanU5MR = mean(U5MR, na.rm=TRUE)) %>% 
  ggplot(aes(x=year,y=meanU5MR, color = Continent))+
  geom_line(size=2)

#6. Save that plot as LASTNAME_Plot_2.png (5 pts)

ggsave("GOLDEN_Plot_2.png")

#7. Create three models of U5MR (20 pts)

# mod1 should account for only Year

mod1 <- glm(data= cleandf,
            formula = U5MR~ year)

# mod2 should account for Year and Continent

mod2 <- glm(data= cleandf,
            formula = U5MR~year + Continent)

#mod3 should account for Year, Continent, and their interaction term

mod3 <- glm(data = cleandf,
            formula = U5MR~ year * Continent)

#8. Compare the three models with respect to their performance

compare_models(mod1,mod2,mod3) 

compare_performance(mod1,mod2,mod3) %>% plot()

# Model 3 is better than the other two models. You can see
# in the plot above that model 3 outperforms the other models
# in every category. Model 2 is second best, but lacks having
# a better BIC and AIC rating than model 3. Model 1 is the worst. 

#9. Plot the 3 models’ predictions like so: (10 pts)

cleandf %>% 
  gather_predictions(mod1,mod2,mod3) %>% 
  ggplot(aes(x=year,y=pred, color= Continent))+
  geom_smooth(methond = "lm") +
  facet_wrap(~model)+
  labs(y="U5MR Prediction")

# Bonus Question

new <- data.frame(year = 2020, Continent = "Americas", CountryName= "Ecuador")

predict(mod3, newdata = new)

# My model 3 predicts the U5MR in 2020 of Ecuador to be -10.6... that's not 
# accurate. 

new <- data.frame(year=2020)
predict(mod1,newdata = new)

# model 1 predicts the U5MR of Ecuador in 2020 to be 11.4. This is closer to 
# the actual number.

new <- data.frame(year=2020, Continent = "Americas")
predict(mod2, newdata = new)

# Model 2 was -13.8 U5MR. Children are not getting more fed in Ecuador so this 
# is inaccurate. 

#I want to visualize how the prediction looks compared to the actual data

observed <- data.frame("CountryName" = c("Ecuador"),
                        "Continent" = c("Americas"),
                        "Region" = c("South America"),
                        "year" = c(2020),
                        "U5MR" = c(13))

moddata <- full_join(observed, cleandf)

mod4 <- glm(data = moddata,
            formula = U5MR~year)
summary(mod4)

moddata %>% 
  gather_predictions(mod4) %>% 
  ggplot(aes(x=year))+
  geom_smooth(aes(y=pred),color= "RED")+
  geom_smooth(aes(y=U5MR))

new <- data.frame(year=2020, Continent = "Americas")

# Ok, here is the model prediction and the actual side-by-side
# I'm not sure how to add the residule automatically 

moddata %>% gather_predictions(mod4) %>% 
  filter(year==2020)

