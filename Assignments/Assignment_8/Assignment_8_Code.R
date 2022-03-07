library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)

# 1. load the “/Data/mushroom_growth.csv” data set

mushroom <- read.csv("../../Data/mushroom_growth.csv")

# 2. creates several plots exploring relationships 
# between the response and predictors

mushroom %>% glimpse()

library(GGally)
ggpairs(mushroom) # I can see some possible correlations with
                  # species, light, humidity, and temperature

# 3. defines at least 4 models that explain the dependent 
# variable “GrowthRate”

mod1 = glm(data=mushroom,
           GrowthRate ~ Light)
summary(mod1)

mod2 = glm(data=mushroom,
           GrowthRate ~ Species)
summary(mod2)

mod3 = glm(data = mushroom,
           GrowthRate ~ Species + Light + Nitrogen + Humidity +
             Temperature)
summary(mod3) # Nitrogen and Temperature are not significant on growth

mod4 = glm(data = mushroom, 
           GrowthRate ~ Species + Light + Humidity)
summary(mod4)

# 4. calculates the mean sq. error of each model

mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2) 
mean(mod4$residuals^2)

# 5. selects the best model you tried

print("model 3 has the smallest r^2 (this is the best model)")

# 6. adds predictions based on new hypothetical values 
# for the independent variables used in your model

pred_data <- estimate_response(mod3)
head(pred_data)

# 7. plots these predictions alongside the real data

pred_data %>%
  ggplot(aes(x = Humidity, y = Predicted)) +
  geom_violin(color = "RED")+
  geom_violin(aes(x = Humidity, y = GrowthRate)) +
  ylab("Growth Rate") +
  theme_modern()

# The model shows that there may be slight correlation between 
# humidity level and growth rate. Higher humidity has a slightly 
# higher predicted growth rate and a slightly higher actual growth rate

summary(mod3)

# the P value of humidity is 7.26e-16, so it is significant

# Non-linear plot of light v.s. GrowthRate

mushroom %>% ggplot(aes(x=GrowthRate,y=Light))+
  geom_smooth()

