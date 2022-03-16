library(tidyverse)
library(modelr)
library(easystats)
library(GGally)

df <- read.csv("../../Data/GradSchool_Admissions.csv")

df %>% glimpse()

df <- df %>% 
  mutate(admit=case_when(admit == 1~ TRUE,
                         admit == 0~ FALSE),
         rank=factor(rank)) # I formatted the column for binary response

df %>% glimpse()

ggpairs(df) # It looks like GPA and admit have a relationship as 
            # rank and admit

df %>% 
  ggplot(aes(x=admit,y=gpa)) +
  geom_boxplot()+
  theme_minimal() # It looks like higher GPA = acceptation into school

mod1 <- glm(data = df,
            formula = admit ~ gpa,
            family = "binomial") # Here is a simple model

mod2 <- glm(data = df,
            formula = admit ~ gpa + gre + rank,
            family = "binomial") # more complex than mod1

mod3 <- glm(data = df,
            formula = admit ~ gpa*gre*rank,
            family = "binomial") # this is the most complex model

mod4 <- glm(data=df,
            formula = admit ~ (gpa*gre)+rank) # this is a bit less complex than mod3

# Let's test the models

comps <- compare_performance(mod1,mod2,mod3,mod4,
                             rank=TRUE)
comps

# By far, mod2 is the best fit model. It's not the most complex either so this
# model is looking appropriate to use

# let's graph these models 

add_predictions(df, mod2, type = "response") %>% 
  ggplot(aes(x=gpa, y= pred, color= rank))+
  geom_smooth() # this is model 2 graphed

add_predictions(df, mod4, type = "response") %>% 
  ggplot(aes(x=gpa, y= pred, color= rank))+
  geom_smooth() # this is model 4 graphed

df %>% 
  ggplot(aes(x=gpa))+
  geom_bar() # this is the actual data of gpa vs number accepted

# Mod 4 has an overall higher chance of getting admitted into a program
# that makes me think mod4 is over-fit and that mod2 is still the best model.
# We also can see that mod 2 (and model 4) predictions match the trend of real 
# data, which is: the higher the GPA, the more likely you are accepted. 

# Let's test model 2 against itself

set.seed(12345)
train <- df %>% 
  slice_sample(prop = .25)
test <- anti_join(df,train) # here we split our data into a test and train set

mod <- train %>% 
  glm(data = .,
      formula = admit ~ gpa + gre + rank) # we train our new model using mod 2 formula

x <- test %>% 
  add_residuals(model = mod) %>% 
  pluck("resid") # We compare how off the predictions of our model are to the remaining
                 # part of the data set

data.frame(x=x %>% unlist ()) %>% 
  ggplot(aes(x=x))+
  geom_density() # We graph the data

# We see that model 2 has residuals around -0.3, this is close to zero 
# so the model is not terrible

# Let's test model 4 just to confirm it isn't as good as model 2

set.seed(12345)
train <- df %>% 
  slice_sample(prop = .25)
test <- anti_join(df,train)

mod <- train %>% 
  glm(data = .,
      formula = admit ~ (gpa*gre)+rank)

x <- test %>% 
  add_residuals(model = mod) %>% 
  pluck("resid")

data.frame(x=x %>% unlist ()) %>% 
  ggplot(aes(x=x))+
  geom_density()

# We see model 4 has a residual of -0.35 which is similar to mod 2
# but has less predictions in this region and model 4 has more predictions
# that are in the 0.6 range, which is less accurate than what was predicted in 
# model 2.







