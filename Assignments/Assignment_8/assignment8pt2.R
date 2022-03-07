library(modelr)
library(easystats)
library(tidyverse)
library(ggplot2)

nonlinear <- read.csv("../../Data/non_linear_relationship.csv")

LinearModel <- lm(response ~ predictor, data=nonlinear) 
print(LinearModel)
summary(LinearModel)

quadratic_model <- lm(response ~ predictor + I(predictor^2), data = nonlinear)
summary(quadratic_model)

plot(nonlinear$predictor, nonlinear$response,
     col  = "steelblue",
     pch = 20,
     xlab = "District Income (thousands of dollars)",
     ylab = "Test Score",
     main = "Estimated Linear and Quadratic Regression Functions")


# add quatratic function to the plot
order_id <- order(nonlinear$predictor)

lines(x = nonlinear$predictor[order_id], 
      y = fitted(quadratic_model)[order_id],
      col = "red", 
      lwd = 2) 
