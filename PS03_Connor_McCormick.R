title: "MATH 218 Homework 2"
author: "Connor"
library(tidyverse)

setwd("~/Documents/Middlebury/Stat Learning")
train <- readr::read_csv("PS03_train.csv")
test <- readr::read_csv("PS03_test.csv")

psuedo_train <- train %>% 
  sample_frac(0.8)
psuedo_test <- train %>% 
  anti_join(psuedo_train, by="ID")

ggplot(psuedo_train, aes(x, y))+
  geom_point(color = "darkblue")+
  geom_smooth(method="loess", fullrange = TRUE, se=FALSE, color = "violetred2")+
  ylim(-60,60)

#generate a linear model
predict <- lm(y ~ x, data=psuedo_train)

predictions <- psuedo_train %>%
  mutate(phat = fitted(predict))

ggplot(predictions, aes(x, y))+
  geom_point(color = "darkblue")+
  geom_smooth(method="loess", fullrange = TRUE, se=FALSE, color = "violetred2")+
  ylim(-60,60) +
  geom_line(aes(x,phat), color = "cadetblue2", size = 1) 

mse <- predictions %>%
  mutate(se = (phat-y)^2) %>%
  summarize(meanse = mean(se)/2000)
kable(mse)


#square it
predict2 <- lm(y ~ x + I(x^2), data=psuedo_train)

predictions2 <- psuedo_train %>%
  mutate(phat = fitted(predict2))

ggplot(predictions2, aes(x, y))+
  geom_point(color = "darkblue")+
  geom_smooth(method="loess", fullrange = TRUE, se=FALSE, color = "violetred2")+
  ylim(-60,60) +
  geom_line(aes(x,phat), color = "cadetblue2", size = 1)

mse2 <- predictions2 %>%
  mutate(se = (phat-y)^2) %>%
  summarize(meanse = mean(se)/2000)
kable(mse2)

#cube
predict3 <- lm(y ~ x + I(x^2)+ I(x^3), data=psuedo_train)

predictions3 <- psuedo_train %>%
  mutate(phat = fitted(predict3))

ggplot(predictions3, aes(x, y))+
  geom_point(color = "darkblue")+
  geom_smooth(method="loess", fullrange = TRUE, se=FALSE, color = "violetred2")+
  ylim(-60,60) +
  geom_line(aes(x,phat), color = "cadetblue2", size = 1)

mse3 <- predictions3 %>%
  mutate(se = (phat-y)^2) %>%
  summarize(meanse = mean(se)/2000)
kable(mse3)

#testing the training data

try1 <- psuedo_test %>%
  mutate(phat = predict(predict3, newdata=psuedo_test, type="response"))

ggplot(try1, aes(x, y))+
  geom_point(color = "darkblue")+
  geom_smooth(method="loess", fullrange = TRUE, se=FALSE, color = "violetred2")+
  ylim(-60,60) +
  geom_line(aes(x,phat), color = "cadetblue2", size = 1)

msetry <- try1 %>%
  mutate(se = (phat-y)^2) %>%
  summarize(meanse = mean(se)/2000)
kable(msetry)

#run test

test1 <- test %>%
  mutate(phat = predict(predict3, newdata=test, type="response"))

ggplot(try1, aes(x, phat))+
  geom_point(color = "darkblue")+
  ylim(-60,60)

sub <- test1 
readr::write_csv(sub, "PS03_Submission_Connor_McCormick")



