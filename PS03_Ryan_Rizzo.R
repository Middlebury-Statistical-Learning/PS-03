library(tidyverse)
library(broom)

train <- read.csv("PS03_train.csv")
test <- read.csv("PS03_test.csv")
submission <- read.csv("PS03_submission.csv")

#********* CROSS VALIDATION ******************************
scores <- rep(0, nrow(train))

train <- train %>%
  mutate(fold = 1:nrow(train))

for(i in 1:nrow(train)){
  
  pseudo_train <- train %>%
    filter(fold != i)
  
  pseudo_test <- train %>%
    filter(fold == i)  
  
  # fit model to training data minus 1
  model_spline <- smooth.spline(pseudo_train$x, pseudo_train$y, df = 8)
  
  # make prediction based off the one that was left out
  predictions <- predict(model_spline, pseudo_test$x)
  #calculate squared error and add into score index so we can average later
  SE <- (predictions$y - pseudo_test$y)^2
  scores[i] <- SE
}

score <- mean(scores)
print(score)

#****** Actual Predictions ******************

#fit model to all training data
spline_model <- smooth.spline(train$x, train$y, df = 8)

#make predictions on unknown-Y test data
predictions <- tbl_df(predict(spline_model, test$x))


ggplot() +
  geom_point(aes(train$x, train$y)) +
  geom_point(aes(predictions$x, predictions$y), col = "red")

write_csv(select(inner_join(predictions, test), ID, y), "PS03_submission_Ryan_Rizzo.csv")
          