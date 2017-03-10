library(tidyverse)
library(broom)

train <- read.csv("Documents/Math218/PS03_train.csv") %>%
  filter(!ID %in% c(863, 912))

#Train and Select Model
scores <- rep(0, nrow(train))

train <- train %>%
  mutate(fold = 1:nrow(train))

for(i in 1:nrow(train)){
  p_train <- train %>%
    filter(fold != i)
  p_test <- train %>%
    filter(fold == i)  
  
  model_spline <- smooth.spline(p_train$x, p_train$y, df = 36)
  
  predict <- predict(model_spline, p_test$x)
  sq_err <- (predict$y - p_test$y)^2
  scores[i] <- sq_err
}
score <- mean(scores)
print(score)

#Make full model and predictions
test <- read.csv("Documents/Math218/PS03_test.csv")

full_model <- smooth.spline(train$x, train$y, df = 36)

model_mse <- full_model %>%
  augment() %>%
  summarize(model_mse = mean(.resid^2))
print(model_mse)

predictions <- tbl_df(predict(full_model, test$x))

#Check visually & Export
ggplot() +
  geom_point(aes(train$x, train$y)) +
  geom_point(aes(predictions$x, predictions$y), col = "red")

write_csv(predictions, "Documents/Math218/PS03_submission_Ben_Czekanski.csv")
