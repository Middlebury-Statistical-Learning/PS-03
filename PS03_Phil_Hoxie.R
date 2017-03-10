# Load Data/Packages ------------------------------------------------------
library(tidyverse)
library(broom)
library(stats)
# You may need to change this to match your computer's directory structure
setwd("~/Machine Learning/HW_03")

# Load test data
train <- readr::read_csv("PS03_train.csv") 
test <- readr::read_csv("PS03_test.csv")
# look at the dat ______________________________

plot <- ggplot(data=train, aes(x, y))+
  geom_point()
plot

test_spline <- smooth.spline(train$x, train$y, df=32)
# WTF does this output mean?
test_spline
train_augmented <- test_spline %>%
  augment() %>%
  # This command simply makes the table easier to look at in the R console:
  tbl_df()
glimpse(train_augmented)

plot <- plot %>% 
  +geom_line(aes(y=train_augmented$.fitted), col = "red", size = 1.5)
plot

# k=5 Fold CV -------------------------------------------------------------

# Assign folds at random
n_folds <- 5
train <- train %>%
  mutate(fold = sample(1:n_folds, replace=TRUE, size=nrow(train)))

# Store your scores here:
scores <- rep(0, n_folds)

for(i in 1:n_folds){
  # 1. Create disjoint pseudo-train and pseudo-test sets based on folding
  # scheme. Now the pseudo_test has more than one observation, it has 178 = ~1/5
  # of data
  pseudo_train <- train %>%
    filter(fold != i)
  pseudo_test <- train %>%
    filter(fold == i)
  
  # 2. Train the model using (i.e. fit the model to) the pseudo_train data.
  #
  mod_spline <- smooth.spline(pseudo_train$x, pseudo_train$y, df=32)
  
  # 3. Get fitted/predicted values y-hat for the pseudo_test data using the
  # trained model
  
  predictions <- predict(mod_spline, x=pseudo_test$x) %>% 
    tbl_df() 
  
  predictions <- predictions %>% 
    rename(y_hat = y)
  
  pseudo_test_aug <- left_join(pseudo_test, predictions, by="x")
 
  # 4 Score
  
  pseudo_test_score <- pseudo_test_aug %>% 
    summarise(MSE=mean((y_hat - y)^2))
  
  # 5. Save your score for this fold
  scores[i] <- pseudo_test_score$MSE
}


scores
mean(scores)

## notes
# df = 25 -> 149.4
# df = 27 -> 149.1
# df = 30 -> 148.8
# df = 32 -> 148.7
# df = 40 -> 148.9
# df = 50 -> 149.4

## The real deal ------------------------------------------

mod_spline <- smooth.spline(train$x, train$y, df=32)

# 3. Get fitted/predicted values y-hat for the pseudo_test data using the
# trained model

predictions <- predict(mod_spline, x=test$x) %>% 
  tbl_df() 

predictions <- predictions %>% 
  rename(y_hat = y)

test_aug <- left_join(test, predictions, by="x")

test_plot <- ggplot(data=test_aug, aes(x=x, y=y_hat))+
  geom_point()
test_plot

write.csv(test_aug, file = "PS03_Submission_Phil_Hoxie.csv")
