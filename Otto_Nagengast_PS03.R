############
### PS03 ###
############

### Getting set up ###
library(tidyverse)
library(ggplot2)
library(broom)

# There was some weird bug, so we just deleted the problem rows. # 
train <- readr::read_csv('~/Documents/Spring_2017/Statistical_Learning/Problem_sets/PS03/PS03_train.csv') %>% 
  filter(!ID %in% c(863, 912))

test <- readr::read_csv('~/Documents/Spring_2017/Statistical_Learning/Problem_sets/PS03/PS03_test.csv')

### Building the model ### 

# Let's first peruse the data. # 
ggplot(train, aes(x=x, y=y)) +
  geom_point() + 
  geom_smooth()
# It looks I should use df = 8.  

# Using spline with df = 8 # 
model <- smooth.spline(train$x, train$y, df=8)

# Creating my predictions. Note: I create a new data frame to do this. # 
predictions <- broom::augment(model, newdata = tble) 
  
# Calculating the MSE # 
score <- predictions %>% 
  summarise(MSE=mean(.resid^2))

score 

# Creating a submission file # 
submission <- predictions %>%  
  select(x, y)

# Creating a csv file with my predictions # 
submission %>% 
  readr::write_csv("~/Documents/Spring_2017/Statistical_Learning/Problem_sets/PS03/PS03_submission_Otto_Nagengast.csv") 

### Cross validation ### 
# This is one function to run cross validation with tble = data and k = number of folds. # 
CV <- function(tble, k) {
  # This creates an empty vector in which I'll record the score from each fold. # 
  scores_kfold <- rep(0, k)
 
  # Here I assign each observation to a fold. # 
  train_cv <- tble %>% 
    mutate(fold=sample(1:k, replace=TRUE, size=nrow(train)))

  # This is the loop. #
  for(i in 1:k) { 
    # I'm creating my pseudo_test and pseudo_train datasets. # 
    pseudo_test <- train_cv %>% 
      filter(fold == i) 
    
    pseudo_train <- train_cv %>%  
      anti_join(pseudo_test, by="fold")
  
    # I'm updating my model using the pseudo_train data 
    # and generating predictions with the model using the pseudo_test data. # 
    model <- smooth.spline(pseudo_train$x, pseudo_train$y, df=8)
    predictions <- broom::augment(model, newdata = pseudo_test) 
    
    # Putting the score for fold i in the empty vector # 
    score_for_i <- predictions %>%
      summarise(MSE=mean(.resid^2)) 
    scores_kfold[i] <- score_for_i$MSE
  }
  # Finding the mean score for all folds # 
    mean(scores_kfold)
}

# Some tests # 
CV(train, 5)
CV(train, 10)
CV(train, 100)




