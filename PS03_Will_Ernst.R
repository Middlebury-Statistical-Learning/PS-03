library(tidyverse)
# Set working directory to ps03 dir:
setwd("~/Documents/Computer Science/stats_learning/problem_sets/ps03/")
# Load train and test data
train <- readr::read_csv("data/PS03_train.csv")
test <- readr::read_csv("data/PS03_test.csv")

#Train a model
#model <- lm(y~x, data=train)
model <- lm(y~poly(x, 5, raw=TRUE),data=train)
model

#Calculate MSE from train data
train_mse <- train %>% 
  #Estimate values for y as yhat
  mutate(yhat = predict(model, newdata = train)) %>%
  #Compute MSE
  summarise(mse=mean((y-yhat)^2))

#print MSE
train_mse

submission <- test %>%
  select(ID) %>%
  mutate(y = predict(model, newdata = test)) %>%
  readr::write_csv("PS03_submission_Will_Ernst.csv")


########### Pseudo Scoring Functions ############
calcPseudoScore <- function(train_data){
  #shuffle train data
  train_shuffled <- sample_frac(train_data,1,replace=TRUE)
  
  #separate pseudo-train and pseudo-test data
  pseudo_train <- train_shuffled %>% 
    sample_frac(0.8)
  pseudo_test <- train_shuffled %>% 
    anti_join(pseudo_train, by="ID")
  
  #predict a model using pseudo_train
  ps_model <- lm(y~x, data=pseudo_train)
  
  #calculate pseudo score
  ps_mse <- pseudo_test %>% 
    #Estimate values for y as yhat
    mutate(yhat = predict(ps_model, newdata = pseudo_test)) %>% 
    #Compute MSE
    summarise(mse=mean((y-yhat)^2))
  
  #print pseudo score
  ps_mse
}
calcPseudoScore(train)

#k fold scoring
kfold <- function(k, data){
  k = 5
  n <- nrow(data)
  data_shuffled <- sample_frac(data,1,replace=TRUE)
  total = 0
  start = 1
  
  for(i in 1:k){
    pseudo_train <- data_shuffled %>% 
      slice(start : (start + n/k))
    
    pseudo_test <- data_shuffled %>% 
      anti_join(pseudo_train, by="ID")
    
    #model <- lm(y~x, data=pseudo_train)
    model <- lm(y~poly(x, 10, raw=TRUE),data=train)
    #model <- lm(y~poly(x, 3, raw=TRUE),data=train)
    
    
    total <- total + pseudo_test %>% 
    
      mutate(yhat = predict(model, newdata = pseudo_test)) %>% 
      summarise(mse=mean((y-yhat)^2))
    
    start <- start + n/k + 1
    
  }
  
  kFoldScore = total/k
  kFoldScore
  
}

kfold(5, train)
