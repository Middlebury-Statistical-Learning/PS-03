# Brenda Li
# Problem Set 3
  
library(tidyverse)
library(broom)

training<-read.csv("PS03_train.csv") %>% filter(!ID %in% c(863, 912))
test <- read_csv("PS03_test.csv")

# PART 1: USING 5-FOLD CV TO DETERMINE WHICH DEGREE OF FREEDOM I SHOULD USE FOR MY SPLINE MODEL

# shuffling the training data and dividing the data into 5 folds
set.seed(17)
shuffled<-training[sample(nrow(training)),] 
shuffled$id<-seq.int(nrow(shuffled))

n_folds<-5

kfolddata<-shuffled %>%
  mutate(fold = rep(1:n_folds, length=n()))

# initializing the vector in which I'm going to store the calculated MSE's
MSE<-rep(0,49)

# testing out values from 2 to 50 for degrees of freedom
for(k in 2:50){
  
  # initiliazing the vector which will store the score of each fold within each model
  scores <- rep(0, n_folds)
  
  # 5-fold validation
  for(i in 1:n_folds){
    
    # creating the training set and test set
    pseudo_train <- kfolddata %>%
      filter(fold != i)
    pseudo_test <- kfolddata %>%
      filter(fold == i)
    
    # creating the spline model with degree of freedom k
    model<-smooth.spline(pseudo_train$x, pseudo_train$y, df=k)
    
    # testing the spline model on the pseudotest data
    test_augmented <- predict(model, x=pseudo_test$x) %>%
      tbl_df() %>%
      left_join(pseudo_test, by="x") %>% 
      mutate(residsq=(y.y-y.x)^2) # calculating the square of the residuals
    
    # storing the MSE of this specific model with these folds into the vector
    scores[i] <- mean(test_augmented$residsq)
  }
  
  # storing the average MSE of the models with df=k
  MSE[k-1]<-mean(scores) #storing it into k-1 since df=1 is invalid so the first value of df would be 2

}

# checking to see which value of df generated the smallest error
# and thus which value for df I shoud use (turns out to be df=33)
which.min(MSE) 



# PART 2: TRAINING MY SPLINE MODEL WITH DF=33 AND CALCULATING THE ERROR RATE

# training the model
prediction_model<-smooth.spline(training$x,training$y,df=33)
augmented <- prediction_model %>%
  augment() %>%
  tbl_df() %>% 
  mutate(sqrd_resid=(.resid)^2) # calculating squared residuals for MSE

# calculating the MSE
MSE_train<-mean(augmented$sqrd_resid)
MSE_train # turns out to be 144.88

# PART 3: USING THE TRAINED MODEL ON THE TEST DATA TO MAKE PREDICTIONS
test_augmented <- predict(prediction_model, x=test$x) %>%
  tbl_df() %>% 
  left_join(test,by="x") %>% 
  select(ID,y)

# based on the previous cross-validation, I would expect the MSE score for my test predictions to be about 149.2
test_augmented %>% readr::write_csv("PS03_submission_Brenda_Li.csv")
