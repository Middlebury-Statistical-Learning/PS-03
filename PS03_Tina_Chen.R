library(tidyverse)
library(MASS)
library(ISLR)
library(broom)

submission <- readr::read_csv("/Users/Tina/Desktop/MATH218/PS03_submission.csv") 
train <- readr::read_csv("/Users/Tina/Desktop/MATH218/PS03_train.csv") %>% 
  filter(!ID %in% c(863, 912))
test <- readr::read_csv("/Users/Tina/Desktop/MATH218/PS03_test.csv")

#spline 
spline_ <- train %>%
  dplyr::select(x,y) 
spline_

model_spline <- smooth.spline(spline_$x, spline_$y, df=4) #cubic spline

test_augmented <- predict(model_spline, x=test$x) %>%
  tbl_df() %>%
  left_join(test, by="x") 

#MSE
model_spline %>% 
  augment() %>% 
  tbl_df() %>% 
  summarise(MSE=mean(.resid^2)) 

#predictions for submission
submission <- submission %>% 
  left_join(test_augmented,by = "ID" ) %>% 
  dplyr::select(ID, y.y)

write.csv(submission, file = "PS03_submission_Tina_Chen.csv")

#graph
ggplot(data=aug_spline, aes(x=x)) +
  geom_point(aes(y=y), alpha = 0.2) +
  geom_line(aes(y=.fitted), col="blue", size=1) 



#cross validation
y <- rep(2, 299)

for(i in 2:300){

  n_folds <- 10
  train <- train %>%
    mutate(fold = sample(1:n_folds, replace=TRUE, size=nrow(train)))
  
  scores <- rep(0, n_folds)
  
  for(k in 1:10){
    
    pseudo_train <- train %>% 
      filter(ID != k)           
    pseudo_test <- train %>% 
      filter(ID == k)       
    
    model_spline <- smooth.spline(pseudo_train$x, pseudo_train$y, df=i)
    
    test <- predict(model_spline, x=pseudo_test$x) %>%
      tbl_df() %>%
      left_join(pseudo_test, by="x")
    
    prediction_score <- test %>% 
      mutate(Difference = y.y - y.x) %>% 
      mutate(MSE = (Difference^2))
    
    scores[k] <- prediction_score$MSE
  }
  y [i-1] <- mean(scores)
}

final_scores <- data.frame(y) 