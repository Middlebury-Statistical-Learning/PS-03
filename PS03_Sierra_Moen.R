#Brenda Li helped me by re-explaining the principles we learned in class. Thanks, Brenda! 

library(tidyverse)
library(broom)
library(dplyr)

train <- readr::read_csv("PS03_train (1).csv") %>% 
  filter(!ID %in% c(863, 912))
test <- readr::read_csv("PS03_test (1).csv")
submission <- readr::read_csv("PS03_submission (1).csv")


#Step 1 = Make a spline!
model_spline <- smooth.spline(train$x, train$y, df=5)

#Step 2 = I augment the spline so I can have fitted and residual columns
train_augmented <- model_spline %>%
  augment() %>%
  tbl_df() %>% 
  left_join(train, by="x")

#Step 3 = I find the MSE
MSE <- train_augmented %>% summarise(MSE=mean((.resid)^2))
MSE

#Step 4 = I test my model using the predict fuction. I then join it to the
  #original 'test' file so I can have the ID number.
test_augmented <- predict(model_spline, x=test$x) %>%
  tbl_df() %>%
  left_join(test, by="x")

#Step 5 = I select columns 'ID' and 'y' from test_augmented 
  #to get submission format
into_csv <- test_augmented %>% dplyr::select(ID, y) %>% 
  write_csv("PS03_submission_Sierra_Moen.csv")










#train, use Splines to create a model
#test, somehow apply test to my model?