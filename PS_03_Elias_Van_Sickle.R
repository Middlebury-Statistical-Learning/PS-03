setwd("~/Dropbox/Middlebury/Semesters/9. Spring 2017/Statistical Learning/Assignments/#3")
library(tidyverse)

train <- readr::read_csv("train.csv")
test <- readr::read_csv("test.csv")

#CV METHOD: LOOCV

#Instantiate vector to hold squared error values
err_sq_vector <- rep(0,2500)

for(i in 1:2500){
  #select one row of training data for pseudo_test
  pseudo_test <- train[i,] 
  #select all rows not in pseudo_test for pseudo_train
  pseudo_train <- train %>%
    anti_join(pseudo_test, by="ID") 
  
  #Obtain predicted y value of the x value in pseudo_test from the spline model
  predicted_val <-  predict(
    smooth.spline(pseudo_train$x, pseudo_train$y, df=11),
    pseudo_test$x
  )

  #Calculate error squared
  err_sq <- (predicted_val$y - pseudo_test$y)^2 
  
  #Store squared error value
  err_sq_vector[i] <- err_sq
}

#Calculate mean squared error
mse <- mean(err_sq_vector)

#Create data frame to store predictions from the model
df <- data.frame(matrix(ncol = 2, nrow = num_entries))
colnames(df) <- c("ID", "Predicted")

#predicted values for actual test data
real_predicted_val <-  predict(
  smooth.spline(train$x, train$y, df=9),
  test$x
)

#add column with predicted values to test data frame
test <- test %>% mutate(y = real_predicted_val$y)

#select proper data for submission
submission <- subset(test, select = c("ID","y"))
  
#write the submission file
write.csv(submission,"PS03_submission_Elias_Van_Sickle.csv")

