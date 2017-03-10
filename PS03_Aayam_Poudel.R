library(tidyverse)

train <- read_csv("~/Desktop/Junior Spring/Statistical Learning/Homework/HW3/train.csv") %>% filter(!ID %in% c(863, 912))
test <- read_csv("~/Desktop/Junior Spring/Statistical Learning/Homework/HW3/test.csv")
submission_sample <- read_csv("~/Desktop/Junior Spring/Statistical Learning/Homework/HW3/PS03_submission_sample.csv")


#Simple Linear Regression Model:
model_SL <- lm(y~x, data=train)

#this is the result we can write to the csv file if we decide to use it
model_SL_result <- broom::augment(model_SL, newdata = test) %>%
  transmute(ID = ID, y = .fitted) 

#Spline Model:
degree_of_freedom <- 20

model_spline <- smooth.spline(train$x, train$y, df=degree_of_freedom)

spline_model_augmented <- model_spline %>%
  broom::augment() %>%
  # This command simply makes the table easier to look at in the R console:
  tbl_df()


#red line is generated through geom_smooth
#blue line is generated using our fitted values
ggplot(data <- spline_model_augmented, aes(x=x)) +
  geom_point(aes(y=y), col = "grey")+
  geom_line(aes(y=.fitted), col = "blue", size = 2) +
  geom_smooth(aes(y=y), col = "red", size = 1) +
  labs(x="x", y="y")

#we are using the spline model:
test_augmented <- predict(model_spline, x=test$x) %>%
  tbl_df() %>%
  left_join(test, by="x")

#this is the result we write to the csv file if we decide to use it

to_csv <- test_augmented %>%
  transmute(ID=ID,y=y)
to_csv

to_csv %>% 
  readr::write_csv("~/Desktop/Junior Spring/Statistical Learning/Homework/HW3/To submit/PS03_submission_Aayam_Poudel.csv")



#Validation for both models and seeing which one to use:

# Assign folds at random
n_folds <- 5

train <- train %>%
  # Equivalent to shuffling the rows:
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n()))
  

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
  model_spline_for_scoring <- smooth.spline(pseudo_train$x, pseudo_train$y, df=degree_of_freedom)
  model_SL_for_socring <- lm(y~x, data=pseudo_train)
 
  
  # 3. Get fitted/predicted values y-hat for the pseudo_test data using the
  # trained model
  # y.x is your fitted value
  
  #if model_SL is used comment this out:
  pseudo_test <- predict(model_spline_for_scoring, x=pseudo_test$x) %>%
    tbl_df() %>%
    left_join(pseudo_test, by="x")
  
  #if model_spline is used comment this out:
  #pseudo_test <- broom::augment(model_SL, newdata = pseudo_test) %>%
    #mutate(ID = ID, y.y = y, y.x = .fitted) 
  
  
  # 4. Compute your score on the pseudo_test data
  #
  # In this case: Since the outcome is binary and not continuous, "Did you guess
  # correct?" is a more appropriate scoring system than MSE. Now instead of a single
  # observation, we take the average over all ~178 predictions
  pseudo_test_score <- pseudo_test %>%
    summarise(score = mean((y.x - y.y)^2))
  
  # 5. Save your score for this fold
  scores[i] <- pseudo_test_score$score
}

# The average over the 10 folds is computed below. Recall the score on Kaggle
# of 0.7655. A close approximation!
scores
mean(scores)

#WHY I DECIDED TO USE MY MODEL:
#Linear regression gave me a score of around 180, which my spline model improved to about 150 ish. I toyed around with df till I got this answer
#did not want to fit to noise, so left df at 20