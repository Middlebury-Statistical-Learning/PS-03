library(tidyverse)
library(broom)
setwd("~/Documents/Statistical Learning")
train <- read_csv("PS03_train.csv")  %>% filter(!ID %in% c(863, 912))
test <- read_csv("PS03_test.csv")


# Initial investigation
train %>% ggplot(aes(x=x, y=y)) +
  geom_point() +
  geom_smooth()

# Making of the model
# I chose to look at a spline with 10 degrees of freedom, just to start out.
# I toy around with this later.
model_spline_df10 <- smooth.spline(train$x, train$y, df=10)
df10 <- model_spline_df10 %>% augment() %>% tbl_df()

# Graphing of model on training data
df10 %>% ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y =.fitted), col="blue", size =1) +
  ggtitle("Training data with line: spline, df=10")

# Graphing of residuals - training
df10 %>% ggplot(aes(x = x, y =.resid)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "blue") +
  ggtitle("Training residuals: spline, df=10")
# these are satisfyingly distributed

# Predicting with the model
predictions_df10 <- predict(model_spline_df10, test$x) %>% tbl_df()
predict_tbl_df10 <- right_join(test, predictions_df10, by="x")


# Graphing of model on test data
predict_tbl_df10 %>% ggplot(aes(x=x, y=y)) +
  geom_point() +
  ggtitle("Test data with spline, df=10")
# Well this is an interesting output for geom_point...



# k=5 Fold CV -------------------------------------------------------------
# This code is modified from https://github.com/rudeboybert/MATH218/blob/gh-pages/assets/PS/PS02_solutions.R

# An experiment with degrees of freedom:
# Make a FUNCTION that takes 1 parameter: degrees of freedom "df"
# Run k=5 fold cross-validation using a spline with df="df" 
# Outputs mean score

test_df <- function(df) {
  n_folds <- 5
  set.seed(44)
  train <- train %>%
    sample_frac(1) %>% # randomize rows
    mutate(fold = rep(1:n_folds, length=n())) # assign k-fold
  
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
    
    model_spline<- smooth.spline(pseudo_train$x, pseudo_train$y, df=df)
    
    # 3. Get fitted/predicted values y-hat for the pseudo_test data using the
    # trained model
    
    predictions <- predict(model_spline, pseudo_test$x) %>% tbl_df()
    predictions_tbl <- right_join(pseudo_test, predictions, by="x")
    
    # 4. Compute your score on the pseudo_test data (MSE)
    
    pseudo_test_score <- predictions_tbl %>%
      mutate(resid = (y.x - y.y)) %>%
      summarise(score = mean(resid^2))
    
    # 5. Save your score for this fold
    scores[i] <- pseudo_test_score$score
  }
  
  return(mean(scores))
}

df_scores <- rep(0, 50)
n_df <- rep(2:51, 1)

for(i in 1:50) {
  df_scores[i] <- test_df(i+1)
}

scores_by_df <- data.frame(n_df, df_scores) %>% tbl_df()

scores_by_df %>%
  ggplot(aes(x = n_df, y = df_scores)) +
  geom_point() +
  labs(title="MSE as a function of degrees freedom on spline",
       x = "Degrees of freedom", y = "MSE")

scores_by_df %>%
  arrange(df_scores) 

# From this, I see that the best model was the one with df=35.
# Now I simply fit the spline.smooth with df=10 and apply the model to test!

model_spline_df35 <- smooth.spline(train$x, train$y, df=35)
predictions_df35 <- predict(model_spline_df35, test$x) %>% tbl_df()
predict_tbl_df35 <- right_join(test, predictions_df35, by="x")

PS03_submission <- predict_tbl_df35 %>% select(ID, y) 
PS03_submission %>% write.csv(file = "PS03_submission.csv", row.names=FALSE)

