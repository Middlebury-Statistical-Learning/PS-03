library(tidyverse)
library(broom)

#load data
PS03_train <- read_csv("https://raw.githubusercontent.com/rudeboybert/MATH218/gh-pages/assets/PS/PS03/PS03_train.csv") %>%
  filter(!ID %in% c(863, 912))

#make a linear model
model <-lm(y~x, data=PS03_train)
PS_augmented <- model %>%
  augment() %>%
  tbl_df()
PS_augmented

#look at data with  model
ggplot(data=PS03_train, aes(x=x, y=y)) +
  geom_line(aes( y=PS_augmented$.fitted), col="blue", size=3) +
  geom_point() +
  labs(x="x", y="y")

#hard to see blue line bc all of the dots but it looks okay

#cross Validate 
#--From PS02 Solutions --
# k=5 Fold CV 

# Assign folds at random
n_folds <- 5
train <- PS03_train %>%
  mutate(fold = sample(1:n_folds, replace=TRUE, size=nrow(PS03_train)))

# Store your scores here:
scores <- rep(0, n_folds)

for(i in 1:n_folds){
  # 1. Create disjoint pseudo-train and pseudo-test sets based on folding
  # scheme. 
  pseudo_train <- train %>%
    filter(fold != i)
  pseudo_test <- train %>%
    filter(fold == i)
  
  # 2. Train the model using (i.e. fit the model to) the pseudo_train data.
  
  model_train <- lm(y~ x,data = pseudo_train)
  PS_augmented <- model_train %>%
    augment() %>%
    # This command simply makes the table easier to look at in the R console:
    tbl_df()  
  
  # 3. Get fitted/predicted values y-hat for the pseudo_test data using the
  # trained model
  
pseudo_test <- augment(model_train, newdata= pseudo_test)
  
  # 4. Compute your score on the pseudo_test data

  pseudo_test_score <- pseudo_test %>%
    summarise(MSE = mean((.fitted-y)^2))
 
  # 5. Save your score for this fold
  scores[i] <- pseudo_test_score$MSE
}

# The average over the 5-folds is computed below.
scores
mean(scores)


#what if we tried a spline model
model_spline <- smooth.spline(PS03_train$x, PS03_train$y, df=10)

PS_spline_augmented <- model_spline %>%
  augment() %>%
  tbl_df()

#look at it 
ggplot(data=PS03_train, aes(x=x, y=y)) +
  geom_line(aes( y=PS_spline_augmented$.fitted), col="blue", size=5) +
  geom_point() +
  labs(x="x", y="y")
#looks like it fits the data better than a straight linear

#time to test it
# Assign folds at random
n_folds <- 5
train <- PS03_train %>%
  mutate(fold = sample(1:n_folds, replace=TRUE, size=nrow(PS03_train)))

# Store your scores here:
scores <- rep(0, n_folds)

for(i in 1:n_folds){
  # 1. Create disjoint pseudo-train and pseudo-test sets based on folding
  # scheme. 
  pseudo_train <- train %>%
    filter(fold != i)
  pseudo_test <- train %>%
    filter(fold == i)
  
  # 2. Train the model using (i.e. fit the model to) the pseudo_train data.
  train_spline <- smooth.spline(pseudo_train$x, pseudo_train$y, df=10)

  
 
  
  # 3. Get fitted/predicted values y-hat for the pseudo_test data using the
  # trained model
  test_augmented <- predict(train_spline, x=pseudo_test$x) %>%
    tbl_df() %>%
    left_join(pseudo_test, by="x")
  
  # 4. Compute your score on the pseudo_test data
  
  pseudo_test_score <- test_augmented %>%
    summarise(MSE = mean((y.x - y.y)^2))
  
  # 5. Save your score for this fold
  scores[i] <- pseudo_test_score$MSE
  

}

# The average over the 5-folds is computed below. 
scores
mean(scores)
#Spline w higher df has a lower MSE - how do we know when we're good enough? went with 10 to be safe


