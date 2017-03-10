## HW3 - 
# Marcos Barrozo
library(tidyverse)
library(broom)

datapath <- "D:\\Users\\users\\OneDrive - Middlebury College\\Stat Learning\\HW3"
setwd(datapath)

train <- readr::read_csv("PS03_train.csv") %>% 
  filter(!ID %in% c(863, 912))
test <- readr::read_csv("PS03_test.csv")

# Plot of values
ggplot(data=train, aes(x=x, y=y)) +
  geom_point() +
  labs(x="x", y="y")

# Trying out some spline models
# LOOCV -------------------------------------------------------------------
n <- nrow(train)
train <- train %>%
  mutate(fold=1:n)

# For each observation, you'll store your score here:
scores <- rep(0, n)

for(i in 1:n){
  # 1. Create disjoint pseudo-train and pseudo-test sets based on folding
  # scheme. In this case pseudo_test is just 1 row:
  pseudo_train <- train %>%
    filter(fold != i)
  pseudo_test <- train %>%
    filter(fold == i)
  
  # 2. Train the model using (i.e. fit the model to) the pseudo_train data.
  loop_spline <- smooth.spline(pseudo_train$x, pseudo_train$y, df=50)
  
  # broom package to the rescue!
  loop_augmented <- loop_spline %>%
    augment() %>%
    tbl_df()
  
  
 
  # 3. Get fitted/predicted values y-hat for the pseudo_test data using the
  # trained model - assigning spline values to pseudo test data
  pseudo_test_predict <- predict(loop_spline, x=pseudo_test$x) %>%
    tbl_df() %>%
    left_join(pseudo_test, by="x") %>%
    mutate( )

  # 4. Compute your score on the pseudo_test data
  #
  # In this case: calculating a squared error measure
  pseudo_test_predict <- pseudo_test_predict %>%
    mutate(sqerror= (y.x-y.y)^2)
  
  # 5. Save your score for this fold
  scores[i] <- pseudo_test_predict$sqerror
}

#score values
scores
mean(scores)
#Playing around with different df values:
#20df = 150.8552
#50df = 149.1895
#100df = 151.836  looks like I'll just stick to 50df!


#######################################################
#### Fit the actual model to the test data
#######################################################
# Fit the spline
model_spline <- smooth.spline(train$x, train$y, df=50)

# broom package to the rescue!
train_augmented <- model_spline %>%
  augment() %>%
  tbl_df()

#quick visual
ggplot(data=train_augmented, aes(x=x, y=y)) +
  geom_point() +
  geom_line(aes(y=.fitted), col="blue", size=1) +
  geom_ribbon(aes(ymin = .fitted - 5, ymax = .fitted + 5), fill = "dodgerblue1", alpha=.4)
  labs(x="x", y="y")

#fit the values to test data  
test_predict <- predict(model_spline, x=test$x) %>%
    tbl_df() %>%
  left_join(test, by="x") %>%
  select(ID,y)

write.csv(test_predict, file = "D:\\Users\\users\\OneDrive - Middlebury College\\Stat Learning\\HW3\\PS03_submission_Marcos_Barrozo.csv", row.names = FALSE)