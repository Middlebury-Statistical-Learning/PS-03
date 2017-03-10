library(tidyverse)
library(broom)

# read in the file
train <- read.csv("~/Desktop/College/Senior Year 2016-2017/Statistical Learning/HW3/PS03_train.csv") %>% 
  filter(!ID %in% c(863, 912))
test <- read.csv("~/Desktop/College/Senior Year 2016-2017/Statistical Learning/HW3/PS03_test.csv")

# MODEL ----------------------------------------
model_lm <- lm(y ~ x, data = train)
tidy(model_lm)
augment(model_lm) %>% summarise(MSE = mean((.resid)^2))

# randomly generate 100 folds of equal size 
n_folds <- 100
train <- train %>%
  mutate(fold = sample(1:n_folds, replace=TRUE, size=nrow(train)))

# vector to store scores 
kfoldscores_lm <- rep(0.0, n_folds)

for(i in 1:n_folds){
  pseudo_train_lm <- train %>%
    filter(fold != i)
  pseudo_test_lm <- train %>%
    filter(fold == i)
  # fit model to pseudo_train data
  model_lm <- lm(y ~ x, data = pseudo_train_lm)
  # vector to store pseudo_test results
  pseudo_test_score_lm <- rep(0, nrow(pseudo_test_lm))
  for (x in 1:nrow(pseudo_test_lm)) {
    # predict using fitted model for every observation in psuedo_test
    predicted_lm <- predict(model_lm, data.frame(pseudo_test_lm[x,]))
    # calculate SE for each predciction
    pseudo_test_score_lm[x] <- mean((predicted_lm - pseudo_test_lm[x,]$y)^2)
  }
  # calculate MSE of the fold and store 
  pseudo_avg_lm <- mean(pseudo_test_score_lm)
  kfoldscores_lm[i] = pseudo_avg_lm
}
average_lm <- mean(kfoldscores_lm)
print(average_lm)

# LOESS ----------------------------------------
n_folds <- 100
train <- train %>%
  mutate(fold = sample(1:n_folds, replace=TRUE, size=nrow(train)))
kfoldscores_loess <- rep(0, n_folds)

for(i in 1:n_folds){
  pseudo_train_loess <- train %>%
    filter(fold != i)
  pseudo_test_loess <- train %>%
    filter(fold == i)
  # fit model to pseudo_train_lin data
  train.lo <- loess(formula = y ~ x, data = pseudo_train_loess, span = 0.1, degree = 2,
                    control = loess.control(surface="direct"))
  # vector to store pseudo_test results
  pseudo_test_score_loess <- rep(0, nrow(pseudo_test_loess))
  for (x in 1:nrow(pseudo_test_loess)) {
    # predict using fitted model for every observation in psuedo_test
    predicted_loess <- predict(train.lo, data.frame(pseudo_test_loess[x,]))
    # calculate SE for each predciction
    pseudo_test_score_loess[x] <- mean((predicted_loess - pseudo_test_loess[x,]$y)^2)
  }
  # calculate MSE of the fold and store 
  pseudo_avg_loess <- mean(pseudo_test_score_loess)
  kfoldscores_loess[i] = pseudo_avg_loess
}
average_loess <- mean(c(kfoldscores_loess))
print(average_loess)

my_prediction <- predict(train.lo, test)
my_solution <- data.frame(ID = test$ID, x = test$x, y = my_prediction)
write.csv(my_solution, file = "~/Desktop/College/Senior Year 2016-2017/Statistical Learning/HW3/PS03_submission.csv", row.names = FALSE)

