library(tidyverse)
library(broom)

## read in data
submission <- readr::read_csv("PS03_submission.csv")
train <- readr::read_csv("PS03_train.csv")
test <- readr::read_csv("PS03_test.csv")


## possible models
# model_sl <- lm(y~x, data=train)
# model_p <- lm(y~I(x^2), data=train)
# model_spline <- smooth.spline(train$x, train$y, df=20)
# model_loess <- loess(y ~ x, train, span=0.8)


## split into pseudo train and test
smp_size <- floor(0.75 * nrow(train))
set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)
ptrain <- train[train_ind,]
ptest <- anti_join(train, ptrain)

## using simple linear regression, do one k mean test
## fits model
pmodel_sl <- lm(y~x, data= ptrain)
## uses model to predict
pmodel_sl_predict <- cbind(ptest, y_hat = predict(pmodel_sl, ptest))
## finds error
pmodel_sl_error <- mean((pmodel_sl_predict$y - pmodel_sl_predict$y_hat)^2)
## displays error
pmodel_sl_error

## using polynomial quadratic regression
## uses similar format to above
pmodel_p <- lm(y~I(x^2), data= ptrain)
pmodel_p_predict <- cbind(ptest, y_hat = predict(pmodel_p, ptest))
pmodel_p_error <- mean((pmodel_p_predict$y - pmodel_p_predict$y_hat)^2)
pmodel_p_error

## using splines
## uses similar format to above
pmodel_spline <- smooth.spline(ptrain$x, ptrain$y, df=20)
pmodel_spline_predict <- cbind(ptest, y_hat = predict(pmodel_spline, ptest$x))
pmodel_spline_error <- mean((pmodel_spline_predict$y - pmodel_spline_predict$y_hat.y)^2)
pmodel_spline_error

## using loess
## uses similar format to above
pmodel_loess <- loess(y ~ x, ptrain, span=.05)
pmodel_loess_predict <- cbind(ptest, y_hat = predict(pmodel_loess, ptest))
pmodel_loess_error <- mean((pmodel_loess_predict$y - pmodel_loess_predict$y_hat)^2)
pmodel_loess_error


#although I didn't actually do k-means but rather just trained on the same
#subset and tested on the same subset, it seems like using the spline model
#came up with the lowest mean squared error


#so I will use splines to fit a model on the full train set
#and make a prediction for the test set
model_spline <- smooth.spline(train$x, train$y, df=20)
model_spline_predict <- cbind(test, y = predict(model_spline, test$x)) %>% 
  select(ID, y.y) %>% 
  rename(y = y.y)

write.csv(model_spline_predict, file = "PS03_submission_Shannia_Fu.csv", quote = FALSE, row.names = FALSE)



