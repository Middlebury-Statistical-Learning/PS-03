install.packages("tidyverse")
library(tidyverse)
library(broom)
setwd("~/Desktop/Middlebury/Stat Learning/PS03")

train <- read_csv("PS03_train.csv")  %>% 
  filter(!ID %in% c(863, 912))
test <- read_csv("PS03_test.csv") 
submission <- read_csv("PS03_submission.csv")

ggplot(data=train, aes(x=x, y=y)) +
  geom_point()


## Final model that I used to predict in my submission
## use model_spline10 because it generated the lowest MSE score
## spline model with df=10
model_spline <- smooth.spline(train$x, train$y, df=10)
model_spline

test_augmented <- predict(model_spline, x=test$x) %>%
  tbl_df() %>%
  left_join(test, by="x") %>% 
  select(ID, y)

submission <- test_augmented
write.csv(submission, "KGray_submission.csv", row.names=FALSE)

# broom package to the rescue!
train_augmented <- model_spline %>%
  augment() %>%
  # This command simply makes the table easier to look at in the R console:
  tbl_df()
train_augmented

ggplot(data=NULL, aes(x=x)) +
  # What we're given - the obs y:
  geom_point(data=train_augmented, aes(y=y)) +
  # Our guess at f(x), i.e. f_hat(x) - our fitted/predicted values:
  geom_line(data=train_augmented, aes(y=.fitted), col="blue", size=1)

train_score <- train_augmented %>% 
  summarise(MSE=mean((.resid)^2))

score_spline10 <- train_score$MSE

## Now I will try out various models on the train data to see which reaps the lowest
## average MSE score

## linear model
model_lm <- lm(y ~ x, data = train)
model_lm

train_augmented <- model_lm %>% 
  augment() %>% 
  tbl_df()
train_augmented

ggplot(data=NULL, aes(x=x)) +
  # What we're given - the obs y:
  geom_point(data=train_augmented, aes(y=y)) +
  # Our guess at f(x), i.e. f_hat(x) - our fitted/predicted values:
  geom_line(data=train_augmented, aes(y=.fitted), col="blue", size=1)

train_score <- train_augmented %>% 
  summarise(MSE=mean((.resid)^2))

score_lm <- train_score$MSE


## polynomial model 
model_poly <- lm(y ~ x + x^2 + x^3, data=train)

train_augmented <- model_poly %>% 
  augment() %>% 
  tbl_df()
train_augmented

ggplot(data=NULL, aes(x=x)) +
  # What we're given - the obs y:
  geom_point(data=train_augmented, aes(y=y)) +
  # Our guess at f(x), i.e. f_hat(x) - our fitted/predicted values:
  geom_line(data=train_augmented, aes(y=.fitted), col="blue", size=1)

train_score <- train_augmented %>% 
  summarise(MSE=mean((.resid)^2))

score_poly <- train_score$MSE

## spline model with df= 5
model_spline <- smooth.spline(train$x, train$y, df=5)
model_spline

train_augmented <- model_spline %>%
  augment() %>%
  tbl_df()
train_augmented

ggplot(data=NULL, aes(x=x)) +
  # What we're given - the obs y:
  geom_point(data=train_augmented, aes(y=y)) +
  # Our guess at f(x), i.e. f_hat(x) - our fitted/predicted values:
  geom_line(data=train_augmented, aes(y=.fitted), col="blue", size=1)

train_score <- train_augmented %>% 
  summarise(MSE=mean((.resid)^2))

score_spline5 <- train_score$MSE

## spline model with df= 6
model_spline <- smooth.spline(train$x, train$y, df=6)
model_spline

train_augmented <- model_spline %>%
  augment() %>%
  tbl_df()
train_augmented

ggplot(data=NULL, aes(x=x)) +
  # What we're given - the obs y:
  geom_point(data=train_augmented, aes(y=y)) +
  # Our guess at f(x), i.e. f_hat(x) - our fitted/predicted values:
  geom_line(data=train_augmented, aes(y=.fitted), col="blue", size=1)

train_score <- train_augmented %>% 
  summarise(MSE=mean((.resid)^2))

score_spline6 <- train_score$MSE

## spline model with df= 7 
model_spline <- smooth.spline(train$x, train$y, df=7)
model_spline

train_augmented <- model_spline %>%
  augment() %>%
  tbl_df()
train_augmented

ggplot(data=NULL, aes(x=x)) +
  # What we're given - the obs y:
  geom_point(data=train_augmented, aes(y=y)) +
  # Our guess at f(x), i.e. f_hat(x) - our fitted/predicted values:
  geom_line(data=train_augmented, aes(y=.fitted), col="blue", size=1)

train_score <- train_augmented %>% 
  summarise(MSE=mean((.resid)^2))

score_spline7 <- train_score$MSE

## spline model with df= 8 
model_spline <- smooth.spline(train$x, train$y, df=8)
model_spline

train_augmented <- model_spline %>%
  augment() %>%
  tbl_df()
train_augmented

ggplot(data=NULL, aes(x=x)) +
  # What we're given - the obs y:
  geom_point(data=train_augmented, aes(y=y)) +
  # Our guess at f(x), i.e. f_hat(x) - our fitted/predicted values:
  geom_line(data=train_augmented, aes(y=.fitted), col="blue", size=1)

train_score <- train_augmented %>% 
  summarise(MSE=mean((.resid)^2))

score_spline8 <- train_score$MSE


## spline model with df=9
model_spline <- smooth.spline(train$x, train$y, df=9)
model_spline

train_augmented <- model_spline %>%
  augment() %>%
  tbl_df()
train_augmented

ggplot(data=NULL, aes(x=x)) +
  # What we're given - the obs y:
  geom_point(data=train_augmented, aes(y=y)) +
  # Our guess at f(x), i.e. f_hat(x) - our fitted/predicted values:
  geom_line(data=train_augmented, aes(y=.fitted), col="blue", size=1)

train_score <- train_augmented %>% 
  summarise(MSE=mean((.resid)^2))

score_spline9 <- train_score$MSE

## Added loess model in class on Wednesday
## loess model with span=.25 
span <-.25
model_loess <- loess(y ~ x, train, span=span)

train_augmented <- model_loess %>%
  augment() %>%
  tbl_df()
train_augmented


# Plot it all
ggplot(data=train_augmented, aes(x=x)) +
  geom_point(aes(y=y)) +
  geom_smooth(aes(y=y), se=TRUE, span=span, size=2) +
  geom_line(aes(y=.fitted), col="red", size=1)

train_score <- train_augmented %>% 
  summarise(MSE=mean((.resid)^2))

score_loess <- train_score$MSE
