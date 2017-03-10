library(readr)
library(tidyverse)
library(ISLR)
library(broom)
library(mosaic)

PS03_submission <- read_csv("~/Documents/Year 2/Sem 2.2/Statistical Learning/Problem Sets/PS03_submission.csv")
PS03_test <- read_csv("~/Documents/Year 2/Sem 2.2/Statistical Learning/Problem Sets/PS03_test.csv")
PS03_train <- read_csv("~/Documents/Year 2/Sem 2.2/Statistical Learning/Problem Sets/PS03_train.csv")


n_folds <- 5
train_shuffle<-shuffle(PS03_train)


#linear regression-did not use for submission!

n_folds <- 5
train<-train_shuffle%>%
  mutate(fold = rep(1:n_folds, length=n())) 

score<-c()

for(i in 1:n_folds){

pseudo_train <- train%>%
    filter(fold != i)
pseudo_test <- train %>%
    filter(fold == i)
  
model<-lm(y~poly(x, 7, raw=TRUE), data=pseudo_train)

data<-pseudo_test %>% 
  select(ID,x)

prediction<-broom::augment(model, newdata = data) %>%
  select(ID, x, .fitted) %>% 
  rename(prediction=.fitted)

pseudo_test<-pseudo_test %>% 
  left_join(prediction, by="ID","x") %>% 
  rename(x=x.x) %>% 
  select(ID, x, y, prediction)

pseudo_test<-pseudo_test %>% 
  summarize(score=mean((y-prediction)^2))

score<-score %>% 
  bind_rows(pseudo_test)
}


overall_score_lr<-score %>% 
  summarize(score=mean(score))


#spline
n_folds <- 5
train<-train_shuffle %>%
  mutate(fold = rep(1:n_folds, length=n())) 

score<-c()

for(i in 1:n_folds){

pseudo_train <- train %>%
    filter(fold != i)

pseudo_test <- train %>%
    filter(fold == i)
  

model_spline <- smooth.spline(pseudo_train$x, pseudo_train$y, df=15)  

pseudo_test <- predict(model_spline, x=pseudo_test$x) %>%
  tbl_df() %>%
  left_join(pseudo_test, by="x") %>% 
  rename(prediction=y.x, y=y.y) %>% 
  select(ID, x, y, prediction)

ggplot(data=pseudo_test, aes(x=x)) +
  geom_line(aes(y=prediction), col="blue", size=1) +
  geom_point(aes(y=y)) +
  labs(x="x", y="y")

pseudo_test<-pseudo_test %>% 
  mutate(diff=(y-prediction)^2)

pseudo_test<-pseudo_test %>% 
  summarize(score = mean(diff))

score<-score %>% 
  bind_rows(pseudo_test)


}

overall_score_spline<-score %>% 
  summarize(score=mean(score))

View(overall_score_spline)




#loess-did not use for submission!

n_folds <- 5
train<-train_shuffle %>%
  mutate(fold = rep(1:n_folds, length=n())) 

score<-c()

for(i in 1:n_folds){
  
  pseudo_train <- train %>%
    filter(fold != i)
  
  pseudo_test <- train %>%
    filter(fold == i)
  
  span <- 0.9
  model_loess <- loess(y~x, pseudo_train, span=span)
  prediction<-predict(model_loess, x=pseudo_test$x) %>%
    tbl_df() 
  
  pseudo_test<-pseudo_test %>% 
    cbind(prediction) %>% 
    rename(prediction=value) %>% 
    select(ID, x, y, prediction)
  
  pseudo_test<-pseudo_test %>% 
    mutate(diff=(y-prediction)^2)
  
  pseudo_test<-pseudo_test %>% 
    summarize(score = mean(diff))
  
  score<-score %>% 
    bind_rows(pseudo_test)
  
}

overall_score_loess<-score %>% 
  summarize(score=mean(score))

View(overall_score_loess)




#MSE for spline with df 15 using 5 fold CV

#153.7151


#Predictions for actual test data
model_spline <- smooth.spline(PS03_train$x, PS03_train$y, df=15)  

PS03_submission <- predict(model_spline, x=PS03_test$x) %>%
  tbl_df() %>%
  left_join(PS03_test, by="x") %>% 
  select(ID, prediction)


PS03_submission %>% 
  write_csv("PS03_submission_Rebecca_Conover.csv")
