library(tidyverse)
library(broom)
setwd("/Users/BiancaGonzalez/Desktop/RudeBoyMachineLearning/hw3")
train <- read.csv("PS03_train.csv")
test <- read.csv("PS03_test.csv")
test<- test %>% 
  sample_frac(.3333) 

# Let's fit a spline model on our training dataset
spline_train <- smooth.spline(train$x, train$y, df = 10) 

# Clean it up
broom::glance(spline_train)  #gives degrees freedom etc

# Fitting with test vals
spline_predicted <- predict(spline_train, x=test$x) %>%  
  tbl_df() %>%
  left_join(test, by="x")     #just joins the ID with the original X val

glimpse(spline_predicted) #now have x, y and ID variables predicted from test data.

#write.csv(spline_predicted, file = 'PS03_submission_Bianca_Gonzalez.csv')

# Compute mean square error of model 

train$y <- as.numeric(train$y)
spline_predicted$y <- as.numeric(spline_predicted$y)

train <- train %>%
  mutate(score = (train$y - spline_predicted$y)^2) %>% 
  summarize(mse = mean(score))

# Use Cross Validation model LOOCV in meantime: 
# LOOCV isn't quite working - but general concepts are down. 

train <- read.csv("PS03_train.csv") 
test <- read.csv("PS03_test.csv")

n <- nrow(train)
train <- train %>%
  mutate(fold=1:n)    #create column fold with number of rows in train

# For each observation, store score here (repeat vector for each n storing)
scores <- rep(0, n)

for(i in 1:n) {
  
  pseudo_train <- train %>%
    filter(fold != i) #filter where fold(n obs) doesnt equal indice, 
  
  pseudo_test <- train %>%  # where fold (n obs equals indice)
    filter(fold == i)       # validation set
  
  # make the spline on whatever is left of the training set.
  
  spline_train <- smooth.spline(pseudo_train$x, pseudo_train$y, df = 10) 
  
  # testing out how well spline model does with the degree of freedom I chose 
  spline_test <- predict(spline_train, x=pseudo_test$x) %>%  #fitting with test vals
    tbl_df() %>%
    left_join(test, by="x") 
  
  #  glimpse(pseudo_train$y) # actual y value
  #  glimpse(spline_test$y)  # predicted y value
  
  #  Compute MSE - difference between what was predicted and what y actually was squared
  pseudo_test <- pseudo_test %>%
    mutate(score = (spline_test$y - pseudo_train$y)^2)

  # store computed MSE
  scores[i] <- pseudo_test$score   
}

mean(scores) 


MathDestruction <- "Explain in three paragraphs Cathy O’Neil’s argument of how supposedly objective mathematical/algorithmic models reinforce inequality in the context of 1) crime recidivism, 2) the thought experiment of hiring in tech firms and 3) teacher evaluations. Cathy O’Neil’s arguments for data as a weapon were largely centered on data going into a model withholding important historical circumstances. Machine learning algorithms cannot account for embedded inequalities due to existing systems of inequity - and hence according to O’neil - reality is not truly reflected in our data and data models. These biased data models, if used for decision making, cyclically reinforce an unjust system. Her arguments call for machine learning algorithms to include a human component. O’neil talks about how a model could bias the workforce to only include males in the tech industry by excluding the historical forces coming into play. If this model tries to pick out the candidates that succeed best, it would include mostly males in tech because those are the people that tend to represent the largest pool and succeed the most. The model does not know that societal pressures have pushed females to a) never join tech (smaller pool) and b) be pushed out of tech once they start. O’neil talks about how because an already established male workforce in the industry, it could be harder for females to feel comfortable in the industry and hence never be offered promotions. In another scenario she offered, she suggested they could be presumably be pushed out due to sexual harassment by their male counterparts. These instances of human behavior and culture are not accounted for in our datasets. If datasets and models do not account for the bias in industries - then they are implicitly biased models themselves."

#write(MathDestruction, file = 'PS03_discussion_Bianca_Gonzalez.txt')

