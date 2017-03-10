## HW3 - 
## David Valentin
library("tidyverse")
library(broom)
library(ggplot2)

datapath <- "/Users/noahlevine/Desktop/R_Code"
setwd(datapath)

train <- readr::read_csv("train.csv")

# Plot of values - allows us to view the data 
ggplot(data=train, aes(x=x, y=y)) +
  geom_point() +
  labs(x="x", y="y")

# Fit a cubic spline using the real_life
model_spline <- smooth.spline(train$x, train$y, df=10)

train_augmented <- model_spline %>% augment() %>% tbl_df()

train_augmented

ggplot(data=train_augmented, aes(x=x, y=y)) +
  geom_point() +
  geom_line(aes(y=.fitted), col="blue", size=1) +
  labs(x="x", y="y")


### Testing LOOCV for spline with df of 10

#Shuffle the observations, and then number them
train_augmented <- train_augmented[sample(nrow(train)),] %>%
  mutate(n= row_number())

# Create the score and filter out each row
score <- train_augmented %>% 
  filter(n==1) %>%
  summarize(sqerror = .resid^2 ) #generate a score for the first

#Iterate through the values - add the values to score
for (k in 2:nrow(train_augmented)) {
  singleScore <- train_augmented %>%
    filter(n==k) %>%
    summarize(sqerror = .resid^2)
  
  score <- bind_rows(score, singleScore) #append current score with those already calculated
}


#Get average score
score %>% summarize(avgscore=mean(sqerror))
