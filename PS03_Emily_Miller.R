# Emily Miller
# Stat 218 Middlebury College
# Problem Set 3


library(tidyverse)
library(RCurl)
library(broom)
setwd("C:/Users/Emily Miller/Documents/Stat_learning")

# Download files from GitHub
temp <- getURL("https://raw.githubusercontent.com/rudeboybert/MATH218/gh-pages/assets/PS/PS03/PS03_train.csv")
train <- read.csv(text = temp)%>%  filter(!ID %in% c(863, 912))

temp <- getURL("https://raw.githubusercontent.com/rudeboybert/MATH218/gh-pages/assets/PS/PS03/PS03_test.csv")
test <- read.csv(text = temp)
rm(temp) # remove temp variable for reading in Data


## Loess Curve Fitting
span <- 0.55
model_loess <- loess(y ~ x, train, span=span)

# Use broom to get augmented data set
train_augmented <- model_loess %>%
  augment() %>%
  tbl_df()


# Plot it all
ggplot(data=train_augmented, aes(x=x)) +
  # Plot points
  geom_point(aes(y=y)) +
  # Same loess() fitted values
  geom_line(aes(y=.fitted), col="red", size=2)


# Leave one out cross validation useing MSE
n <- nrow(train)
scores <- numeric()
train <- train %>%
  mutate(fold=1:n)

for(i in 1:n){
  pseudo_train <- train %>%
    filter(fold != i)
  pseudo_test <- train %>%
    filter(fold == i)
  # Train the model on pseudo train
  span <- 0.55
  model_loess <- loess(y ~ x, pseudo_train, span=span)
  # 3. Get fitted/predicted values y-hat for the pseudo_test data 
  y_predict = predict(model_loess, pseudo_test[,2])
  # Find MSE
  MSE = (train[n,3] - y_predict)^2
  # 5. Save your scorea
  scores = append(scores, MSE)
}
print(mean(scores, na.rm = TRUE))

# Create predictions for test data set
test$y_predict <- 0 #initialize new variable
obs = seq(nrow(test))
for (n in obs){
  x <- test[n, 2]
  test$y_predict[n] <- predict(model_loess, x)
}

#Save test data set as csv
write.csv(test, file = "PS03_submission_Emily_Miller.csv")

