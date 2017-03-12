#Submission time!
library(tidyverse)
library(broom)

#load data
PS03_test <- read_csv("https://raw.githubusercontent.com/rudeboybert/MATH218/gh-pages/assets/PS/PS03/PS03_test.csv")
glimpse(PS03_test)
PS03_train <- read_csv("https://raw.githubusercontent.com/rudeboybert/MATH218/gh-pages/assets/PS/PS03/PS03_train.csv") %>%
  filter(!ID %in% c(863, 912))
glimpse(PS03_train)


#train model
#went with spline model because of lower MSE
model_spline <- smooth.spline(PS03_train$x, PS03_train$y, df=10)
PS_spline_augmented <- model_spline %>%
  augment() %>%
  tbl_df()


#augment test data
test_augmented <- predict(model_spline, x=PS03_test$x) %>%
  tbl_df() %>%
  left_join(PS03_test, by="x")

#look at it for fun
ggplot(data= test_augmented, aes(x=x, y=y))+
  geom_point() 

write.csv(test_augmented, file = "Ps03_submission_Hoekstra.csv")
