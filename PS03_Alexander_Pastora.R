library(tidyverse)
library(broom)

training <- read.csv("train.csv")
test <- read.csv("test.csv")


ggplot(data=training, aes(x=x, y=y)) +geom_point()

model_spline <- smooth.spline(x=training$x, y=training$y,w=NULL, df=500)
model_spline


training_augmented <- model_spline %>%
  augment() %>%
  tbl_df()
training_augmented

test_augmented <- predict(model_spline, x=test$x) %>%
  tbl_df() %>%
  left_join(test, by="x")



training_augmented2 <- predict(model_spline, x=training$x) %>%
  tbl_df() %>% 
  left_join(training_augmented, by="x")
  

write.csv(test_augmented, "PS03_submission_Alexander_Pastora.csv")

# Honesty wasn't sure how to train the stupid model, so predictions are probably 
# wrong.


#MSE
Summation =as.numeric(0)
for(i in 1:2500) 
{
  Summation = Summation + as.numeric((training_augmented2$.fitted[i] - training_augmented2$y.x[i])^2)
  print(Summation)
}
MSE = Summation / count(training_augmented2)

