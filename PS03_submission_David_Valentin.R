# HW 03
# David Valentin



datapath <- "/Users/noahlevine/Desktop/R_Code"
setwd(datapath)


train <- readr::read_csv("train.csv")
test <- readr::read_csv("test.csv")
outcome <- readr::read_csv("submission.csv")

model_spline <- smooth.spline(train$x, train$y, df=10)

pred <- predict(model_spline, test)






