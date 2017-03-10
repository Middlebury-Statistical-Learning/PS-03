PS03_train_xy <- PS03_train %>%
  dplyr::select(x,y)
PS03_train_xy

#the model being trained
PS03_train_xy_augmented2 <- smooth.spline(PS03_train_xy$x, PS03_train_xy$y, df=9) %>%
  augment() %>%
  tbl_df()

#the plot of my model
ggplot(data=NULL, aes(x=x)) +
  geom_point(data=PS03_train_xy_augmented2, aes(y=y)) +
  geom_line(data=PS03_train_xy_augmented2, aes(y=.fitted), col="blue", size=1) 

#finding the error using k=5 fold cross validation
rows <- nrow(PS03_train)
scores <- 0
k <- 5
for (i in 1:k) {
  n <- i*(rows/k)
  pseudo_test <- dplyr::slice(PS03_train, ((i-1)*(rows/k)+1):(i*(rows/k)))
  pseudo_train <- PS03_train %>% 
    anti_join(pseudo_test, by = "ID")
  pseudo_train_model <- smooth.spline(pseudo_train$x, pseudo_train$y, df=9) 
  predictions <- predict(smooth.spline(pseudo_train$x, pseudo_train$y, df=9), pseudo_test$x)$y
  pseudo_test %>% 
    mutate(predictions)
  score <- mean((pseudo_test$y-predictions)^2)
  scores <- scores + score
  }

#this is the MSE
avg <- scores/k
avg 
#running the test data through the model
y <- predict(smooth.spline(PS03_train$x, PS03_train$y, df=9), PS03_test$x)$y

#adding the y column and removing the x data for the finished product
PS03_test <- PS03_test %>% mutate(y)
PS03_test$x <- NULL
PS03_test
