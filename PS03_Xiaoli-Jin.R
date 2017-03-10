library(tidyverse)
library(broom)

PS03_train <- PS03_train %>% 
  filter(!ID %in% c(863, 912))

score_spline <- rep(0, times = 199)

for (i in 2:200) {

  n_folds <- 10
  PS03_train <- PS03_train %>% 
    mutate(fold = sample(1:n_folds, replace = TRUE, size = nrow(PS03_train)))
  
  score_cv <- rep(0, n_folds)
  
  for(k in 1:n_folds) {
    pseudo_train <- PS03_train %>% 
      filter(fold != k) 
    
    pseudo_test <- PS03_train %>% 
      filter(fold == k) 
    
    model_spline_cv <- smooth.spline(pseudo_train$x, pseudo_train$y, df=i)
    
    Prediction <- predict(model_spline_cv, x=pseudo_test$x) %>% 
      tbl_df()
    
    pseudo_test_predict <- pseudo_test %>% 
      mutate(fitted = Prediction$"y") %>% 
      mutate(SE = (y - fitted) * (y - fitted))
   
    score_cv[k] <- mean(pseudo_test_predict$SE)
  }
  
  score_spline[i-1] <-mean(score_cv)
}

score <- data.frame(score_spline) %>% 
  mutate(df = 2:200)

# apply the model to the real test data, using df = 38 based on the result of the previous codes
model_spline_final <- smooth.spline(PS03_train$x, PS03_train$y, df = 38)

submission <- predict(model_spline_final, x = PS03_test$x) %>% 
  tbl_df()

PS03_submission$y <- submission$y



