train <- read.csv("PS03_train.csv")
train <-  filter(train, !ID %in% c(863, 912))

ggplot(train, aes(x=x, y=y)) + geom_point()

BaseLine <- lm(train$y~train$x, data=train)
#Slope = 0.263, intercept = -2.7004

plot(train$x, train$y, abline(-2.7004, 0.2603))
#I wanted to plot it to see what it looks like in base R, it doesn't seem to be working in ggplot 


ggplot(train, aes(x=x, y=y)) + geom_point() + geom_line(aes(y=BaseLine$fitted.values))
#I got it to work in ggplot, this is what the raw model looks like, we want this line to now look smooth (like what occured in intro splines)

Line_spline <- smooth.spline(train$x, train$y, df = 14)

Line_spline_augmented <- augment(Line_spline)

ggplot(train, aes(x=x, y=y)) + geom_point() + geom_line(data=Line_spline_augmented, aes(y=.fitted), col = "red") + geom_smooth()
#this slightly differed from the intro splines scirpt: 
  #I specified the data within the aes() of ggplot, not sure why Professor said data = NULL and decided to overlay data within geom_layer?

  #Hopefully I can extract the values from my spline line after I have the appropriate degrees of freedom
    #ARE THE DF CHANGING THE FITTED  VALUES? I WOULD IMAGINE THEY ARE (but maybe I can test this with MSE)
      #It seems that between 10, 12, 14 degrees of freedom I am getting very close to geom_smooth()
  #By changing the df I am attempting to fit to the train data 
  
ggplot(train, aes(x=x, y=y)) + geom_point() + geom_smooth()
#this may be cheating but, but geom_smooth()'s model looks like a pretty good dance to follow 

Line_splineTest1 <- smooth.spline(train$x, train$y, df = 12)
Line_spline_augmentedTest1 <- augment(Line_splineTest1)

Line_spline_augmented$.fitted-Line_spline_augmentedTest1$.fitted
#So the fitted values appear to be different with different df because when I subtract the columsn I don't get 0

#Lemme assess the MSE 
MSEdf14 <- mean((train$y-Line_spline_augmented$.fitted)^2)
#MSE = 151.93

MSEdf12 <- mean((train$y-Line_spline_augmentedTest1$.fitted)^2)
#MSE = 153.53 

#Though we want to be careful for overfitting***

#I guess its time to do cross-validation for the smooth spline line with 14 df 

#I SEE, REMEMBER THAT LOOCV is a K n-fold CV so we must assign 2498 folds for LOOCV because that's how much obs we have

train <- mutate(train, fold = 1:2498)
#So each observation is now assigned a fold 1:2498 

#Somehow I need to store different df lines in an object 

MSEtest <- data.frame(MSEdf = as.numeric(c(2:200)))

#My for loop is not working....I couldn't assess the MSE for 2:200 df spline_lines
for(MSE in 2:200) {for(df in 2:200) {
  MSEtest$MSE <- mean((Line_spline_augmented$.resid)^2)
  MSEdf <- data.frame(MSEdf = MSEtest$MSE, df = df)}}

test <- read.csv("PS03_test.csv")


Line_splineTest <- smooth.spline(test$x, test$y, df = 14)
Line_splineTest_augmented <- augment(Line_splineTest)

Submission <- read.csv("PS03_submission.csv")

Submission$y <- Line_splineTest_augmented$y

Submission$y

write_csv(Submission, "PS03_submission_Malik_Gomez")

#My for loop worked but what is it accomplishing? 
  #We are trying to cross-validate the line_spline model 
  #We cut up our model into psuedo_train and psuedo_test 
  #We do this to create a psuedo_score (an MSE) that will give a score to our model
  #The MSE will tell us how close our model is as whole to the actual values
  #We cross-validate to test our model, the actual test is suppose to have differing conditions then the training 
  #Because the test file PSO3_test.csv file has unknown outcomes
    #We leverage this by using outcomes that we do know and we make predictions on them (psuedo_train makes predictions on outcomes on psuedo_test and then we score the correctness in MSE)



