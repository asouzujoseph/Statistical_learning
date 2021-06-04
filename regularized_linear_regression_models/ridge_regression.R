##### Comparison of linear, ridge and lasso regresion models

#Task: 

library(glmnet)
library(ISLR)
library(caret)

### dataset from the ISLR package
df <- College
df <- na.omit(df)
names(df)

### create training dataset
set.seed(1000) # allows reproducibilty
index <- sample(1:nrow(df),0.8*nrow(df)) # use random sample (80%) as training data
train <- df[index,] # training dataset
test <- df[-index,] # test dataset

### scale numeric features
names(df)
cols <- c("Private","Accept","Enroll","Top10perc","Top25perc",
          "F.Undergrad","P.Undergrad","Outstate","Room.Board",
          "Books","Personal","PhD","Terminal","S.F.Ratio",
          "perc.alumni","Expend","Grad.Rate")
pre_proc <- preProcess(train[,cols], method =c("center", "scale"))
train[,cols] <- predict(pre_proc,train[,cols])
test[,cols] <- predict(pre_proc, test[,cols])
summary(train)

### Linear regression model
df.lm <- lm(Apps~., data = train)
summary(df.lm)
## Model assessment
predictions <- predict(df.lm,test)
RMSE(predictions, test$Apps)
MSE <- RMSE(predictions, test$Apps)**2
MSE

### Regularized linear regression models
set.seed(1000)
x_train <- model.matrix(Apps~.,train)[,-1] #predictor variables
y_train <- train$Apps # response variables
x_test <- model.matrix(Apps~.,test)[,-1]
y_test <- test$Apps

### Ridge regression model
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)
cv_ridge$lambda.min
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = cv_ridge$lambda.min)
coef(ridge_model)

## Predictions and model assessment
predictions <- predict(ridge_model,x_test)
RMSE <- RMSE(predictions, y_test)
MSE <- RMSE(predictions, y_test)**2
data.frame(RMSE, MSE)

### Lasso regression
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
cv_lasso$lambda.min
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = cv_lasso$lambda.min)
coef(lasso_model)

## Predictions and model assessment
predictions <- predict(lasso_model,x_test)
RMSE <- RMSE(predictions, y_test)
MSE <- RMSE(predictions, y_test)**2
data.frame(RMSE, MSE)





