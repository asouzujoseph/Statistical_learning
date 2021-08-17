library(glmnet)
library(ISLR)
library(caret)
library(PerformanceAnalytics)
library(mgcv)

### dataset from the ISLR package
load("C:/Users/Nnamdi/Desktop/prostate2.Rdata")
head(prostate)
names(prostate)

### Exploratory data analysis
boxplot(prostate[,-1], scale = TRUE, col = "red", main="Boxplot of predictor variables")
prostate$svi <- as.factor(prostate$svi)
chart.Correlation(prostate[,-6], histogram=TRUE, pch=19)
ggplot(prostate,aes(svi,Cscore,color = svi))+geom_boxplot() + theme_classic()


### scale numeric features
cols <- c("Cscore","lcavol","lweight","age","lbph","svi","lcp","lpsa")
pre_proc <- preProcess(train[,cols], method =c("center", "scale"))
train[,cols] <- predict(pre_proc,train[,cols])
test[,cols] <- predict(pre_proc, test[,cols])
summary(train)

### create training dataset
set.seed(1000) # allows reproducibilty
index <- sample(1:nrow(prostate),0.8*nrow(prostate)) # use random sample (80%) as training data
train <- prostate[index,] # training dataset
test <- prostate[-index,] # test dataset
x_train <- model.matrix(Cscore~.,train)[,-1] #predictor variables
y_train <- train$Cscore # response variables
x_test <- model.matrix(Cscore~.,test)[,-1]
y_test <- test$Cscore

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
