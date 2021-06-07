library(ISLR)
library(caret)
library(tidyverse)
library(glmnet)
library(gam)
library(PerformanceAnalytics)
library(mgcv)

load("C:/Users/Nnamdi/Desktop/prostate2.Rdata")
prostate <- na.omit(prostate)
head(prostate)

#exploratory sata analysis
prostate$svi <- as.factor(prostate$svi)
chart.Correlation(prostate[,-6], histogram=TRUE, pch=19)
ggplot(prostate,aes(svi,Cscore,color = svi))+geom_boxplot() + theme_classic()

## create training and test dataset
set.seed(1000) # allows reproducibilty
index <- sample(1:nrow(prostate),0.8*nrow(prostate)) # use random sample (80%) as training data
train <- prostate[index,] # training dataset
test <- prostate[-index,] # test dataset

## preprocess data
cols <- c("Cscore","lcavol","lweight","age","lbph","svi","lcp","lpsa")
pre_proc <- preProcess(train[,cols], method =c("center", "scale")) ### scale numeric features
train[,cols] <- predict(pre_proc,train[,cols])
test[,cols] <- predict(pre_proc, test[,cols])
summary(train) # confirm that the mean is zero for the predictor variables

## Lasso model
set.seed(1000)
x_train <- model.matrix(Cscore~.,train)[,-1] #predictor variables
y_train <- train$Cscore # response variables
x_test <- model.matrix(Cscore~.,test)[,-1]
y_test <- test$Cscore
# use cross validation to select the best lambda value with the lowest error
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
cv_lasso$lambda.min
# Apply the best lambda value in the lasso model
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = cv_lasso$lambda.min, family = gaussian(link="identity"))
coef(lasso_model)
predictions <- predict(lasso_model,x_test)
RMSE <- round(RMSE(predictions, y_test), digits = 2)
MSE <- round(RMSE**2, digits = 2)
print(paste("The lasso model has an RMSE value of",RMSE))
print(paste("The lasso model has an MSE value of",MSE))


##  GAM model
gam_model <- gam(Cscore ~ s(lcavol) + s(lweight) + s(age) + s(lbph) + s(lcp) + s(lpsa) + svi, data = train, method = "REML", family = gaussian(link="identity"), select=T)
plot(gam_model,pages = 1,rug = TRUE, shade = TRUE,shift = coef(gam_model)[1],  col = "brown")
summary(gam_model)
gam.check(gam_model)
predictions <- predict(gam_model, test, type="response")
RMSE_GAM <- round(RMSE(predictions, test$Cscore),digits = 2)
MSE_GAM <- round(RMSE_GAM**2, digits = 2)
print(paste("The GAM model has an RMSE value of",RMSE_GAM))
print(paste("The GAM model has an MSE value of",MSE_GAM))

## Comparison of Lasso and GAM models
RMSE_compare <- c(RMSE,RMSE_GAM)
MSE_compare <- c(MSE,MSE_GAM)
accuracy <- data.frame(RMSE_compare,MSE_compare)
row.names(accuracy) <- c("Lasso","GAM")
accuracy









