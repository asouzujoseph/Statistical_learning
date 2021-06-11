### Tree based methods
library (randomForest)
library(ISLR)
library(tree)
library(gbm)
attach(Carseats)

# generate training and test data sets
set.seed(222)
ratio <- sample(1:nrow(Carseats),nrow(Carseats)*0.5)
test <- Carseats[-ratio,]
train <- Carseats[ratio,]

#### Bagging
set.seed(222)
bag_model <- randomForest(Sales~., data= train, mtry = 10, importance = TRUE, ntree=1000)
bag_model
importance(bag_model)
varImpPlot(bag_model)
predictions_bag <- predict(bag_model,newdata = test)
MSE_bagging <- mean((predictions_bag - test$Sales)^2)

#### Random forest
set.seed(222)
rf_model <- randomForest(Sales~., data= train, mtry = 4, importance = TRUE,ntree=1000)
rf_model
importance(rf_model)
varImpPlot(rf_model)
predictions_rf <- predict(rf_model,newdata = test)
MSE_rf <- mean((predictions_rf - test$Sales)^2)

#### Boosting
set.seed(222)
boost_model =gbm(Sales~.,data=train, distribution="gaussian",n.trees =1000 , interaction.depth =4)
summary(boost_model)
predictions_boosting <- predict(boost_model,newdata = test)
MSE_boosting <- mean((predictions_boosting - test$Sales)^2)


#### comparison of model performance
data.frame(MSE_bagging,MSE_rf, MSE_boosting)






