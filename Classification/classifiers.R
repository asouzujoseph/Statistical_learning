library(ISLR)
library(MASS)
library(repr)
library(ggplot2)
head(Boston)
summary(Boston)

# Generate the response variable

crime_rate <- rep(0,506)
crime_rate[Boston$crim >median(Boston$crim)]=1
df <- Boston[,-1]
df <- data.frame(df, crime_rate)
df$crime_rate <- as.factor(df$crime_rate)

# Exploratory data analysis
ggplot(df,aes(x=crime_rate,y=zn, color = crime_rate))+geom_boxplot()+theme_classic()+ 
  labs(title="zoned residential plots vs crime_rate")
ggplot(df,aes(x=crime_rate,y=indus, color = crime_rate))+geom_boxplot()+theme_classic()+
  labs(title="industrial land plots vs crime_rate")
ggplot(df,aes(x=crime_rate,y=rm, color = crime_rate))+geom_boxplot()+theme_classic()+
  labs(title="rooms per home vs crime_rate")
ggplot(df,aes(x=crime_rate,y=nox, color = crime_rate))+geom_boxplot()+theme_classic()+
  labs(title = "nitrogen oxides concentration vs crime_rate")
ggplot(df,aes(x=crime_rate,y=age, color = crime_rate))+geom_boxplot()+theme_classic()+
  labs(title = "age vs crime_rate")
ggplot(df,aes(x=crime_rate,y=dis, color = crime_rate))+geom_boxplot()+theme_classic()+
  labs(title = "distance to employment centres vs crime_rate")
ggplot(df,aes(x=crime_rate,y=rad,color = crime_rate))+geom_boxplot()+theme_classic() +
  labs(title = "access to radial highways vs crime_rate")
ggplot(df,aes(x=crime_rate,y=ptratio, color = crime_rate))+geom_boxplot()+theme_classic()+
  labs(title = "pupil-teacher ratio vs crime_rate")
ggplot(df,aes(x=crime_rate,y=black, color = crime_rate))+geom_boxplot()+theme_classic() +
  labs(title = "proportion of blacks vs crime_rate")
ggplot(df,aes(x=crime_rate,y=lstat, color = crime_rate))+geom_boxplot()+theme_classic()+
  labs(title = "percentage of low income earners vs crime_rate")
ggplot(df,aes(x=crime_rate,y=medv, color = crime_rate))+geom_boxplot()+theme_classic()+
  labs(title = "owner occupied houses vs crime_rate")
ggplot(df,aes(x=crime_rate,y=tax, color = crime_rate))+geom_boxplot()+theme_classic()+
  labs(title = "full-value property-tax rate vs crime_rate")

# Split the dataset into test and training sets
set.seed(1110)
df_split = sort(sample(nrow(df), nrow(df)*0.8)) ## 80% of the dataset randomly selected
train<-df[df_split,]
test<-df[-df_split,]

## COMPARISON OF CLASSIFIERS

# Logistic regression model
df_logit <- glm(crime_rate~., data = train,family = binomial(link=logit))
summary(df_logit)

# Predictive accuracy of the logistic regression model
fitted.results <- predict(df_logit,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
table(fitted.results,test$crime_rate)
Accuracy.logistic <- round(mean(fitted.results == test$crime_rate), digits = 2)*100
print(paste('Accuracy is ',Accuracy.logistic,"%"))
print(paste('Test error is ',100-Accuracy.logistic,"%"))

### ROC and AUC
library("ROCR")
predict <- fitted(df_logit)
pred <- prediction(predict, train$crime_rate)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main="sensitivity vs false positive rate",colorize=TRUE)


## Linear discriminant analysis (LDA)
lda.fit=lda(crime_rate~., data = train)
lda.fit
lda.pred=predict(lda.fit,test)
lda.class =lda.pred$class
table(lda.class, test$crime_rate)

# Predictive accuracy of LDA model
accuracy.lda <- round(mean(lda.class == test$crime_rate), digits =2)*100
print(paste('Accuracy is ',accuracy.lda,"%"))
print(paste('Test error is ',100-accuracy.lda,"%"))


### KNN (K-Nearest Neighbour) model
library(class)
train.Y <- train[,14] # extract the response variable in the training dataset
test.Y <- test[,14] # extract the response variable in the test dataset
train.X <- scale(train[,-14]) # normalize the predictor variables in the training dataset
test.X <- scale(test[,-14]) # normalize the predictor variables in the test dataset

# choose the best value for k
for (i in 1:10){
  knn.pred <- knn(train.X,test.X,train.Y,k=i)
  print(paste("accuracy for k =", i, "is ",round(mean(test.Y == knn.pred), digits = 2)))
}
knn.pred <- knn(train.X,test.X,train.Y,k=3)
table(knn.pred, test.Y)
accuracy.knn <- round(mean(test.Y == knn.pred), digits = 2)*100
print(paste('Accuracy is ',accuracy.knn,"%"))
print(paste('Test error is ',100-accuracy.knn,"%"))












