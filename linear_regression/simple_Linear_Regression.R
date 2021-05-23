### Simple linear regression ###

# load dataset
library(ISLR)
df <- Auto
head(df)

# Correlation analysis
cor.test(df$horsepower, df$mpg)

# regression analysis
df.lm<- lm(mpg~horsepower,data = df)
summary(df.lm)

# Prediction of values
predict(df.lm,data.frame(horsepower=98), interval = "confidence")
predict(df.lm,data.frame(horsepower=98), interval = "prediction")

# visualization of the relationship between response and predictor variables
plot(df$horsepower, df$mpg, col="red")
abline(df.lm)

# model diagnostics
plot(df.lm)

# non-linear transformation of the response variable
df$mpg <- log(df$mpg)
df.lm2<- lm(mpg~horsepower,data = df)
summary(df.lm2)

# visualization of the relationship between response and predictor variables
plot(df$horsepower, df$mpg, col="red")
abline(df.lm2)

# prediction of values
predict(df.lm2,data.frame(horsepower=98), interval = "confidence")
predict(df.lm2,data.frame(horsepower=98), interval = "prediction")





