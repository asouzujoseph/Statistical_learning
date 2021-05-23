### multiple linear regression ####
# load dataset
library(ISLR)
df <- Auto
df <- Auto[,-9] #remove name variable
head(df)

# exploratory analysis
pairs(df, col = "blue")

# correlation analysis
cor(df)

# regression analysis
df.lm <- lm(mpg~., data = df)
summary(df.lm)

# Model diagnostics
plot(df.lm)

# Non-linear transformation
df <- df[-14,]
df$mpg <- log(df$mpg)
df.lm2 <- lm(mpg~., data = df)
summary(df.lm2)

# model diagnostics
plot(df.lm2)

