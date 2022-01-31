### DATA ANALYTICS ASSIGNMENT 3

## LOADING IN PACKAGES 


library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library("VIM")
library("mice")
library(MASS)

require(dplyr)
#install.packages('caret', dependencies = T)
#install.packages("lubridate")
library('caret')

library(stats4)

#install.packages('glmnet')
library(glmnet)

library(stats4)

#install.packages('expss')
library(expss)
library(rpart)
library("partykit") 
library("party")
library("lattice")
library("ggplot2")
library("caret")
library("e1071")
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)

setwd("/Users/nathancarney/Documents/College/4th Year/Data Analytics/Labs") 

df <- read.csv("Income Data.csv")

attach(df)

set.seed = 123

df[,1] <- as.factor(df[,1])
df[,2] <- as.factor(df[,2])
df[,3] <- as.factor(df[,3])
df[,7] <- as.factor(df[,7])
df[,8] <- as.factor(df[,8])

my_imp = mice(df, method = c("logreg", "polyreg", "logreg", "pmm", "pmm", "pmm", "logreg", "logreg", "pmm", "pmm"), seed = 123,  m=1,print=FALSE)

dat <- complete(my_imp)

str(dat)

index = sample(1:nrow(dat), 0.7*nrow(dat)) 

train = dat[index,] # Create the training data 
test = dat[-index,] # Create the test data

cols = c('white', 'educ_r', 'age60m', 'ssi', 'welfare', 'immig', 'r_sex', 'charity', 'assets')

pre_proc_val <- preProcess(train[,cols], method = c("center", "scale"))

train[,cols] = predict(pre_proc_val, train[,cols])
test[,cols] = predict(pre_proc_val, test[,cols])

lr = lm(Income ~ ., data = train)

summary(lr)

#Step 1 - create the evaluation metrics function

eval_metrics = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  r2 = as.character(round(summary(model)$r.squared, 2))
  adj_r2 = as.character(round(summary(model)$adj.r.squared, 2))
  print(adj_r2) #Adjusted R-squared
  print(as.character(round(sqrt(sum(resids2)/N), 2))) #RMSE
}

# Step 2 - predicting and evaluating the model on train data
predictions = predict(lr, newdata = train)
lm_train_eval <- eval_metrics(lr, train, predictions, target = 'Income')

# Step 3 - predicting and evaluating the model on test data
predictions = predict(lr, newdata = test)
lm_test_eval <- eval_metrics(lr, test, predictions, target = 'Income')

cols_reg = c('white', 'educ_r', 'age60m', 'ssi', 'welfare', 'Income', 'immig', 'r_sex', 'charity', 'assets')

dummies <- dummyVars(Income ~ ., data = dat[,cols_reg])

train_dummies = predict(dummies, newdata = train[,cols_reg])

test_dummies = predict(dummies, newdata = test[,cols_reg])

x = as.matrix(train_dummies)
y_train = train$Income

x_test = as.matrix(test_dummies)
y_test = test$Income

lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)

# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)

summary(lasso_model)

predictions_train <- predict(lasso_model, s = lambda_best, newx = x)
lasso_train_eval <- eval_results(y_train, predictions_train, train)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
lasso_test_eval <- eval_results(y_test, predictions_test, test)

lm_test_eval # R Squared = .1
lm_train_eval # R Squared = .1
lasso_train_eval
lasso_test_eval

Intercept <- lasso_model$a0
Coef <- lasso_model$beta
Betas <-rbind(Intercept, Coef)
rownames(Betas)[1] = "(Intercept)"
colnames(Betas) = "Lasso"
Betas

