### DATA ANALYTICS ASSIGNMENT 4 ###

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
library(stats4)
library(glmnet)
library(expss)
library(rpart)
library("partykit") 
library("party")
library("lattice")
library("e1071")
library(ROCR)

set.seed = 123

setwd("/Users/nathancarney/Documents/College/4th Year/Data Analytics/Labs") 

df <- read.csv("Income Data.csv")

df[,1] <- as.factor(df[,1])
df[,2] <- as.factor(df[,2])
df[,3] <- as.factor(df[,3])
df[,7] <- as.factor(df[,7])
df[,8] <- as.factor(df[,8])

no_na_data <- na.omit(df)

# Imputation on missing values in non-income columns

my_imp = mice(df[, -6], method = c("logreg", "polyreg", "logreg", "pmm", "pmm", "logreg", "logreg", "pmm", "pmm"), seed = 123,  m=1,print=FALSE)

dat <- complete(my_imp)

# Create an indicator variable (non-zero is 1, zero value is 0)

indicator <- c(ifelse(no_na_data$Income == 0, 0, 1))

no_na_data$indicator <- indicator

no_na_data$indicator <- factor(no_na_data$indicator, levels = c("0", "1"))

# Use the non-zero and zero values to create a logistic regression model to find probability that indicator value will be 0 or 1

# Generate training dataset, removing Income column

no_na_data <- no_na_data[, -6]
data <- sort(sample(nrow(no_na_data), nrow(no_na_data)*.7))

# Put 70% into training dataset
train <- no_na_data[data,]

#Splitting the remaining 30% of data into test and validation sets, each being 15% of full dataset 
leftover_data <- no_na_data[-data,]
test_val_index <- sort(sample(nrow(leftover_data), nrow(leftover_data)*.5))
test <-leftover_data[test_val_index,]
validation <- leftover_data[-test_val_index,]

# creating general model
model <- glm(formula = indicator ~ white + educ_r + age60m + ssi + welfare + immig + r_sex + charity + assets, family = binomial, data = train)

model.fit <- predict(model, test, type="response")

model.fit <- ifelse(model.fit > 0.5,1,0)

misClasificError <- mean(model.fit != test$indicator)
# print(paste('Accuracy',1-misClasificError))

# "Accuracy 0.824324324324324"

p <- predict(model, test, type="response")
pr <- prediction(p, test$indicator)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
# plot(prf)

# from the above graph, 0.65 seems like an appropriate cutoff 

model.fit <- predict(model, test, type="response")
model.fit <- ifelse(model.fit > 0.65,1,0)
misClasificError <- mean(model.fit != test$indicator)
print(paste('Accuracy',1-misClasificError))

# "Accuracy 0.763513513513513"

# Therefore, I will keep the cutoff at 0.5

model.fit <- predict(model, test, type="response")
# ifelse(model.fit > 0.5,1,0) 

# for elastic model, factor names for most variables were not accepted, and was the same case for lasso and ridge 

# Use model to predict missing data indicator values

na_indices <- which(is.na(df$Income))

indic_pred <- predict(model, dat[na_indices,], type = "response")
indic_pred <- ifelse(indic_pred > 0.5, 1, 0)

# 7 missing data observations are likely to be 0 according to the model
# length(which(indic_pred == 0))

income <- df$Income

dat$Income <- income

# Changing missing Income values to estimated values from model, I am also adding the 1 values as there are no 1 values for income in the original dataset
# which(df$Income ==1)

dat[na_indices, 10] <- indic_pred

# Use non-zero part of data and create a linear regression model to estimate non-zero missing values 
# non-zero missing values in this case have been changed to 1 (as in comment above)

non_missing_indices <- which(dat$Income != 0 & dat$Income != 1)

non_zero_indices <- which(dat$Income == 1)

non_missing_df <- dat[non_missing_indices, ]

lmIncome <- lm(Income~ ., data = non_missing_df) #Create the linear regression

prediction <- predict(lmIncome, newdata = dat[non_zero_indices,])

# Mean value from estimated income figures is 44744.78
#mean(prediction)





