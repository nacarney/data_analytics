### DATA ANALYTICS ASSIGNMENT 2

library("VIM")
library("mice")
library(rpart)
library("partykit") 
library("party")
library("lattice")
library("ggplot2")
library("caret")
library("e1071")
library("tibble")

setwd("/Users/nathancarney/Documents/College/4th Year/Data Analytics/Labs")

set.seed(123)

#str(Data)

### Model 1 #### 

# Predictors are X4 and X1

# 77.36% unpruned
# 75.34% pruned

Data <- read.csv("lab5.csv")

attach(Data)

# Removing ID Column, Group Column 
Data <- Data[,-c(1, 3)]

# Setting Response variable to factor instead of binary variable
Data[,1] <- as.factor(Data[,1])

#most data missing from X3 and Y3
#md.pattern(Data)

# Y1, Y2, Y3, Y4, Y5, Y7 are binary variables. Y6 has 3 values: 0, 1 and 2. Hence, all can be converted to factors
Data[,9] <- as.factor(Data[,9])
Data[,10] <- as.factor(Data[,10])
Data[,11] <- as.factor(Data[,11])
Data[,12] <- as.factor(Data[,12])
Data[,13] <- as.factor(Data[,13])
Data[,14] <- as.factor(Data[,14])
Data[,15] <- as.factor(Data[,15])

# MICE imputation

# Assuming that 0's are valid figures in all columns as no info given about what cols represent 

# X variables are all continuous, hence I have chosen to use Predictive Mean Matching to impute their missing values. 
# Y variables are all binary, hence logistic regression imputation was chosen 
# (except for Y6 which has 3 levels. For Y6, polytomous regression was used. from R documentation: 'By default, 
# unordered factors with more than two levels are imputed by mice.impute.polyreg().'

# Response, X4 and Y4 did not contain missing values

my_imp = mice(Data, method = c("", "pmm", "pmm", "pmm", "", "pmm", "pmm", "pmm", "logreg", "logreg", "logreg", "", "logreg", "polyreg", "logreg"), seed = 123,  m=1,print=FALSE)

Data <- complete(my_imp)

#Removing Y Variables
Data <- Data[,-c(9:17)]

DT_Model <-rpart(Response~., data=Data, control=rpart.control(minsplit=50, minbucket=10, maxdepth=2)) 

plot(as.party(DT_Model))

opt <- which.min(DT_Model$cptable [, "xerror"]) 
opt
# Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]
cp
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)
# Now let's analyse the performance of the model.
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = Data ,type = "prob")
#Pred_DT_Model_pruned
# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "1", "0")
#Pred_DT_Model_pruned_YN
# saving predicted vector as factor 
Pred <- as.factor(Pred_DT_Model_pruned_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(Data$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM

Pred_DT_Model <- predict(DT_Model, data = Data ,type = "prob")
#Pred_DT_Model
Pred_DT_Model_YN <- ifelse(Pred_DT_Model[,2] > 0.50, "1", "0")
#Pred_DT_Model_YN
Pred <- as.factor(Pred_DT_Model_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(Data$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM


### Model 2 ####

# Predictors are Y5 and Y4

# 73.99% pruned and not pruned

Data <- read.csv("lab5.csv")
# Removing ID Column, Group Column 
Data <- Data[,-c(1, 3)]
# Setting Response variable to factor instead of binary variable
Data[,1] <- as.factor(Data[,1])
# Y1, Y2, Y3, Y4, Y5, Y7 are binary variables. Y6 has 3 values: 0, 1 and 2. Hence, all can be converted to factors
Data[,9] <- as.factor(Data[,9])
Data[,10] <- as.factor(Data[,10])
Data[,11] <- as.factor(Data[,11])
Data[,12] <- as.factor(Data[,12])
Data[,13] <- as.factor(Data[,13])
Data[,14] <- as.factor(Data[,14])
Data[,15] <- as.factor(Data[,15])

my_imp = mice(Data, method = c("", "pmm", "pmm", "pmm", "", "pmm", "pmm", "pmm", "logreg", "logreg", "logreg", "", "logreg", "polyreg", "logreg"), seed = 123,  m=1,print=FALSE)
Data <- complete(my_imp)

# Removing X columns
Data <- Data[,-c(2:8)]

DT_Model <-rpart(Response~., data=Data, control=rpart.control(minsplit=50, minbucket=10, maxdepth=2)) 
plot(as.party(DT_Model))

opt <- which.min(DT_Model$cptable [, "xerror"]) 
#opt
# Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]
#cp
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
#print(DT_Model_pruned)
# Now let's analyse the performance of the model.
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = Data ,type = "prob")
Pred_DT_Model_pruned
# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "1", "0")
Pred_DT_Model_pruned_YN
# saving predicted vector as factor 
Pred <- as.factor(Pred_DT_Model_pruned_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(Data$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM

Pred_DT_Model <- predict(DT_Model, data = Data ,type = "prob")
#Pred_DT_Model
Pred_DT_Model_YN <- ifelse(Pred_DT_Model[,2] > 0.50, "1", "0")
#Pred_DT_Model_YN
Pred <- as.factor(Pred_DT_Model_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(Data$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM

### Model 3 ####

# Predictors are X4 and X1

# 77.36% unpruned

Data <- read.csv("lab5.csv")
# Removing ID Column, Group Column 
Data <- Data[,-c(1, 3)]
# Setting Response variable to factor instead of binary variable
Data[,1] <- as.factor(Data[,1])
# Y1, Y2, Y3, Y4, Y5, Y7 are binary variables. Y6 has 3 values: 0, 1 and 2. Hence, all can be converted to factors
Data[,9] <- as.factor(Data[,9])
Data[,10] <- as.factor(Data[,10])
Data[,11] <- as.factor(Data[,11])
Data[,12] <- as.factor(Data[,12])
Data[,13] <- as.factor(Data[,13])
Data[,14] <- as.factor(Data[,14])
Data[,15] <- as.factor(Data[,15])

my_imp = mice(Data, method = c("", "pmm", "pmm", "pmm", "", "pmm", "pmm", "pmm", "logreg", "logreg", "logreg", "", "logreg", "polyreg", "logreg"), seed = 123,  m=1,print=FALSE)
Data <- complete(my_imp)

DT_Model <-rpart(Response~., data=Data, control=rpart.control(minsplit=50, minbucket=10, maxdepth=2)) 
plot(as.party(DT_Model))

opt <- which.min(DT_Model$cptable [, "xerror"]) 
opt
# Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]
cp
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)
# Now let's analyse the performance of the model.
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = Data ,type = "prob")
Pred_DT_Model_pruned
# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "1", "0")
Pred_DT_Model_pruned_YN
# saving predicted vector as factor 
Pred <- as.factor(Pred_DT_Model_pruned_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(Data$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM

Pred_DT_Model <- predict(DT_Model, data = Data ,type = "prob")
#Pred_DT_Model
Pred_DT_Model_YN <- ifelse(Pred_DT_Model[,2] > 0.50, "1", "0")
#Pred_DT_Model_YN
Pred <- as.factor(Pred_DT_Model_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(Data$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM


### Model 4 ####

# Predictor is X4

# 72.92% pruned and not pruned

Data <- read.csv("lab5.csv")
Data[,3] <- as.factor(Data[,3])
Group <- Data[,3]
attach(Data)
# Removing ID Column, Group Column 
Data <- Data[,-c(1, 3)]
# Setting Response variable to factor instead of binary variable
Data[,1] <- as.factor(Data[,1])
#most data missing from X3 and Y3
#md.pattern(Data)
#Data[, 9:15] <- lapply(data[,9:15], factor)  ## as.factor() could also be used
#summary(Data[,9:15])
# Y1, Y2, Y3, Y4, Y5, Y7 are binary variables. Y6 has 3 values: 0, 1 and 2. Hence, all can be converted to factors
Data[,9] <- as.factor(Data[,9])
Data[,10] <- as.factor(Data[,10])
Data[,11] <- as.factor(Data[,11])
Data[,12] <- as.factor(Data[,12])
Data[,13] <- as.factor(Data[,13])
Data[,14] <- as.factor(Data[,14])
Data[,15] <- as.factor(Data[,15])

# MICE imputation
# Assuming that 0's are valid figures in all columns as no info given about what cols represent 
# X variables are all continuous, hence I have chosen to use Predictive Mean Matching to impute their missing values. 
# Y variables are all binary, hence logistic regression imputation was chosen 
# (except for Y6 which has 3 levels. For Y6, polytomous regression was used. from R documentation: 'By default, 
# unordered factors with more than two levels are imputed by mice.impute.polyreg().'
# Response, X4 and Y4 did not contain missing values

my_imp = mice(Data, method = c("", "pmm", "pmm", "pmm", "", "pmm", "pmm", "pmm", "logreg", "logreg", "logreg", "", "logreg", "polyreg", "logreg"), seed = 123,  m=1,print=FALSE)
Data <- complete(my_imp)

add_column(Data, Group, .after = "Response")
males_index <- which(Group == 0)
males <- Data[males_index,]
females <- Data[-(males_index),]
#Removing Group Variable
males <- males[,-2]
females <- females[,-2]
# Removing Y Variables
males <- males[, -c(9:15)]

DT_Model <-rpart(Response~., data=males, control=rpart.control(minsplit=50, minbucket=10, maxdepth=2)) 
plot(as.party(DT_Model))

opt <- which.min(DT_Model$cptable [, "xerror"]) 
opt
# Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]
cp
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
#print(DT_Model_pruned)
# Now let's analyse the performance of the model.
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = males ,type = "prob")
#Pred_DT_Model_pruned
# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "1", "0")
#Pred_DT_Model_pruned_YN
# saving predicted vector as factor 
Pred <- as.factor(Pred_DT_Model_pruned_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(males$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM

Pred_DT_Model <- predict(DT_Model, data = males ,type = "prob")
#Pred_DT_Model
Pred_DT_Model_YN <- ifelse(Pred_DT_Model[,2] > 0.50, "1", "0")
#Pred_DT_Model_YN
Pred <- as.factor(Pred_DT_Model_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(males$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM

### Model 6 ####

# Predictor is Y4

# 70.83% pruned and not pruned

Data <- read.csv("lab5.csv")
Data[,3] <- as.factor(Data[,3])
Group <- Data[,3]
attach(Data)
# Removing ID Column, Group Column 
Data <- Data[,-c(1, 3)]
# Setting Response variable to factor instead of binary variable
Data[,1] <- as.factor(Data[,1])
#most data missing from X3 and Y3
#md.pattern(Data)
#Data[, 9:15] <- lapply(data[,9:15], factor)  ## as.factor() could also be used
#summary(Data[,9:15])
# Y1, Y2, Y3, Y4, Y5, Y7 are binary variables. Y6 has 3 values: 0, 1 and 2. Hence, all can be converted to factors
Data[,9] <- as.factor(Data[,9])
Data[,10] <- as.factor(Data[,10])
Data[,11] <- as.factor(Data[,11])
Data[,12] <- as.factor(Data[,12])
Data[,13] <- as.factor(Data[,13])
Data[,14] <- as.factor(Data[,14])
Data[,15] <- as.factor(Data[,15])

# MICE imputation

# Assuming that 0's are valid figures in all columns as no info given about what cols represent 
# X variables are all continuous, hence I have chosen to use Predictive Mean Matching to impute their missing values. 
# Y variables are all binary, hence logistic regression imputation was chosen 
# (except for Y6 which has 3 levels. For Y6, polytomous regression was used. from R documentation: 'By default, 
# unordered factors with more than two levels are imputed by mice.impute.polyreg().'
# Response, X4 and Y4 did not contain missing values

my_imp = mice(Data, method = c("", "pmm", "pmm", "pmm", "", "pmm", "pmm", "pmm", "logreg", "logreg", "logreg", "", "logreg", "polyreg", "logreg"), seed = 123,  m=1,print=FALSE)
Data <- complete(my_imp)

add_column(Data, Group, .after = "Response")
males_index <- which(Group == 0)
males <- Data[males_index,]
females <- Data[-(males_index),]
#Removing Group Variable
males <- males[,-2]
females <- females[,-2]
# Removing X Variables
males <- males[, -c(2:8)]

DT_Model <-rpart(Response~., data=males, control=rpart.control(minsplit=50, minbucket=10, maxdepth=2)) 
plot(as.party(DT_Model))

opt <- which.min(DT_Model$cptable [, "xerror"]) 
#opt
# Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]
#cp
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
#print(DT_Model_pruned)
# Now let's analyse the performance of the model.
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = males ,type = "prob")
#Pred_DT_Model_pruned
# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "1", "0")
#Pred_DT_Model_pruned_YN
# saving predicted vector as factor 
Pred <- as.factor(Pred_DT_Model_pruned_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(males$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM

Pred_DT_Model <- predict(DT_Model, data = males ,type = "prob")
#Pred_DT_Model
Pred_DT_Model_YN <- ifelse(Pred_DT_Model[,2] > 0.50, "1", "0")
#Pred_DT_Model_YN
Pred <- as.factor(Pred_DT_Model_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(males$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM

### Model 8 ####

# Predictor is X4

# 72.92% pruned and not pruned

Data <- read.csv("lab5.csv")
Data[,3] <- as.factor(Data[,3])
Group <- Data[,3]
attach(Data)
# Removing ID Column, Group Column 
Data <- Data[,-c(1, 3)]
# Setting Response variable to factor instead of binary variable
Data[,1] <- as.factor(Data[,1])
#most data missing from X3 and Y3
#md.pattern(Data)
#Data[, 9:15] <- lapply(data[,9:15], factor)  ## as.factor() could also be used
#summary(Data[,9:15])
# Y1, Y2, Y3, Y4, Y5, Y7 are binary variables. Y6 has 3 values: 0, 1 and 2. Hence, all can be converted to factors
Data[,9] <- as.factor(Data[,9])
Data[,10] <- as.factor(Data[,10])
Data[,11] <- as.factor(Data[,11])
Data[,12] <- as.factor(Data[,12])
Data[,13] <- as.factor(Data[,13])
Data[,14] <- as.factor(Data[,14])
Data[,15] <- as.factor(Data[,15])

# MICE imputation
# Assuming that 0's are valid figures in all columns as no info given about what cols represent 
# X variables are all continuous, hence I have chosen to use Predictive Mean Matching to impute their missing values. 
# Y variables are all binary, hence logistic regression imputation was chosen 
# (except for Y6 which has 3 levels. For Y6, polytomous regression was used. from R documentation: 'By default, 
# unordered factors with more than two levels are imputed by mice.impute.polyreg().'
# Response, X4 and Y4 did not contain missing values

my_imp = mice(Data, method = c("", "pmm", "pmm", "pmm", "", "pmm", "pmm", "pmm", "logreg", "logreg", "logreg", "", "logreg", "polyreg", "logreg"), seed = 123,  m=1,print=FALSE)
Data <- complete(my_imp)

add_column(Data, Group, .after = "Response")
males_index <- which(Group == 0)
males <- Data[males_index,]
females <- Data[-(males_index),]
#Removing Group Variable
males <- males[,-2]
females <- females[,-2]

DT_Model <-rpart(Response~., data=males, control=rpart.control(minsplit=50, minbucket=10, maxdepth=2)) 
plot(as.party(DT_Model))

opt <- which.min(DT_Model$cptable [, "xerror"]) 
#opt
# Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]
#cp
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
#print(DT_Model_pruned)
# Now let's analyse the performance of the model.
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = males ,type = "prob")
#Pred_DT_Model_pruned
# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "1", "0")
#Pred_DT_Model_pruned_YN
# saving predicted vector as factor 
Pred <- as.factor(Pred_DT_Model_pruned_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(males$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM

Pred_DT_Model <- predict(DT_Model, data = males ,type = "prob")
#Pred_DT_Model
Pred_DT_Model_YN <- ifelse(Pred_DT_Model[,2] > 0.50, "1", "0")
#Pred_DT_Model_YN
Pred <- as.factor(Pred_DT_Model_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(males$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM

### Model 5 ####

# Predictors are X4 and X7
# 80% unpruned

Data <- read.csv("lab5.csv")
Data[,3] <- as.factor(Data[,3])
Group <- Data[,3]
attach(Data)
# Removing ID Column, Group Column 
Data <- Data[,-c(1, 3)]
# Setting Response variable to factor instead of binary variable
Data[,1] <- as.factor(Data[,1])
#most data missing from X3 and Y3
#md.pattern(Data)
#Data[, 9:15] <- lapply(data[,9:15], factor)  ## as.factor() could also be used
#summary(Data[,9:15])
# Y1, Y2, Y3, Y4, Y5, Y7 are binary variables. Y6 has 3 values: 0, 1 and 2. Hence, all can be converted to factors
Data[,9] <- as.factor(Data[,9])
Data[,10] <- as.factor(Data[,10])
Data[,11] <- as.factor(Data[,11])
Data[,12] <- as.factor(Data[,12])
Data[,13] <- as.factor(Data[,13])
Data[,14] <- as.factor(Data[,14])
Data[,15] <- as.factor(Data[,15])

# MICE imputation
# Assuming that 0's are valid figures in all columns as no info given about what cols represent 
# X variables are all continuous, hence I have chosen to use Predictive Mean Matching to impute their missing values. 
# Y variables are all binary, hence logistic regression imputation was chosen 
# (except for Y6 which has 3 levels. For Y6, polytomous regression was used. from R documentation: 'By default, 
# unordered factors with more than two levels are imputed by mice.impute.polyreg().'
# Response, X4 and Y4 did not contain missing values

my_imp = mice(Data, method = c("", "pmm", "pmm", "pmm", "", "pmm", "pmm", "pmm", "logreg", "logreg", "logreg", "", "logreg", "polyreg", "logreg"), seed = 123,  m=1,print=FALSE)
Data <- complete(my_imp)

add_column(Data, Group, .after = "Response")
males_index <- which(Group == 0)
males <- Data[males_index,]
females <- Data[-(males_index),]
#Removing Group Variable
males <- males[,-2]
females <- females[,-2]
# Removing Y Variables
females <- females[, -c(9:15)]

DT_Model <-rpart(Response~., data=females, control=rpart.control(minsplit=60, minbucket=10, maxdepth=2)) 
plot(as.party(DT_Model))

opt <- which.min(DT_Model$cptable [, "xerror"]) 
#opt
# Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]
#cp
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
#print(DT_Model_pruned)
# Now let's analyse the performance of the model.
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = females ,type = "prob")
#Pred_DT_Model_pruned
# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "1", "0")
#Pred_DT_Model_pruned_YN
# saving predicted vector as factor 
Pred <- as.factor(Pred_DT_Model_pruned_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(females$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM

Pred_DT_Model <- predict(DT_Model, data = females ,type = "prob")
#Pred_DT_Model
Pred_DT_Model_YN <- ifelse(Pred_DT_Model[,2] > 0.50, "1", "0")
#Pred_DT_Model_YN
Pred <- as.factor(Pred_DT_Model_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(females$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM

### Model 7 ####

# Predictor is Y5

# 76% pruned and unpruned 

Data <- read.csv("lab5.csv")
Data[,3] <- as.factor(Data[,3])
Group <- Data[,3]
attach(Data)
# Removing ID Column, Group Column 
Data <- Data[,-c(1, 3)]
# Setting Response variable to factor instead of binary variable
Data[,1] <- as.factor(Data[,1])
#most data missing from X3 and Y3
#md.pattern(Data)
#Data[, 9:15] <- lapply(data[,9:15], factor)  ## as.factor() could also be used
#summary(Data[,9:15])
# Y1, Y2, Y3, Y4, Y5, Y7 are binary variables. Y6 has 3 values: 0, 1 and 2. Hence, all can be converted to factors
Data[,9] <- as.factor(Data[,9])
Data[,10] <- as.factor(Data[,10])
Data[,11] <- as.factor(Data[,11])
Data[,12] <- as.factor(Data[,12])
Data[,13] <- as.factor(Data[,13])
Data[,14] <- as.factor(Data[,14])
Data[,15] <- as.factor(Data[,15])

# MICE imputation
# Assuming that 0's are valid figures in all columns as no info given about what cols represent 
# X variables are all continuous, hence I have chosen to use Predictive Mean Matching to impute their missing values. 
# Y variables are all binary, hence logistic regression imputation was chosen 
# (except for Y6 which has 3 levels. For Y6, polytomous regression was used. from R documentation: 'By default, 
# unordered factors with more than two levels are imputed by mice.impute.polyreg().'
# Response, X4 and Y4 did not contain missing values

my_imp = mice(Data, method = c("", "pmm", "pmm", "pmm", "", "pmm", "pmm", "pmm", "logreg", "logreg", "logreg", "", "logreg", "polyreg", "logreg"), seed = 123,  m=1,print=FALSE)
Data <- complete(my_imp)

add_column(Data, Group, .after = "Response")
males_index <- which(Group == 0)
males <- Data[males_index,]
females <- Data[-(males_index),]
#Removing Group Variable
males <- males[,-2]
females <- females[,-2]
# Removing X Variables
females <- females[, -c(2:8)]

DT_Model <-rpart(Response~., data=females, control=rpart.control(minsplit=50, minbucket=10, maxdepth=2)) 
plot(as.party(DT_Model))

opt <- which.min(DT_Model$cptable [, "xerror"]) 
#opt
# Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]
#cp
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
#print(DT_Model_pruned)
# Now let's analyse the performance of the model.
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = females ,type = "prob")
#Pred_DT_Model_pruned
# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "1", "0")
#Pred_DT_Model_pruned_YN
# saving predicted vector as factor 
Pred <- as.factor(Pred_DT_Model_pruned_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(females$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM

Pred_DT_Model <- predict(DT_Model, data = females ,type = "prob")
#Pred_DT_Model
Pred_DT_Model_YN <- ifelse(Pred_DT_Model[,2] > 0.50, "1", "0")
#Pred_DT_Model_YN
Pred <- as.factor(Pred_DT_Model_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(females$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM

### Model 9 ####

# Predictors are X4 and X7
# 80% not pruned

Data <- read.csv("lab5.csv")
Data[,3] <- as.factor(Data[,3])
Group <- Data[,3]
attach(Data)
# Removing ID Column, Group Column 
Data <- Data[,-c(1, 3)]
# Setting Response variable to factor instead of binary variable
Data[,1] <- as.factor(Data[,1])
#most data missing from X3 and Y3
#md.pattern(Data)
#Data[, 9:15] <- lapply(data[,9:15], factor)  ## as.factor() could also be used
#summary(Data[,9:15])
# Y1, Y2, Y3, Y4, Y5, Y7 are binary variables. Y6 has 3 values: 0, 1 and 2. Hence, all can be converted to factors
Data[,9] <- as.factor(Data[,9])
Data[,10] <- as.factor(Data[,10])
Data[,11] <- as.factor(Data[,11])
Data[,12] <- as.factor(Data[,12])
Data[,13] <- as.factor(Data[,13])
Data[,14] <- as.factor(Data[,14])
Data[,15] <- as.factor(Data[,15])

# MICE imputation
# Assuming that 0's are valid figures in all columns as no info given about what cols represent 
# X variables are all continuous, hence I have chosen to use Predictive Mean Matching to impute their missing values. 
# Y variables are all binary, hence logistic regression imputation was chosen 
# (except for Y6 which has 3 levels. For Y6, polytomous regression was used. from R documentation: 'By default, 
# unordered factors with more than two levels are imputed by mice.impute.polyreg().'
# Response, X4 and Y4 did not contain missing values

my_imp = mice(Data, method = c("", "pmm", "pmm", "pmm", "", "pmm", "pmm", "pmm", "logreg", "logreg", "logreg", "", "logreg", "polyreg", "logreg"), seed = 123,  m=1,print=FALSE)
Data <- complete(my_imp)

add_column(Data, Group, .after = "Response")
males_index <- which(Group == 0)
males <- Data[males_index,]
females <- Data[-(males_index),]
#Removing Group Variable
males <- males[,-2]
females <- females[,-2]

DT_Model <-rpart(Response~., data=females, control=rpart.control(minsplit=50, minbucket=10, maxdepth=2)) 
plot(as.party(DT_Model))

opt <- which.min(DT_Model$cptable [, "xerror"]) 
#opt
# Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]
#cp
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
#print(DT_Model_pruned)
# Now let's analyse the performance of the model.
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = females ,type = "prob")
#Pred_DT_Model_pruned
# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "1", "0")
#Pred_DT_Model_pruned_YN
# saving predicted vector as factor 
Pred <- as.factor(Pred_DT_Model_pruned_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(females$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM

Pred_DT_Model <- predict(DT_Model, data = females ,type = "prob")
#Pred_DT_Model
Pred_DT_Model_YN <- ifelse(Pred_DT_Model[,2] > 0.50, "1", "0")
#Pred_DT_Model_YN
Pred <- as.factor(Pred_DT_Model_YN)
# ordering the vectors
Predicted <- ordered(Pred, levels = c("1", "0"))
Actual <- ordered(females$Response,levels = c("1", "0"))
# making confusion matrix
CM <- confusionMatrix(table(Predicted,Actual))
CM



### Combine Confusion Matrices for Males and Females #### 

male_percentage = 96/296

female_percentage = 200/296

# Males - X Variables = 0.7292
# Females - X Variables = 0.80

# Males - Y Variables = 0.7083
# Females - Y Variables = 0.76

# Males - X and Y Variables = 0.7292
# Females - X and Y Variables = 0.80

mfx <- (male_percentage*0.7292) + (female_percentage*0.80) # 77.7%
mfy <- (male_percentage*0.7083) + (female_percentage*0.76) # 74.32%
mfxy <- (male_percentage*0.7292) + (female_percentage*0.80) # 77.7%













