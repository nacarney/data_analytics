### DATA ANALYTICS ASSIGNMENT 1

library(rpart)
library("partykit") 
library("party")
library("lattice")
library("ggplot2")
library("caret")
library("e1071")

setwd("/Users/nathancarney/Documents/College/4th Year/Data Analytics/Labs") 

Data <- read.csv("lab5.csv")

set.seed(123)

#str(Data)

## PART 1 ####

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

 
### Model 1 #### 

# Predictors are X4 and X1

# 77.36% without pruning
# 75.34% with pruning

#Removing Y Variables
Data <- Data[,-c(9:17)]

str(Data)
#head(Data)

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


### Model 2 ####

# Predictors are Y5 and Y4

# 73.99% both pruned and not pruned

# Removing X columns
Data <- Data[,-c(2:8)]

str(Data)
#head(Data)

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








### Model 3 ####

# Predictors are X4 and X1

# 77.36% without pruning
# 75.34% with pruning

#head(Data)

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




## PART 2 ####

# Removing ID Column
Data <- Data[,-1]

# Setting Response and Group variables as factors instead of binary variables
Data[,1] <- as.factor(Data[,1])
Data[,2] <- as.factor(Data[,2])


### 1. Split file based on Group ####

males_index <- which(Group == 0)
males <- Data[males_index,]

females <- Data[-(males_index),]

#Removing Group Variable
males <- males[,-2]
females <- females[,-2]



### (a) Males ####

str(males)

### (i) X Variables ####

# Predictor is X4

# 72.92% pruned and not pruned

# Removing Y Variables
males <- males[, -c(9:15)]

DT_Model <-rpart(Response~., data=males, control=rpart.control(minsplit=20, minbucket=10, maxdepth=2)) 

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
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = males ,type = "prob")
Pred_DT_Model_pruned

# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "1", "0")
Pred_DT_Model_pruned_YN

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


### (ii) Y Variables ####

# Predictor is Y4

# 70.83% pruned and not pruned

# Removing X Variables
males <- males[, -c(2:8)]

DT_Model <-rpart(Response~., data=males, control=rpart.control(minsplit=50, minbucket=10, maxdepth=2)) 

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
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = males ,type = "prob")
Pred_DT_Model_pruned

# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "1", "0")
Pred_DT_Model_pruned_YN

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

### (iii) X and Y Variables ####

# 72.92% pruned and not pruned

DT_Model <-rpart(Response~., data=males, control=rpart.control(minsplit=50, minbucket=10, maxdepth=2)) 

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
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = males ,type = "prob")
Pred_DT_Model_pruned

# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "1", "0")
Pred_DT_Model_pruned_YN

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






### (b) Females ####

str(females)

### (i) X Variables ####

# 80.5% not pruned 
# 77.5% pruned 

# Removing Y Variables
females <- females[, -c(9:15)]

DT_Model <-rpart(Response~., data=females, control=rpart.control(minsplit=60, minbucket=10, maxdepth=2)) 

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
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = females ,type = "prob")
Pred_DT_Model_pruned

# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "1", "0")
Pred_DT_Model_pruned_YN

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

### (ii) Y Variables ####

# 77% not ptuned 
# 76% pruned

# Removing X Variables
females <- females[, -c(2:8)]

DT_Model <-rpart(Response~., data=females, control=rpart.control(minsplit=50, minbucket=10, maxdepth=2)) 

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
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = females ,type = "prob")
Pred_DT_Model_pruned

# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "1", "0")
Pred_DT_Model_pruned_YN

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

### (iii) X and Y Variables ####

# 80.5% not pruned 
# 77.5% pruned

DT_Model <-rpart(Response~., data=females, control=rpart.control(minsplit=50, minbucket=10, maxdepth=2)) 

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
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = females ,type = "prob")
Pred_DT_Model_pruned

# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "1", "0")
Pred_DT_Model_pruned_YN

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



### 3. Combine Confusion Matrices for Males and Females #### 

male_percentage = 96/296

female_percentage = 200/296

# Males - X Variables = 0.7292
# Females - X Variables = 0.805

# Males - Y Variables = 0.7083
# Females - Y Variables = 0.77

# Males - X and Y Variables = 0.7292
# Females - X and Y Variables = 0.805

mfx <- (male_percentage*0.7292) + (female_percentage*0.805) # 78.04%
mfy <- (male_percentage*0.7083) + (female_percentage*0.77) # 74.99892%
mfxy <- (male_percentage*0.7292) + (female_percentage*0.805) # 78.04%













