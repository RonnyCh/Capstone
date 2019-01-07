library(ggplot2)
library(caret)
library(dplyr)
library(ggthemes)

setwd("/Users/Ronny/Capstone_EDX/Titanic")
dl <- read.csv("train.csv")
#test <- read.csv("test.csv")

# create partition on test data
index <- createDataPartition(dl$Survived,times = 1,p=0.7,list=FALSE)
train <- dl[index,]
test <- dl[-index,]

# check the dimesions
dim(dl)
dim(train)
dim(test)


str(train)
summary(train$Age)

# fix the train data due to NA issue
avg <- median(train$Age, na.rm = TRUE)
train$Age <- replace(train$Age,is.na(train$Age),avg)
train$Embarked[!(train$Embarked %in% c('C','Q','S'))] <- 'S'

# fix the test data due to NA issue
avg <- median(test$Age, na.rm = TRUE)
test$Age <- replace(test$Age,is.na(test$Age),avg)
avg <- median(test$Fare, na.rm = TRUE)
test$Fare <- replace(test$Fare,is.na(test$Fare),avg)


# convert train to factors
train$Survived <- factor(train$Survived)
train$Pclass <- factor(train$Pclass)

# convert test to factors
test$Survived <- factor(test$Survived)
test$Pclass <- factor(test$Pclass)

# new feature family size
train$FamilySize <- train$Parch + train$SibSp + 1
test$FamilySize <- test$Parch + test$SibSp + 1

# new feature age group
train$AgeGrp[train$Age<=18] <- 'Kids'
train$AgeGrp[train$Age>18 & train$Age<=59] <- 'Adults'
train$AgeGrp[train$Age>59] <- 'Seniors'
train$AgeGrp <- factor(train$AgeGrp)

test$AgeGrp[test$Age<=18] <- 'Kids'
test$AgeGrp[test$Age>18 & test$Age<=59] <- 'Adults'
test$AgeGrp[test$Age>59] <- 'Seniors'
test$AgeGrp <- factor(test$AgeGrp)



##### run a few models #########
# create 10 fold validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

set.seed(7)

fit.rf <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare, data=train, method="rf", metric=metric, trControl=control)
fit.rpart <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare, data=train, method="rpart", metric=metric, trControl=control)
fit.glm <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare, data=train, method="glm", metric=metric, trControl=control)
fit.knn <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare, data=train, method="knn", metric=metric, trControl=control)
fit.xgbTree <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare, data=train, method="xgbTree", metric=metric, trControl=control)
fit.SVM <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare, data=train, method="svmRadial", metric=metric,trControl=control)
fit.Neural <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare, data=train, method="nnet", metric=metric,trControl=control)

results <- resamples(list(DecisionTree=fit.rpart,RandomForest=fit.rf,XgBoost=fit.xgbTree,  KNN=fit.knn, SupportVector=fit.SVM,  NeuralNetwork=fit.Neural))
summary(results)

dotplot(results)
################################



####### run best model (random forest) ########
library(caret)
train$Survived <- factor(train$Survived)


# create 10 fold validation
control <- trainControl(method="repeatedcv", number=15, repeats=2)
metric <- "Accuracy"

# Random Forest
set.seed(7)
fit.rf_final <- train(Survived ~  Sex + Pclass + AgeGrp + FamilySize + Fare, data=train, metric=metric, method="rf", tuneGrid = data.frame(mtry = seq(3, 5, 1)), trControl=control)

# print model
print(fit.rf_final)

################################################
  

reviewing fare




### trying to give examples why all factors not capture in dataset.


train %>% filter(Survived==0 & Survived2==1) %>% head(20)
train %>% filter(Age==28 & Sex=='female' & Fare < 10) %>% select(Survived,Pclass,Name,Sex,Embarked,FamilySize,Survived2) %>% arrange(Embarked)
train %>% filter(Age==28 & Sex=='female' & PassengerId %in% c(101,129)) 


p <- train %>% filter(Fare<50) %>%
  ggplot(aes(x = Fare, fill=Survived)) 

p + geom_density(alpha=0.5) + facet_grid(~Pclass) + labs(title="Age Feature") + theme_economist_white()

varImp(fit.rf_final, scale = FALSE)


train$Survived2 <- predict(fit.xgbTree,train)
train$Survived2 <- factor(train$Survived2)
confusionMatrix(train$Survived2,train$Survived)

test$Survived2 <- predict(fit.xgbTree,test)
test$Survived2 <- factor(test$Survived2)
confusionMatrix(test$Survived2,test$Survived)




