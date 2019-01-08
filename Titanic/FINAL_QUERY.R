
##### run a few models #########
# create 10 fold validation

control <- trainControl(method="repeatedcv", number=15, repeats=2)
metric <- "Accuracy"

set.seed(7)
fit.rf_v1 <- train(Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare, data=train, method="rf")
fit.rf_v2 <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare2, data=train, method="rf")

results <- resamples(list(original=fit.rf_v1,modified=fit.rf_v2))
summary(results)


fit.rf_final <- train(Survived ~  Sex + Pclass + Age + FamilySize + Fare2 , data=train, metric=metric, method="rf", tuneGrid = data.frame(mtry = seq(3, 5, 1)), trControl=control)
fit.rf_final


# Random Forest
set.seed(7)






fit.rpart <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare, data=train, method="rpart", metric=metric, trControl=control)
fit.glm <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare, data=train, method="glm", metric=metric, trControl=control)
fit.knn <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare, data=train, method="knn", metric=metric, trControl=control)
fit.xgbTree <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare, data=train, method="xgbTree", metric=metric, trControl=control)
fit.SVM <- train(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare, data=train, method="svmRadial", metric=metric,trControl=control)

results <- resamples(list(DecisionTree=fit.rpart,RandomForest=fit.rf,XgBoost=fit.xgbTree,  KNN=fit.knn, SupportVector=fit.SVM))
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
fit.rf_final <- train(Survived ~  Sex + Pclass + Age + FamilySize + Fare2 , data=train, metric=metric, method="rf", tuneGrid = data.frame(mtry = seq(3, 5, 1)), trControl=control)

# print model
print(fit.rf_final)

# check against train data
train$Survived2 <- predict(fit.rf_final1,train)
train$Survived2 <- factor(train$Survived2)
confusionMatrix(train$Survived2,train$Survived)

# check against test data
test$Survived2 <- predict(fit.rf_final1,test)
test$Survived2 <- factor(test$Survived2)
confusionMatrix(test$Survived2,test$Survived)

################################################





# playing around with RF and different predictors
##################################
set.seed(7)
fit.rf_final1 <- train(Survived ~  Sex + Pclass + Age + FamilySize + Fare, data=train, metric=metric, method="rf", tuneGrid = data.frame(mtry = seq(3, 5, 1)), trControl=control)
fit.rf_final2 <- train(Survived ~  Sex + Pclass + AgeGrp + FamilySize + Fare2 , data=train, metric=metric, method="rf", tuneGrid = data.frame(mtry = seq(3, 5, 1)), trControl=control)
fit.rf_final3 <- train(Survived ~  Sex + Pclass + Age , data=train, metric=metric, method="rf", tuneGrid = data.frame(mtry = seq(3, 5, 1)), trControl=control)
fit.rf_final4 <- train(Survived ~  Sex + Pclass + Age + Embarked, data=train, metric=metric, method="rf", tuneGrid = data.frame(mtry = seq(3, 5, 1)), trControl=control)

results <- resamples(list(rf1=fit.rf_final1,rf2=fit.rf_final2,rf3=fit.rf_final3,rf4=fit.rf_final4))
summary(results)



##################################


# simple decision tree (accuracy does not really improve with this)
fit <- rpart(Survived ~ Sex + Pclass + AgeGrp + FamilySize + Fare2,
             data=train, control = rpart.control(cp = 0.1, minsplit = 5),
             method="class")


plot(fit, margin=0.1)
text(fit, cex=0.6)


# check against train data
train$Survived2 <- predict(fit,train, type='class')
train$Survived2 <- factor(train$Survived2)
confusionMatrix(train$Survived2,train$Survived)

# check against test data
test$Survived2 <- predict(fit,test, type='class')
test$Survived2 <- factor(test$Survived2)
confusionMatrix(test$Survived2,test$Survived)

################################################


##################




### Ad hoc Queries

## check why specifity is low
library(dplyr)
test %>% filter(Survived==1 & Survived2==0) %>% select(Age,PassengerId,Sex,Pclass,Cabin,AgeGrp,FamilySize,Embarked,Survived,Survived2, Ticket) 


## check ticket, question why this passenger survived even though he is class 3, single and embarked from southampton
download %>% filter(Ticket==65306)

# find someone else in similar situation but not survived. Use this as explanation you can't predict 100%
download %>% filter(Sex=='male' & Pclass==3 & Embarked=='S' & Fare<10 & Survived==0 & SibSp==0 & Parch==0)

## explanation
#to many examples where people died so ticket 65306 survived by sheer of luck



# query embarkation point
summary(train$Embarked)
train %>% filter(!(Embarked %in% c('Q','S')))

