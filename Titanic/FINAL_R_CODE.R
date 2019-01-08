library(ggplot2)
library(caret)
library(dplyr)
library(ggthemes)

setwd("/Users/Ronny/Capstone_EDX/Titanic")
download <- read.csv("train.csv")
#test <- read.csv("test.csv")

# create partition on test data
index <- createDataPartition(dl$Survived,times = 1,p=0.7,list=FALSE)
train <- download[index,]
test <- download[-index,]

# check the dimesions
dim(download)
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

train$Fare2[train$Fare<10] <- 'Low Fare'
train$Fare2[train$Fare>=10 & train$Fare<=200] <- 'Normal Fare'
train$Fare2[train$Fare>200 & train$Fare<=300] <- 'Outlier1'
train$Fare2[train$Fare>300] <- 'Outlier2'

test$Fare2[test$Fare<10] <- 'Low Fare'
test$Fare2[test$Fare>=10 & test$Fare<=200] <- 'Normal Fare'
test$Fare2[test$Fare>200 & test$Fare<=300] <- 'Outlier1'
test$Fare2[test$Fare>300] <- 'Outlier2'

# new feature due to regrouping of tickets
combi <- rbind(train,test)
mapTicket <- combi %>% group_by(Ticket) %>% filter(n()>1) %>% summarise(count=n()) 
combi <- combi %>% left_join(mapTicket,by='Ticket') %>% mutate(FFam=count) 
combi <- combi[-15]
combi$FFam[is.na(combi$FFam)] <- 0
#combi$FFam[combi$SibSp!=0 | combi$Parch!=0] <- 0

train <- combi[1:713,]
test <- combi[714:891,]



