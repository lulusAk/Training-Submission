test <- read.csv("E:/titanic/test.csv")
train <- read.csv("E:/titanic/train.csv")

train$IsTrainSet <- TRUE
test$IsTrainSet <- FALSE


install.packages(tidyverse)
library(tidyverse)

view(test)
ncol(train)
ncol(test)

## Add column
test$Survived <- NA

## Combine 2 data
titanic <-rbind(train , test)


titanic[titanic$Embarked=="","Embarked"] <-'S'
titanic[titanic$Age=="","Age"] 
table(is.na(titanic$Age))

median.age <-median (titanic$Age, na.rm= TRUE)
titanic[is.na(titanic$Age),"Age"]<- median.age

median.fare <-median (titanic$Fare, na.rm= TRUE)
titanic[is.na(titanic$Fare),"Fare"]<- median.fare
table(is.na(titanic$Fare))

##Categorical casting
titanic$Pclass <-as.factor(titanic$Pclass)
titanic$Sex <-as.factor(titanic$Sex)
titanic$Embarked <-as.factor(titanic$Embarked)


## split back out into train and set
train <-titanic[titanic$IsTrainSet==TRUE,]
test <-titanic[titanic$IsTrainSet==FALSE,]

train$Survived<-as.factor(train$Survived)

survived.equation <-"Survived ~ Pclass + Age + Sex + Embarked + SibSp + Parch + Fare"
survived.formula <-as.formula(survived.equation)

install.packages("randomForest")
library(randomForest)

titanic.model <- randomForest( formula = survived.formula, data = train, ntree = 500, mtry = 3, nodesize = 0.01* nrow(test))
features.equation <-"Pclass + Age + Sex + Embarked + SibSp + Parch + Fare"

Survived <- predict (titanic.model, newdata=test)

PassengerId <- test$PassengerId
Output.df <- as.data.frame (PassengerId)
Output.df$Survived <- Survived

tail(Output.df)

write.csv (Output.df, file = "titanic_kaggle_submission.csv", row.names = FALSE)
