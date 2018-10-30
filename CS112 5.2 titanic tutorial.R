#Part 1

setwd("~/Documents/GitHub/r-minerva/titanic_data")
train <- read.csv("~/Documents/GitHub/r-minerva/titanic_data/train.csv")
test <- read.csv("~/Documents/GitHub/r-minerva/titanic_data/test.csv")

table(train$Survived)
prop.table(table(train$Survived))

test$Survived <- rep(0,418)

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

#Part 2

summary(train$Sex)
prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived),1)

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
submit2 <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit2, file = "onlymenperish.csv", row.names = FALSE)

summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data = train, FUN = sum)
aggregate(Survived ~ Child + Sex, data = train, FUN = function(x) {sum(x)/length(x)})

train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

submit3 <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit3, file = "thirdsubmission.csv", row.names = FALSE)

library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
plot(fit)
text(fit)

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit4 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit4, file = "myfirstdtree.csv", row.names = FALSE)

test$Survived <- NA
combi <- rbind(train, test)
