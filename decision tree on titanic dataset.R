library(dplyr)
library(rpart)
library(rpart.plot)
library(pROC)

set.seed(679)
titanic <- read.csv(file.choose(),header=T,stringsAsFactors = T)
head(titanic)
summary(titanic)
class(titanic)
str(titanic)
is.na(titanic)

Randomize <- sample(1:nrow(titanic))
titanic <- titanic[Randomize,]
head(titanic)

clean_titanic <- titanic %>% 
  select(-c(Cabin,Name,Ticket)) %>%
  #convert to factor level
  mutate(Pclass = factor(Pclass,levels = c(1,2,3),
                         labels = c('Upper','Middle','Lower')),
         Survived = factor(Survived,levels = c(0,1),
                         labels = c('No','Yes'))) %>%
  na.omit()

str(clean_titanic)
ind = sample(2,nrow(clean_titanic),
             replace = T, prob = c(0.80,0.20))

titanic_train <- clean_titanic[ind==1,]
titanic_test <- clean_titanic[ind==2,]
dim(titanic_train)
dim(titanic_test)
NROW(titanic_train$Survived)

titanic_decision_tree_model <- rpart(Survived~.,data=titanic_train,method = 'class')
rpart.plot(titanic_decision_tree_model,extra=106)
titanic_predict <- predict(titanic_decision_tree_model,titanic_test,type='class')
titanic_predict

titanic_predict_table <- table(titanic_test$Survived,titanic_predict)
titanic_predict_table

titanic_performance <- (sum(diag(titanic_predict_table))/sum(titanic_predict_table))*100
titanic_performance
