data <- read.csv(file.choose(),header = T)
str(data)
View(data)
library(naivebayes)
library(ggplot2)
library(psych)
library(dplyr)

#convert into factors
data$diagnosis <- as.factor(data$diagnosis)

str(data)

#data partition
set.seed(2146)
div <- sample(2,nrow(data),replace=T,prob=c(0.8,0.2))
div
train <- data[div==1,]
test <- data[div==2,]
str(train)
View(train)
nrow(train)
#delete id col from train and test
train = train[-1]
test = test[-1]

model <- naive_bayes(diagnosis~.,data=train,usekernel = T)

train%>%filter(admit=="1")%>%summarise(mean(gre),sd(gre))
plot(model)

#prediction
pred <- predict(model,train,type='prob')
?naive_bayes

head(cbind(pred,train),200)

#confusion matrix
p1 <- predict(model,train)
tab1 <- table(p1,train$diagnosis)
sum(diag(tab1))/sum(tab1)

p2 <- predict(model,test)
tab2 <- table(p2,test$diagnosis)
sum(diag(tab2))/sum(tab2)
