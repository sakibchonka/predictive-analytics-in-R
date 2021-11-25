data <- read.csv(file.choose(),header = T)
str(data)
View(data)
library(naivebayes)
library(ggplot2)
library(psych)
library(dplyr)
#Create a contingency table
xtabs(~admit+rank,data=data)
?xtabs
#convert into factors
data$rank <- as.factor(data$rank)
data$admit <- as.factor(data$admit)
str(data)

#exploratory analysis
pairs.panels(data[-1])
data%>%ggplot(aes(x=admit,y=gre,fill=admit))+geom_boxplot()+
        ggtitle("Box Plot")
data%>%ggplot(aes(x=admit,y=gpa,fill=admit))+geom_boxplot()+
        ggtitle("Box Plot")

#data partition
set.seed(2145)
div <- sample(2,nrow(data),replace=T,prob=c(0.8,0.2))
div
train <- data[div==1,]
test <- data[div==2,]
str(train)
View(train)

model <- naive_bayes(admit~.,data=train,usekernel = T)

train%>%filter(admit=="1")%>%summarise(mean(gre),sd(gre))
plot(model)

#prediction
pred <- predict(model,train,type='prob')
?naive_bayes

head(cbind(pred,train),200)

#confusion matrix
p1 <- predict(model,train)
tab1 <- table(p1,train$admit)
sum(diag(tab1))/sum(tab1)

p2 <- predict(model,test)
tab2 <- table(p2,test$admit)
sum(diag(tab2))/sum(tab2)
