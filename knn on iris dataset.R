df <- data(iris)
head(iris)
ran <- sample(1:nrow(iris),0.9 * nrow(iris))
str(iris)
range(iris$Sepal.Length)
range(iris$Petal.Width)
#normalize/scaling/standardization
nor <- function(x){(x - min(x)) / (max(x) - min(x))}
View(iris)
#run normalization on first 4 cols as they are predictors
iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], nor))
summary(iris)
summary(iris_norm)

#extracting training set
iris_train <- iris_norm[ran,]
#extracting test set
iris_test <- iris_norm[-ran,]
View(iris_train)

#extracting 5th col as it will be used as 'cl' argument in knn
iris_target_category <- iris[ran,5]
#extracting 5th col to measure accuracy
iris_test_category <- iris[-ran,5]

#knn is in class library
library(class)
?knn
#create prediction
pr <- knn(iris_train,iris_test,cl=iris_target_category,k=2)

#create confusion matrix
tab <- table(pr,iris_test_category)
tab
accuracy <- function(x){(sum(diag(x)/sum(rowSums(x))))*100}
accuracy(tab)