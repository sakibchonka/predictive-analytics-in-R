library(MASS)
library(neuralnet)
set.seed(123)
dataset <- Boston
str(dataset)
View(dataset)
help("Boston")
hist(dataset$medv)
dim(dataset)
head(dataset,5)
apply(dataset,2,range)

#scale
maxval <- apply(dataset,2,max)
maxval
minval <- apply(dataset,2,min)
minval

Dataframe <- as.data.frame(scale(dataset, center = minval, 
                                 scale = maxval-minval))
#train and test set
new <- sample(1:nrow(Dataframe),400)
new
trainset <- Dataframe[new,]
testset <- Dataframe[-new,]

#neuralnet
allvar <- colnames(Dataframe)
allvar
predictvar <- allvar[!allvar%in%"medv"]
predictvar
predictvar <- paste(predictvar,collapse = "+")
form <- as.formula(paste("medv~",predictvar))
form
newmodel <- neuralnet(formula = form,
                      hidden = c(4,2),
                      linear.output = T,
                      data = trainset)
plot(newmodel)

#prediction
pred <- compute(newmodel,testset[,1:13])
prediction <- pred$net.result * 
          (max(testset$medv)-min(testset$medv))+min(testset$medv)
actualvalue <- testset$medv *
          (max(testset$medv)-min(testset$medv))+min(testset$medv)
#mean squared error
MSE <- sum(prediction - actualvalue)^2 / nrow(testset)
MSE
plot(testset$medv,prediction,col='blue',main = 'Actual vs Prediction',
     pch = 1, cex=0.9, type='p',xlab = 'Actual', ylab = 'predicted')
#pch means plot character same as type
#cex is size of plotting points
?plot
