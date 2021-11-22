bc <- read.csv(file.choose(),sep=",")
View(bc)
str(bc)
summary(bc)
table(bc$diagnosis)
bc$diagnosis <- factor(bc$diagnosis,levels=c("B","M"),
                       labels = c("Benign","Malignant"))
bc$diagnosis

#normalize/scaling/standardization
nor <- function(x){ (x - min(x)) / max(x)-min(x) }
#run normalization except 2nd col
bc_norm <- as.data.frame(lapply(bc[,c(-2)],nor))
summary(bc_norm)
View(bc_norm)

#extracting train and test data
train <- bc_norm[1:469,]
test <- bc_norm[470:569,]

#extract train and test labels
train_label <- bc[1:469,2] #cl in knn
train_label
test_label <- bc[470:569,2]
test_label

library(class)
?knn
#create prediction
pr <- knn(train,test,cl=train_label,k=21)

#confusion matrix
tab <- table(pr,test_label)
tab

accuracy <- function(x){(sum(diag(x))/sum(rowSums(x)))*100}
accuracy(tab)