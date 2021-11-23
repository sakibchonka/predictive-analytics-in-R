#Read the Data
data = read.csv(file.choose(), sep = ",")
str(data)
#random sampling
samplesize = 0.60 * nrow(data)
samplesize
set.seed(80)
index = sample(seq_len(nrow(data)), size = samplesize)
index

#create training and test set
datatrain <- data[index,]
datatest <- data[-index,]

#scale data for neural network
max <- apply(data[,4:16],2,max) #1 for row and 2 for column c(1,2) for both
min <- apply(data[,4:16],2,min)
max
min

scaled <- as.data.frame(scale(data[,4:16],center = min , scale = max - min))
?scale

#install library
install.packages("neuralnet")
library(neuralnet)

#creating training and test set
trainNN <- scaled[index,]
testNN <- scaled[-index,]
#fit neural network
set.seed(2)
allvar <- colnames(datatrain)
allvar
predictvar <- allvar[c(4:8)]
predictvar
predictvar <- paste(predictvar,collapse = "+")
form <- as.formula(paste("rating~",predictvar))
form
NN <- neuralnet(formula = form,
                hidden = 3,
                linear.output = T,
                data = datatrain)
#plot neural network
plot(NN)

