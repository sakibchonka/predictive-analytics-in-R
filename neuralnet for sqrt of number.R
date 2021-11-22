library("neuralnet")
#Going to create a neural network to perform square root for input 

#Generate 50 random numbers uniformly distributed between 0 and 100
#and store them as dataframe
traininginput <- as.data.frame(runif(50,min=0,max=100))
traininginput
trainingoutput <- sqrt(traininginput)
trainingoutput

#column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")
View(trainingdata)

#train the neural network
#Going to have 10 neurons in hidden layers
#threshold is a numeric value specifying the threshold for partial 
#derivatives of the error function as stopping criteria
net.sqrt <- neuralnet(Output~Input,
                      trainingdata,
                      hidden = 10,threshold = 0.01)
print(net.sqrt)

#plot the neural network
plot(net.sqrt)

#test neural network on some training data
testdata <- as.data.frame((1:10)^2)#generate some squared number
net.results <- compute(net.sqrt,testdata)#Run through neural network
#properties of net.sqrt
ls(net.results)
print(net.results$net.result)

#Lets display better version of results
cleanoutput <- cbind(testdata,sqrt(testdata),as.data.frame
                     (net.results$net.result))
colnames(cleanoutput) <- c("input","Expected o/p","NeuralNet O/p")
print(cleanoutput)