library("neuralnet")
#Going to create a neural network to perform log for input 

#Generate 20 random numbers uniformly distributed between 0 and 100
#and store them as dataframe
traininginput <- as.data.frame(runif(20,min=0,max=100))
traininginput
trainingoutput <- log(traininginput)
trainingoutput

#column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")
View(trainingdata)

#train the neural network
#Going to have 10 neurons in hidden layers
#threshold is a numeric value specifying the threshold for partial 
#derivatives of the error function as stopping criteria
net.log <- neuralnet(Output~Input,
                      trainingdata,
                      hidden = 10,threshold = 0.01)
print(net.log)

#plot the neural network
plot(net.log)

#test neural network on some training data
testdata <- as.data.frame((1:10))#generate some log number
net.results <- compute(net.log,testdata)#Run through neural network
#properties of net.log
ls(net.results)
print(net.results$net.result)

#Lets display better version of results
cleanoutput <- cbind(testdata,log(testdata),as.data.frame
                     (net.results$net.result))
colnames(cleanoutput) <- c("input","Expected o/p","NeuralNet O/p")
print(cleanoutput)