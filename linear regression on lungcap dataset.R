LUNGCAP = read.csv(file.choose(), sep=',', header = T)
attach(LUNGCAP)
names(LUNGCAP)
summary(LUNGCAP)
class(LUNGCAP$Age)
plot(Age,LungCap,main = "scatterplot")
LINEARMODEL = lm(LungCap~Age)
summary(LINEARMODEL)
attributes(LINEARMODEL)
plot(Age,LungCap,main = "scatterplot")
abline(LINEARMODEL,col=2,lwd=3) #color and line width
checkLUNGS = data.frame(Age=5)
checkLUNGS
result = predict(LINEARMODEL,checkLUNGS)
result
