
print("Use of neural network for Machine Learning")

df <- read.csv("sleepstudy.csv")

index <- sample(1:nrow(df),round(0.9*nrow(df)))

print("standardization of data is done here")
maxs <- apply(df,2,max)
mins <- apply(df,2,min)

scaled <- as.data.frame(scale(df,center = mins,scale = maxs - mins))

train <- scaled[index,]
test <-  scaled[-index,]

library("neuralnet")
myFormula <- Reaction ~ Days + Subject 
print("fitting of the neural network")
nn <- neuralnet(formula = myFormula ,data = train,hidden = c(5,3))
plot(nn)

print("predictive analysis of the test datase")
pr_nn <- compute(nn,test[,2:3])

nn_output <- pr_nn$net.result *(max(df$Reaction)- min(df$Reaction)) + min(df$Reaction) 
test_output <- test$Reaction *(max(df$Reaction)- min(df$Reaction)) + min(df$Reaction)

finalOutput <- cbind(nn_output,test_output)
colnames(finalOutput) <- cbind("predicted","actual")
View(finalOutput)

print("graph to show the variation between actual values and predicted values")
plot(nn_output, type = "b", col = "blue",ylim = c(200,500),ylab ="Reaction time in ms",main = "Actual values (black) vs predicted values (blue)")
lines(test_output,type = "b")

print("mean absolute percentage error")
mape_nn = (mean(abs(test_output - nn_output)/test_output))*100

print(mape_nn)




















