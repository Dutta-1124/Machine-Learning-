#SET WORKING DIRECTORY
getwd()

#READ THE DATA
winedata<- read.csv(file.choose(),header = T)
summary(winedata)
View(winedata)

#SEE THE STRUCTURE
str(winedata)

hist(winedata$quality)
#you see its closely resembling normal distribution
#with center around 6 , that is to say
#most wine quality are average and very few are bad or good


#LETS DIVIDE THE TRAINGING AND TESTING DATA
wine_train<-  winedata[1:3750,]
wine_test<- winedata[3751:4898,]

dev.off()
par(mfrow=c(2,1))
hist(wine_train$quality)
hist(wine_test$quality)
#we see both graphs are almost similar and quality is evenly distributed in both histograms

dev.off()

#TRAINING A MODEL ON THE COLLECTED DATA
library(rpart)
library(rpart.plot)

#we will use quality as response variable
#so the resulting model looks like this
model_rpart<- rpart(quality~.,data = wine_train)
model_rpart


#nodes indicated by * are terminal or leaf nodes which means that they result in 
#a prediction 
#for example node 5 has yval=5.97 i.e the mean that when any wine sample with alcohol<10.85
#and volatile.acidity<0.2275
#would therefore be predicted to have quality of 5.9

#VIZUALISE DECISION TREE
dev.off()
rpart.plot(model_rpart,digits=6,type=2)
rpart.plot(model_rpart,digits=6,type=3,fallen.leaves = T)


#EVALUATING THE MODEL
predict_rpart<- predict(model_rpart,wine_test)
predict_rpart


#we see a problem
summary(predict_rpart)
summary(wine_test$quality)

#the prediction falls on a much narrower range than actual values
#this tells us that the model is not identifying correctly the extreme cases
#inperticular the best and worst wines


#the correlation b/w predictor and actual quality provides a simple way to check the model performance
cor(predict_rpart,wine_test$quality)
plot(predict_rpart,wine_test$quality)

#A correlation of 0.54 is ok to be accepted but it is only a measure
#how strongly the predictions are related to true values
#it is not a measure of how far off the predictons are from actual values
#SO we perform MEAN ABSOLUTE ERROR(MAE)


#we create MAE
MAE<- function(x,y){
  mean(abs(x-y))
}


MAE(wine_test$quality,predict_rpart)
#This implies that on average the difference b/w models prediction and actual
#valus is 0.58
#on a scale of 3 to 9 , it says the model is doing ok.. but


#Pruning the tree to avoid overfitting
#convention is to have a small tree
#and least cross validation error given by printcp() funciton

rpart::printcp(model_rpart)

#select the one having least cross validated
#error and use it to prone the tree
#this function returns the optimum cp value associated with min erros


comparam<-model_rpart$cptable[which.min(model_rpart$cptable[,"xerror"]),"CP"]
comparam


#plot the cp
rpart::plotcp(model_rpart)

#Prune the tree to create an optimal decison tree
ptree<- prune(model_rpart,cp = comparam)
ptree


#use pruned tree to predict
predict_after_prune<- predict(ptree,wine_test)
predict_after_prune

MAE(predict_after_prune,wine_test$quality)

#BOTH MODELS ARE PROVIDING SAME THING, SINCE PRUNE MODEL IS LESS PRONE TO 
#OVERFITTING WE CAN GO WITH IT.
