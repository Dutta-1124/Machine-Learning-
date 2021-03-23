rm(list = ls())

cx<- read.csv(file.choose(),header = T)
str(cx)
summary(cx)
cx$credit.rating<- as.factor(cx$credit.rating)
str(cx$credit.rating)

cx<- cx[cx$credit.amount<8000,]
boxplot(cx$credit.amount)
set.seed(123)
index<- sample(1:nrow(cx),size= 0.7*nrow(cx)))

index<- sample(1:nrow(cx),size = 0.7*nrow(cx))

Train<- cx[index,]
Test<- cx[-index,]

Test.class<- Test[,1]
Test.features<- Test[,-1]
View(Test.class)

View(Test.features)
head(Test.class)
head(Test.features)


lr.model<- glm(credit.rating~.,data = Train,family = "binomial")

summary(lr.model)


library(fmsb)
NagelkerkeR2(lr.model)


lr.prediction<- predict(lr.model,Test,type = "response")
head(lr.prediction)

Test$predictedprobs<- lr.prediction
Test$predicted_values<- ifelse(Test$predictedprobs>0.5,1,0)

Test$predicted_values<- as.factor(Test$predicted_values)
str(Test$predicted_values)

length(lr.prediction)
misClassicError<- mean(Test$credit.rating != Test$predicted_values)
misClassicError
print(paste("Accuracy is ", 1- misClassicError))


library(ROCR)
Roc_pred<- prediction(lr.prediction,Test.class)

Roc_pref<- ROCR::performance(prediction.obj = Roc_pred,measure = "tpr",x.measure = "fpr")

plot(Roc_pref)
abline(a = 0,b = 1,col="red")

#AUC
auc<- ROCR::performance(prediction.obj = Roc_pred,measure = "auc")
auc<- as.numeric(auc@y.values)


auc
min_auc<-(round(auc,digits = 2))
min_auc
minauct<- paste(c("min(AUC)="),min_auc,sep = "")
minauct
legend(0.55,0.5,c(min_auc,"\n"),cex = 0.9,box.col = "red")


table(Test$credit.rating,Test$predicted_values)
 
prop.table(table(Test$credit.rating,Test$predicted_values))

