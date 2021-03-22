getwd()
#IMPORTING DARTA INTO R
credit<- read.csv(file.choose(),header = T)
head(credit)
credit<- credit[,-1]
dim(credit)
str(credit)
summary(credit)
boxplot(credit$Age)
boxplot(credit$balance)
#balance has an outlier 
#balance also has min value 0.
boxplot(credit$income)
names(credit)

cor(credit[c("balance","income","Age")])


#PLOT VARIABLES TO UNDERSTAND CORRELATION
library(psych)
pairs.panels(credit[c("balance","income","Age")])
#we don't see any strong correlation between any variables.
#There is some positive correlation b/w Age and income.
#balance,income and balance, Age are negatively correlated.



#Removing outliers in balance 
credit<- credit[credit$balance< max(credit$balance),]

boxplot(credit$balance)


#creating dummy variables
library(dummies)
credit<- dummy.data.frame(credit)
credit<- credit[,-1]
credit<- credit[,-2]
head(credit)


#Train Test split
index<- sample(1:nrow(credit),size = 0.7*nrow(credit))
Traindata<- credit[index,]
Testdata<- credit[-index,]
str(Traindata)
str(Traindata)


library(fmsb)
#SIMPLE LOGISTIC REGRESSION OF:
#DEFAULT VS STUDENT
model_1<- glm(defaultYes~studentYes,data = Traindata,family = "binomial")
model_1

#model_1 = -3.26358 +(-0.09216)*studentYes
#log(p/1-p)= intercept+ (slope)*student


#DEFAULT VS BALANCE
model_2<- glm(defaultYes~balance,data = Traindata,family = "binomial")
model_2
#model_2=-11.497923+(0.006251)*balance
#log(p/1-p)=intercept+(slope)*balance

#DEFAULT VS AGE
model_3<- glm(defaultYes~Age,data = Traindata,family = "binomial")
model_3
#model_3= -3.637702 +(0.006051)*Age  
#log(p/1-p)=intercept+(slope)*Age

#DEFAULT VS INCOME
model_4<- glm(defaultYes~income,data = Traindata,family = "binomial")
model_4
#model_4=  -3.643e+00 +(1.027e-05)*income 
#log(p/1-p)=intercept+(slope)*income



#SUMMARY VALUES OF P,AIC,...
summary(model_1)
NagelkerkeR2(model_1)
#p-value of student is greater than 0.5 , i.e it is not significant.
#with change in  unit of student  the log odds of defaults is
#going to  be decreased by  0.09216.
#AIC is 90.192
#nagelkerker R2 is  0.0002347692 which is very less


summary(model_2)
#p-value is less than 0.5,balance feature is highly significant.
#with change in 1 unit of balance the log odds of defaults is gonna increase
# by   0.006251
#AIC is  50.745
NagelkerkeR2(model_2)
#nagelkerker R2 is 0.4962104, the model is good

summary(model_3)
NagelkerkeR2(model_3)
#p-value is 0.74704 ,which is less than 0.5. It is not significant.
#with increase in 1 unit of Age the log odds of defaults is gonna increase
# by   0.006251
#AIC is 90.105
#nagelkerker R2 is 0.001405623, the value of pseudo R2 is very less


summary(model_4)
NagelkerkeR2(model_4)
#p-value is  0.666 ,which is less than 0.5. It is not significant.
#with increase in 1 unit of income the log odds of defaults is gonna increase
# by    1.027e-05
#AIC is 90.023
#nagelkerker R2 is 0.002517118, the value of pseudo R2 is very less


#
#AIC of model_2 is lesser than others,showing model_2 (default vs balance ) has better quality 
#compared to others.



#CONFIDENCE INTERVAL OF THE FIT
#model1
p1<-predict(model_1,interval="confidence")

#model_2
p2<- predict(model_2,interval="confidence")
#model_3
p3<- predict(model_3,interval="confidence")
#model_4
p4<- predict(model_4,interval="confidence")

#..........................................................................
#MULTIPLE LOGISTIC REGRESSION

Multi_lr<- glm(defaultYes~.,data = Traindata,family = "binomial")
Multi_lr

summary(Multi_lr)
#Fishers information shows iterations=10, showing the model has gone 5 iterations.
#Null-deviance is higher than Residual deviance
#Of all the features, balance is highly significant(0.000706).
#The Deviance is also some what close to zero.(showing somewhat good relationship)


prediction_lr<- predict(Multi_lr,Testdata,type = "response")
head(prediction_lr)
range(prediction_lr)

Testdata$predicted_prob<- prediction_lr
Testdata$predicted_value<- ifelse(Testdata$predicted_prob>0.5,1,0)

Testdata$predicted_value<- as.factor(Testdata$predicted_value)
str(Testdata)


MissClassError<- mean(Testdata$defaultYes!= Testdata$predicted_value)

MissClassError

Accuracy <- print(paste("Accuracy is", 1- MissClassError))


#ROCR
A<- ROCR::prediction(prediction_lr,Testdata$defaultYes)
A
B<- ROCR::performance(A,"tpr","fpr")
B
plot(B,col="red",lwd=2, lty=3, xlab="1-specificity",ylab="sensitivity")



#AUC
auc<- ROCR::performance(A,measure = "auc")
auc
auc<- as.numeric(auc@y.values)
auc
minauc<- round(auc,2)
minauct<- print(paste("AUC is", minauc))

legend(x = 0.7,y = 0.4,legend = minauct,cex=0.9)

#.........................................................................

#NEW DATAFRAME
student=c("No","Yes","Yes","No","No","No")
balance=c(1500,1000,2000,2500,1600,1900)
Age<- c(34,82,71,36,68,77)
income<- c(10000,18000,21000,37000,40000,24000)

newdf<- data.frame(student,balance,Age,income)
newdf

newdf1<- dummy.data.frame(newdf)
newdf1<-newdf1[,-1]
str(newdf1)
#PREDICTING OUTCOME OF NEW DATA FRAME
prediction_newdf1<- predict(Multi_lr,newdata = newdf1)
prediction_newdf1

 prediction_newdf1
newdf1$predicted_probs=prediction_newdf1
newdf1$predicted_value<- ifelse(newdf1$predicted_probs>0.5,1,0)

newdf1$predicted_value<- as.factor(newdf1$predicted_value)

MissClassError_newdf1<- mean(newdf1$studentYes!=newdf1$predicted_value)
MissClassError_newdf1

print(paste("accuracy is", 1- MissClassError_newdf1))





#ROCR for newdata
A_new<- ROCR::prediction(prediction_newdf1,newdf1$studentYes)
B_new<-ROCR::performance(A_new,"tpr","fpr")
plot(B_new)
#AUC
auc_new<- ROCR::performance(A_new,measure = "auc")
auc_new<- as.numeric(auc_new@y.values)
auc_new_t<- print(paste("AUC is", auc_new))
legend(0.8,0.2,legend = auc_new_t,cex=0.5)

