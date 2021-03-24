#SETTING WORKING DIRECTORY
getwd()

#IMPORTING DATA INTO R
credit<- read.csv(file.choose(),header = T,stringsAsFactors = T)
View(credit)

#we get 17 features which are combination of factors and integer data types.
str(credit)

#lets look at table() for a couple of 
#loan features that seem likely to predict a default
#the checking and savings account balance may prove 
#to be important non numeric predictors

table(credit$checking_balance)
table(credit$savings_balance)

prop.table(table(credit$checking_balance))
prop.table(table(credit$savings_balance))



#for numeric features we cab explore them with
summary(credit$months_loan_duration)
summary(credit$amount)

#we see here the loan amount ranges from 250DM to 18420DM 
#across 4 to 72 months


#the default vector indicate wheather the loan applicant was unable to meet the aggreed 
#payment terms and went into default
table(credit$default)
prop.table(table(credit$default))



#DATA SPLITTING INTO TRAINING AND TESTING SET

set.seed(1234)
index<- sample(1:nrow(credit),size = 0.9*nrow(credit))
Traindata<- credit[index,]
Testdata<- credit[-index,]


table(Traindata$default)
table(Testdata$default)


prop.table(table(Traindata$default))
prop.table(table(Testdata$default))


#if the above train and test split is not equally distributed
#then we can rerun the above lines to create new samples


#TRAIN MODEL
library(C50)
names(Traindata)
credit_model<- C5.0(Traindata[-17],Traindata$default)
credit_model

#now credit_model object contains C50 decision Tree
#we can see some basic data about the model
#The tree size of 59 indicates that the tree is 59 decisions deep
#to see the decision tree we can call the summary() function on the model


summary(credit_model)


#EVALUATING MODEL PERFORMANCE
credit_predict<- predict(credit_model,Testdata)
credit_predict

#this creates a vector of predicted class values,
#which can be compared to actual class values using crosstable() function

library(gmodels)
CrossTable(Testdata$default,credit_predict,prop.r = F,prop.c = F,prop.chisq = F,dnn = c("Actual default","Predicted default"))

#check the accuracy and the error rate and compare it with the performance on training data


#IMPROVE THE MODEL
#if our model rate is too high to deploy
#it in real time credit scoring application
#we need to improve the model performance

#you can BOOST the accuracy of decision trees
#c5.0 is advanced over its predecessors c4.5 because of its adapting boosting
#BOOSTING is done by combining a number of weak performing learners and create a team
#that is much stronger than any of the learners alone.

#C5.0 function makes it easy to add boosting to our C5.0 decisiontree
#we need to add additonal "traials" parameter indicating the numbr of decision trees to 
#use in the boosted team.

#the trial parameter sets an upper limit -the algorithm will stop adding trees if 
#it recognizes that
#additional trial donot seem to be improving accuracy

#we will start with trials=10
#researchers say that this reduces the error rate of test data by 25%

credit_model_boost<- C5.0(Traindata[-17],Traindata$default,trials = 10)
credit_model_boost

summary(credit_model_boost)

#PREDICT THE NEW MODEL
credit_model_boost_pred<- predict(credit_model_boost,Testdata)
credit_model_boost_pred

CrossTable(Testdata$default,credit_model_boost_pred,prop.r = F,prop.c = F,prop.chisq = F,dnn = c("Actual","Predicted"))
