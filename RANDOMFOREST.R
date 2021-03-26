rm(list = ls())
#SETTING WORKING DIRECTORY
getwd()

#IMPORTING DATA INTO R
credit.df<- read.csv(file.choose(),header = T)

library(randomForest)
library(caret)
library(e1071)
library(ROCR)

#DATA TYPE TRANSFORMATIONS- FACTORS
convert.to.factors<- function(df,v){
  for (variable in v) {
    df[[variable]]<- as.factor(df[[variable]])
  }
  return(df)
}

categorical.vars<- credit.df<- read.csv(file.choose(),header = T)

library(randomForest)
library(caret)
library(e1071)
library(ROCR)

str(credit.df)

convert.to.factors<- function(df,v){
  for (variable in v) {
    df[[variable]]<- as.factor(df[[variable]])
  }
  return(df)
}

names(credit.df)


categorical.vars<- c('credit.rating','account.balance','previous.credit.payment.status',
                     'credit.purpose','savings','employment.duration','installment.rate',
                     'other.credits','apartment.type','bank.credits','occupation',
                     'marital.status','guarantor','residence.duration','current.assets',
                     'dependents','telephone','foreign.worker')

                  

credit.df<- convert.to.factors(credit.df,categorical.vars)


#STANDARTIZATION- SCALING
scale.features<- function(df,variables){
  for (variable in variables) {
    df[[variable]]<- scale(df[[variable]],center = T,scale = T)
  }
  return(df)
}


numeri.vars<- c("credit.duration.months","age","credit.amount")

credit.df<- scale.features(credit.df,numeri.vars)


#SPLITTING DATA INTO TRAINING AND TEST DATASETS(60:40 RATIO)
indexes<- sample(1:nrow(credit.df),size = 0.6*nrow(credit.df))
Traindata<- credit.df[indexes,]
Testdata<- credit.df[-indexes,]

#SEPARATE FEATURES AND CLASS VARIABLES
test.features.vars<- Testdata[,-1]
test.class.vars<- Testdata[,1]

prop.table(table(Traindata$credit.rating))
prop.table(table(Testdata$credit.rating))


#BUILD INITIAL MODEL WITH TRAINING DATA
rf.model<- randomForest(credit.rating~.,data = Traindata,importance=T,
                        mtry=5,nodesize=5,ntrees=1000)
#VIEW MODEL DETAILS
print(rf.model)
#giving us info about OOBE

randomForest::varImpPlot(rf.model,sort = T,n.var = 20,type = 1)

#PREDICT AND EVALUATE RESULTS
rf.predictions<- predict(rf.model,test.features.vars,type = "class")
caret::confusionMatrix(data = rf.predictions,reference= test.class.vars,positive="1") 

#BUILD A MODEL WITH SELECTED FEATURES
formula.new<- "credit.rating~ account.balance+ savings + previous.credit.payment.status+credit.amount + credit.duration.months + previous.credit.payment.status"

formula.new<- as.formula(formula.new)

rf.model.new<- randomForest(formula.new,data = Traindata,importance=T)

rf.model.new

varImpPlot(rf.model.new,sort = T,type = 1)


#PREDICT AND EVALUATE RESULTS
rf.predictions.new<- predict(rf.model.new,test.features.vars,type = "class")

caret::confusionMatrix(data = rf.predictions.new,reference=test.class.vars,positive="1")




#HYPERPARAMETER OPTIMIXATIONS
#RUN GRID SEARCH
nodesize.vals<-c(5,7)
ntree.vals<-c(500,1000)
formula.new<- "credit.rating~ account.balance+ savings + previous.credit.payment.status+credit.amount + credit.duration.months + previous.credit.payment.status"
formula.new<- as.formula(formula.new)
tuning.results<- tune.randomForest(formula.new,data = Traindata, mtry = 3,nodesize = nodesize.vals,ntree = ntree.vals)

print(tuning.results)


rf.model.best<- tuning.results$best.model
rf.model.best
rf.predictions.best<- predict(rf.model.best,test.features.vars,type = "class")

confusionMatrix(data = rf.predictions.best,reference = test.class.vars)
