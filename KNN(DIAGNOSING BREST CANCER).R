#SETTING WORKIND DIRECTORY
getwd()

#READ THE DATA
cancerdata<- read.csv(file.choose(),stringsAsFactors = T)

#EXPLORING THE DATA
str(cancerdata)

#LETS DROP ID FROM DATASET
cancerdata<- cancerdata[,-1]


table(cancerdata$diagnosis)
prop.table(table(cancerdata$diagnosis))


#CHANGING THE DATATYPE OF DIAGNOSIS TO FACTOR
cancerdata$diagnosis<- factor(cancerdata$diagnosis,levels = c("B","M"),labels = c("Benign","Malignant"))



round(prop.table(table(cancerdata$diagnosis))*100,digits = 1)



summary(cancerdata[c("radius_mean","area_mean","smoothness_mean")])
#all the features are on different scales


#NORMALIZATION 
normaliztion<- function(x){
  z=((x-min(x))/(min(x)-max(x)))
  return(z)
}



#USING LAPPLY() FUNCTION

listdata<- lapply(cancerdata[2:31],normaliztion)
cancerdata_norm<- as.data.frame(listdata)


summary(cancerdata_norm$area_mean)
summary(cancerdata$area_mean)



#DATA PARTITION
cancerdata_train<- cancerdata_norm[1:469,]
cancerdata_test<- cancerdata_norm[470:569,]



#CREATING LABELS FOR TRAINGING AND TEST DATA
cancerdata_train_label<- cancerdata[1:469,1]
cancerdata_test_label<- cancerdata[470:569,1]


#TRAINING THE MODEL
library(class)
cancerdata_test_pred<- knn(train =cancerdata_train ,test =cancerdata_test ,cl =cancerdata_train_label ,k = 5)


cancerdata_test_pred


#EVALUATING THE MODEL
library(gmodels)
CrossTable(x = cancerdata_test_label,y = cancerdata_test_pred,prop.chisq = F,prop.r = F,prop.c = F)





#IMPROVING THE MODEL

#TO CREATE A Z-SCORE STANDARDIZATION OF DATA

cancerdata_zscore<- as.data.frame(scale(cancerdata[-1]))
summary(cancerdata_zscore$area_mean)

cancerdata_train<- cancerdata_zscore[1:469,]
cancerdata_test<- cancerdata_zscore[470:569,]


cancerdata_train_label<- cancerdata[1:469,1]
cancerdata_test_label<- cancerdata[470:569,1]


#PREDICT USING ZSCORED DATA
cancerdata_test_pred<- knn(train = cancerdata_train,test = cancerdata_test,cl = cancerdata_train_label,k = 23)


CrossTable(cancerdata_test_label,cancerdata_test_pred,prop.r = F,prop.chisq = F,prop.c = F)

#unfortunately we couldnt do better, 1st model was good

#TESTING ALT. VALUES OF K
cancerdata_train<- cancerdata_norm[1:469,]
cancerdata_test<-cancerdata_norm[470:569,]


cancerdata_test_pred<- knn(train = cancerdata_train,test = cancerdata_test,cl = cancerdata_train_label,k = 1)
CrossTable(cancerdata_test_label,cancerdata_test_pred,prop.r = F,prop.chisq = F,prop.c = F)

#accuracy got reduced


cancerdata_test_pred<- knn(train = cancerdata_train,test = cancerdata_test,cl = cancerdata_train_label,k = 5)
CrossTable(cancerdata_test_label,cancerdata_test_pred,prop.r = F,prop.chisq = F,prop.c = F)

#we have 98% accuracy

cancerdata_test_pred<- knn(train = cancerdata_train,test = cancerdata_test,cl = cancerdata_train_label,k = 11)
CrossTable(cancerdata_test_label,cancerdata_test_pred,prop.r = F,prop.chisq = F,prop.c = F)



##USING CROSS VALIDATION
cancerdata_train$diagnosis<- cancerdata_train_label
cancerdata_test$diagnosis<- cancerdata_test_label


library(caret)
trcl<- trainControl(method = "cv",number = 3)
set.seed(123)



knn_fit<- train(diagnosis~.,data= cancerdata_train,method="knn",trControl=trcl,preProc=c("center","scale"))

knn_fit


knn_predict<- predict(knn_fit,newdata = cancerdata_test)
CrossTable(cancerdata_test_label,knn_predict,prop.r = F,prop.chisq = F,prop.t = F)

