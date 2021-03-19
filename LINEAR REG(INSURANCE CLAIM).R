getwd()

#READ THE DATA
insurancedata<- read.csv(file.choose(),header = T, stringsAsFactors = T)

#EXPLORE THE DATA
View(insurancedata)
#SEE STRUCTURE
str(insurancedata)


#SEE FIRST 10 ROWS OF DATAFRAME
head(insurancedata,10)
#LAST 6 ROWS
tail(insurancedata)
# SUMMARY OF DATAFRAME
summary(insurancedata)



#EXPLORING RELATIONSHIP AMONG FEATURES

cor(insurancedata[c("age","bmi","children","claim")])
# we can see none of the correlations are strong but can see notable associations.
#age, bmi, children seem to have weak but positive correlation.
#there is also moderate positive correlation between age & claim, children & claim, bmi & claim
# the associations imply as age,bmi,children increase the medical claim also incerease.




#VISUAL RELATIONSHIP AMONG FEATURES
#SCATTERPLOT

library(psych)
pairs.panels(insurancedata[c("age","bmi","children","claim")])
#the oval shaped object on each scatterplot represents strength of correlation(correlation ellipse).
#dot at the centre represents point at mean values of x,y.
#the ellipse shape indicates correlation- more strech the ellipse is  more is the correlation
#an almost perfectly oval ellipse means weak correlation





#LINEAR REGRESSION DOESNT STRICTLY REQUIRE NORMALLY DISTRIBUTED DEPENDANT.VARIABLE.
#BUT MODEL OFTEN FITS BETTER WHEN ITS TRUE, SO BETTER CHECK
dev.off()
par(mfrow=c(1,2))
hist(insurancedata$claim)
hist(log(insurancedata$claim))

# histogram for claims shows a right skewed distribution
#majority of people have claims between 0k to 15k
#after applying LOG the distribution is approx. normal.



#CHECK CLAIMS COLUMN FOR NORMAL DISTRIBUTION
#WITH shapiro wilk test

#NULL- hypothesis is data is normally distributed.
#ALT- hypothesis is data is not normally distributed.
shapiro.test(insurancedata$claim)
shapiro.test(log(insurancedata$claim))

# we got p- values < 0.05 in both the test,hence reject the NULL
# the data is not normally distributed




#DISTRIBUTION OF CATEGORICAL DATA
table(insurancedata$region)
prop.table(table(insurancedata$region))


table(insurancedata$smoker)
prop.table(table(insurancedata$smoker))

table(insurancedata$sex)
prop.table(table(insurancedata$sex))



# WE SPLIT THE DATA INTO TRAIN AND SPLIT DATASET
index<- sample(1:nrow(insurancedata),size = 0.7*nrow(insurancedata))

Traindata<- insurancedata[index,]
Testdata<- insurancedata[-index,]


nrow(Traindata)+nrow(Testdata)


#TO AVOID SCIENTIFIC NOTATION
options(scipen = 0)
shapiro.test(insurancedata$claim)


ins_model_1<- lm(claim~age+children+bmi+sex+smoker+region, data = insurancedata)


summary(ins_model_1)

#Since  there is F-stat and associated P-value<0.05, we reject the NULL(ie all coefficients are equal)
#Atleast one independent variable has some coefficinet associated with it.
#P-values associated with variables (age,children,bmi,smoker ) are highly significant.
#Notice that we mentioned 6 variables but linear regression has reported 8 coefficients.
#This is because lm() automatically applied a technique known as DUMMY CODING.

#DUMMYCODING allows a nominal feature to be treated as numeric by creating a binary variable called dummy variable.
#when adding a dummy variable one variable is always left out for reference.
#here R, automatically held out genderfemale,smokerno and regionnortheast variables  as reference variables.
#we say male is going to spend $496.018 lesser than female.

#interpretation of model
#RESIDUALS- it is the difference between a true value and 
#predicted value,
#for most regressions we want our residuals to look like a normal distribution when plotted.
#If our residuals are normaly distributed this indicates
# the mean of the difference between our predicted  and actual values is close to 0


par(mfrow=c(1,1))

#LOOK INTO DIAGNOSTIC PLOT
plot(ins_model_1)
#RESIDUAL VS FITTED

#Q-Q-PLOT
#SCALE LOCATION
#RESIDUALS VS LEVERAGES


summary(ins_model_1)

#CHECK MULTICOLLINEARITY WITH VARIENCE INFLATION FACTOR
library(faraway)
install.packages("faraway")
library(faraway)

vif(ins_model_1)

#independence of observation is the same as auto- correlation being 0
# check  autocorrelation using Durbin watsaon test
# NULL-HYPOTHESIS- no autocorrelation; ALT-HYPOTHESIS- autocorrelation present
#how to rectify: consider removing predictors
install.packages("lmtest")
library(lmtest)

lmtest::dwtest(ins_model_1)


#Alternate check for Breusch- pagan test for Heteroscedasticity
#NULL-HYPOTHESIS-Homoscedasticity(varience of residuals is constant)
#ALT-HYPOTHESIS- Heteroscedasticity
lmtest::bptest(ins_model_1)




###########################################################################################
summary(ins_model_1)

#NOW BUILDING A MODEL WITH DEPENDENT VARIABLE CLAIM & SIGNIFICANT VARIABLES
ins_model_sig<- lm(claim~ age+children+bmi+smoker,data = Traindata)
summary(ins_model_sig)

#CHECK MULTICOLLINEARITY WITH VARIENCE INFLATION FACTOR

vif(ins_model_sig)

#independence of observation is the same as auto- correlation being 0
# check  autocorrelation using Durbin watsaon test
# NULL-HYPOTHESIS- no autocorrelation; ALT-HYPOTHESIS- autocorrelation present
#how to rectify: consider removing predictors

lmtest::dwtest(ins_model_sig)


#Alternate check for Breusch- pagan test for Heteroscedasticity
#NULL-HYPOTHESIS-Homoscedasticity(varience of residuals is constant)
#ALT-HYPOTHESIS- Heteroscedasticity
lmtest::bptest(ins_model_sig)


#predicting response variable from test data
predictions <- predict(ins_model_sig,Testdata)
predictions

#check RMSE
install.packages("Metrics")
library(Metrics)
rmse(actual = Testdata$claim,predicted = predictions)


#################################################################################
#ADDING INTERACTIONS TO MODEL
ins_model_with_interactions<- lm(claim~age+children+ bmi+smoker+bmi*smoker,data = Traindata)
summary(ins_model_with_interactions)

#independence of observation is the same as auto- correlation being 0
# check  autocorrelation using Durbin watsaon test
# NULL-HYPOTHESIS- no autocorrelation; ALT-HYPOTHESIS- autocorrelation present
#how to rectify: consider removing predictors

lmtest::dwtest(ins_model_with_interactions)


#Alternate check for Breusch- pagan test for Heteroscedasticity
#NULL-HYPOTHESIS-Homoscedasticity(varience of residuals is constant)
#ALT-HYPOTHESIS- Heteroscedasticity
lmtest::bptest(ins_model_with_interactions)


#predicting response variable from test data
predictions_interactions <- predict(ins_model_with_interactions,Testdata)
predictions

#check RMSE
rmse(actual = Testdata$claim,predicted = predictions_interactions)



