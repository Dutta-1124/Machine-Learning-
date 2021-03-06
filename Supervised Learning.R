#SETTING WORKING DIRECTORY
setwd("C:/Users/shiva/Desktop/R working directory/MODULE -3")

#ACCESS THE MTCARS DATASET
cars<- mtcars

dim(cars)
str(cars)

#CHECKING RELATIONSHIP  AMONG VARIABLES
cor(cars[c("mpg","hp","wt")])

#CORRELATION BETWEEN mpg & hp
cor(cars$mpg,cars$hp)

#VISUALIZE RELATIONSHIP AMONG FEATURES
plot(cars$mpg,cars$hp,xlab = "CAR MPG",ylab = "CAR HP", main = "SCATTERPLOT OF MPG VS HP")
#mpg and hp display negative correlation




#FOR LINEAR REGRESSION THE DEPENDENT VARIABLE HAS TO BE NORMALLY DISTRIBUTED.
#LETS CHECK USING HISTOGRAM AND SHAPIRO TEST.

hist(cars$mpg, xlab = "MPG")
hist(log(cars$mpg),xlab = "MPG")

#shapiro test
#null-hypothesis: Normally distributed
#alt-hypothesi: not normally distributed
shapiro.test(cars$mpg)
#hence p-value is greater than 0.05, we should reject the null,
#which denotes the distribution is Normal.



#SPLOTTING THE DATA INTO TRAINING AND TESTING DATA
index<- sample(1:nrow(cars),size = 0.7*nrow(cars))
Traindata<- cars[index,]
Testdata<- cars[-index,]


sum(nrow(Traindata),nrow(Testdata))


#SIMPLE LINEAR REGRESSION
simple_model<- lm(mpg~hp, data = Traindata)

simple_model


#DURBIN WATSON TEST
library(lmtest)
dwtest(simple_model)
#There is no autocorrelation

#BRUSCH PAGEN TEST
bptest(simple_model)
#There is no HOMOSCEDASTICITY


#IDENTIFYING Y-INTERCEPT & SLOPE ESTIMATE
mpg=  29.01125 + (-0.05852)*hp
#[mpg= (y-intercept)+ (slope)*hp]

summary(simple_model)
#F-stat has p-value<0.05, it means at least one variable has some coefficient associated with it.
#hp has a significance level less than 0.05,which means it is significant but negative.
#estimates show with one unit increase in hp, mpg decreases by  0.06291 .
# here we have R2 as 0.6114 and it may increase if more independent variables are added.
# 95% of the time true value lies in + or - {2*standard dev} 2( 0.01121)from mean


prediction_simple<- predict(simple_model, Testdata)

#FINDING CONFIDENCE INTERVAL OF THE FIT
predict(simple_model,interval = "confidence")

#USING THE FOLLOWING UNKOWN DATASET, PREDICT THE MILEGE GIVEN THE HORSEPOWER BASED ON LINEAR REGRESSION

df1<- data.frame(hp=c(126,167,189,211,272,312))

prediction_df<- predict(simple_model,newdata = df1)
prediction_df

#----------------------------------------------------------------------------------------
#PLOT mpg VS hp VS wt of the cars

library(psych)
pairs.panels(cars[c("mpg","hp","wt")])
#there is negative correlation between mpg,hp & pg,wt.
#and there is positive relation betwenn hp,wt
#since the ellipse are more streached out , it infers correlation is more.


#FIT A MULTILINEAR REGRESSION MODEL 
multi_model<- lm(mpg~hp+wt,data = Traindata)

multi_model


#DURBIN WATSON TEST
dwtest(multi_model)
#There is no autocorrelation

#BRUSCH PAGEN TEST
bptest(multi_model)
#There is no HOMOSCEDASTICITY

#MULTI-COLLINEARITY
library(faraway)
vif(multi_model)
#SINCE VIF IS LESS THAN 8 , THERE IS NO MULTI-COLLINEARITY

#IDENTIFYING Y-INTERCEPT & SLOPE ESTIMATE
#mpg=  36.88527+ (-0.02998)*hp + (-3.80829)wt
#[mpg= (y-intercept)+ (slope1)*hp + (slope2)wt]


summary(multi_model)
#F-stat has p-value<0.05, it means at least one variable has some coefficient associated with it.
#hp and wt has p-values<0.05, both of them are significant, but wt is more significant  than the other as it has more (***).
#estimates show with one unit increase in hp, mpg decreases by  0.029982 .
#and also with 1 unit increase in wt , mpg decreases by 3.808288
# here we have R2 as 0.8396 and it is a good sign.




prediction_multi<- predict(multi_model, Testdata)
prediction_multi
#FIND CONFIDENCE INFIDENCE OF THE FIT
predict(multi_model,interval = "confidence")

#--------------------------------------------------------------------------------

#COMPARE WITH MULTILINEAR MODEL WITH INTERACTIONS
multi_model_interactions<- lm(mpg~hp+wt+hp*wt, data = Traindata)

multi_model_interactions
#DURBIN WATSON TEST
dwtest(multi_model_interactions)
#There is no autocorrelation

#BRUSCH PAGEN TEST
bptest(multi_model_interactions)
#There is no HOMOSCEDASTICITY


#WHAT DO YOU INFER?
summary(multi_model_interactions)

#here R2 value has gone up . adj R2 was even high (it is a good sign)
#F-stat has p-value<0.05, which means at least one variable has coefficient associated with it.
#Residuals show that 50% of the data lies between Q1 and Q3, and it looks normally distributed.
#since we have added interactions the model is able to perform better.
#Adding interaction terms to a regression model can greatly expand understanding of the relationships
#among the variables in the model .

prediction_multi_interactions<- predict(multi_model_interactions,Testdata)
prediction_multi_interactions


#FIND CONFIDENCE INTERVAL OF THE FIT
predict(multi_model_interactions,interval = "confidence")



#WHICH MODEL PERFORMS BETTER?

#Conditions for a model to perform better
#R2 value should be maximum and
#RMSE should be less
#we check the Root Mean Square Error(RMSE),it shows how close are  the actual data points to the predicted data.

library(Metrics)
rmse(actual = Testdata$mpg,predicted = prediction_simple)
rmse(actual = Testdata$mpg,predicted = prediction_multi)
rmse(actual = Testdata$mpg,predicted = prediction_multi_interactions)


#Here we found RMSE for our 3 models.
#1st model has RMSE- 3.699(It has Multiple R2 value-0.611)
#2nd model has RMSE- 2.526(It has Multiple R2 value-0.839)
#3rd model has RMSE- 2.107(It has Multiple R2 value-0.894)

# SO, FROM THE ABOVE 3 MODELS - THE MODEL WITH INTERACTIONS IS BETTER COMPARED TO THE REST,
#BECAUSE IT HAS LEAST RMSE & MORE MULTIPLE R2 COMPARED TO THE REST.




#PREDICT MPG BASED ON NEW DATASET
df_new<-data.frame(hp=c(126,167,189,211,272,312), wt=c(1.5,2.2,2.9,3.2,3.8,4.2))

df_new
#MULTILINEAR REGRESSION
prediction_df_new<- predict(multi_model,newdata = df_new)
prediction_df_new

#MULITILINEAR REGRESSION (WITH INTERACTIONS )
prediction_df_new_interactions<- predict(multi_model_interactions,newdata = df_new)
prediction_df_new

