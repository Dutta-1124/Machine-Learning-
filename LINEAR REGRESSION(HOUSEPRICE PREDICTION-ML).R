

rm(list=ls())
df<- read.csv(file.choose(),header = T)
str(df)


mean(df$price)
summary(df)
hist(df$crime_rate)
pairs(df)
barplot(table(df$bus_ter))
barplot(table(df$airport))
pairs(~price+crime_rate+n_hot_rooms+rainfall, data = df)


uv<- 3*quantile(df$n_hot_rooms, 0.99)
uv


sum(df$n_hot_rooms>uv)
df[df$n_hot_rooms>uv,]<- uv
sum(df$n_hot_rooms>uv)
summary(df$n_hot_rooms)



plot(df$price~df$rainfall)

lv<- 0.3*quantile(df$rainfall,0.01)
lv


df$rainfall[df$rainfall<lv]<- lv
summary(df$rainfall)




sum(is.na(df))
which(is.na(df))
mean(df$n_hos_beds, na.rm = T)
which(df$n_hos_beds)
whih(is.na(df$n_hos_beds))
which(is.na(df$n_hos_beds))



df$n_hos_beds[is.na(df$n_hos_beds)]<- mean(df$n_hos_beds)
is.na(df$n_hos_beds)
which(is.na(df$n_hos_beds))



df$n_hos_beds[is.na(df$n_hos_beds)]<- mean(df$n_hos_beds,na.rm = T)



pairs(~price+crime_rate, data = df)

df$crime_rate<- log(1+df$crime_rate)
plot(df$price,df$crime_rate)
df$dist<- (df$dist1+df$dist2+df$dist3+df$dist4)/4
df[,-c("dist1","dist2","dist3","dist4")]<- df


df1<- df[,-7:-10]
df1
df<- df1


rm(df1)
df
names(df)


df<- df[,-14]
names(df)


install.packages("dummies")


df<- dummy.data.frame(df)



?dummies
install.packages("dummies")

library(dummies)
df<- dummy.data.frame(df)

df<- df[,-9]
df<- df[,-14]
names(df)

cor(df)
round(cor(df),digits = 2)


df<- df[,-16]
names(df)

############################################################################################################
#SIMPLE LINEAR REGRESSION

dev.off()
par(mfrow=c(1,1))
simple_model<- lm(price~room_num,data = df)


summary(simple_model)

plot(df$room_num,df$price,xlim = c(2,10),ylim = c(0,60))
abline(simple_model)

multiple_model<- lm(price~.,data = df)
summary(multiple_model)
