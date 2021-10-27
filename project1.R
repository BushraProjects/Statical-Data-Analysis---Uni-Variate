rm(list=ls())
setwd("D:/Software engineering/Stat Data Analysis Fall 2019/Project")
library(e1071)
library(DescTools)
library(xtable)
library(frequency)
library(Hmisc)
library(plyr)
library(ggplot2)
#Read the Bushra Naeemi Table
x = read.csv("Bushra Naeemi.csv")
dim(x)
#Numerical Variables for the first section
farm_work = x$Work.on.farm.FAMILY
farm_off = x$Work.off.farm.Family
farm_notemployed = x$X.Not..employed...
farm_female = x$Female..work.on.farm
farm_income = x$Crop.Net.Income
farm_t4 = x$t4
farm_r4 = x$r4


f = function(x){
  q= mean(x)
  w= sqrt(var(x))
  e= min(x)
  r= max(x)
  t= skewness(x)
  y = kurtosis(x)
  b = c(q, w, e, r, t, y)
  return(b)
}

f(farm_r4)


#Crop Net Income with outlier



png(filename= "income.png",550,250)
par(mfrow=c(1,2))
boxplot(farm_income , horizontal = TRUE , col= "pink", main='Boxplot for Crop Net Income')
plot(density(farm_income ), col = "burlywood2", main='Density plot for Crop Net Income')
min(farm_income)
max(farm_income)
dev.off()
dev.off()

#temperature with outlier
png(filename= "temperature.png",550,250)
par(mfrow=c(1,2))
boxplot(farm_t4 , horizontal = TRUE , col= "pink", main='Boxplot for Temperature') 
plot(density(farm_t4 ), col = "burlywood2", main='Density plot for Temperature')
dev.off()
dev.off()
#rainfall with outlier
png(filename= "rainfall.png",550,250)
par(mfrow=c(1,2))
boxplot(farm_r4 , horizontal = TRUE , col= "pink", main='Boxplot for Rainfall',) 
plot(density(farm_r4 ), col = "burlywood2", main='Density plot for Rainfall')
dev.off()
dev.off()
 #removing the outlier:
UB = quantile(farm_income, 0.75) + 0.8*(quantile(farm_income , 0.75)- quantile(farm_income, 0.25))
LB = quantile(farm_income, 0.75) - 0.8*(quantile(farm_income , 0.75)- quantile(farm_income, 0.25))
fincome =farm_income[farm_income<UB & farm_income>LB]
png(filename= "income2.png",550,250 )
boxplot(fincome, horizontal = T)
dev.off()
length(fincome)/length(farm_income)

#variables(Results and Discussions)

png(filename= "variables.png",600,400)
par(mfrow=c(3,3))
hist(fincome , horizontal = TRUE , main="Crop net income") 
hist(farm_t4 , horizontal = TRUE , main="Temperature")
hist(farm_r4 , horizontal = TRUE , main="Rainfall")
boxplot(fincome , horizontal = TRUE , main="Crop net income")
boxplot(farm_t4 , horizontal = TRUE , main="Temperature")
boxplot(farm_r4 , horizontal = TRUE , main="Rainfall")
plot(density(fincome) ,  horizontal = TRUE , main="Crop net income") 
plot(density(farm_t4) , horizontal = TRUE , main="Temperature")
plot(density(farm_r4), horizontal = TRUE , main="Rainfall") 
dev.off()
dev.off()
#Summary Statistics of Categorical Variables
i = x$Importance.of.farming.0.not.atll.Impornt.1.importnt.2.Very.impornt
ri = count(i)
percent = round(ri$freq/sum(ri$freq)*100)

##################################
farm_importance = x$Importance.of.farming.0.not.atll.Impornt.1.importnt.2.Very.impornt
fi = count(farm_importance)
percent = round(fi$freq/sum(fi$freq)*100)
tablep=cbind(fi, percent)
tpp = rename(tablep,c("x" = "categories",
                      "freq"="frequency",
                      "percent"="percent"))



png(filename = "farmimportance.png")
par(mfrow=c(1,1))
pie(tpp$frequency,
    labels = c(tpp$percent),
    col = rainbow(length(tpp[,1])),
    main = "Pie Chart of farming importance to family")
legend("topright",
       c("not at all","important","very important"),
       fill = rainbow(length(tpp[,1])),
       horiz = FALSE,
       border = "black")
box(which = "plot", lty = 4 , bg = "grey")

dev.off()

##subsampling
province=x$Province
ms = function(x){
  a = c(mean(x),sqrt(var(x)))
  return(a)
}
#province 1

farm_notemployed1 = x$X.Not..employed...[province==1]
farm_female1 = x$Female..work.on.farm[province==1]
farm_income1 = x$Crop.Net.Income[province==1]
farm_importance1 = x$Importance.of.farming.0.not.atll.Impornt.1.importnt.2.Very.impornt[province==1]
farm_t41 = x$t4[province==1]
farm_r41 = x$r4[province==1]

ms(farm_female1)
ms(farm_income1)
ms(farm_importance1)
ms(farm_t41)
ms(farm_r41)
# province 2

farm_notemployed2 = x$X.Not..employed...[province==2]
farm_female2 = x$Female..work.on.farm[province==2]
farm_income2 = x$Crop.Net.Income[province==2]
farm_importance2 = x$Importance.of.farming.0.not.atll.Impornt.1.importnt.2.Very.impornt[province==2]
farm_t42 = x$t4[province==2]
farm_r42 = x$r4[province==2]

ms(farm_female2)
ms(farm_income2)
ms(farm_importance2)
ms(farm_t42)
ms(farm_r42)
#province 3

farm_notemployed3 = x$X.Not..employed...[province==3]
farm_female3 = x$Female..work.on.farm[province==3]
farm_income3 = x$Crop.Net.Income[province==3]
farm_importance3 = x$Importance.of.farming.0.not.atll.Impornt.1.importnt.2.Very.impornt[province==3]
farm_t43 = x$t4[province==3]
farm_r43 = x$r4[province==3]

ms(farm_female3)
ms(farm_income3)
ms(farm_importance3)
ms(farm_t43)
ms(farm_r43)

##------------------------
#Graphical
png(filename= "cropnetin.png")
par(mfrow=c(3,1))
hist(farm_income1 , main="Crop Net Income in Province 1") 
hist(farm_income2 , main="Crop Net Income in Province 2") 
hist(farm_income3 , main="Crop Net Income in Province 3")
dev.off()
dev.off()
############################

png(filename= "tem4.png")
par(mfrow=c(3,1))
hist(farm_t41 , main="Temperature in April in Province 1") 
hist(farm_t42  , main="Temperature in April in Province 2") 
hist(farm_t43 , main="Temperature in April in Province 3")
dev.off()
dev.off()

#########################
png(filename= "rain4.png")
par(mfrow=c(3,1))
hist(farm_r41  , main="Rain Fall in April in Province 1") 
hist(farm_r42  , main="Rain Fall in April in Province 2") 
hist(farm_r43  , main="Rain Fall in April in Province 3")
dev.off()
dev.off()

