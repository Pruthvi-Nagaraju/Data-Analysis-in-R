library(vcd)
library(car)
library(MASS)

#histogram
View(whiteside)
hist(whiteside$Gas)
density(whiteside$Gas) #creates empirical density function
plot(density(whiteside$Gas))
polygon(density(whiteside$Gas), col="green")
View(Salaries)
symbols(Salaries$yrs.service,Salaries$salary,circles = Salaries$yrs.since.phd, inches=0.1, bg="red")
View(UScereal)
y= as.matrix(UScereal[,c(3,4)])
y
symbols(UScereal$sugars,UScereal$calories,rectangles = y, inches = 0.4, bg ="red") 
#rectangle represents column 3 and 4 i.e; (y)- protein and fat
x1= rnorm(10000)
y1=rnorm(10000)
plot(x1,y1)
smoothScatter(x1,y1) #gives better insight on where data actually lies
x1=runif(10000) #random uniform
y1=runif(10000)
plot(x1,y1)
smoothScatter(x1,y1)

View(Boston)
sunflowerplot(Boston$ptratio,Boston$tax) #makes a petal when there are overlapping points

install.packages("rgl")
library(rgl)
plot3d(Boston$crim,Boston$indus,Boston$age, col="red", size=4)
install.packages("plotrix")
library(plotrix)
l1=c("US","UK","Germany","France")
v1=c(17,12,16,30)
pie(x=v1, lables=l1,col=rainbow(4))
fan.plot(x=v1,labels=l1)
install.packages("vioplot")
library(vioplot)
vioplot(whiteside$Gas)

install.packages("corrgram")
library(corrgram)

#used when there are lots of columns with numeric data
cor(Boston)
corrgram(Boston , order=T, upper.panel = panel.pie)
#check ggplot

# Programming and Data Analysis
NumericPred =read.csv("C:/R/Session 3/Session 3/data/NumericPred.csv")
View(NumericPred)
colnames(NumericPred)=c("target","model1","model2")

a=NumericPred$target
a
m=NumericPred$model1
m
#MAd MSE MAPE MPSE rSquare TMAd- used to evaluate model

metrics =c(MAD=0,MSE=0,MAPE=0,MPSE=0,R2=0,TMAD=0)
metrics
metrics["MAD"]=mean(abs(a-m)) # mean absolute deviation
metrics["MSE"]=mean((a-m)^2)  #mean squared error
metrics["MAPE"]=mean(abs(a-m)/a) #mean abs percentage error
metrics["MPSE"]=mean(((a-m)/a)^2) #mean squared prediction error
metrics["TMAD"]=mean(abs(a-m),trim=0.05) # trim off top 5% and bottom 5%
# to trim off only bottom 5 %, use quantile and subset with lower 5% removed
#r square- variance explained by the model
#SST variance in the outcome variable
SST= sum((a-mean(a))^2)
SSE= sum((a-m)^2)
metrics["R2"]= 1-(SSE/SST)

nummetrics = function (a,m)
{
  metrics =c(MAD=0,MSE=0,MAPE=0,MPSE=0,R2=0,TMAD=0)
  metrics
  metrics["MAD"]=mean(abs(a-m)) 
  metrics["MSE"]=mean((a-m)^2)
  metrics["MAPE"]=mean(abs(a-m)/a)
  metrics["MPSE"]=mean(((a-m)/a)^2)
  metrics["TMAD"]=mean(abs(a-m),trim=0.05) 
  SST= sum((a-mean(a))^2)
  SSE= sum((a-m)^2)
  metrics["R2"]= 1-(SSE/SST)
  return(metrics)
  
}

class(nummetrics(a=c(1.2,1.3,1.7),m=c(2,4,3)))
nummetrics(a=NumericPred$target,m=NumericPred$model2)

dump("nummetrics",file = "MymodelFunctions.R")
getwd()

#when you want to use this function in some other location,
source(file.choose()) #after running, select the file where the function was stored.
# now the function will be loaded
#just give name of the function and run it to see the code written inside.

ensemble = lm(NumericPred$target ~ NumericPred$model1+NumericPred$model2)
summary(ensemble)
#indicates both models are significant, and model2 is more important
NumericPred$model3=-1.11362+0.29938*NumericPred$model1+0.75044*NumericPred$model2 #from liner regression
View(NumericPred)

nummetrics(a=NumericPred$target,m=NumericPred$model3)
plot(NumericPred$target,NumericPred$model3)
lines(NumericPred$target,NumericPred$target)
s1=NumericPred[abs(NumericPred$target-NumericPred$model3) > 5, ]
s1
points(s1$target,s1$model3, pch=19, col="red")

a=NumericPred$target
m=NumericPred$model3
cost= ifelse( abs(a-m )<5, 0 ,2*abs(a-m))
sum(cost)
NumericPred$baseline= mean(a)
View(NumericPred)

a=NumericPred$target
m=NumericPred$baseline
cost= ifelse( abs(a-m )<5, 0 ,2*abs(a-m))
sum(cost)




