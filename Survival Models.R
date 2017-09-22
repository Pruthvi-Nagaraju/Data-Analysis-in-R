library(bbmle)
aml=read.csv("C:/R/Session 6/Data/aml.csv")


View(aml)
x=rexp(1000, rate=1/15)
x
mean(x)
plot(density(x))

f1=function(l)
{
  LLsum=sum(dexp(aml$time,rate=1/l, log=T))
  return(-1*LLsum)
}

res=mle2(minuslogl = f1, start = list(l=5))
summary(res)

#Censored

f1=function(l)
{
  L=ifelse(aml$status==0, 1-pexp(aml$time,rate=1/l), dexp(aml$time,rate=1/l))
  LLsum=sum(log(L))
  return(-1*LLsum)
}
res=mle2(minuslogl = f1, start = list(l=5))
summary(res)



#survival Model

as.numeric(aml$x)
mt=ifelse(aml$x=="Maintained",1,0)
mt
f1=function(a0,a1)
{
  X=a0+a1*mt
  l=exp(X)
  L=ifelse(aml$status==0, 1-pexp(aml$time,rate=1/l), dexp(aml$time,rate=1/l))
  LLsum=sum(log(L))
  return(-1*LLsum)
}
res=mle2(minuslogl = f1, start= list(a0=0,a1=0))# does not work

res=mle2(minuslogl = f1, start= list(a0=5,a1=0))
summary(res)

x=3.14+0.958*mt
x=10
A=dexp(x,rate=1/15)
B=1-pexp(x,rate=1/15)
A/B

x=20
A=dexp(x,rat1e=1/15)
B=1-pexp(x,rate=1/15)
A/B

#for an exponential distribution A/B is always same( this is called HASSLE-( which is always constant))

#A chance that you wil die now, B chance that you have survived thus far
# you survived 1 year, the chance of dying is same as after you survived 100 years( hassle)


lung=read.csv("C:/R/Session 6/Data/lung.csv")
View(lung)


f1=function(a0,a1,a2,a3)
{
  X=a0+a1*lung$age+a2*lung$sex+a3*lung$ph.karno
  l=exp(X)
  L=ifelse(lung$status==1,  dexp(lung$time,rate=1/l), 1-pexp(lung$time,rate=1/l))
  LLsum=sum(log(L))
  return(-1*LLsum)
}

res=mle2(minuslogl = f1, start= list(a0=log(mean(lung$time)),a1=0,a2=0,a3=0))
summary(res)  #check


hsb2=read.csv("C:/R/Session 6/Data/hsb2.csv")
View(hsb2)

as.numeric(hsb2$ses)
reg1=lm(hsb2$read~ as.numeric(hsb2$female) +as.numeric(hsb2$ses)+ hsb2$socst)
reg2=lm(hsb2$math~ as.numeric(hsb2$female) +as.numeric(hsb2$ses)+ hsb2$science)
summary(reg1)
summary(reg2)

library(mvtnorm)



#seemingly unrelated

LLSUR = function(a0,a1,a2,a3,b0,b1,b2,b3,s1,s2,s12) 
{ y1 = a0 + a1*(as.numeric(hsb2$female)) + a2*(as.numeric(hsb2$ses)) + a3*(hsb2$socst) 
  y2 = b0 + b1*(as.numeric(hsb2$female)) + b2*(as.numeric(hsb2$ses)) + b3*(hsb2$science) 
  e1 = hsb2$read-y1 
  e2 = hsb2$math-y2 
  S = matrix(c(s1,s12,s12,s2),nrow = 2,ncol = 2) 
  LLsum = sum(dmvnorm(cbind(e1,e2),mean = c(0,0),sigma = S,log = T)) 
  return(-1*LLsum) 
}

res1 = mle2(minuslogl = LLSUR, start = list(a0=mean(hsb2$read),a1=0,a2=0,a3=0,b0=mean(hsb2$math),b1=0,b2=0,b3=0,s1=100,s2=100,s12=cov(hsb2$read,hsb2$math)))
summary(res1)
                                                                                                   
#Regression

View(women)
attach(women)
plot(women$height,women$weight)
reg1=lm(women$weight~ women$height)
summary(reg1)
reg1$coefficients
reg1$residuals
reg1$fitted.values
abline(reg1,col="blue")
library(MASS)
View(Boston)
reg=lm(medv ~ ., data = Boston)
summary(reg)
reg2= lm(medv ~ .-crim, data=Boston)
summary(reg2)


#Scenario 1

x=runif(500,1,100)
y=250+x+rnorm(500,0,10)

reg1=lm(y ~ x)
summary(reg1)
err= reg1$residuals
err
shapiro.test(err)
par(mfrow=c(2,2))
plot(reg1)
par(opar)

#Scenario2

x = runif(500,1,20) 
y = 100+2*x + x*rnorm(500) 
reg1 = lm(y~x)

summary(reg1)
opar=par()
par(mfrow=c(2,2))
plot(reg1)  #funnel shape
par(opar)
err= reg1$residuals
shapiro.test(err)

#Scenario 3

x = runif(500,1,20) 
y = ifelse(x<15,100+2*x +rnorm(500),100+5*x+rnorm(500)) 
reg1 = lm(y~x)
summary(reg1)
opar=par()
par(mfrow=c(2,2))
plot(reg1) 
par(opar)
err= reg1$residuals
shapiro.test(err)

#Scenario 4

x = runif(500,1,20) 
y = 100+2*x +0.5*x^2 + rnorm(500) #Quadratic
reg1 = lm(y~x)
summary(reg1)  #61.6+12.38*x (far from wat we assume)
opar=par()
par(mfrow=c(2,2))
plot(reg1) 
par(opar)
err= reg1$residuals
shapiro.test(err)#non linear


#Scenario 5

x1 = runif(500,1,20) 
x2 = runif(500,1,20) 
y = x1+4*x2+0.5*x1*x2 + rnorm(500) 
reg1 = lm(y~x1+x2)
summary(reg1)  
opar=par()
par(mfrow=c(2,2))
plot(reg1) 
par(opar)
err= reg1$residuals
shapiro.test(err)


reg1=lm(women$weight~ women$height)

summary(reg1)  
opar=par()
par(mfrow=c(2,2))
plot(reg1) 
par(opar)
err= reg1$residuals
shapiro.test(err)
#assuming relation btw height and weight as linear is not right, there is some non linearity
#but results were good(R2)

#Scenario 6
x = runif(500,1,100)
y = 250 + x + rnorm(500,0,10) 
x[499] = 860 #extreme value
reg1 = lm(y~x)
summary(reg1)  
opar=par()
par(mfrow=c(2,2))
plot(reg1) 
par(opar)
err= reg1$residuals

#280+0.4x ( this is  different from what we assumed)

#Scenario 7: Extreme Values (y)
x = runif(500,1,100) 
y = 250 + x + rnorm(500,0,10) 
y[499] = 2000 
reg1 = lm(y~x)

summary(reg1)  
opar=par()
par(mfrow=c(2,2))
plot(reg1) 
par(opar)
err= reg1$residuals
# in this case there is not much deviation from what we assumed

#Scenario 8: Multicollinearity 
x1 = runif(500,1,10) 
lambda = 0.8 
x2 = (lambda*x1) + (1-lambda)*runif(500,1,10) 
cor(x1,x2) 
y = 2*x1 + x2 + rnorm(500,0,10) 
reg1 = lm(y~x1+x2)

summary(reg1)  
opar=par()
par(mfrow=c(2,2))
plot(reg1) 
par(opar)
err= reg1$residuals # plots will look fine, but the estimates were not the same.

library(MASS)
library(car)

x=runif(500,1,100)
y=250+x+rnorm(500,0,10)
reg1=lm(y ~ x)
ncvTest(reg1)
 # to test homoscadicity p>0.05 so it is fine

#Ho -it is homoscadastic

x = runif(500,1,20) 
y = ifelse(x<15,100+2*x +rnorm(500),100+5*x+rnorm(500)) 
reg1 = lm(y~x)
ncvTest(reg1) #p is very less. no homoscadasticity
spreadLevelPlot(reg1)

d=cooks.distance(reg1)
d
round(d,4)
cutoff= 4/500 #(n=500)
d[d>cutoff]

x=runif(500,1,100)
y=250+x+rnorm(500,0,10)
x[499] = 860
reg1=lm(y ~ x)
d=cooks.distance(reg1)
d
round(d,4)
cutoff= 4/500 #(n=500)
round(d[d>cutoff],4)
plot(reg1,which=4,cook.levels=cutoff)
abline(h=cutoff,col="red")

?plot

View(Boston)
reg1=lm(Boston$medv ~ ., data=Boston)
cutoff=4/nrow(Boston)
d=cooks.distance(reg1)
round(d,4)
cutoff= 4/nrow(Boston) 
round(d[d>cutoff],4)
plot(reg1,which=4,cook.levels=cutoff)
abline(h=cutoff,col="red")
length(d[d>cutoff]) # no of influencial rows of obs #369 is the highest

# multicolliniarity
x1 = runif(500,1,10) 
lambda = 0.2 
x2 = (lambda*x1) + (1-lambda)*runif(500,1,10) 
cor(x1,x2) 
y = 2*x1 + x2 + rnorm(500,0,10) 
reg1 = lm(y~x1+x2)
vif(reg1) # 1.07 so its okay.

#with lambda =0.5
x1 = runif(500,1,10) 
lambda = 0.5 
x2 = (lambda*x1) + (1-lambda)*runif(500,1,10) 
cor(x1,x2) 
y = 2*x1 + x2 + rnorm(500,0,10) 
reg1 = lm(y~x1+x2)
vif(reg1) # 2.1 so its still okay.

#with lambda =0.8
x1 = runif(500,1,10) 
lambda = 0.8
x2 = (lambda*x1) + (1-lambda)*runif(500,1,10) 
cor(x1,x2) 
y = 2*x1 + x2 + rnorm(500,0,10) 
reg1 = lm(y~x1+x2)
vif(reg1) # very high, so collinearity exists

#Boston data diagnostics

#1. plot

reg1=lm(Boston$medv ~ ., data=Boston)
summary(reg1)  
opar=par()
par(mfrow=c(2,2))
plot(reg1) 
par(opar)


#ncv test

ncvTest(reg1) # no homoscadasticity

# cooks distance

cutoff=4/nrow(Boston)
d=cooks.distance(reg1)
round(d,4)
cutoff= 4/nrow(Boston) 
round(d[d>cutoff],4)
plot(reg1,which=4,cook.levels=cutoff)
abline(h=cutoff,col="red") 

#vif

vif(reg1) # tax, rad and nox have vif >4


# women data

View(women)

reg1=lm(women$height ~ ., data=women)

#1. plot

reg1=lm(women$height ~ ., data=women)
summary(reg1)  
opar=par()
par(mfrow=c(2,2))
plot(reg1) 
par(opar)


#ncv test

ncvTest(reg1) # no homoscadasticity


# weighted least square and robust regression


x=runif(500,1,100)
y=250+x+rnorm(500,0,10)
x[499] = 860
reg1=lm(y ~ x)
summary(reg1)

d=cooks.distance(reg1)
cutoff=4/ 500
w=ifelse(d<cutoff,1,cutoff/d)
reg2= lm(y ~ x, weights = w)
summary(reg2) #results much closer to the truth

d1=cooks.distance(reg2)
length(d1[d1>cutoff])

d1=cooks.distance(reg2)
w=ifelse(d1<cutoff,1,cutoff/d1)
reg3= lm(y ~ x, weights = w)

d1=cooks.distance(reg3)
length(d1[d1>cutoff])


length(d1[d1>cutoff])
# there are still extreme points, so we need to iterate trough a few times

reg3=rlm(y~x) # robust linear regression (takes care of extreme value)
summary(reg3)


#boston

reg1=lm(Boston$medv ~ ., data = Boston)
reg2=rlm(Boston$medv ~ ., data = Boston)
summary(reg2)
c1=round(reg1$coefficients,3)
c2=round(reg2$coefficients,3)
cbind(c1,c2) #huge difference, influential factors have huge impact

#regression structure

x = runif(500,1,100) 
y = 250 + x + rnorm(500,0,10) 
reg1 = lm(y~x)

boxCox(reg1, family="yjPower", plotit = TRUE)
truehist()

#to find transform for y

reg1=lm(Boston$medv ~ ., data = Boston)
boxCox(reg1, family="yjPower", plotit = TRUE)  
# take log of y peak around 0

reg1=lm(log(Boston$medv) ~ ., data = Boston)
boxCox(reg1, family="yjPower", plotit = TRUE) 


reg1=lm(women$weight ~ ., data = women)
boxCox(reg1, family="yjPower", plotit = TRUE) 

# to find transform for x
#Box-Tidwell transformation

x = runif(500,1,100) 


x = runif(500,1,20) 
y = 100+2*x +0.5*x^2 + rnorm(500)  # mle is 1.8, hence add power 2 term

boxTidwell(y~x)

reg1= lm(weight ~ height, data = women)
boxTidwell(weight~ height , data=women)  # add height raised  to the power 4, lambda from test = 4

y = 250 + x + rnorm(500,0,10) 
reg1 = lm(y~x)

boxTidwell(y~x)  # lme is 1..
reg1=lm(weight ~ height, data = women)
reg2= lm(weight ~ height+I(height^4), data = women)
AIC(reg1,reg2) # by adding height to the power 4, results are much better (lower AIC)
anova(reg1,reg2)


boxTidwell(medv~ . , data=Boston)  #error since it has negative value. 
boxTidwell(medv~ age+dis+rad , data=Boston) 


reg1= lm(medv ~.,data=Boston)
res=step(reg1,~.^2)
res$anova
reg2=lm(medv~.+rm:lstat+rad:lstat, data=Boston)
reg2
AIC(reg1,reg2)
anova(reg1,reg2)
summary(reg1)






x1 = runif(500,1,20) 
x2 = runif(500,1,20) 
y = 25 + 3*x1 + 2*x2+ rnorm(500,0,10) 
reg1 = lm(y~x1+x2) 
res = step(reg1,~.^2) 
res$anova



