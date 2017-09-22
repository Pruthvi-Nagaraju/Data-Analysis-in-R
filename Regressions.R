library(bbmle)

#ordinary regression

library(manipulate)
corn=read.csv("C:/R/Session 5/Data/Corn.csv")
plot(corn$nitrate, corn$yield)

View(corn)
f1=function(a0,a1,s)
{
  err=corn$yield-a0-a1*corn$nitrate
  LLsum= sum(dnorm(err, mean =0, sd = s, log=T))
  return(-1*LLsum)
}


#start with the baseline values

f1(mean(corn$yield),0, sd(corn$yield))


res=mle2(minuslogl = f1, start = list(a0=mean(corn$yield), a1=0, s=sd(corn$yield)), method = "L-BFGS-B",
         lower=c(s=0)) # providing lower bound for sd

summary(res)

yield=6148 + 51.523*nitrate + error

lm(corn$yield ~ corn$nitrate)


admit=read.csv("C:/R/Session 5/Data/admit.csv")
View(admit)
exp(0.88)

#Binary regression

f1= function(a0,a1,a2,a3)
{
  x=a0+a1*admit$gre+a2*admit$gpa+a3*admit$rank
  p=exp(x)/(1+exp(x))
  L=ifelse(admit$admit==0, 1-p,p)
  LLSum=sum(log(L))
  return(-1*LLSum)
}

f1(0,0,0,0)

res=mle2(minuslogl = f1, start=list(a0=0,a1=0,a2=0,a3=0))
summary(res)


#probit regression


f1= function(a0,a1,a2,a3)
{
  x=a0+a1*admit$gre+a2*admit$gpa+a3*admit$rank
  p=pnorm(x,mean=0,sd=1)
  L=ifelse(admit$admit==0, 1-p,p)
  LLSum=sum(log(L))
  return(-1*LLSum)
}

f1(0,0,0,0)

res=mle2(minuslogl = f1, start=list(a0=0,a1=0,a2=0,a3=0))
summary(res)


#Poisson regression

student=read.csv("C:/R/Session 5/Data/student.csv")
View(student)

f1=function(a0,a1,a2,a3)
{
  x=a0+a1*student$gender+a2*student$math+a3*student$prog
  l=exp(x)
  LLsum=sum(dpois(student$daysabs, lambda = l, log = T))
  return(-1*LLsum)
}

f1(0,0,0,0)
res= mle2(minuslogl = f1, start= list(a0=0,a1=0,a2=0,a3=0))

summary(res)


#heteroskedasticity

head(cars)
attach(cars)
plot(speed,dist)

f1=function(a0,a1,s1,s2)
{
  err=dist-a0-a1*speed
 L= ifelse(speed<15, dnorm(err,mean=0,sd=s1), dnorm(err, mean=0, sd=s2))
 LLSum=sum(log(L))
 return(-1*LLSum)
}

f1(mean(dist),0,sd(dist),sd(dist))
res=mle2(minuslogl = f1, start =list(a0=mean(dist),a1=0,s1=sd(dist),s2=sd(dist)))
summary(res)







































