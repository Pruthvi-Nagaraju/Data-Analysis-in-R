# session 7
#statistical Estimation 

data1= read.csv("C:/R/Session 5/Data/data1.csv")
x1= data1$x1
m=5
dnorm(3.814,mean=5,sd=2) # assuming mean=5, sd=2
dnorm(x1, mean=m, sd=2, log= T)
sum(dnorm(x1, mean=m, sd=2, log= T)) # log likelihood

m=6
sum(dnorm(x1, mean=m, sd=2, log= T))

m=7
sum(dnorm(x1, mean=m, sd=2, log= T))

mseq= seq(0,10, by=0.05)
mseq

f1=function(m){
  LL= sum(dnorm(x1, mean=m, sd=2, log= T))
  return(LL)
}

LLres=sapply(mseq,f1) # runs function f1 for each value in mseq
LLres
plot(LLres)
i=which.max(LLres) # index in vector LLres which has max value
mseq[i]  

# March 22- after exam
install.packages("bbmle")
x=data1$x1

#DGP Data generation process  N(M,2) normal function with sd =2 and mean unknown
library(bbmle)
M=5
f1=function(M)
{
  LLSum=sum(dnorm(x, mean= M, sd= 2, log=T))
  return(-1*LLSum) # since the function only minimizes
}

f1(1)
res= mle2(minuslogl = f1, start = list(M=1)) #mle2 function used for optimisation  
summary(res)


#DGP when u do not know the stamdard deviation too N(M,s)

f1=function(M,s)
{
  LLSum= sum(dnorm(x, mean = M, sd =s, log= T))
  return(-1*LLSum)
}

f1(1,1)

res= mle2(minuslogl = f1, start = list(M=1,s=1))

summary(res)

#negative values try sd=10

f1(1,10)


res= mle2(minuslogl = f1, start = list(M=1,s=1),
                  method ="L-BFGS-B" , lower=c(s=0))

summary(res)

#DGP -Pois(l)
data2= read.csv("C:/R/Session 5/Data/data2.csv")
x= data2$x1


f1=function(l)
{
  LLSum=sum(dpois(x, lambda = l,log=T))
  return(-1*LLSum) # since the function only minimizes
}

f1(1)

res=mle2(minuslogl = f1, start = list(l=1))
summary(res)

#Poisson used to represent counts(how many times some1 visited the hospital?)

# Zero inflated Poisson

x=data2$x2
x #has way too many zeros,-> this is zero inflated
plot(table(x))


f1=function(p,l)
{
  L=ifelse(x==0, p+(1-p)*dpois(0,l), (1-p)*dpois(x,l))
  LLSum=sum(log(L))
  return(-1*LLSum)
}

f1(0.5,1)

res=mle2(minuslogl = f1, start = list(p=0.5,l=1))
summary(res)
p=0.3357 #prob of not getting sick
l=1.851631
#prob that person gets sick and dont go

dpois(0,1.8516) #?

dpois(0,1.8516)*(1-0.3357) #? diff















