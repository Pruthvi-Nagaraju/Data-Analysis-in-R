#Binary outcomes
BinaryPred= read.csv("C:/R/Session 3/Session 3/data/BinaryPred.csv")
View(BinaryPred)
# to evaluate binary outcomes, Log Likekihood, AIC,BIC, r square

a=BinaryPred$Target
m=BinaryPred$Model1
k=10 #default (used in AIC and BIC)

metrics= c(LL=0,AIC=0,BIC=0,R2=0)
metrics["LL"]= sum(ifelse(a==1, log(m), log(1-m)))
metrics
metrics["AIC"]= -2*metrics["LL"]+2*k
metrics["BIC"]= -2*metrics["LL"]+2*k*log(length(a))
SST= sum((a-mean(a))^2)
SSE= sum((a-m)^2)
metrics["R2"]=1-(SSE/SST)

binmetrics = function(a,m,k=10)
{
  metrics= c(LL=0,AIC=0,BIC=0,R2=0)
  metrics["LL"]= sum(ifelse(a==1, log(m), log(1-m)))
  metrics["AIC"]= -2*metrics["LL"]+2*k
  metrics["BIC"]= -2*metrics["LL"]+2*k*log(length(a))
  SST= sum((a-mean(a))^2)
  SSE= sum((a-m)^2)
  metrics["R2"]=1-(SSE/SST)
  return(metrics)
  
}

binmetrics(a=BinaryPred$Target,BinaryPred$Model1)
binmetrics(a=BinaryPred$Target,BinaryPred$Model2)

p=0.7
mnew= ifelse( m>0.7,1,0)
x=table(factor(mnew),factor(a))
View(x)
class(x)
#coersion- makes a table into a vector
vec=as.vector(x)
vec
#vec()=(TN,FP,FN,TP)

vec=c(p,vec)
vec

increment=0.05
cutoffs=seq(min(m),max(m),by=increment)
cutoffs

mresult= data.frame()
for(p in cutoffs)
{
  mnew= ifelse( m>p,1,0)
  x=table(factor(mnew),factor(a))
  vec=as.vector(x)
  vec=c(p,vec)
  mresult=rbind(mresult,vec) # contains missclassification rate for all p values
}
#rbind to add extra row to data frame

colnames(mresult)=c("cutoff","TN","FP","FN", "TP")
View(mresult)
xyz=function(a,m,increment=0.05)
  {
  cutoffs=seq(min(m),max(m),increment)
  mresult= data.frame()
  for(p in cutoffs)
  {
    mnew= ifelse( m>p,1,0)
    x=table(factor(mnew),factor(a))
    vec=as.vector(x)
    vec=c(p,vec)
    mresult=rbind(mresult,vec)
  }
  colnames(mresult)=c("cutoff","TN","FP","FN", "TP")
  return((mresult))
  
}

x=xyz(a=BinaryPred$Target,m=BinaryPred$Model2)
View(x)
x1=x$FP/max(x$FP)
y1=x$TP/ max(x$TP)
plot(x1,y1, type="l") #ROC curve ( normlised FP vs normalised TP)
AUC=mean(y1)
AUC

dump('xyz', file= "Mymodelfunction.R", append=T)# to save the function. 
#append to save to an already existing file
getwd()


####################################################################################
  
#Session 4
#Discrete distribution

v=c("H","T")
p=c(0.5,0.5)
sample(size=6,x=v,prob=p,replace=T)# to draw a sample, 
#replace tells that once we get a Heads, we can get Heads next time

v=seq(1,6)
p=rep(1/6,6)
sample(size=3,x=v,prob=p,replace=T)

dnorm(x=50, mean=67,sd=6)
pnorm(80, mean=67,sd=6)
qnorm(p=0.75, mean=67, sd=6)
rnorm(n=100, mean=67, sd=6)

#convolutions
r1= rnorm(3000)
r2=rnorm(3000)
u1=runif(3000)
u2=runif(3000)
u3=runif(3000)
u4=runif(3000)
u5=runif(3000)
u6=runif(3000)
x=r1+r2  #addition of 2 normal dis is normal
x=r1*r2 # multiplication of two normal- not normal
x=r1^2 # not normal
plot(density(x))

x=u1+u2+u3+u4+u5  # as u keep adding uniform dis, it becomes normal distribution
#central limit theoram

shapiro.test(x) #test for normality

#tasks starting in sequence
t1=rnorm(n=100000,mean=5,sd=3)
t2=runif(100000, min=3, max=7)
t=t1+t2
t
s1=subset(t,t<10)
length(s1)
prob=length(s1)/100000
prob
quantile(prob,probs = 0.75)

#tasks start in parallel
t1=rnorm(n=100000,mean=5,sd=3)
t2=runif(100000, min=3, max=7)
t=ifelse(t1>t2, t1,t2)
plot(density(t))
s1=subset(t,t<10)
length(s1)
prob=length(s1)/100000
prob
quantile(prob,probs = 0.75)

#Sampling Errors
x=rnorm(20,mean=12, sd=6)
mean(x)
sd(x)
#ideally mean should be =12, sd=6



















