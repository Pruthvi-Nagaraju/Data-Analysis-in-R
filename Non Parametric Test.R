#Session 6

admission= read.csv("C:/R/Session 4/Session 4/Data/admission.csv")
View(admission)
GMAT= admission$GMAT
GPA=admission$GPA

#Test of correlation

#Compute the test statistic
tstat= cor(GMAT, GPA)
tstat

#Ho - no correlation between gpa and gmat

#describe the population based on the hypothesis, 
#draw a synthetic sample and compute the metric

f1= function()
{
  x1=rnorm(length(GMAT), mean = mean(GMAT), sd = sd(GMAT))
  x2=rnorm(length(GMAT), mean = mean(GPA), sd = sd(GPA))
  return(cor(x1,x2))
}

#create a sampling distribution 
sdist=replicate(10000, f1())

#Draw a sampling distribution and compute p value
plot(density(sdist))
abline(v=tstat, col="blue") 
gap=abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap, col="red")
abline(v=mean(sdist)+gap, col="red")
s1=sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]
pvalue=length(s1)/length(sdist)
pvalue

#Test2
#Shape test
plot(density(GMAT))
plot(density(GPA))
# to check if the two shapes are same or not

GPA1= (GPA-mean(GPA))/sd(GPA)
GMAT1= (GMAT-mean(GMAT))/sd(GMAT)
plot(density(GPA1))
lines(density(GMAT1), col="blue")

#Compute the test statistic
q=c(0.1,0.2,0.3,0.67,0.96)
n1=quantile(GPA1, probs = q)
n2=quantile(GMAT1, probs=q)
tstat=sum(abs(n1-n2))
tstat
#if these two qunatile values are close the two values are, the similar they are

#describe the population based on the hypothesis, 
#draw a synthetic sample and compute the metric
f1=function()
{
  x1=rnorm(length(GMAT))
  x2=rnorm(length(GPA))
  n1=quantile(x1, probs=q)
  n2=quantile(x2, probs=q)
  return(sum(abs(n1-n2)))
}

#create a sampling distribution 
sdist=replicate(10000, f1())

#Draw a sampling distribution and compute p value
plot(density(sdist))
abline(v=tstat, col="blue") 
length(sdist[sdist>tstat])/length(sdist)



#confidence interval for mean
f1= function ()
{
  x=rnorm(length(GMAT),mean=mean(GMAT), sd=sd(GMAT))
  return(mean(x))
}

#create a sampling distribution 
sdist=replicate(10000, f1())

#compute confidence interval
plot(density(sdist))
quantile(sdist, probs = c(0.025,1-0.025))  #5% divided between left and right tail

#confidence interval for median
f1= function ()
{
  x=rnorm(length(GMAT),mean=mean(GMAT), sd=sd(GMAT))
  return(median(x))
}

#create a sampling distribution 
sdist=replicate(10000, f1())

#compute confidence interval
plot(density(sdist))
quantile(sdist, probs = c(0.025,1-0.025))  #5% divided between left and right tail

f1= function ()
{
  x=rnorm(length(GMAT),mean=mean(GMAT), sd=sd(GMAT))
  return(quantile(x,probs = 0.75))
}

#create a sampling distribution 
sdist=replicate(10000, f1())

#compute confidence interval
plot(density(sdist))
quantile(sdist, probs = c(0.025,1-0.025))


#Non  Parametric test of median

# compute the tstat
tstat= sum(ifelse(GMAT>500, 1,0))

#describe the population based on the hypothesis, 
#draw a synthetic sample and compute the metric
f1= function()
{
  v=c(0,1)
  p=c(0.5,0.5)
  x=sample(size=length(GMAT), x=v, prob=p, replace = T)
  return(sum(x))
}

#create a sampling distribution 
sdist=replicate(10000, f1())

#Draw a sampling distribution and compute p value
plot(density(sdist))
abline(v=tstat, col="blue") 
gap=abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap, col="red")
abline(v=mean(sdist)+gap, col="red")
s1=sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]
pvalue=length(s1)/length(sdist)
pvalue


#bootstrapped Confidence intervals
#mean
f1= function()
{
  x=sample(size=length(GMAT), x=GMAT,replace=T)
  return(mean(x))
}

#create a sampling distribution 
sdist=replicate(10000, f1())

#compute confidence interval
plot(density(sdist))
quantile(sdist, probs = c(0.025,1-0.025))

#median

f1= function()
{
  x=sample(size=length(GMAT), x=GMAT,replace=T)
  return(median(x))
}

#create a sampling distribution 
sdist=replicate(10000, f1())

#compute confidence interval
plot(density(sdist))
quantile(sdist, probs = c(0.025,1-0.025))


#75th quantile
f1= function()
{
  x=sample(size=length(GMAT), x=GMAT,replace=T)
  return(quantile(x,probs = 0.75))
}

#create a sampling distribution 
sdist=replicate(10000, f1())

#compute confidence interval
plot(density(sdist))
quantile(sdist, probs = c(0.025,1-0.025))



sleepMoney= read.csv("C:/R/Session 4/Session 4/Data/Sleep and Money.csv")
View(sleepMoney)
#Compute the test statistic
tstat= cor(sleep,money)
tstat
sleep=sleepMoney$sleep
money=sleepMoney$money

#Ho - no correlation between gpa and gmat

#describe the population based on the hypothesis, 
#draw a synthetic sample and compute the metric

f1= function()
{
  x1=rnorm(length(sleep), mean = mean(sleep), sd = sd(sleep))
  x2=rnorm(length(money), mean = mean(money), sd = sd(money))
  return(cor(x1,x2))
}

#create a sampling distribution 
sdist=replicate(10000, f1())

#Draw a sampling distribution and compute p value
plot(density(sdist))
abline(v=tstat, col="blue") 
gap=abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap, col="red")
abline(v=mean(sdist)+gap, col="red")
s1=sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]
pvalue=length(s1)/length(sdist)
pvalue

#session 7 testing continuation
# two sample test

twosample <- read.csv("C:/R/Session 4/Session 4/Data/twosample.csv") 
View(twosample)
treatment = twosample[twosample$group=="Treatment",2] 
control = twosample[twosample$group=="Control",2] 
treatment
control
nt=length(treatment)
nc=length(control)
nt
nc
#tstat
tstat = mean(treatment)-mean(control)
tstat

#describe the population and create synthetic samples
#sample()- used to get a random sample

f1= function()
{
  x=c(treatment,control)
  x=sample(x)
  m1=mean(x[1:nt])
  m2=mean(x[(nt+1):(nt+nc)])
  return(m1-m2)
}

#create a sampling distribution 
sdist=replicate(10000, f1())

#Draw a sampling distribution and compute p value
plot(density(sdist))
abline(v=tstat, col="blue") 
gap=abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap, col="red")
abline(v=mean(sdist)+gap, col="red")
s1=sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]
pvalue=length(s1)/length(sdist)
pvalue


#to test median values in the two groups are the same

#tstat
tstat = median(treatment)-median(control)

f1= function()
{
  x=c(treatment,control)
  x=sample(x)
  m1=median(x[1:nt])
  m2=median(x[(nt+1):(nt+nc)])
  return(m1-m2)
}

#create a sampling distribution 
sdist=replicate(10000, f1())

#Draw a sampling distribution and compute p value
plot(density(sdist))
abline(v=tstat, col="blue") 
gap=abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap, col="red")
abline(v=mean(sdist)+gap, col="red")
s1=sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]
pvalue=length(s1)/length(sdist)
pvalue

#interquantile range- 75th - 25th

#using multivariate distribution 

library(mvtnorm) # for multivariate normal distribution

m=c(10,6)
s=matrix(c(4,2,2,5), nrow=2,ncol=2) # covariance matrix
rmvnorm(n=50,mean =m, sigma=s)

#tstat
tstat= cor(GPA, GMAT)

#describe the population based on the hypothesis, 
#draw a synthetic sample and compute the metric
f1= function()
{
  M=c(mean(GPA),mean(GMAT))
  S=matrix(c(var(GPA), 0.6*sd(GPA)*sd(GMAT),0.6*sd(GPA)*sd(GMAT), var(GMAT)),
           nrow = 2, ncol=2)
  x=rmvnorm(length(GMAT), mean =M, sigma =S)
  return(cor(x[,1],x[,2]))
           
}
f1()

#create a sampling distribution 
sdist=replicate(10000, f1())

#Draw a sampling distribution and compute p value
plot(density(sdist))
abline(v=tstat, col="blue") 
gap=abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap, col="red")
abline(v=mean(sdist)+gap, col="red")
s1=sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]
pvalue=length(s1)/length(sdist)
pvalue


































