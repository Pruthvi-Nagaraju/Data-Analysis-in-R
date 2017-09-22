#Sampling Errors
x=rnorm(20,mean=12, sd=6) # x is a sample with population with mean 12 and sd 6.
mean(x) #but mean is not exactly 12
sd(x)
#ideally mean should be =12, sd=6

#Sampling distribution

#flip a fair coin 10 times
f1= function()
{
  v=c(0,1)
  p=c(0.5,0.5)
  x=sample(size=10, x=v,prob=p,replace=T)
  sum(x) #gives number of heads, since 1 is represented by 1.
  return(sum(x))
}
samplingDist= rep(f1(), 10000) #runs f1 once, repeats it 10000 times
samplingDist= replicate(n=10000,f1()) # runs the function each 10000 times
samplingDist
table(samplingDist)
prop.table(table(samplingDist))
plot(table(samplingDist), type="h")

#statistical testing
admission= read.csv("C:/R/Session 4/Session 4/Data/admission.csv")
View(admission)
GMAT= admission$GMAT

#compute the test statistics
tstat=mean(GMAT)
tstat

#describe the population based on the hypothesis, 
#draw a synthetic sample and compute the metric
f1=function()
{
  x=rnorm (length(GMAT), mean =510, sd=sd(GMAT)) #lenght(gmat) since sample size of synthetic sample should be same as real sample
  return(mean(x))
}

#create a sampling distribution 
sdist=replicate(10000, f1())

#Draw a sampling distribution and compute p value
plot(density(sdist))
abline(v=tstat, col="blue") # to compare sample mean with hypothesis mean
# actual sample mean is in the tails of distribution 

gap=abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap, col="red")
abline(v=mean(sdist)+gap, col="red")
s1=sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]
pvalue=length(s1)/length(sdist)
pvalue


#2) test for standard deviation=78

#Compute test statistics
tstat=sd(GMAT)

#describe the population based on the hypothesis, 
#draw a synthetic sample and compute the metric
f1=function()
{
  x=rnorm (length(GMAT), mean =mean(GMAT), sd=78) #lenght(gmat) since sample size of synthetic sample should be same as real sample
  return(sd(x))
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

# 3)
#Compute test statistics
tstat=median(GMAT)

#describe the population based on the hypothesis, 
#draw a synthetic sample and compute the metric
f1=function()
{
  x=rnorm (length(GMAT), mean =500, sd=sd(GMAT)) #lenght(gmat) since sample size of synthetic sample should be same as real sample
  return(median(x))
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


#4) Claim 75th percentile = 600

qnorm(p=0.75, mean=545, sd=81.5)

#Compute test statistics
tstat=quantile(GMAT, probs = 0.75)
sd(GMAT)

#describe the population based on the hypothesis, 
#draw a synthetic sample and compute the metric
f1=function()
{
  x=rnorm (length(GMAT), mean =545, sd=81.5) #lenght(gmat) since sample size of synthetic sample should be same as real sample
  return(quantile(x,probs = 0.75))
}
?plot
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

#claim mean is 6.5 times sd
m=mean(GMAT)
s=sd(GMAT)
m/s

#Compute test statistics
tstat=mean(GMAT)/sd(GMAT)

#describe the population based on the hypothesis, 
#draw a synthetic sample and compute the metric
f1=function()
{
  x=rnorm (length(GMAT), mean =6.5, sd=1 )#lenght(gmat) since sample size of synthetic sample should be same as real sample
  return(mean(x)/sd(x))
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


#5  claim- 40 % of students got an admit

table(admission$De)
tstat= prop.table(table(admission$De))[1]
tstat
#describe the population based on the hypothesis, 
#draw a synthetic sample and compute the metric
f1=function()
{
  v=c("admit","other")
  p=c(0.4,0.6)
  s=sample(size=length(GMAT), x = v, prob = p, replace=T)
  return(prop.table(table(s))[1])
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












