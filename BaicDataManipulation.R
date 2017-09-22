#Our First R Code
log(769)
6^5
rnorm(10)
getwd()
setwd("C:/Users/rgopal/Documents")
library()
library(MASS)
library(help=MASS)
help("whiteside")
weight = c(60,72,57,90,95,72)
height = c(1.75,1.8,1.65,1.9,1.74,1.91)
mean(weight)
sd(height)
median(weight)
quantile(height,probs = 0.75)
quantile(height,probs = c(0.3,0.75))
length(height)
range(height)
t.test(weight,mu = 80)

bmi = weight/height^2
bmi

plot(height,weight,pch=19,
     main="First Graph", col="orange",ylim=c(0,100))
colors()
length(weight)
weight = c(weight,86)
height
height = c(height,NA)
height
View(whiteside)
mean(weight)
mean(height,na.rm = T)
gender = c("M","F","M","F","F","M","F")
gender
names(gender)
names(gender)=c("Bob","Susan","Jim","Mary","Jane","Tim","Nicole")
plot(factor(gender),height)

x = seq(1,100,length.out = 4)
x=seq(1,100, by= 3)
x
?seq()
x = rep(7,10)
x
x = rep("A",5)
x
x = rep(c("A","B","C"),5)
x
x = rep(c("A","B"),c(4,5))
x
# Indexing and Subsetting
weight[5]
weight[c(2,6)]
s1 = weight[1:4]
s1
s2 = height[height>1.7 & is.na(height)==F]
s2

form1 = gender=="F"
form1
ghw = data.frame(gender,height,weight)
ghw$height
plot(ghw$height,ghw$weight)
x = edit(ghw)
fix(ghw)
View(ghw)
ghw[2,3]
ghw
ghw[1,]
ghw[c(1,4),]
s3 = ghw[ghw$gender=="M",]
edit(s3)
edit(whiteside)
plot(whiteside$Insul,whiteside$Gas)

str(whiteside)
dim(whiteside)
summary(whiteside)
str(ghw)
class(whiteside)
attributes(whiteside)

library(help=MASS)
edit(Cars93)
data()

install.packages("ISwR")

