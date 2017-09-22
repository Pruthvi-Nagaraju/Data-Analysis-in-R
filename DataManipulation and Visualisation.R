getwd()

car1<- read.csv("C:/R/car insurance.csv")
fix(car1)
colnames(car1)[5] = "amnt"
View(car1)
write.csv(car1, "car2.csv")
mean(car1$veh_value)
mean(veh_value)
attach(car1)
detach(car1)

# summarize data
# install packages vcd car dplyr
install.packages("vcd")
install.packages(c("car", "dplyr"))
install
library(vcd)
library(car)
library(dplyr)
View(whiteside)
s1=whiteside[whiteside$Insul=="After",]
View(s1)
mean(s1$Temp)
mean(s1$Gas)
View(Arthritis)
s2=Arthritis[Arthritis$Sex =="Male" & Arthritis$Treatment =="Treated",]
View(s2)
mean(s2$Age)
table(Arthritis$Treatment)
x = table(Arthritis$Treatment, Arthritis$Improved)
x
prop.table( x )
prop.table( x,1 ) #row percentages
prop.table( x,2 ) #column percentages
x= table(Arthritis$Sex, Arthritis$Treatment, Arthritis$Improved)
ftable(x) #formats table
range(Arthritis$Age)
Arthritis$Age
agegroups = cut (Arthritis$Age,breaks = c(-Inf,40,60,74))
agegroups
table(agegroups)
table(agegroups, Arthritis$Improved)

tapply(Arthritis$Age, list(Arthritis$Sex,Arthritis$Treatment), mean)
tapply(Arthritis$Age, Arthritis$Sex, mean)

x=aggregate(Arthritis$Age, list(Arthritis$Sex,Arthritis$Treatment), mean)
class(x)
View(x)
View(Salaries)
aggregate(Salaries$salary ,list(Salaries$sex),mean)

mean()#dpylr package
attach(Salaries)
formula1 = (sex=="Male" & rank=="Prof")|salary>100000 
f1 = filter(Salaries,formula1) 
View(f1)
x= arrange(Salaries ,sex, rank)
x = group_by(Salaries,discipline,sex,rank)
View(x)
summarize(x,mean(salary),mean(yrs.service),median(yrs.since.phd))
summarise(x)

#visualization

attach(whiteside)
plot(Insul)
plot(Insul, Gas) #factor against numeric box plot
plot(Temp, Gas)
plot(whiteside)
plot(Temp, Gas, type="h", las=2, cex=2) #type =p (points) cex (character expansion)
plot(Temp, Gas, type="p", las=0)
abline(h=mean(Gas), col="orange", lwd =3)
abline(v=mean(Temp), col="magenta", lwd =3)
l1=paste("Temp = ", round(mean(Gas),1)) #paste is used to combine two or more strings
l1
text(0,4,l1)
l1=paste("Avg Temp = ", round(mean(Temp),1))
l1
text(5,5,l1,srt=90)
s1= whiteside[Insul=="Before",]
View(s1)
points(s1$Temp , s1$Gas, pch=19, col="blue")

s2= whiteside[Temp< mean(Temp) & Gas <mean(Gas),]
s2
points(s2$Temp , s2$Gas, pch=19, col="red", cex=1.5)

help(par)

opar=par()
par(bg="white", col="red",las=2, mfrow=c(2,2))
plot(Temp,Gas) 
plot(Temp,Gas,type="h")
plot(Insul,Gas,ylab="Gas")
plot(Insul,Temp,ylab="Temp")

par(opar)


#other graph types
View(whiteside)
boxplot(Gas~Insul, col="green")
boxplot(Gas~Insul, col=c("green","orange"), notch=T, horizontal=T)

stripchart(Gas~Insul, pch=19, col=c("green","orange"), method="stack")
stripchart(Gas~Insul, pch=19, col=c("green","orange"), method="jitter")
x=aggregate(Salaries$salary, list(Salaries$rank), mean)
View(x)
pie(x$x, labels=x$Group.1, col=rainbow(3))
dotchart(x$x, labels=x$Group.1, col=rainbow(3))
















