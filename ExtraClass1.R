corn=read.csv("C:/R/Extra Classes/data/Corn.csv")
yield= corn$yield
nitrate=corn$nitrate


f1=function(a0,a1)
{
  err = yield-  (a0 + a1*nitrate)
  terror= sum(err^2) # sum of square errors
  return(terror)
}

res1=mle2(minuslogl = f1, start = list(a0= mean(yield), a1=0))
summary(res1)

lm(yield~ nitrate)
plot(nitrate, yield)
abline(res1, col = "blue")
# regression tries to minimize sum of squared errors
# maybe we can try to minimize just the errors rather than sum of squared errors

fab=function(a0,a1)
{
  err = yield-  (a0 + a1*nitrate)
  terror= sum(abs(err)) # absolute errors
  return(terror)
}

res2=mle2(minuslogl = fab, start = list(a0= mean(yield), a1=0))
summary(res2)

abline(res2, col="red")

fcost=function(a0,a1)
{
  err = yield-  (a0 + a1*nitrate)
  errcost= sum(ifelse(abs(err)< 500, 0, abs(err))) #assuming if error < 500 is okay, but not if  err > 500 ( ur business objective)
  return(errcost)
}

res3=mle2(minuslogl = fcost, start = list(a0= mean(yield), a1=0))
summary(res3)

abline(res3, col="green")


# 80th percentile ( 80 % of the time you want the prediction to be accurate) ( if this is ur business objective)


fquantile=function(a0,a1)
{
  err = yield-  (a0 + a1*nitrate)
  errcost= quantile(abs(err), probs= 0.8)
  return(errcost)
}


res4=mle2(minuslogl = fquantile, start = list(a0= mean(yield), a1=0))
summary(res4)

abline(res4, col="magenta")

#but when you use these kind of functions, you are not sure if it gives an optimum solution (its not stable)

fcost(a0=6148, a1=51)  # checking cost with OLS regression coefficients
fcost(a0= 7549, a1=23)  # when you did the 3rd regression , cost is definitely lower



# giving random values for a0 and a1 and trying to find the least cost
xyz= function()
{
  a0=runif(1, min=5000, max = 10000)
  a1=runif(1, min=20, max = 100)
  y=fcost(a0,a1)
  return(c(a0,a1,y))
}


results= replicate( 20000, xyz())
View(results)

X= which.min(results[3,])
X

results[,X]


xyz= function()
{
  a0=runif(1, min=5000, max = 7000)
  a1=runif(1, min=20, max = 50)
  y=fcost(a0,a1)
  return(c(a0,a1,y))
}

results= replicate( 20000, xyz())
View(results)

X= which.min(results[3,])
X

results[,X]

#Network analysis

#igraph -used for visualization of networks
#statnet - used for intensive analysis on networks

library(igraph)
g1=graph(c(1,2, 1,3, 2,3, 3,5), n = 5)  # default is directional, u can specify if direction is not required
plot(g1)

g2=graph(c(1,2, 1,3 ,2,3 ,3,5), n=5, directed= F)
plot(g2)



tkplot(g2) # more interactive
V(g2) # list of vertices
E(g2)  # list of edges

g2 = graph.empty() + vertices(letters[1:10], color="red") 
g2 = g2 + vertices(letters[11:20], color="blue") 
g2 = g2 + edges(sample(V(g2), 30, replace=TRUE), color="green") 
plot(g2)

sample1=read.csv("C:/R/Extra Classes/data/sample1.txt", header = F)
g3= graph.data.frame(sample1)
tkplot(g3)
V(g3)
E(g3)

sample2=read.csv("C:/R/Extra Classes/data/sample2.txt") # weight in ths datais the strength of the edge
g4=graph.data.frame(sample2)
plot(g4)
edge.attributes(g4)  # gives weights
vertex.attributes(g4)


firenze <- read.csv("C:/R/Extra Classes/data/firenze.csv", row.names=1) #rownames=1 means first column has the row names
gfirenze = as.matrix(firenze)
gfirenze = graph.adjacency(gfirenze,mode = "undirected")
plot(gfirenze)
degree(gfirenze)
closeness(gfirenze)
betweenness(gfirenze)
page.rank(gfirenze)

#ucinet,  pajek - software to create network

karate <- read.graph("http://cneurocvs.rmki.kfki.hu/igraph/karate.net", format="pajek")
plot(karate)

degree(g2) # number of edges incident on a vertex
closeness(g2) #how close is a vertex to others in the network




#Extra class 2 (5/16/2017)

#statnet ( when you work with statmnet make sure you detach igraph)

library(statnet)
detach("package:igraph")

#Reading files


courses= read.csv("C:/R/Extra Classes/data/courses.csv", header=T, row.names=1)
View(courses)
coursesM = as.matrix(courses)
coursesM
coursesN= as.network(coursesM, bipartite = T)
summary(coursesN)
gplot(coursesN, displaylabels = T, gmode = "twomode")

sample1= read.csv("C:/R/Extra Classes/data/sample1.txt", header=F)
sample1M = as.matrix(sample1) 
sample1N = as.network(sample1M,matrix.type = "edgelist",directed = F) 
gplot(sample1N,displaylabels = T,usearrows = F)

p=c(0.7, 0.3)
v=c(0,1)
y=sample(x=v, size=100, replace=T, prob=p)
valM = matrix(y, nrow=10, ncol=10)
View(valM)
valN=as.network(valM, loops=F, directed = F) #loop=F makes diagonal elemnts are 0,
#cuz u dont want a connection  from node to itself
summary(valN)
gplot(valN)


row_proj = coursesM %*% t(coursesM)  # %*% is matrix multiplication
#row_proj is a network bw people. gives how many courses a student has taken or common courses between students
row_proj[,] 
col_proj = t(coursesM) %*% coursesM 
col_proj[,]

sex = c(1,1,1,1,1,0,0,0,0,0) 
valN %v% "gender"=sex  # specifying vertex/nodal properties

levelN = valM
levelN[1,4] = 10 
levelN[1,10]=10 
levelN[4,7]=10 
levelN[7,8]=10 
levelN[1,4] 
valN %e% "strength" = levelN # specifying edge properties


#metrics

gden(valN) 
centralization(valN,betweenness) 
centralization(valN,closeness) 
centralization(valN,degree) 
summary(valN~triangles) #number of triangles in a network


#Analysis
#Monadic
data("faux.magnolia.high")
faux=faux.magnolia.high
gplot(faux)
summary(faux)
list.vertex.attributes(faux)
list.edge.attributes(faux)
get.node.attr(faux,"Grade")
summary(get.node.attr(faux,"Grade"))

# to test if grade is correlated to popularity

x=degree(faux) # number of connections to a node
y=get.node.attr(faux, "Grade")
cbind(x,y)
#monadic cuz ur focus is on node

#Ho no correlation between popularity and grade
tstat = cor(x,y) 
f1 = function() 
  { y1 = sample(y) 
  return(cor(x,y1)) 
  } 
sdist = replicate(10000,f1()) 
plot(density(sdist)) 
abline(v=tstat,col="red") 
pvalue = length(sdist[sdist<tstat|sdist>-tstat])/length(sdist) 
pvalue 
# fail to reject

x = degree(faux) 
y = get.node.attr(faux,"Sex") 
g = data.frame(y,x) 
g1 = g[g$y=="F",2] 
g2 = g[g$y=="M",2] 
t.test(g1,g2)


data(florentine) 
opar = par() 
par(mfrow=c(1,2)) 
gplot(flobusiness,displaylabels = T) 
gplot(flomarriage,displaylabels = T) 
par(opar)

list.vertex.attributes(flobusiness) 
list.edge.attributes(flobusiness) 
list.vertex.attributes(flomarriage) 
list.edge.attributes(flomarriage)

x = betweenness(flomarriage) 
y = get.node.attr(flomarriage,"wealth") 
cor.test(x,y) 
x = degree(flomarriage) 
y = get.node.attr(flomarriage,"wealth") 
cor.test(x,y)
















