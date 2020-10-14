setwd("~/Desktop/Supply Chain Anlaytics/Multivariate Analysis/Project/student")
set.seed(20201013)
#install.packages("ggplot2")
library(ggplot2)

d1=read.table("student-mat.csv",sep=";",header=TRUE)
colnames(d1)[31] = "FirstMath"
colnames(d1)[32] = "SecondMath"
colnames(d1)[33] = "FinalMath"
d1 = d1[,c("school","sex","age","address","famsize","Pstatus","Medu", "Fedu", "Mjob", "Fjob", "internet"
					 ,"guardian","traveltime","famsup","romantic","famrel","freetime","FirstMath","SecondMath","FinalMath")]


d2=read.table("student-por.csv",sep=";",header=TRUE)
colnames(d2)[31] = "FirstPort"
colnames(d2)[32] = "SecondPort"
colnames(d2)[33] = "FinalPort"
d2 = d2[,c("school","sex","age","address","famsize","Pstatus","Medu", "Fedu", "Mjob", "Fjob", "internet"
					 ,"guardian","traveltime","famsup","romantic","famrel","freetime","FirstPort","SecondPort","FinalPort")]


d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu", "Fedu", "Mjob", "Fjob", "internet"
										,"guardian","traveltime","famsup","romantic","famrel","freetime"))
print(nrow(d3)) # 370 students

colnames(d3)


str(d3)

#convert data type
d3$traveltime = as.factor(d3$traveltime)


# Descriptive Statistics

summary(d3)

# mode function.
getmode = function(v) {
	uniqv = unique(v)
	uniqv[which.max(tabulate(match(v, uniqv)))]
}

apply(d3[,c("school","sex","address","famsize","Pstatus","Medu", "Fedu", "Mjob", "Fjob", "internet"
						,"guardian","traveltime","famsup","romantic","famrel","freetime")], 2, getmode)

# Question 1: Deos Family conditions affect students' final grade in Math?
##		DV: Final Grade in Math
##		ID: Family Size		Parents' Cohabitation		Quality of family relationships		family's educational support

d3.q1  = d3[,c("FinalMath","famsize", "Pstatus", "famrel", "famsup")]
d3.q2  = d3[,c("FirstMath","Medu", "Fedu", "Mjob", "Fjob")]

d3$FinalAvg =  (d3$FinalMath + d3$FinalPort)/2

d3.q3  = d3[,c("FinalAvg","internet", "traveltime", "romantic", "freetime")]

## Changing travel time to continuous data
v = as.numeric(d3.q3$traveltime)
d3.q3$traveltime = ifelse(v==1,replace(v, which(v %in% 1), sample(1:15, length(which(v %in% 1)), replace = TRUE)),
													ifelse(v==2,replace(v, which(v %in% 2), sample(16:30, length(which(v %in% 2)), replace = TRUE)),
																 ifelse(v==3,replace(v, which(v %in% 3), sample(31:60, length(which(v %in% 3)), replace = TRUE)),
																 			 ifelse(v==4,replace(v, which(v %in% 4), sample(60:90, length(which(v %in% 4)), replace = TRUE)),v
																 			 ))))


## Cluster Analysis
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms

install.packages('factoextra')
library(factoextra) # clustering algorithms & visualization

### Question 1
levels(d3.q1$famsize) = c(1,0)
levels(d3.q1$Pstatus) = c(1,0)
levels(d3.q1$famsup) = c(0,1)

d3.q1$famsize = as.numeric(d3.q1$famsize)

d3.q1$Pstatus = as.numeric(d3.q1$Pstatus)

d3.q1$famsup = as.numeric(d3.q1$famsup)

str(d3.q1)

scale.d3q1 = scale(d3.q1[,-1])

dist.d3q1 = dist(scale.d3q1, method="euclidean")
cluster.d3q1 = hclust(dist.d3q1, method = "single")

plot(cluster.d3q1)		#Clearly, we can't use dendrogram since there is too many rows


### Kmeans

### k = 2,3,4,5

(q1.k2 = kmeans(scale.d3q1,2,nstart = 10))
(q1.varPctk2 = round(100*(1 - q1.k2$betweenss/q1.k2$totss),1))

(q1.k3 = kmeans(scale.d3q1,3,nstart = 10))
(q1.varPctk3 = round(100*(1 - q1.k3$betweenss/q1.k3$totss),1))

(q1.k4 = kmeans(scale.d3q1,4,nstart = 10))
(q1.varPctk4 = round(100*(1 - q1.k4$betweenss/q1.k4$totss),1))

(q1.k5 = kmeans(scale.d3q1,5,nstart = 10))
(q1.varPctk5 = round(100*(1 - q1.k5$betweenss/q1.k5$totss),1))

(q1.k6 = kmeans(scale.d3q1,6,nstart = 10))
(q1.varPctk6 = round(100*(1 - q1.k6$betweenss/q1.k6$totss),1))

q1.Varlist = c(q1.varPctk2, q1.varPctk3, q1.varPctk4, q1.varPctk5, q1.varPctk6)
(q1.Varlist = as.data.frame(q1.Varlist))

rownames(q1.Varlist)  = paste("clu.Var",2:6,sep="")
colnames(q1.Varlist) = ""
q1.Varlist

### Choose 5 clusters

#fviz_cluster(k5, data = scale.d3q1, ellipse = F, geom = "point")

### Compare for each cluster
# plots to compare
q1.p2 = fviz_cluster(q1.k2, geom = "point", data = scale.d3q1) + ggtitle("k = 2")
q1.p3 = fviz_cluster(q1.k3, geom = "point",  data = scale.d3q1) + ggtitle("k = 3")
q1.p4 = fviz_cluster(q1.k4, geom = "point",  data = scale.d3q1) + ggtitle("k = 4")
q1.p5 = fviz_cluster(q1.k5, data = scale.d3q1, geom = "point") + ggtitle("k = 5")
q1.p6 = fviz_cluster(q1.k6, geom = "point",  data = scale.d3q1) + ggtitle("k = 6")

library(gridExtra)
grid.arrange(q1.p2, q1.p3, q1.p4, q1.p5, q1.p6, nrow = 2)

fviz_nbclust(scale.d3q1, kmeans, method = "wss")

q1.grp1 = d3.q1[which(q1.k5$cluster ==1),]; mean(q1.grp1[,1])
q1.grp2 = d3.q1[which(q1.k5$cluster ==2),]; mean(q1.grp2[,1])
q1.grp3 = d3.q1[which(q1.k5$cluster ==3),]; mean(q1.grp3[,1])
q1.grp4 = d3.q1[which(q1.k5$cluster ==4),]; mean(q1.grp4[,1])
q1.grp5 = d3.q1[which(q1.k5$cluster ==5),]; mean(q1.grp5[,1])

q1.grpmean = t(t(as.vector( c(mean(q1.grp1[,1]), mean(q1.grp2[,1]), mean(q1.grp3[,1]), mean(q1.grp4[,1]), mean(q1.grp5[,1])))))
rownames(q1.grpmean) = paste("Group",1:5,sep="")
colnames(q1.grpmean) = "Mean"


### Question 2

unique(d3.q2$Medu)
levels(d3.q2$Medu) = c(1,2,3,4,5)
unique(d3.q2$Fedu)
levels(d3.q2$Fedu) = c(1,2,3,4,5)

unique(d3.q2$Mjob)
levels(d3.q2$Mjob) = c(1,2,3,4,5)
unique(d3.q2$Fjob)
levels(d3.q2$Fjob) = c(1,2,3,4,5)


d3.q2$Medu = as.numeric(d3.q2$Medu)

d3.q2$Fedu = as.numeric(d3.q2$Fedu)

d3.q2$Mjob = as.numeric(d3.q2$Mjob)

d3.q2$Fjob = as.numeric(d3.q2$Fjob)


scale.d3q2 = scale(d3.q2[,-1])

dist.d3q2 = dist(scale.d3q2, method="euclidean")
cluster.d3q2 = hclust(dist.d3q2, method = "single")


### Kmean

### k = 2,3,4,5

(q2.k2 = kmeans(scale.d3q2,2,nstart = 10))
(q2.varPctk2 = round(100*(1 - q2.k2$betweenss/q2.k2$totss),1))

(q2.k3 = kmeans(scale.d3q2,3,nstart = 10))
(q2.varPctk3 = round(100*(1 - q2.k3$betweenss/q2.k3$totss),1))

(q2.k4 = kmeans(scale.d3q2,4,nstart = 10))
(q2.varPctk4 = round(100*(1 - q2.k4$betweenss/q2.k4$totss),1))

(q2.k5 = kmeans(scale.d3q2,5,nstart = 10))
(q2.varPctk5 = round(100*(1 - q2.k5$betweenss/q2.k5$totss),1))

(q2.k6 = kmeans(scale.d3q2,6,nstart = 10))
(q2.varPctk6 = round(100*(1 - q2.k6$betweenss/q2.k6$totss),1))

q2.Varlist = c(q2.varPctk2, q2.varPctk3, q2.varPctk4, q2.varPctk5, q2.varPctk6)
(q2.Varlist = as.data.frame(q2.Varlist))

rownames(q2.Varlist)  = paste("clu.Var",2:6,sep="")
colnames(q2.Varlist) = ""
q2.Varlist

### Choose 5 clusters

### Compare for each cluster
# plots to compare
q2.p2 = fviz_cluster(q2.k2, geom = "point", data = scale.d3q2) + ggtitle("k = 2")
q2.p3 = fviz_cluster(q2.k3, geom = "point",  data = scale.d3q2) + ggtitle("k = 3")
q2.p4 = fviz_cluster(q2.k4, geom = "point",  data = scale.d3q2) + ggtitle("k = 4")
q2.p5 = fviz_cluster(q2.k5, data = scale.d3q2, geom = "point") + ggtitle("k = 5")
q2.p6 = fviz_cluster(q2.k6, geom = "point",  data = scale.d3q2) + ggtitle("k = 6")

library(gridExtra)
grid.arrange(q2.p2, q2.p3, q2.p4, q2.p5, q2.p6, nrow = 2)

fviz_nbclust(scale.d3q2, kmeans, method = "wss")

q2.grp1 = d3.q2[which(q2.k5$cluster ==1),]; mean(q2.grp1[,1])
q2.grp2 = d3.q2[which(q2.k5$cluster ==2),]; mean(q2.grp2[,1])
q2.grp3 = d3.q2[which(q2.k5$cluster ==3),]; mean(q2.grp3[,1])
q2.grp4 = d3.q2[which(q2.k5$cluster ==4),]; mean(q2.grp4[,1])
q2.grp5 = d3.q2[which(q2.k5$cluster ==5),]; mean(q2.grp5[,1])

q2.grpmean = t(t(as.vector( c(mean(q2.grp1[,1]), mean(q2.grp2[,1]), mean(q2.grp3[,1]), mean(q2.grp4[,1]), mean(q2.grp5[,1])))))
rownames(q2.grpmean) = paste("grp",1:5,sep="")
colnames(q2.grpmean) = "Mean"
q2.grpmean


### Question 3


levels(d3.q3$internet) = c(0,1)

levels(d3.q3$romantic) = c(0,1)




d3.q3$internet = as.numeric(d3.q3$internet)

d3.q3$romantic = as.numeric(d3.q3$romantic)

install.packages("car")
library(car)

p = powerTransform(d3.q3$traveltime, family="bcPower")
# ----------- Estimated transformation parameter = 0.1641914 

d3.q3$normtraveltime = d3.q3$traveltime**p$lambda

scale.d3q3 = scale(d3.q3[,c(2,4,5,6)])

dist.d3q3 = dist(scale.d3q3, method="euclidean")
cluster.d3q3 = hclust(dist.d3q3, method = "single")


### Kmean

### k = 2,3,4,5

(q3.k2 = kmeans(scale.d3q3,2,nstart = 10))
(q3.varPctk2 = round(100*(1 - q3.k2$betweenss/q3.k2$totss),1))

(q3.k3 = kmeans(scale.d3q3,3,nstart = 10))
(q3.varPctk3 = round(100*(1 - q3.k3$betweenss/q3.k3$totss),1))

(q3.k4 = kmeans(scale.d3q3,4,nstart = 10))
(q3.varPctk4 = round(100*(1 - q3.k4$betweenss/q3.k4$totss),1))

(q3.k5 = kmeans(scale.d3q3,5,nstart = 10))
(q3.varPctk5 = round(100*(1 - q3.k5$betweenss/q3.k5$totss),1))

(q3.k6 = kmeans(scale.d3q3,6,nstart = 10))
(q3.varPctk6 = round(100*(1 - q3.k6$betweenss/q3.k6$totss),1))

q3.Varlist = c(q3.varPctk2, q3.varPctk3, q3.varPctk4, q3.varPctk5, q3.varPctk6)
(q3.Varlist = as.data.frame(q3.Varlist))

rownames(q3.Varlist)  = paste("clu.Var",2:6,sep="")
colnames(q3.Varlist) = ""
q3.Varlist

### Choose 6 clusters

### Compare for each cluster
# plots to compare
q3.p2 = fviz_cluster(q3.k2, geom = "point", data = scale.d3q3) + ggtitle("k = 2")
q3.p3 = fviz_cluster(q3.k3, geom = "point",  data = scale.d3q3) + ggtitle("k = 3")
q3.p4 = fviz_cluster(q3.k4, geom = "point",  data = scale.d3q3) + ggtitle("k = 4")
q3.p5 = fviz_cluster(q3.k5, data = scale.d3q3, geom = "point") + ggtitle("k = 5")
q3.p6 = fviz_cluster(q3.k6, geom = "point",  data = scale.d3q3) + ggtitle("k = 6")

library(gridExtra)
grid.arrange(q3.p2, q3.p3, q3.p4, q3.p5, q3.p6, nrow = 2)

fviz_nbclust(scale.d3q3, kmeans, method = "wss")


q3.grp1 = d3.q3[which(q3.k5$cluster ==1),]; mean(q3.grp1[,1])
q3.grp2 = d3.q3[which(q3.k5$cluster ==2),]; mean(q3.grp2[,1])
q3.grp3 = d3.q3[which(q3.k5$cluster ==3),]; mean(q3.grp3[,1])
q3.grp4 = d3.q3[which(q3.k5$cluster ==4),]; mean(q3.grp4[,1])
q3.grp5 = d3.q3[which(q3.k5$cluster ==5),]; mean(q3.grp5[,1])
q3.grp5 = d3.q3[which(q3.k6$cluster ==5),]; mean(q3.grp6[,1])

q3.grpmean = t(t(as.vector( c(mean(q3.grp1[,1]), mean(q3.grp2[,1]), mean(q3.grp3[,1]), mean(q3.grp4[,1]), mean(q3.grp5[,1]), mean(q3.grp6[,1])))))
rownames(q3.grpmean) = paste("grp",1:6,sep="")
colnames(q3.grpmean) = "Mean"
q3.grpmean




