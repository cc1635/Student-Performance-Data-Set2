setwd("~/Desktop/Supply Chain Anlaytics/Multivariate Analysis/Project/student")
set.seed(20201029)
#install.packages("ggplot2")
library(ggplot2)
library(psych)

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

## Changing travel time to continuous data
v = as.numeric(d3.q3$traveltime)
d3.q3$traveltime = ifelse(v==1,replace(v, which(v %in% 1), sample(1:15, length(which(v %in% 1)), replace = TRUE)),
													ifelse(v==2,replace(v, which(v %in% 2), sample(16:30, length(which(v %in% 2)), replace = TRUE)),
																 ifelse(v==3,replace(v, which(v %in% 3), sample(31:60, length(which(v %in% 3)), replace = TRUE)),
																 			 ifelse(v==4,replace(v, which(v %in% 4), sample(60:90, length(which(v %in% 4)), replace = TRUE)),v
																 			 ))))


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



## Multiregression Analysis
### Question 1

attach(d3.q1)
head(d3.q1)
str(d3.q1)

levels(d3.q1$famsize) = c(1,2)
levels(d3.q1$Pstatus) = c(1,0)
levels(d3.q1$famsup) = c(0,1)

d3.q1$famsize = as.numeric(d3.q1$famsize)
d3.q1$famsize = ifelse(d3.q1$famsize==2,replace(d3.q1$famsize, which(d3.q1$famsize %in% 2), sample(1:3, length(which(d3.q1$famsize %in% 2)), replace = TRUE)),replace(d3.q1$famsize, which(d3.q1$famsize %in% 1), sample(3:10, length(which(d3.q1$famsize %in% 1)), replace = TRUE)))


FM1 = subset(d3.q1, FinalMath >= mean(FinalMath))
FM2 = subset(d3.q1, FinalMath < mean(FinalMath))

nrow(d3.q1)
nrow(FM1)
nrow(FM2)
nrow(FM1) + nrow(FM2)

## transfer dependent variable to binary

FM1$FinalMath = 1
FM2$FinalMath = 0

FM.data = rbind(FM1, FM2)

nrow(FM.data)

FM.data$FinalMath = as.factor(FM.data$FinalMath)

str(FM.data)
head(FM.data)

xtabs(~ FinalMath + Pstatus, data = FM.data)
xtabs(~ FinalMath + famsup, data = FM.data)

## Simple logistic regression
q1.logreg1 = glm(FinalMath ~ famsup, data = FM.data, family="binomial")
summary(q1.logreg1)

q1.pre1 = data.frame(P.logreg1=logreg1$fitted.values,FamilySupport=FM.data$famsup)
q1.pre1


## Multi logistic regression
q1.logreg2 = glm(FinalMath ~ ., data = FM.data, family="binomial")
summary(q1.logreg2)

q1.pre2 = predict(q1.logreg2,newdata=FM.data,type="response" )
q1.pre2

roc(FM.data$FinalMath, q1.logreg1$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, col="#377eb8", lwd=4, print.auc=TRUE)
# Lets add the other graph
plot.roc(FM.data$FinalMath, q1.logreg2$fitted.values, percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)


detach(d3.q1)




### Question 2
attach(d3.q2)

d3.q2  = d3[,c("FirstMath","Medu", "Fedu", "Mjob", "Fjob")]
unique(d3.q2$Medu)
levels(d3.q2$Medu) = c(1,2,3,4,5)
unique(d3.q2$Fedu)
levels(d3.q2$Fedu) = c(1,2,3,4,5)

unique(d3.q2$Mjob)
levels(d3.q2$Mjob) = c(1,2,3,4,5)
unique(d3.q2$Fjob)
levels(d3.q2$Fjob) = c(1,2,3,4,5)

str(d3.q2)
head(d3.q2)

d3.q2$Medu = as.factor(d3.q2$Medu)

d3.q2$Fedu = as.factor(d3.q2$Fedu)

FM1 = subset(d3.q2, d3.q2$FirstMath >= mean(d3.q2$FirstMath))
FM2 = subset(d3.q2, d3.q2$FirstMath < mean(d3.q2$FirstMath))

FM1$FirstMath = 1
FM2$FirstMath = 0

FM.q2data = rbind(FM1,FM2)
FM.q2data$FirstMath = as.factor(FM.q2data$FirstMath)

str(FM.q2data)

xtabs(~ FirstMath + Medu , data = FM.q2data)
xtabs(~ FirstMath + Fedu, data = FM.q2data)
xtabs(~ FirstMath + Mjob , data = FM.q2data)
xtabs(~ FirstMath + Fjob , data = FM.q2data)


# Simple Logistic regression
q2.logreg1 = glm(FirstMath ~ Mjob, data = FM.q2data, family="binomial")
summary(q2.logreg1)

q2.pre1 = data.frame(P.logreg1=q2.logreg1$fitted.values,MotherJob=FM.q2data$Mjob)
q2.pre1

## Multi logistic regression
q2.logreg2 = glm(FirstMath ~ ., data = FM.q2data, family="binomial")
summary(q2.logreg2)

q2.pre2 = predict(q2.logreg2,newdata=FM.q2data,type="response" )
q2.pre2

roc(FM.q2data$FirstMath, q2.logreg1$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, col="#377eb8", lwd=4, print.auc=TRUE)
# Lets add the other graph
plot.roc(FM.q2data$FirstMath, q2.logreg2$fitted.values, percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)


detach(d3.q2)


### Question 3
attach(d3.q3)

str(d3.q3)
head(d3.q3)

#install.packages("car")
library(car)

p = powerTransform(d3.q3$traveltime, family="bcPower")
# ----------- Estimated transformation parameter = 0.1641914 

d3.q3$normtraveltime = d3.q3$traveltime**p$lambda

d3.q3$internet[d3.q3$internet == "yes"] = 1
d3.q3$internet[d3.q3$internet == "no"] = 0

levels(d3.q3$internet) = c(0,1)
levels(d3.q3$romantic) = c(0,1)

FM1 = subset(d3.q3, d3.q3$FinalAvg >= mean(d3.q3$FinalAvg))
FM2 = subset(d3.q3, d3.q3$FinalAvg < mean(d3.q3$FinalAvg))

FM1$FinalAvg = 1
FM2$FinalAvg = 0

FM.q3data = rbind(FM1,FM2)
FM.q3data$FinalAvg = as.factor(FM.q3data$FinalAvg)

FM.q3data = FM.q3data[,c(1,2,4,5,6)]

str(FM.q3data)

xtabs(~ FinalAvg + internet , data = FM.q3data)
xtabs(~ FinalAvg + romantic , data = FM.q3data)


# Simple Logistic regression
q3.logreg1 = glm(FinalAvg ~ ., data = FM.q3data, family="binomial")
summary(q3.logreg1)

q2.pre1 = data.frame(P.logreg1=q2.logreg1$fitted.values,MotherJob=FM.q2data$Mjob)
q2.pre1

roc(FM.q3data$FinalAvg, q3.logreg1$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, col="#377eb8", lwd=4, print.auc=TRUE)


detach(d3.q3)













