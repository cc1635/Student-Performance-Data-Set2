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



## Multiregression Analysis
### Question 1
levels(d3.q1$famsize) = c(1,2)
levels(d3.q1$Pstatus) = c(1,0)
levels(d3.q1$famsup) = c(0,1)

d3.q1$famsize = as.numeric(d3.q1$famsize)
d3.q1$famsize = ifelse(d3.q1$famsize==2,replace(d3.q1$famsize, which(d3.q1$famsize %in% 2), sample(1:3, length(which(d3.q1$famsize %in% 2)), replace = TRUE)),replace(d3.q1$famsize, which(d3.q1$famsize %in% 1), sample(3:10, length(which(d3.q1$famsize %in% 1)), replace = TRUE)))

str(d3.q1)
head(d3.q1)
attach(d3.q1)


GGally::ggpairs(data=d3.q1)

## Transformation
#install.packages("car")
library(car)

p1 = powerTransform(d3.q1$famsize, family="bcPower")
# ----------- Estimated transformation parameter = 0.1641914 

d3.q1$normfs = d3.q1$famsize**p1$lambda
GGally::ggpairs(data=d3.q1)


mod1 = lm(FinalMath~famsize + Pstatus + famrel + famsup, data = d3.q1)
summary(mod1)
plot(mod1)

mod2 = lm(FinalMath~famsize, data = d3.q1)
summary(mod2)
plot(mod2)


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

mod2.1 = lm(FirstMath~Medu +Fedu +Mjob +Fjob, data = d3.q2)
summary(mod2.1)
plot(mod2.1)

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

mod3.1 = lm(FinalAvg~internet +romantic +freetime +normtraveltime, data = d3.q3)
summary(mod3.1)
plot(mod3.1)

mod3.2 = lm(FinalAvg~internet +romantic +normtraveltime, data = d3.q3)
summary(mod3.2)
plot(mod3.2)

detach(d3.q3)





