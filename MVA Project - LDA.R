setwd("~/Desktop/Supply Chain Anlaytics/Multivariate Analysis/Project/student")
set.seed(20201108)
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

d3.q1$famsize = as.factor(d3.q1$famsize)
d3.q1$Pstatus = as.factor(d3.q1$Pstatus)
d3.q1$famsup = as.factor(d3.q1$famsup)
d3.q1$famrel  = as.factor(d3.q1$famrel)

levels(d3.q1$famsize) = c(1,2)
levels(d3.q1$Pstatus) = c(1,0)
levels(d3.q1$famsup) = c(0,1)

d3.q1$famsize = as.numeric(d3.q1$famsize)
d3.q1$famsize = ifelse(d3.q1$famsize==2,replace(d3.q1$famsize, which(d3.q1$famsize %in% 2), sample(1:3, length(which(d3.q1$famsize %in% 2)), replace = TRUE)),replace(d3.q1$famsize, which(d3.q1$famsize %in% 1), sample(3:10, length(which(d3.q1$famsize %in% 1)), replace = TRUE)))


## Convert Final math
q = quantile(FinalMath)

d3.q1$FinalMath = ifelse(FinalMath >= q[1] & FinalMath <= q[2], 'Q1',
			 ifelse(FinalMath > q[2] & FinalMath <= q[3], 'Q2',
			 			 ifelse(FinalMath >= q[3] & FinalMath <= q[4], 'Q3',
			 			 			 ifelse(FinalMath >= q[4], 'Q4',NA))))

d3.q1$FinalMath = as.factor(d3.q1$FinalMath)

d3.q1.train = d3.q1[sample(nrow(d3.q1),nrow(d3.q1)*0.75),]
d3.q1.test = d3.q1[-sample(nrow(d3.q1),nrow(d3.q1)*0.75),]

d3.q1.lda = lda(formula = d3.q1.train$FinalMath ~ ., data = d3.q1.train)
d3.q1.lda
summary(d3.q1.lda)
plot(d3.q1.lda)
plot(d3.q1.lda, dimen = 1, type = "b")

d3.q1.lda.predict = predict(d3.q1.lda, newdata = d3.q1.test)
d3.q1.lda.predict.posteriors = as.data.frame(d3.q1.lda.predict$posterior)

#install.packages('klaR')
library(klaR)
# Lets focus on accuracy. Table function
lda.train.finalmath = predict(d3.q1.lda)
d3.q1.train$lda = lda.train.finalmath$class
table(d3.q1.train$lda,d3.q1.train$FinalMath)
# running accuracy on the training set shows how good the model is. It is not an indication of "true" accuracy. We will use the test set to approximate accuracy
lda.test.finalmath = predict(d3.q1.lda,d3.q1.test)
d3.q1.test$lda = lda.test.finalmath$class
table(d3.q1.test$lda,d3.q1.test$FinalMath)

detach(d3.q1)


### Question 2
attach(d3.q2)

d3.q2  = d3[,c("FirstMath","Medu", "Fedu", "Mjob", "Fjob")]
head(d3.q2)
unique(d3.q2$Medu)
levels(d3.q2$Medu) = c(1,2,3,4,5)
unique(d3.q2$Fedu)
levels(d3.q2$Fedu) = c(1,2,3,4,5)

d3.q2$Mjob = as.factor(d3.q2$Mjob)
unique(d3.q2$Mjob)
levels(d3.q2$Mjob) = c(1,2,3,4,5)
d3.q2$Fjob = as.factor(d3.q2$Fjob)
unique(d3.q2$Fjob)
levels(d3.q2$Fjob) = c(1,2,3,4,5)

str(d3.q2)
head(d3.q2)

d3.q2$Medu = as.factor(d3.q2$Medu)

d3.q2$Fedu = as.factor(d3.q2$Fedu)


## Convert First math
q = quantile(FirstMath)

d3.q2$FirstMath = ifelse(FirstMath >= q[1] & FirstMath <= q[2], 'Q1',
												 ifelse(FirstMath > q[2] & FirstMath <= q[3], 'Q2',
												 			 ifelse(FirstMath >= q[3] & FirstMath <= q[4], 'Q3',
												 			 			 ifelse(FirstMath >= q[4], 'Q4',NA))))

d3.q2$FirstMath = as.factor(d3.q$FirstMath)

d3.q2.train = d3.q2[sample(nrow(d3.q2),nrow(d3.q2)*0.75),]
d3.q2.test = d3.q2[-sample(nrow(d3.q2),nrow(d3.q2)*0.75),]

d3.q2.lda = lda(formula = d3.q2.train$FirstMath ~ ., data = d3.q2.train)
d3.q2.lda
summary(d3.q2.lda)
plot(d3.q2.lda)
plot(d3.q2.lda, dimen = 1, type = "b")

d3.q2.lda.predict = predict(d3.q2.lda, newdata = d3.q2.test)
d3.q2.lda.predict.posteriors = as.data.frame(d3.q2.lda.predict$posterior)

#install.packages('klaR')
library(klaR)
# Lets focus on accuracy. Table function
lda.train.firstmath = predict(d3.q2.lda)
d3.q2.train$lda = lda.train.firstmath$class
table(d3.q2.train$lda,d3.q2.train$FirstMath)
# running accuracy on the training set shows how good the model is. It is not an indication of "true" accuracy. We will use the test set to approximate accuracy
lda.test.firstmath <- predict(d3.q2.lda,d3.q2.test)
d3.q2.test$lda <- lda.test.firstmath$class
table(d3.q2.test$lda,d3.q2.test$FirstMath)

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

d3.q3$romantic[d3.q3$romantic == "yes"] = 1
d3.q3$romantic[d3.q3$romantic == "no"] = 0

levels(d3.q3$internet) = c(0,1)
levels(d3.q3$romantic) = c(0,1)


d3.q3$internet = as.factor(d3.q3$internet)
d3.q3$romantic = as.factor(d3.q3$romantic)

d3.q3 = d3.q3[,c(1,2,4,5,6)]

## Convert First math
q = quantile(FinalAvg)

d3.q3$FinalAvg = ifelse(FinalAvg >= q[1] & FinalAvg <= q[2], 'Q1',
												 ifelse(FinalAvg > q[2] & FinalAvg <= q[3], 'Q2',
												 			 ifelse(FinalAvg >= q[3] & FinalAvg <= q[4], 'Q3',
												 			 			 ifelse(FinalAvg >= q[4], 'Q4',NA))))

d3.q3$FinalAvg = as.factor(d3.q3$FinalAvg)

d3.q3.train = d3.q3[sample(nrow(d3.q3),nrow(d3.q3)*0.75),]
d3.q3.test = d3.q3[-sample(nrow(d3.q3),nrow(d3.q3)*0.75),]

d3.q3.lda = lda(formula = d3.q3.train$FinalAvg ~ ., data = d3.q3.train)
d3.q3.lda
summary(d3.q3.lda)
plot(d3.q3.lda)
plot(d3.q3.lda, dimen = 1, type = "b")

d3.q3.lda.predict = predict(d3.q3.lda, newdata = d3.q3.test)
d3.q3.lda.predict.posteriors = as.data.frame(d3.q3.lda.predict$posterior)

#install.packages('klaR')
library(klaR)
# Lets focus on accuracy. Table function
lda.train.FinalAvg = predict(d3.q3.lda)
d3.q3.train$lda = lda.train.FinalAvg$class
table(d3.q3.train$lda,d3.q3.train$FinalAvg)
# running accuracy on the training set shows how good the model is. It is not an indication of "true" accuracy. We will use the test set to approximate accuracy
lda.test.FinalAvg <- predict(d3.q3.lda,d3.q3.test)
d3.q3.test$lda <- lda.test.FinalAvg$class
table(d3.q3.test$lda,d3.q3.test$FinalAvg)

detach(d3.q3)

