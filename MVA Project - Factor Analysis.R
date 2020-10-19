setwd("~/Desktop/Supply Chain Anlaytics/Multivariate Analysis/Project/student")
set.seed(20201018)
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



## Factor Analysis
### Question 1
levels(d3.q1$famsize) = c(1,0)
levels(d3.q1$Pstatus) = c(1,0)
levels(d3.q1$famsup) = c(0,1)

d3.q1$famsize = as.numeric(d3.q1$famsize)

d3.q1$Pstatus = as.numeric(d3.q1$Pstatus)

d3.q1$famsup = as.numeric(d3.q1$famsup)

str(d3.q1)

d3q1.pc = principal(d3.q1[-1], nfactors=3, rotate="varimax")
d3q1.pc
round(d3q1.pc$values, 3)
d3q1.pc$loadings

# Communalities
d3q1.pc$communality
# Rotated factor scores
d3q1.pc$scores

# Factor recommendation
fa.parallel(d3.q1[-1], show.legend = T) 

# Correlations within Factors
fa.plot(d3q1.pc) 

# Visualize the relationship
fa.diagram(d3q1.pc) 

# Factor recommendations for a simple structure
vss(d3.q1[-1]) 



### Question 2
d3.q2  = d3[,c("FirstMath","Medu", "Fedu", "Mjob", "Fjob")]
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


d3q2.pc = principal(d3.q2[-1], nfactors=3, rotate="varimax")
d3q2.pc
round(d3q2.pc$values, 3)
d3q2.pc$loadings

# Communalities
d3q2.pc$communality
# Rotated factor scores
d3q2.pc$scores

# Factor recommendation
fa.parallel(d3.q2[-1], show.legend = T) 

# Correlations within Factors
fa.plot(d3q2.pc) 

# Visualize the relationship
fa.diagram(d3q2.pc) 

# Factor recommendations for a simple structure
vss(d3.q2[-1]) 


### Question 3


levels(d3.q3$internet) = c(0,1)

levels(d3.q3$romantic) = c(0,1)




d3.q3$internet = as.numeric(d3.q3$internet)

d3.q3$romantic = as.numeric(d3.q3$romantic)

#install.packages("car")
library(car)

p = powerTransform(d3.q3$traveltime, family="bcPower")
# ----------- Estimated transformation parameter = 0.1641914 

d3.q3$normtraveltime = d3.q3$traveltime**p$lambda

d3q3.pc = principal(d3.q3[,c(2,4,5,6)], nfactors=3, rotate="varimax")
d3q3.pc
round(d3q3.pc$values, 3)
d3q3.pc$loadings


# Communalities
d3q3.pc$communality
# Rotated factor scores
d3q3.pc$scores

# Factor recommendation
fa.parallel(d3.q3[,c(2,4,5,6)], show.legend = T) 

# Correlations within Factors
fa.plot(d3q3.pc) 

# Visualize the relationship
fa.diagram(d3q3.pc) 

# Factor recommendations for a simple structure
vss(d3.q3[,c(2,4,5,6)]) 






