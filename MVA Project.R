setwd("~/Desktop/Supply Chain Anlaytics/Multivariate Analysis/Project/student")

install.packages("ggplot2")
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
str(d3.q1)

summary(d3.q1)


t1 = as.data.frame(table(d3.q1$famsize))
colnames(t1) = c("GroupOf3","F1")


t2 = as.data.frame(table(d3.q1$Pstatus))
colnames(t2) = c("AorT","F2")


t3 = as.data.frame(table(d3.q1$famsup))
colnames(t3) = c("Famsup","F3")

t = cbind(t1,t2, t3)

f = as.data.frame(table(d3.q1$famrel))

barchat = function(d, v, f, xlab, title){
	ggplot(data = d, aes(x = v, y = f)) +
		geom_bar(stat="identity", fill="steelblue") +
		xlab(xlab) +
		ylab("Frequency") +
		labs(title = title) +
		geom_text(aes(label=f), position=position_dodge(width=0.9), vjust=-0.25) +
		theme(axis.title.x = element_text(size=10),
					axis.title.y = element_text(size=10),
					axis.text.x = element_text(size = 10),
					axis.text.y = element_text(size = 10),
					
					plot.title = element_text(hjust = 0.5)
					# legend.title = element_text(size = 10),
					# legend.text = element_text(size = 10),
					# legend.position = c(1,1),
					# legend.justification = c(1,1),
		)
}

### Final Math
var(d3.q1$FinalMath)

fm = ggplot(data = d3.q1, aes(x = FinalMath)) +
		geom_histogram(binwidth = 1, fill = "orange") +

	labs(title = "Final grade in Math") +
	theme(axis.title.x = element_text(size=10),
				axis.title.y = element_text(size=10),
				axis.text.x = element_text(size = 10),
				axis.text.y = element_text(size = 10),
				
				plot.title = element_text(hjust = 0.5)
				# legend.title = element_text(size = 10),
				# legend.text = element_text(size = 10),
				# legend.position = c(1,1),
				# legend.justification = c(1,1),
	)
fm

### Family Size
fs = barchat(t, t$GroupOf3, t$F1, "Less than 3 or More than 3", "Family Size"); fs

### Parents Status
ps = barchat(t, t$AorT, t$F2, xlab = "Away or Living together", "Parents Statis"); ps

### Family Relationship
fr = barchat(f, f$Var1, f$Freq, xlab = "Very Bad                                                            Very Good", "Family Relationship"); fr

### Family Support
famsup = barchat(t, t$Famsup, t$F3, xlab = "Yes or No", "Family Support"); famsup

# Question 2: Does parents' jobs and education level influence students' first period of grade in Math?
##		DV: First Grade in Math
##		ID: parents' education		parents' jobs		

d3.q2  = d3[,c("FirstMath","Medu", "Fedu", "Mjob", "Fjob")]
str(d3.q2)

summary(d3.q2)

d3.q2$Medu = as.factor(d3.q2$Medu)
levels(d3.q2$Medu) = c("none", "PrimaryEdu", "5-9thGrade", "SecEdu", "HigherEdu")

d3.q2$Fedu = as.factor(d3.q2$Fedu)
levels(d3.q2$Fedu) = c("none", "PrimaryEdu", "5-9thGrade", "SecEdu", "HigherEdu")



### First grade in Math
var(d3.q2$FirstMath)

fm = ggplot(data = d3.q2, aes(x = FirstMath)) +
	geom_histogram(binwidth = 1, fill = "purple") +
	
	labs(title = "First grade in Math") +
	theme(axis.title.x = element_text(size=10),
				axis.title.y = element_text(size=10),
				axis.text.x = element_text(size = 10),
				axis.text.y = element_text(size = 10),
				
				plot.title = element_text(hjust = 0.5)
				# legend.title = element_text(size = 10),
				# legend.text = element_text(size = 10),
				# legend.position = c(1,1),
				# legend.justification = c(1,1),
	)
fm

e1 = as.data.frame(table(d3.q2$Medu))
colnames(e1) = c("MEduLevel","F1")


e2 = as.data.frame(table(d3.q2$Fedu))
colnames(e2) = c("FEduLevel","F2")

e = cbind(e1,e2)

j1 = as.data.frame(table(d3.q2$Mjob))
colnames(j1) = c("MJobLevel","F1")

j2 = as.data.frame(table(d3.q2$Fjob))
colnames(j2) = c("FJobLevel","F2")

j = cbind(j1,j2)

### Parents Education
### 		Mother's education
me = barchat(e, e$MEduLevel, e$F1, "Education Level", "Mother's Education"); me

### 		Father's education
fe = barchat(e, e$FEduLevel, e$F2, "Education Level", "Father's Education"); fe


### Parents' Job
### 		Mother's Job
mj = barchat(j, j$MJobLevel, j$F1, "Job Types", "Mother's Job"); mj

### 		Father's Job
fj = barchat(j, j$FJobLevel, j$F2, "Job Types", "Father's Job"); fj



# Question 3: Does student's learning conditions really impact students' final grade math score and Portuguese scores in average?
##		DV: Average of final grade math score and Portuguese scores
##		ID: Internet access		Home to school travel time		romantic relationship		free time after school
##				Alcohol consumption

d3$FinalAvg =  (d3$FinalMath + d3$FinalPort)/2

d3.q3  = d3[,c("FinalAvg","internet", "traveltime", "romantic", "freetime")]
str(d3.q3)

summary(d3.q3)

## Changing travel time to continuous data
v = as.numeric(d3.q3$traveltime)
d3.q3$traveltime = ifelse(v==1,replace(v, which(v %in% 1), sample(1:15, length(which(v %in% 1)), replace = TRUE)),
			 ifelse(v==2,replace(v, which(v %in% 2), sample(16:30, length(which(v %in% 2)), replace = TRUE)),
			 	ifelse(v==3,replace(v, which(v %in% 3), sample(31:60, length(which(v %in% 3)), replace = TRUE)),
			 		ifelse(v==4,replace(v, which(v %in% 4), sample(60:90, length(which(v %in% 4)), replace = TRUE)),v
			 ))))


cov(d3.q3$FinalAvg,d3.q3$traveltime)

### FinalAverage
var(d3.q3$FinalAvg)

qqnorm(d3.q3$FinalAvg, main = "FinalAvg"); qqline(d3.q3$FinalAvg)

### Internet
i = as.data.frame(table(d3.q3$internet))
colnames(i) = c("IsInternet","Freq")

int = barchat(i, i$IsInternet, i$Freq, "", "Internet access at home"); int


##Travel time
var(d3.q3$traveltime)

qqnorm(d3.q3$traveltime, main = "traveltime"); qqline(d3.q3$traveltime)


tt = ggplot(data = d3.q3, aes(x = traveltime)) +
	geom_histogram(binwidth = 10, fill = "purple") +
	
	labs(title = "Home to school travel time") +
	theme(axis.title.x = element_text(size=10),
				axis.title.y = element_text(size=10),
				axis.text.x = element_text(size = 10),
				axis.text.y = element_text(size = 10),
				
				plot.title = element_text(hjust = 0.5)
				# legend.title = element_text(size = 10),
				# legend.text = element_text(size = 10),
				# legend.position = c(1,1),
				# legend.justification = c(1,1),
	)
tt       #very right skew

###### Shapiro test for normality
shapiro.test(d3.q3$traveltime)     #P-value << 0.05. not significantly normal distributed


###### Power transformation
install.packages("car")
library(car)

p = powerTransform(d3.q3$traveltime, family="bcPower")
# ----------- Estimated transformation parameter = 0.1641914 

d3.q3$normtraveltime = d3.q3$traveltime**p$lambda

qqnorm(d3.q3$normtraveltime, main = "traveltime_normal transformation"); qqline(d3.q3$normtraveltime)

ttn = ggplot(data = d3.q3, aes(x = normtraveltime)) +
	geom_histogram(binwidth = 0.1, fill = "purple") +
	
	labs(title = "Home to school travel time (normal transformation)") +
	theme(axis.title.x = element_text(size=10),
				axis.title.y = element_text(size=10),
				axis.text.x = element_text(size = 10),
				axis.text.y = element_text(size = 10),
				
				plot.title = element_text(hjust = 0.5)
				# legend.title = element_text(size = 10),
				# legend.text = element_text(size = 10),
				# legend.position = c(1,1),
				# legend.justification = c(1,1),
	)

ttn   #Looks Normal distributed

##### T test on travel time and internet assess
with(data=d3.q3,t.test(normtraveltime[internet=="yes"],normtraveltime[internet=="no"],var.equal=TRUE))


### Romantic
r = as.data.frame(table(d3.q3$romantic))
colnames(r) = c("IsRomantic","Freq")

rom = barchat(r, r$IsRomantic, r$Freq, "", "Parents with a romantic relationship"); rom

### Free time (free time after school)
ft = as.data.frame(table(d3.q3$freetime))
colnames(ft) = c("FreeTimeLevel","Freq")

ftime = barchat(ft, ft$FreeTimeLevel, ft$Freq, "Very Bad                                                            Very Good", "Family Relationship"); ftime

