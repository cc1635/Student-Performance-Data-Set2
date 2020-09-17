# Student-Performance-Data-Set2

## Team Members
  1. Akshay Arora:
     - MBA Student with a focus in Marketing & Strategy
     - Currently working with Signify (Philips Lighting) in Product Marketing role (New Jersey)
     - Previous Work Experience: OTC & Pharmaceuticals Marketing (Delhi, India)
     
  2. Chun-Jung Chen (Bruce): 
     - Currently studying Master of Supply Chain Analytics.
     - Graduated in 2019 from the Department of Statistics, Rutgers University.
     - One year experience at Dr. Leonard's Healthcare Corp. as a Marketing Data Analyst.

## Data Source: 
- UCI machine Learning Repository:
  - http://archive.ics.uci.edu/ml/datasets/Student+Performance#

## Data Description:
The dataset contains 395 data, which provides the students' performance on Math and Portuguese, and the basic information about the students including gender, age, family condition such as family size, and parents' education level and career information. We would like to understand if there is a relationship between students' subject performance and different family conditions.


## Problem Statement:
1. Does Father and Mother's education level affect students' grade in Math?
2. Does living in city or countryside influence students' in Math?
3. Does absences really impact students' math score and Portuguese scores?
4. Are Math and Portuguese scores independent?


## Data Dictionary:
 No. | Column Name | Data type | Descriptions
------------ | ------------ | ------------- | -------------
1  | school | binary | student's school (GP - Gabriel Pereira or MS - Mousinho da Silveira)
2  | sex | binary  | student's sex (F - female or M - male)
3  | age | numeric  | from 15 to 20
4  | address|binary|student's home address type (U - urban or R - rural)
5|famsize|binary| family size (LE3 - less or equal to 3 or GT3 - greater than 3)
6| Pstatus|binary|parent's cohabitation status (T - living together or A - apart)
7| Medu|numeric|mother's education (0 - none, 1 - primary education (4th grade), 2 - 5th to 9th grade, 3 - secondary education or 4 - higher education)
8| Fedu|numeric| father's education (0 - none, 1 - primary education (4th grade), 2 - 5th to 9th grade, 3 - secondary education or 4 - higher education)
9| Mjob|nominal| mother's job (teacher, health care related, civil services (e.g. administrative or police), at_home or other)
10| Fjob| nominal|father's job (teacher, health care related, civil services (e.g. administrative or police), at_home or other)
11| reason|nominal| reason to choose this school (close to home, school reputation, course preference, or other)
12| guardian|nominal| student's guardian (mother, father, or other)
13| traveltime| numeric|home to school travel time (1 - <15 min., 2 - 15 to 30 min., 3 - 30 min. to 1 hour, or 4 - >1 hour)
14| studytime|numeric| weekly study time (1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours)
15| failures| numeric|number of past class failures (n if 1<=n<3, else 4)
16|schoolsup| binary|extra educational support (yes or no)
17| famsup| binary|family educational support (yes or no)
18| paid| binary|extra paid classes within the course subject (Math or Portuguese) (yes or no)
19| activities|binary| extra-curricular activities (yes or no)
20| nursery| binary|attended nursery school (yes or no)
21| higher|binary| wants to take higher education (yes or no)
22| internet| binary|Internet access at home (yes or no)
23| romantic | binary| with a romantic relationship (yes or no)
24| famrel |numeric| f quality of family relationships (rom 1 - very bad to 5 - excellent)
25| freetime |numeric|  free time after school (from 1 - very low to 5 - very high)
26| goout | numeric| going out with friends (from 1 - very low to 5 - very high)
27| Dalc | numeric| workday alcohol consumption (from 1 - very low to 5 - very high)
28| Walc |numeric| weekend alcohol consumption ( from 1 - very low to 5 - very high)
29| health | numeric|current health status ( from 1 - very bad to 5 - very good)
30| absences | numeric|number of school absences ( from 0 to 93)
31| G1_M|numeric|first period grade in Math (from 0 to 20)
31| G2_M|numeric|second period grade in Math (from 0 to 20)
32| G3_M|numeric|final grade in Math (from 0 to 20, output target)
33| G1_M|numeric|first period grade in Portuguese (from 0 to 20)
34| G2_M|numeric|second period grade in Portuguese (from 0 to 20)
35| G3_M|numeric|final grade in Portuguese (from 0 to 20, output target)
