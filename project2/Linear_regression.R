# Clear environment
rm(list = ls()) 

######### 1. Import data #########
# set working directory
# setwd("D:/HCMUT/HK202/Statistics and Probabilities/BTL")

# install packages to use
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio)

# import file
grade_csv <- import("grade.csv")

# choose useful information
grade_csv <- subset(grade_csv,select =c(sex,age,studytime,failures,higher,absences,G1,G2,G3))

# show the table after choose subset
head(grade_csv)

######## 2. Data cleaning ########
# Number of data have NA value
sum(is.na(grade_csv))

# Saving the columns that have NA value
remCols = names(which(colSums(is.na(grade_csv)) > 0))

# Removing the columns with NA value
grade_csv <- grade_csv[!is.na(grade_csv[, remCols]),]

# After cleaning number of data have NA value is 0
sum(is.na(grade_csv))


######### 3. Descriptive Statistics #########
### ----- a. Transformation ----- ###

# check correlation between G1~G3 G2~G3 to decide which 
# transformation to use or not use transformation
pairs(G3~G1, grade_csv)
pairs(G3~G2, grade_csv)

### ----- b. Descriptive statistic ----- ###

# ----> For continuous variable <----

# choose column
con_var <- c(7,8,9)
# calculate some statistic value
mean <- apply(grade_csv[,con_var], 2, mean)
medium <- apply(grade_csv[,con_var], 2, median)
sd <- apply(grade_csv[,con_var], 2, sd)
min <- apply(grade_csv[,con_var], 2, min)
max <- apply(grade_csv[,con_var], 2, max)
# put it all data frame
con_table <- t(data.frame(mean,medium,sd,min,max))

con_table



# ----> For discrete variable <----
cat_sex <- table(grade_csv$sex)
cat_age <- table(grade_csv$age)
cat_studytime <- table(grade_csv$studytime)
cat_failures <- table(grade_csv$failures)
cat_higher <- table(grade_csv$higher)
cat_absences <- table(grade_csv$absences)

cat_sex
cat_age
cat_studytime
cat_failures
cat_higher
cat_absences


### ----- c. Plotting graph ----- ###

# hist for G3
grid.arrange(
  ggplot(grade_csv, aes(sex)) + geom_histogram(stat = "count"),
  ggplot(grade_csv, aes(age)) + geom_histogram(stat = "count"),
  ggplot(grade_csv, aes(studytime)) + geom_histogram(stat = "count"),
  ggplot(grade_csv, aes(failures)) + geom_histogram(stat = "count"),
  ggplot(grade_csv, aes(higher)) + geom_histogram(stat = "count"),
  ggplot(grade_csv, aes(absences)) + geom_histogram(stat = "count"),
  ggplot(grade_csv, aes(G1)) + geom_histogram(stat = "count"),
  ggplot(grade_csv, aes(G2)) + geom_histogram(stat = "count"),
  ggplot(grade_csv, aes(G3)) + geom_histogram(stat = "count"),
  
  ncol=3)

# boxplot of G3 for studytime, failures, higher
boxplot(G3~sex, grade_csv, main = "Distribution of G3 for each sex")
boxplot(G3~studytime, grade_csv, main = "Distribution of G3 for each studytime")
boxplot(G3~failures, grade_csv, main = "Distribution of G3 for each failures")
boxplot(G3~higher, grade_csv, main = "Distribution of G3 for each higher")

# pair graph
pairs(G3~G2,grade_csv)
pairs(G3~G1,grade_csv)
pairs(G3~age,grade_csv)
pairs(G3~absences,grade_csv)


# install some package to plot more beautiful
if (!require("gridExtra")) install.packages("gridExtra")
library(gridExtra)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

grid.arrange(
             ggplot(grade_csv, aes(sex)) + geom_histogram(stat = "count"),
             ggplot(grade_csv, aes(age)) + geom_histogram(stat = "count"),
             ggplot(grade_csv, aes(studytime)) + geom_histogram(stat = "count"),
             ggplot(grade_csv, aes(failures)) + geom_histogram(stat = "count"),
             ggplot(grade_csv, aes(higher)) + geom_histogram(stat = "count"),
             ggplot(grade_csv, aes(absences)) + geom_histogram(stat = "count"),
             ggplot(grade_csv, aes(G1)) + geom_histogram(stat = "count"),
             ggplot(grade_csv, aes(G2)) + geom_histogram(stat = "count"),
             ggplot(grade_csv, aes(G3)) + geom_histogram(stat = "count"),
             
             ncol=3)























############ Fitting linear regression line ###############
linearModel <- lm(G3~sex + age + studytime + failures + higher + absences + G1 + G2, data=grade_csv)
summary(linearModel)

linear1 <- lm(G3~ G1 +G2, data=grade_csv)
summary(linear1)

anova(linearModel, linear1)

######## selecting model properly #########
plot(linearModel)
plot(linear1)

####### Predict ##########

## function to check fail or pass
failpass <- function(x){
  if(x >= 10) return("Pass")
  else        return("Fail")
}

## Check G3 column fail or pass and add it to column evaluate
evaluate <- c(apply(grade_csv["G3"], MARGIN = 1, FUN = failpass))
grade_csv <- cbind(grade_csv, evaluate)
View(grade_csv)


## Install package 
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

## create a new table and add predict column to a new table
new_grade <- grade_csv %>% select(sex, age, studytime, failures, higher, absences, G1, G2, G3)
predict_grade <- predict(linear1)
new_grade <- cbind(new_grade, predict_grade)

## check fail or pass of prediction in new table
evaluate <- c(apply(new_grade["predict_grade"], MARGIN = 1, FUN = failpass))
new_grade <- cbind(new_grade, evaluate)


## create a data frame to compare between G3(real data) and predicted value
evaluate1 = prop.table(table(grade_csv$evaluate == "Pass"))
evaluate2 = prop.table(table(new_grade$evaluate == "Pass"))
Output = data.frame(cbind(evaluate1, evaluate2))
colnames(Output)=c("Real","Predicted")
rownames(Output)=c("Fail", "Pass")
Output




