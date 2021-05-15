# set working directory
setwd("D:/HCMUT/HK202/Statistics and Probabilities/BTL")

# install packages to use
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio)

# # Load previouse working space
# load("D:/HCMUT/HK202/Statistics and Probabilities/BTL/.RData")

# import file
grade_csv <- import("grade.csv")

# show the first 6 rows data
head(grade_csv)

# choose useful information
grade_csv <- subset(grade_csv,select =c(sex,age,studytime,failures,higher,absences,G1,G2,G3))

# show the table after choose subset
head(grade_csv)

# Number of data has NA value
sum(is.na(grade_csv))

# check which column has NA value
names(which(colSums(is.na(grade_csv)) > 0))

# Only column G2 has NA value
# Replace the value by the mean of this column
grade_csv[is.na(grade_csv[,"G2"]), "G2"] <- mean(grade_csv[,"G2"], na.rm = TRUE)


# After cleaning number of data has NA value is 0
sum(is.na(grade_csv))

head(grade_csv)

######### Descriptive Statistics #########

# Descriptive statistic for continuous variable
con_var <- c(6,7,8,9)

mean <- apply(grade_csv[,con_var], 2, mean)
medium <- apply(grade_csv[,con_var], 2, median)
sd <- apply(grade_csv[,con_var], 2, sd)
min <- apply(grade_csv[,con_var], 2, min)
max <- apply(grade_csv[,con_var], 2, max)

con_table <- t(data.frame(mean,medium,sd,min,max))

con_table

# Descriptive statistic for categorical variable
cat_sex <- table(grade_csv$sex)
cat_age <- table(grade_csv$age)
cat_studytime <- table(grade_csv$studytime)
cat_failures <- table(grade_csv$failures)
cat_higher <- table(grade_csv$higher)


cat_sex
cat_age
cat_studytime
cat_failures
cat_higher


######### Plotting graph #########

# hist for G3
hist(grade_csv$G3, main="Distribution of G3", xlab = "Score", breaks = 20)

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


############ Fitting linear regression line ###############
linearModel <- lm(G3~sex + age + studytime + failures + higher + absences + G1 + G2, data=grade_csv)
summary(linearModel)

######## selecting model properly #########

## model with all dependent variable
modelAll <- lm(G3~sex + age + studytime + failures + higher + absences + G1 + G2, data=grade_csv)

## model reject sex, studytime, failures, higher
modelreject <- lm(G3 ~ age + absences + G1 + G2, data = grade_csv)
summary(modelreject)
anova(modelAll, modelreject)

## model reject "higher" variable
model_without_higher <- lm(G3~sex + age + studytime + failures + absences + G1 + G2, data=grade_csv)
summary(model_without_higher)
anova(modelAll,model_without_higher)


## model reject "failures" variable
model_without_failures_higher <- lm(G3~sex + age + studytime + absences + G1 + G2, data=grade_csv)
anova(model_without_higher, model_without_failures_higher)

summary(model_without_failures_higher)

plot(model_without_failures_higher)

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
predict_grade <- predict(model_without_failures_higher)
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

# Clear environment
rm(list = ls())

