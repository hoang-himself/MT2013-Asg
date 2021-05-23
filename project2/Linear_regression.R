# Clear environment
rm(list = ls())

######### 1. Import data #########
# Set working directory
# setwd("D:/HCMUT/HK202/Statistics and Probabilities/BTL")

# Install packages to use
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(pacman, rio)

# Import file
grade_csv <- import("grade.csv")

# Choose useful information
grade_csv <-
  subset(grade_csv,
         select = c(sex, age, studytime, failures, higher, absences, G1, G2, G3))

# Show the table after choose subset
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

# Check correlation between G1~G3 G2~G3 to decide which
# transformation to use or not use transformation
pairs(G3 ~ G1, grade_csv)
pairs(G3 ~ G2, grade_csv)

### ----- b. Descriptive statistic ----- ###

# ----> For continuous variable <----
# Choose column
con_var <- c(6, 7, 8, 9)
# Calculate some statistic value
mean <- apply(grade_csv[, con_var], 2, mean)
median <- apply(grade_csv[, con_var], 2, median)
sd <- apply(grade_csv[, con_var], 2, sd)
min <- apply(grade_csv[, con_var], 2, min)
max <- apply(grade_csv[, con_var], 2, max)
# Put it all in data frame
con_table <- t(data.frame(mean, median, sd, min, max))

con_table



# ----> For categorical variable <----
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


### ----- c. Plotting graph ----- ###

# Install some package to plot more beautiful
if (!require("gridExtra"))
  install.packages("gridExtra")
library(gridExtra)
if (!require("ggplot2"))
  install.packages("ggplot2")
library(ggplot2)

# Histogram plot
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
  
  ncol = 3
)


# Box plot of G3 for sex, age, studytime, failures, higher
grid.arrange(
  ggplot(grade_csv, aes(x = as.character(sex), y = G3)) + geom_boxplot() + scale_x_discrete(name =
                                                                                              "Sex"),
  ggplot(grade_csv, aes(x = as.character(age), y = G3)) + geom_boxplot() + scale_x_discrete(name =
                                                                                              "Age"),
  ggplot(grade_csv, aes(x = as.character(studytime), y = G3)) + geom_boxplot() + scale_x_discrete(name =
                                                                                                    "Study time"),
  ggplot(grade_csv, aes(x = as.character(failures), y = G3)) + geom_boxplot() + scale_x_discrete(name =
                                                                                                   "Failures"),
  ggplot(grade_csv, aes(x = as.character(higher), y = G3)) + geom_boxplot() + scale_x_discrete(name =
                                                                                                 "Higher"),
  ncol = 2
)

# Pair graph
pairs(G3 ~ G2, grade_csv)
pairs(G3 ~ G1, grade_csv)
pairs(G3 ~ age, grade_csv)
pairs(G3 ~ absences, grade_csv)




############ 4. Fitting linear regression model ###############

####### --------> create model full factor <-----------
M_all <-
  lm(G3 ~ sex + age + studytime + failures + higher + absences + G1 + G2, data = grade_csv)
summary(M_all)


####### -------> model just contain age, absences, G1, G2 <----------
M1 <- lm(G3 ~ age + absences + G1 + G2, data = grade_csv)
summary(M1)
# anova between M_all and M1
anova(M_all, M1) # there no difference between M_all and M1 when we remove some factor
# -> choose M1 because it simpler than M_all


####### ---------> from model M1 we remove factor age <---------
M2 <- lm(G3 ~ absences + G1 + G2, data = grade_csv)
summary(M2)
# anova between M1 and M2
anova(M1, M2) # there difference between M1 and M2 when we remove age factor
### ===> age affect the final grade
### ================>>>> decided to choose model M1 <<<<================ ###

## plot the residual plot
plot(M1)




####### 5. Prediction ##########

## we want to create a function to check fail or pass of student
failpass <- function(x) {
  if (x >= 10)
    return("Pass") # x >= 10 Pass
  else
    return("Fail") # x < 10 Fail
}

## create a new table and add predict column to a new table
new_grade <- grade_csv
predict_G3 <- predict(M1)
new_grade <- cbind(new_grade, predict_G3)
## check fail or pass of prediction in new table
predict_evaluate <-
  c(apply(new_grade["predict_G3"], MARGIN = 1, FUN = failpass))
new_grade <- cbind(new_grade, predict_evaluate)


## Check G3 column fail or pass and add it to column evaluate
evaluate <- c(apply(grade_csv["G3"], MARGIN = 1, FUN = failpass))
grade_csv <- cbind(grade_csv, evaluate)

## create a data frame to compare between G3(real data) and predicted value
evaluate1 = prop.table(table(grade_csv$evaluate == "Pass"))
evaluate2 = prop.table(table(new_grade$predict_evaluate == "Pass"))
Output = data.frame(cbind(evaluate1, evaluate2))
colnames(Output) = c("Real", "Predicted")
rownames(Output) = c("Fail", "Pass")
Output
