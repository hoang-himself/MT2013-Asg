rm(list = ls())

if (!require("readxl"))
  install.packages("readxl")
library("readxl")

#Import the data
Ques3_dataset <- read_xlsx("data.xlsx", sheet = "Sheet4")

#Create a two-way ANOVA
av <- aov(value ~ day_of_week + highschool,data=Ques3_dataset)

#Summarize two-way ANOVA
print(summary(av))

#Check critical value
print(qf(0.99,3,9))
