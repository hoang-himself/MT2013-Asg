# Clear environment
rm(list = ls())

# Import the data
Ques3_dataset <- read.csv(file = "resources/ex3.csv")
print(Ques3_dataset)

# Create a two-way ANOVA
av <- aov(value ~ day_of_week + highschool, data = Ques3_dataset)

# Summarize two-way ANOVA
print(summary(av))

# Check critical value
print(qf(0.99, 3, 9))
