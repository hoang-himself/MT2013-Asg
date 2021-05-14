#Import the data
Ques3_dataset <- read.csv(file = "ex3.csv")
head(Ques3_dataset)

#Create a two-way ANOVA
av <- aov(value ~ block + column,data=Ques3_dataset)

#Summarize two-way ANOVA
print(summary(av))

#Check and output if we can reject h0 or not
ifelse(
  summary(av)[[1]][["Pr(>F)"]] > qf(0.99,3,9),
  print("Reject H0 since F >", qf(0.99,3,9)),
  print("Fail to reject H0 since F <", qf(0.99,3,9))
)