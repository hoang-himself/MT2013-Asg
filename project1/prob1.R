# # Import the data
# data_file <- read.csv(file="ex1.csv")

# # Built-in one-way ANOVA
# av = aov(data_file$value~data_file$group)

# # Results
# print(summary(av))
# print(paste("F Crit", qf(0.97,4,24)))

# ifelse(
#   summary(av)[[1]][["Pr(>F)"]] > qf(0.97,4,24),
#   print("Reject H0 by comparing with critical point"),
#   print("Fail to reject H0 by comparing with critical point")
# )

# Import the data
data_file <- read.csv(file="ex1.csv")
group_names <- unique(data_file$group)

# Assign the number of observations and columns
N <- length(data_file$value)
C <- length(unique(data_file$group))

#
group_means <- aggregate(data_file$value~data_file$group, data=data.frame(group_names), mean)
total_mean <- mean(group_means$`data_file$value`)

#
df_c <- C - 1
df_e <- N - C
df_t <- N - 1