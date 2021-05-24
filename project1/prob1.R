# Clear environment
rm(list = ls())

# Import the data
data_file <- read.csv(file = "resources/ex1.csv")

# Built-in one-way ANOVA
av = aov(data_file$value ~ data_file$group)

# Results
print(summary(av))

############

# Clear environment
rm(list = ls())

# Import the data
data_file <- read.csv(file = "resources/ex1.csv")

# Extract group names to data frame
fr_gr_names <- data.frame(unique(data_file$group))

# Variables that aid calculations
fr_gr_sums <- aggregate(data_file$value ~ data_file$group,
                        fr_gr_names, sum)
fr_gr_quans <- aggregate(data_file$value ~ data_file$group,
                         fr_gr_names, length)
gr_sums <- fr_gr_sums$`data_file$value`
gr_quans <- fr_gr_quans$`data_file$value`

# Degree of freedom
N <- length(data_file$value)
i <- length(unique(data_file$group))
df_tr <- i - 1
df_e <- N - i
df_t <- N - 1

# Sums of squares
SST <- sum(data_file$value^2) - sum(data_file$value)^2 / N
SSTr <- sum(gr_sums^2 / gr_quans) - sum(data_file$value)^2 / N
SSE <- SST - SSTr

# Means of squares
MSTr <- SSTr / df_tr
MSE <- SSE / df_e

F <- MSTr / MSE

# Console output
cat("Sums", gr_sums, "\n")
cat("Averages", gr_sums / gr_quans, "\n")
cat("Overall sum", sum(gr_sums), "\n")
cat("Overall mean", mean(gr_sums / gr_quans), "\n")
cat("df_tr", df_tr, "\n")
cat("df_e", df_e, "\n")
cat("df_t", df_t, "\n")
cat("SSTr", SSTr, "\n")
cat("SSE", SSE, "\n")
cat("SST", SST, "\n")
cat("MSTr", MSTr, "\n")
cat("MSE", MSE, "\n")
cat("F", F, "\n")
