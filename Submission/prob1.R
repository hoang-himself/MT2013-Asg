rm(list = ls())
if (!require("readxl"))
  install.packages("readxl")
library("readxl")



# Import the data
data_file <- read_excel("data.xlsx", sheet = "Sheet2")

# Built-in one-way ANOVA
av = aov(data_file$value~data_file$group)

# Results
print(summary(av))

############

# Import the data
data_file <- read.csv(file="ex1.csv")

# Extract group names to data frame
fr_gr_names <- data.frame(unique(data_file$group))

# Variables that aid calculations
fr_gr_sums <- aggregate(data_file$value~data_file$group,
                        fr_gr_names, sum)
fr_gr_quans <- aggregate(data_file$value~data_file$group,
                         fr_gr_names, length)
gr_sums <- fr_gr_sums$`data_file$value`
gr_quans <- fr_gr_quans$`data_file$value`

# Degree of freedom
N <- length(data_file$value)
F <- length(unique(data_file$group))
df_f <- F - 1
df_e <- N - F
df_t <- N - 1

# Sums of squares
SST <- sum(data_file$value^2) - sum(data_file$value)^2 / N
SSF <- sum(gr_sums^2 / gr_quans) - sum(data_file$value)^2 / N
SSE <- SST - SSF

# Means of sums of squares
MSF <- SSF / df_f
MSE <- SSE / df_e

F <- MSF / MSE

# Console output
cat("Sums", gr_sums, "\n")
cat("Averages", gr_sums / gr_quans, "\n")
cat("Overall sum", sum(gr_sums), "\n")
cat("Overall mean", mean(gr_sums / gr_quans), "\n")
cat("df_f", df_f, "\n")
cat("df_e", df_e, "\n")
cat("df_t", df_t, "\n")
cat("SSF", SSF, "\n")
cat("SSE", SSE, "\n")
cat("SST", SST, "\n")
cat("MSF", MSF, "\n")
cat("MSE", MSE, "\n")
cat("F", F, "\n")

cat("F Crit", qf(0.97,4,24), "\n")
