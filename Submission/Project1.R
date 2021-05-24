# ================================ Problem1 =================================


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

rm(list = ls())
if (!require("readxl"))
  install.packages("readxl")
library("readxl")

# Import the data
data_file <- read_excel("data.xlsx", sheet = "Sheet2")

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







# ================================ Problem2 =================================


rm(list = ls())

if (!require("readxl"))
  install.packages("readxl")
library("readxl")

# Import the data
income <- read_xlsx("data.xlsx", sheet = "Sheet3", col_names = FALSE, col_types = NULL)
colnames(income) = c("0-1","1-2","2-3","3-4","4-6",">6")
rownames(income) = c("40-50","50-60")
data <- as.matrix(income)

# Computing Chi-square
chisq <- chisq.test(data) 

# Print observed counts & expected counts
print(chisq$observed)
print(round(chisq$expected, 2))
print(chisq)

# Retrieving value
alpha = 0.05
X_squared = chisq$statistic # Statistic
df = chisq$parameter        # Degree of freedom
pval = chisq$p.value        # P-value
c = qchisq(1 - alpha, df)    # Computing critical point

# Check for rejection by comparing with critical point
ifelse(
  X_squared > c,
  "Reject H0 by comparing with critical point",
  "Accept H0 by comparing with critical point"
)

#Check for rejection by comparing with significance level
ifelse(
  pval < alpha,
  "Reject H0 by comparing with significance level",
  "Accept H0 by comparing with significance level"
)







# ================================ Problem3 =================================


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





# ================================ Problem4 =================================

rm(list = ls())

if (!require("readxl"))
  install.packages("readxl")
library("readxl")

# Import the data
type <- read_xlsx("data.xlsx", sheet = "Sheet5", col_names = FALSE, col_types = NULL)
colnames(type) = c("A","B","C")
rownames(type) = c("4-8","8-12","12-16","16-20","20-24")
data <- as.matrix(type)

# Computing Chi-square
chisq <- chisq.test(data) 

# Print observed counts & expected counts
print(chisq$observed)
print(round(chisq$expected, 2))
print(chisq)

# Retrieving value
alpha = 0.05
X_squared = chisq$statistic # Statistic
df = chisq$parameter        # Degree of freedom
pval = chisq$p.value        # P-value
c = qchisq(1 - alpha, df)    # Computing critical point

# Check for rejection by comparing with critical point
ifelse(
  X_squared > c,
  "Reject H0 by comparing with critical point",
  "Accept H0 by comparing with critical point"
)

#Check for rejection by comparing with significance level
ifelse(
  pval < alpha,
  "Reject H0 by comparing with significance level",
  "Accept H0 by comparing with significance level"
)

