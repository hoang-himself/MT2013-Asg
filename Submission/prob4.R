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
