# Clear environment
rm(list = ls())

# Import the data
file_path <- 'resources/ex2.csv'
income <-
  read.csv(
    file = file_path,
    sep = ",",
    row.names = 1,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
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

# Check for rejection by comparing with significance level
ifelse(
  pval < alpha,
  "Reject H0 by comparing with significance level",
  "Accept H0 by comparing with significance level"
)
