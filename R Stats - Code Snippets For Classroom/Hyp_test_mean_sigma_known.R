# The following is for hypothesis test of a single mean
# Use this if the population mean (mu) is given
# and population standard deviation (sigma) is given
# Assume a simple random sample representative of the population
# AND either the population is approximately "normal" (bell-shaped)
#      or our sample size is at least 30

mu <- 115
sigma <- 30
sample_size <- 20
sample_mean <- 125.7
alpha <- 0.05

# Calculate the test statistic, critical value and p-value(s)
# DO NOT make any changes to the following:

zscore <- ((sample_mean - mu)*sqrt(sample_size))/sigma
print("The calculated z value is")
zscore
print("WARNING: If this is inconsistent with the alternate hypothesis then use a 2-tailed test.")
print("#####  Note that the alternate hypothesis of < requires a negative z score,")
print("#####  while an alternate hypothesis of > requires a positive z score.")

test <- c("One Tailed", "One Tailed", "Two Tailed")
alternate <- c("Ha <", "Ha >", "Ha /=")
z_score <- c(zscore, zscore, zscore)
p_value <- c(pnorm(zscore), 1 - pnorm(zscore), 2*(1 - pnorm(abs(zscore))))
crit_value <- c(qnorm(alpha), qnorm(1 - alpha), qnorm(1 - alpha/2))

table1 <- t(data.frame(test, alternate, z_score, p_value, crit_value))
table1
