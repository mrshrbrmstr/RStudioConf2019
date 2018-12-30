# The following is for hypothesis test of a single proportion
# Use this for the population proportion given in the null hypothesis

# Assume there are at LEAST 10 observations with success
# AND at least 10 observations without success

p <- 0.25
n <- 150

# Now either we have p-hat given to us, or we have the raw number of success
# observations. Comment out what is not needed.
# UN-comment what IS needed:

# p_hat <- 0.18
x <- 27
p_hat <- x/n

# Supply the value for the Type I error:
alpha <- 0.05

# Calculate the test statistic, critical value and p-value(s)
# DO NOT make any changes to the following. COPY and PASTE EXACTLY:

zscore <- (p_hat-p)*sqrt(n)/sqrt(p*(1 - p))
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
