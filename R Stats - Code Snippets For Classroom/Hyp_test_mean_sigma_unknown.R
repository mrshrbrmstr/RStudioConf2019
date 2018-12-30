# The following is for hypothesis test of a single mean
# Use this if the population mean (mu) is given
# and population standard deviation (sigma) is NOT KNOWN
# Assume a simple random sample representative of the population
# AND either the population is approximately "normal" (bell-shaped)
#      or our sample size is at least 30

mu <- 115

sample_size <- 20
sample_mean <- 125.7
sample_std_dev <- 27.5
alpha <- 0.05

# Calculate the test statistic, critical value and p-value(s)
# DO NOT make any changes to the following:

tscore <- ((sample_mean - mu)*sqrt(sample_size))/sample_std_dev
df = sample_size - 1
print("The calculated t value is")
tscore
print("WARNING: If this is inconsistent with the alternate hypothesis then use a 2-tailed test.")
print("#####  Note that the alternate hypothesis of < requires a negative t score,")
print("#####  while an alternate hypothesis of > requires a positive t score.")

test <- c("One Tailed", "One Tailed", "Two Tailed")
alternate <- c("Ha <", "Ha >", "Ha /=")
t_score <- c(tscore, tscore, tscore)
p_value <- c(pt(tscore,df), 1 - pt(tscore, df), 2*(1 - pt(abs(tscore),df)))
crit_value <- c(qt(alpha,df), qt(1 - alpha,df), qt(1 - alpha/2, df))

table1 <- t(data.frame(test, alternate, t_score, p_value, crit_value))
table1
