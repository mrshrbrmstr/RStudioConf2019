x <-  c(111.8,80.1,121.3,125.8,126.1,122.0,90.7,89.0,102.7,118.4,102.9,120.1,165.9,185.5,64.6,83.8,118.7,
        165.2,155.4,159.2,152.6,137.3,110.7,121.0,165.4,155.9,131.0,94.8,165.0,60.7,137.1,159.7,132.2,
        134.1,151.2,140.8,184.7,125.6,158.8,141.3,146.0,124.2,142.4,193.2,93.8,92.0,117.3,129.3, 108.3,194.5)
sample_size <- length(x)
sample_mean <- mean(x)
sample_std_dev <- sd(x)

mu <- 120
alpha <- 0.05

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
