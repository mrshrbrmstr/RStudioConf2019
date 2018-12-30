# The following is for hypothesis test of a single mean
# Use this if the population mean (mu) is given
# and standard deviation (sigma) is given

mu <- 115
sigma <- 30
sample_size <- 20
sample_mean <- 135.2
xmin <- 0
xmax <- 200

# The following will create our 20 sample values

generate_sample <- function(n, mean, sd)
{ 
  x <- rnorm(n = sample_size - 1, 
           mean = sample_mean,
           sd = sigma)
  x[sample_size] <- sample_mean * sample_size - sum(x)
  return(x)
}
x <- generate_sample(sample_size, sample_mean, sigma)

if (x[sample_size] < xmin) {x <- generate_sample(sample_size, sample_mean, sigma)}
if (x[sample_size] > xmax) {x <- generate_sample(sample_size, sample_mean, sigma)}

# Calculate the test statistic and p-value(s)

z_score <- ((sample_mean - mu)*sqrt(sample_size))/sigma # Note z-score can be + or -

one_tailed_p_value <- 1 - pnorm(abs(z_score))

two_tailed_p_value <- 2 * one_tailed_p_value
