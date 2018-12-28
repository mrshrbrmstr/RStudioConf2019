library(ggplot2)
library(ggpubr)
# ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
#  geom_density(alpha = 0.7) +
#  scale_colour_brewer(type = "qual", aesthetics = "fill")

# Using ggplot2 to create multiple simulated data distributions
# on a single graph

phillies_data1 <- rpois(162,7.23) # strikeout data per 9 IP
phillies_data2 <- rpois(162, 3.86) # runs per game data
phillies_data3 <- rpois(162, 0.80) # homers per game data
phillies_data <- as.numeric(c(phillies_data1, phillies_data2, phillies_data3))

measure_type <- c(rep("strikeouts", 162), rep("runs per game", 162),
                            rep("homers per game", 162))
baseball_data <- cbind.data.frame(phillies_data, measure_type)

plot1 <- ggplot(baseball_data, aes(x = phillies_data, fill = measure_type)) +
      geom_bar(alpha = 0.7, position = "dodge", width = 0.6) +
      scale_colour_brewer(type = "qual") +
      theme(legend.position="none")
plot2 <- ggplot(baseball_data, aes(x = phillies_data, fill = measure_type)) +
        geom_density(alpha = 0.7, stat = "count") +
        scale_colour_brewer(type = "qual")

ggarrange(plot1, plot2, ncol = 2, nrow = 1)

