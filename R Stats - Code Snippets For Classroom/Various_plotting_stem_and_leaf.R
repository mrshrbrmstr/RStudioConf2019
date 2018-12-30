library(purrr)
x <- rdunif(10000,5,1)

edges <- c(0.5,1.5,2.5,3.5,4.5,5.5)
hist(x, freq=FALSE, breaks = edges)

y <- rbinom(10000,size=4,p=0.5) + 1
edges <- c(0.5,1.5,2.5,3.5,4.5,5.5)
hist(y, freq=FALSE, breaks = edges)

y <- rbinom(1, size = 4, p = 0.5) + 1
edges <- c(0.5,1.5,2.5,3.5,4.5,5.5)
hist(y, freq=FALSE, breaks = edges)

library(stats)
library(graphics)
library(ggplot2)

chk <- chickwts
boxplot(weight ~ feed, data = chk, col = "yellow",
        main = "chickwt data",
        ylab = "Weight at six weeks (gm)")

p2 <- ggplot(chk, aes(x = weight, fill = feed)) +
  geom_density(alpha = 0.7, stat = "density") +
  scale_colour_brewer(type = "qual")
p2
# Separate out each feed type as its own data
chk_horsebean <- chk[1:10,1]
chk_linseed <- chk[11:22,1]
chk_soybean <- chk[23:36,1]
chk_sunflower <- chk[37:48,1]
chk_meatmeal <- chk[49:59,1]
chk_casein <- chk[60:71,1]

stem(chk_casein)
stem(chk_horsebean)
stem(chk_linseed)
stem(chk_meatmeal)
stem(chk_soybean)
stem(chk_sunflower)
