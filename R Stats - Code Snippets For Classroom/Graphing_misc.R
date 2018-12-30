# graphing functions with x values in multiple of pi
# two functions on one graph
require(grDevices)
x <- seq(-4, 4, len = 101)
y <- cbind(sin(x), cos(x))
matplot(x, y, type = "l", xaxt = "n", lty = c(1,1), col=c(1,2),
        main = expression(paste(plain(sin) * phi, "  and  ",
                                plain(cos) * phi)),
        ylab = expression("sin" * phi, "cos" * phi), # only 1st is taken
        xlab = expression(paste("Phase Angle ", phi)),
        col.main = "blue")
axis(1, at = c(-pi, -pi/2, 0, pi/2, pi),
     labels = expression(-pi, -pi/2, 0, pi/2, pi))
abline(h = 0, v = pi/2 * c(-1,1), lty = 2, lwd = .1, col = "gray70")

# graphing quadratic functions and their reciprocals 
# on intervals of specified left- and right-endpoints
require(grDevices)
left_endpoint <- -1.5 # enter each time
right_endpoint <- 0.5 # enter each time
a1<- 1 # enter each time
b1<- 1 # enter each time
c1<- 1 # enter each time
a2 <- as.character(a1)
b2 <- as.character(b1)
c2 <- as.character(c1)
y <- cbind(a1*x^2+b1*x+c1, 1/(a1*x^2+b1*x+c1))

interval_width <- right_endpoint - left_endpoint
increment_x_by <- interval_width/8
x <- seq(left_endpoint,right_endpoint, len = 101)
x_axis_labels <- seq(left_endpoint, right_endpoint, increment_x_by)

par(oma=c(3,0,0,0))
matplot(x, y, type = "l", xaxt = "n", lty = c(1,1), col=c(1,2),
        main = expression(paste("y= ",ax^2+bx+c," and ",frac(1,ax^2+bx+c))),
        # ylab = expression(paste("y= ",ax^2+bx+c," and ",frac(1,ax^2+bx+c))),
        # xlab = expression("x"),
        col.main = "blue")
mtext(text=paste(
  "quadratic function and its reciprocal, a = ",a2,", b = ",b2,", c = ",c2),
      side=1,line=0,outer=TRUE,cex=1.1, col = "blue")
axis(1, at = x_axis_labels,
     labels = x_axis_labels)
# abline(h = 1, v = -0.5, col = "gray70")