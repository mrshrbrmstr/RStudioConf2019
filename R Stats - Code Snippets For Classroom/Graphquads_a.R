# graphing quadratic functions and their reciprocals 
# on intervals of specified left- and right-endpoints
graphquads <- function(a=1,b=1,c=1,l=-2,r=2) {
  require(grDevices)
  a2 <- as.character(a)
  b2 <- as.character(b)
  c2 <- as.character(c)
  x <- seq(l,r, len = 1001)
  y <- cbind(a*x^2+b*x+c, 1/(a*x^2+b*x+c))

  interval_width <- r-l
  increment_x_by <- interval_width/8

  x_axis_labels <- seq(l, r, increment_x_by)

  par(oma=c(3,0,0,0))
  matplot(x, y, type = "l", xaxt = "n", lty = c(1,1), col=c(1,2),
        main = expression(paste("y= ",ax^2+bx+c," and ",frac(1,ax^2+bx+c))),
        col.main = "blue")
  mtext(text=paste(
     "quadratic function and its reciprocal, a = ",a2,", b = ",b2,", c = ",c2),
     side=1,line=0,outer=TRUE,cex=1.1, col = "blue")
  axis(1, at = x_axis_labels,
     labels = x_axis_labels)
# abline(h = 1, v = -0.5, col = "gray70")
}