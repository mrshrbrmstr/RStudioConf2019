##############################################################
##                                                          ##
##  The following program uses the R Programming Language   ##
##  The downloaded content consists of 10 datasets,         ##
##  meant to illustrate the power of visualization          ##
##  over just summary stats when modeling data.             ##
##                                                          ##
##  The dataset used here is called 'datasauRus_dozen'      ##
##  and consists of 13 pairs of x and y values.             ##
##  Each random variable has a sample size of 142           ##
##  Following each # is a comment or explanation of what    ##
##  each line of code actually does.                        ##
##  DatasauRus is a creation of Alberto Cairo and is        ##
##  inspired by the 4 data sets created by Francis          ##
##  Anscombe in 1973.                                       ##
##                                                          ##
##############################################################

# First we must download the plotting system and the data for dinosaur
# because they do not come with R pre-installed.
# If using an R console ONLINE, such as Snippets - URL: https://rdrr.io/snippets/
# a large number of packages are already installed
# If running this using R or RStudio on your personal computer,
# remove the # from in front of lines 26-30 here:

# install.packages("ggplot2", 
#      repos = 'http://cran.r-project.org/src/contrib/ggplot2_2.2.1.tar.gz')
#
# install.packages("datasauRus", 
#      repos = 'http://cran.r-project.org/src/contrib/datasauRus_0.1.2.tar.gz')

# Next we must put those downloaded 'packages' into our library
# Any time we want to add new features to R - in the form of a 'package'
# we must install and add it to the available library

 library(ggplot2)

 library(datasauRus)

# The following code creates 13 graphs of x- and y- ordered pairs
# and sets/chooses the various features of the graphs
# BY THE WAY... change ncol=3 to ncol=4 and then ncol=5 and see what happens
# When displaying these graphs on a "wide" format, ncol = 5 is good
# When exporting to a pdf in letter and portrait mode, ncol = 3 is good
  
ggplot(datasaurus_dozen, aes(x=x, y=y, colour=dataset))+
    geom_point()+
    theme_void()+
    theme(legend.position = "none")+
    facet_wrap(~dataset, ncol=5)

####   STOP HERE   ############################################################
# In introductory statistics class, you can stop here. But, for the intrepid
# and eager student, or if you are interested to see regression results... keep going

# The following calculates residuals for all 13 of the linear regressions
# We will create and store the original variables along with the fitted y values
# and the residuals into a new data set (which we're calling 'data')
# The following can only be executed after the first half is complete (lines 1 - 51)

graph_names <- unique(datasaurus_dozen$dataset)
lm_residuals <- numeric()
fitted_y <- numeric()

for (i in 1:13) {
  current_data <- subset(datasaurus_dozen, 
                         datasaurus_dozen$dataset == graph_names[i], select = c(x,y))
  linear_parameters <- lm(current_data)
  current_residuals <- linear_parameters$residuals
  current_fitted <- linear_parameters$fitted.values
  fitted_y <- c(fitted_y, current_fitted)
  lm_residuals <- c(lm_residuals,current_residuals)
  }

data <- cbind.data.frame(datasaurus_dozen, fitted_y, lm_residuals)

# Now, as they say, our data is complete. We can show two more plots.
# The first set of 13 plots is "residuals versus fitted y values",
# otherwise known as a residual plot

ggplot(data, aes(x=fitted_y, y=lm_residuals, colour=dataset))+
  geom_point()+
  theme_void()+
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol=5)

# The second set of 13 plots shows the original x and y values,
# and also plots the line of "best fit" which, in all 13 cases,
# is essentially a flat line

ggplot()+
  geom_point(data=data, aes(x=x, y=y, colour=dataset))+
  geom_line(data=data, aes(x=x, y=fitted_y), colour="black")+
  theme_void()+
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol=5)

##