##################################################
#
# Creating QQ plots to determine data distribution
#
# Student name(s): 
#
##################################################
#
# Directions: (1) read through and run the code to
# understand what it does.  Here we are plotting
# QQ plots "by hand" instead of using the black
# box commands of ggplot to plot them.
# (2) copy the code and revise it to plot 
# the QQ plot for a uniform distribution and 
# a chi-square (df = 3) distribution in addition to 
# the normal distribution.  Nota bene:  Only copy
# the code needed and change all comments to 
# reflect the distribution being tested
# (3) Add titles, captions, labels, etc., to the 
# plots
# (4) Use your code to visually determine the 
# best estimate # of the distributions in the 
# data you are given.
#################################################
#
# Answers:
#  Dist1 is the _____ distribution
#  Dist2 is the _____ distribution
#  Dist3 is the _____ distribution
#  Dist4 is the _____ distribution
#
############## Setup ##############
# We will use tibbles, pipes, and ggplot
require(tidyverse)

#  "percents" is a list 
# of the quantiles we will use in the QQ plots
percents <- seq(from = 0, to = 1, by = 0.01)

qqnormline <- function(mydata){
  # this function takes a vector and returns the slope
  # and the intercept of a line through the point of the QQ plot
  # at the 25th-percentile and the 75th-percentile
  # using a normal distribution for the comparison
  pt1 <- c(qnorm(0.25), quantile(mydata, 0.25))
  pt2 <- c(qnorm(0.75), quantile(mydata, 0.75)) 
  m <- (pt1[[2]] - pt2[[2]])/(pt1[[1]] - pt2[[2]])
  b <- -pt1[[1]]*m + pt1[[2]]
  
  # To use the output, the slope is in element [[1]] and
  # the y-intercept is in element [[2]]
  return(c(m, b))
}

# Read in the assignment data, look at its structure,
# and pick one of the columns to test by storing it in "x"
myDists <- read_csv("QQPlotData.csv")

str(myDists)

x <- myDists$Dist1

######## QQ plot comparing to normal dist #########

# Create a tibble with three columns: 
#    (1) the percentages for each quantile
#    (2) quantiles of the normal distribution and
#    (3) quantiles of the data in question
mytib <- tibble(percents,
                comparison = qnorm(percents),
                mydist = quantile(x, percents))

# Calculate the line through the 25th and 75th
# quantile points on the QQ plot
linedata <- qqnormline(x)

# The QQ plot - points and line
mytib %>% 
  ggplot(aes(x = comparison, y = mydist)) +
  geom_point() +
  geom_abline(slope = linedata[[1]], intercept = linedata[[2]])
