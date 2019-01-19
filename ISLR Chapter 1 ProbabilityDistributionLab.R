##################################################
#
# Plotting Probability Distributions and
# Calculating Probabilities
#
# Student name(s): 
#
##################################################
#
# Directions: (1) read through and run the code to
# understand what it does. 
# (2) Do the exercises, copying code and changing
# comments to match to content for each question
#
#################################################
#
# Answers:  Submit an R script that will produce
# the plots that answer the exercises.
#
############## Setup ##############
# We will use tibbles, pipes, and ggplot
require(tidyverse)


######## Example of probabilities with a normal curve #########
# Question:
# Children's IQ scores are normally distributed with a
# mean of 100 and a standard deviation of 15. What
# proportion of children have an IQ between 80 and 120?

# Set the parameters for the distribution
mu <- 100      # mean
sigma <- 15    # standard deviation
lower <- 80
upper <- 120

# Generate 100 x-values between mu +/- 4sigma 
# These will be used to generate points on the 
# normal curve
# for ggplot to work we need the data in a tibble
data <- tibble(x = seq(mu - 4*sigma,
                       mu + 4*sigma,
                       length = 100))

# plot the normal distribution and
# shade the region
# Note that this calculates the y values 
# on the fly using "dnorm"  ("fun" = function)
myPlot <- data %>% 
  ggplot(aes(x = x)) +
  stat_function(fun = dnorm,                # shade the region
                args = list(mean = mu, 
                            sd = sigma),    # parameters for "dnorm" 
                xlim = c(lower,upper),
                geom = "area",
                fill = "lightblue")  +
  stat_function(fun = dnorm,                # draw the curve
                args = list(mean = mu,
                            sd = sigma)) +  # parameters for "dnorm"
  xlab("IQ") +
  ylab("")

# we saved the plot so that we could add the title to it later
# to look at it now, we need to execute this line
myPlot

# Now calculate the probability 
#  P(80 < x < 100) = P(z_lower < z < z_upper)
# That is, find the area shaded in blue
# For this we use the pnorm function
area <- pnorm(upper, mean = mu, sd = sigma) - 
  pnorm(lower, mean = mu, sd = sigma)

# add probability to the plot formatted properly
result <- paste("P(",lower,"< IQ <",upper,") =",
   signif(area, digits=3))

myPlot + ggtitle("Normal Distribution",
                 subtitle = result) 

##################################################
#
# Exercises - Part A: For each of these problems, 
# create a standard normal plot like above
# with the area shaded and title showing the
# area.
# 
#   (1)  The length of human gestation has a normal 
#   distribution with a mean of 266 days and a standard 
#   deviation of 16 days.  What proportion of pregnancies
#   will last between 240 and 270 days?
#   
#   (2) Entrance t a certain University is determined 
#   by a national test.  The scores on this test are 
#   normally distributed with mean 500 and standard   
#   deviation 100.  What is the probability that a  
#   student makes less than 585 on the exam?
#   
#   (3) If the mean of a random variable with a 
#   normal distribution is 81.1 and the standard 
#   deviation is 4.7, then find the probability 
#   of randomly selecting a value more than 83?
#   
#
##################################################

######## Example of probabilities with an exponential curve #########
# Question:
# The number of miles that a particular car can run before 
# its battery wears out is exponentially distributed 
# with an average of 10,000 miles. The owner of the 
# car needs to take a 5000-mile trip. What is the probability 
# that the driver will be able to complete the trip without having 
# to replace the car battery?

# Set the parameters for the distribution
lambda <- 10000      # lambda = average "lifetime"
rate <- 1/lambda     # parameter for "dexp" is rate = 1/lambda
lower <- 5000
upper <- 4*lambda    # We have to stop the graph somewhere 

# Generate 100 x-values between 0 and 4*lambda
# These will be used to generate points on the 
# exponential curve
# for ggplot to work we need the data in a tibble
data <- tibble(x = seq(0, 4*lambda,length = 100))

# plot the exponential distribution and
# shade the region
# Note that this calculates the y values 
# on the fly using "dexp" 
myPlot <- data %>% 
  ggplot(aes(x = x)) +
  stat_function(fun = dexp,                   
                args = list(rate = rate),     # parameters for "dexp"   
                xlim = c(lower,upper),
                geom = "area",                # shade the region
                fill = "lightgreen")  +
  stat_function(fun = dexp,                   # draw the curve
                args = list(rate = rate)) +   # parameters for "dexp" 
  xlab("Life of battery (miles)") +
  ylab("")

# we saved the plot so that we could add the title to it later
# to look at it now, we need to execute this line
myPlot

# Now calculate the probability 
#  P( x < upper) 
# That is, find the area shaded in green
# For this we use the pexp function
# Note that 
area <- 1 - pexp(lower, rate = 1/lambda)

# add probability to the plot formatted properly
result <- paste("P(Life of battery >",lower,") =",
   signif(area, digits=3))

myPlot + ggtitle(paste("Exponential distribution with lambda = ",
                       lambda),
                 subtitle = result) 

##################################################
#
# Exercises - Part B: For each of these problems, 
# create an plot like above
# with the area shaded and title showing the
# area.
# 
#   (4)  a) On the average, a certain computer part 
#   lasts ten years. The length of time the computer 
#   part lasts is exponentially distributed. What is the
#   probability that the part will last between 7 and 11
#   years?
#   (4)  b) Eighty percent of these computer parts last 
#   at most how long?  (Hint: what R function is the 
#   inverse of pexp?)
#   
#   (5) What is the probability that a chi-square random 
#   variable with 10 degrees of freedom is greater 
#   than 15.99?  (This curve can be graphed on the
#   interval [0,25].)
#
#   (6) Find the probability that an F-statistics is
#   less than 0.595 if the degrees of freedom are 
#   df = 11 and df = 6.  (This curve can be graphed on the
#   interval [0,5].)
#
##################################################
