##################################################
#
# Applying KNN Classification to Midge data
#
# Student name(s): 
#
##################################################
#
# Directions: (1) read through and run the code to
# understand what it does. 
# (2) Answer the questions at the end, submitting
# the lab in groups of three.
#
#################################################
#
# Answers:  Discuss your answers in class.  There
# is nothing to turn in.
#
#############################################
#          Setup
#############################################
############## load packages ##############
# We will use tibbles, pipes, and ggplot
# modelr is installed with tidyverse, but not loaded
# with it
require(tidyverse)     # manipulation and visualization
require(modelr)        # data_grid()
require(class)         # knn()


#################### Function for drawing circles ##########
# We need to draw a circle from center and point on circumference
# so let's create a function for that
addCircle <- function(center, point) {
  # Calculate the radius between the center and point on circumference
  r <- sqrt((center[[1]] - point[[1]])^2 + 
            (center[[2]] - point[[2]])^2)
  
  # Use parametric function of a circle to create 180 points 
  # on the circle centered at "center" of radius "r"
  myCircle <- 
    tibble(x = center[[1]] + r*cos(seq(0, 2*pi, by = pi/180)),
           y = center[[2]] + r*sin(seq(0, 2*pi, by = pi/180)))
  
  # Use geom_path() to draw a path through the points
  # make the circle red and dashed and thin
  geom_path(data = myCircle,
            aes(x = x, y = y),
            color = "red", linetype = "dashed", size = 0.1) 
}

# An example of how to use addCircle
# coord_fixed() makes the aspect ration 1:1
# so that circles look like circles instead of ellipses
ggplot() + addCircle(c(2,5), c(7,1)) +
  addCircle(c(0,0), c(10,1)) +
  coord_fixed()


###################### Get the training data ############
# This is the same midge data as for the first midge lab
midge.train <- as.tibble(read.csv("ISLR Chapter 1 Midge.txt", 
                                  header = TRUE))

# Look at the factors of the Type
levels(midge.train$Type)

table(midge.train$Type)

# Save a plot of the midges for later
midge.train %>% 
  ggplot(aes(x = WingLength, 
             y = AntennaLength)) +
  geom_point(aes(color = Type,
                 shape = Type), size = 2)   -> midgePlot

################# Create test data #####################  
midge.test <- tibble(Type = c("unknown", "unknown", "unknown"),
                     WingLength = c(1.80, 1.84, 2.04),
                     AntennaLength = c(1.24, 1.28,1.4),
                     Index = c(1, 2, 3))

# Plot the test points on the midge plot
midgePlot +
  geom_point(data = midge.test, 
             aes(color = Type, shape = Type)) +
  geom_text(data = midge.test, 
            aes(label = Index),
            nudge_x = 0.02)

##################### All grid points #############
# create a grid of equally spaced WingLengths and
# AntennaLengths
midge.train %>% 
  data_grid(WingLength = 
              seq_range(WingLength, by = 0.01),
            AntennaLength = 
              seq_range(AntennaLength, by = 0.01)
  ) -> midge.grid


# Add the grid to the midge plot and look at it
midgePlot +
  geom_point(data = midge.grid,
             shape = 3, size = 0.5, alpha = 0.3) +
  theme(panel.background = element_blank())


##################################################
#
# How does KNN work? 
# plots circles around the test midge points
# through each of the known midges
# so that we can determine the classification of the 
# k nearest neighbors for various k
#
##################################################

# Choose which test point to use and store the center
j <- 2
center <- c(midge.test[j, 2], midge.test[j,3])
center

# Initialize myPlot
myPlot <- midgePlot

# loop through the known midges
# adding a circle through the known midge centered
# at the test midge
for (i in 1:15){
  point <- c(midge.train[i, 2], midge.train[i, 3])
  myPlot <- myPlot + addCircle(center, point)
}

# Plot the circles, test midge and square up the axes
myPlot + 
  geom_point(data = slice(midge.test, j)) +
  coord_fixed()

# Repeat with the other 2 test midges

#####################################
#
#   Using R to do knn classification
#
#####################################
#
################ Classify all points in the grid ##########
# knn() requires data frames with
# no extra columns
# 
# training inputs
train.input <- midge.train %>% select(WingLength, AntennaLength)

# test inputs
test.input <- midge.grid %>% select(WingLength, AntennaLength)

# training outputs - must be a vector, not a tibble
train.output <- midge.train$Type

neighbors <- 1

midge.grid$Type <- knn(train = train.input, 
                      test  = test.input, 
                      cl    = train.output, 
                      k     = neighbors)

# Add the grid to the midge plot and look at it
midgePlot +
  geom_point(data = midge.grid,
             aes(color = Type),
             shape = 3, size = 0.5) +
  geom_contour(data = midge.grid, 
               aes(z = as.numeric(Type) ),
               breaks = c(1.5),
               color = "black") +
  theme(panel.background = element_blank()) +
  geom_point(data = midge.test,
             color = "black")



######################################
#
#  Questions
#
######################################
#
# 1. What happens with ties in KNN?  Clearly
#    explain the options for ties and how 
#    each works.
#
#  2. What happens when we use even values of 
#     k to classify the midges?  What does
#     the decision boundary look like?
#     Can we change the behavior of ties to 
#     change the boundary?  Provide examples
#     in R code.
#
#