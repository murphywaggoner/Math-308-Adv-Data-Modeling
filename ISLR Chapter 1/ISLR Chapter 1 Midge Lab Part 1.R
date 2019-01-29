##################################################
#
# More about ggplot, introduction to classification
# training sets 
#
# Student name(s): 
#
##################################################
#
# Directions: (1) read through and run the code to
# understand what it does. 
# (2) Participate in the discussion question below
#
#################################################
#
# Answers:  Discuss your answers in class.  There
# is nothing to turn in.
#
############## Setup ##############
# We will use tibbles, pipes, and ggplot
# modelr is installed with tidyverse, but not loaded
# with it
require(tidyverse)
require(modelr)        # for data_grid()

# download the Midge.txt file from the Scholar site
# if needed, upload it to the RStudio cloud
midge.train <- as.tibble(read.csv("Chapter 1 Midge.txt", header = TRUE))

# Let's see the data set
midge.train %>% 
  ggplot(aes(x = WingLength, 
             y = AntennaLength,
             color = Type)) +
  geom_point()

# create a grid of all possible WingLengths and
# AntennaLengths
midge.train %>% 
  data_grid(WingLength, AntennaLength) -> midge.grid

#Look at the grid
midge.grid %>% 
  ggplot(aes(x = WingLength, 
             y = AntennaLength)) +
  geom_point()

# not equally spaced points, so...
# create a grid of equally spaced WingLengths and
# AntennaLengths
midge.train %>% 
  data_grid(WingLength = 
              seq_range(WingLength, by = 0.01),
            AntennaLength = 
              seq_range(AntennaLength, by = 0.01)
            ) -> midge.grid

#Look at the grid
midge.grid %>%
  ggplot(aes(x = WingLength,
             y = AntennaLength)) +
  geom_point()

# Change the points' shape, size and transparency
midge.grid %>% 
  ggplot(aes(x = WingLength, 
             y = AntennaLength)) +
  geom_point(shape = 3, size = 0.5, alpha = 0.3)

# Now plot the midges on top of the grid
midge.train %>% 
  ggplot(aes(x = WingLength, 
             y = AntennaLength)) +
  geom_point(aes(color = Type)) +
  geom_point(data = midge.grid,
             shape = 3, size = 0.5, alpha = 0.3)

# I don't like the gray background
midge.train %>% 
  ggplot(aes(x = WingLength, 
             y = AntennaLength)) +
  geom_point(aes(color = Type,
                 shape = Type), size = 2) +
  geom_point(data = midge.grid,
             shape = 3, size = 0.5, alpha = 0.3) +
  theme(panel.background = element_blank())

##################################################
#
# Discussion questions
# 1) Our purpose is to classify
# some midges for which we have measured the wing 
# length and antenna length.  To prepare for that
# we want to classify all the points in the grid 
# as Af or Apf.  Brainstorm different ways to do 
# that.  Come up with as many different ways that 
# you can.  This is a good exercise for the modeling
# competition.
# 
# 2) Choose one of the methods and see if you can 
# develop the method for doing it so that you can
# actually apply it to this training set. 
#
##################################################