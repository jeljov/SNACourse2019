###############################
# Vectors
###############################

# Numeric


# Integer


# Character


# Logical (true/false)


# Creating vector with more elements


###############################
# Vector arithmetics
###############################

# (+, -, *, /, ^)



# get the max value


# get the min value


# get the range of values


# calculate the sum of all elements


# calculate the product of all elements


################################
# Generating regular sequences
################################

# generate a sequence from 1 to 10


# generate a sequence from 10 to 1


# generate a sequence from 3.2 to 4.7, with a step 0.2


################################
# Factor values
################################

# create a vector of character values and 
# convert it to a vector of factor values


# print the levels of a factor variable


# change the order of levels


# print the summary for a factor variable


################################
# Missing values
################################

# check which values are NAs


# NaN - not a number; should not
# be mistaken with missing values


################################
# Data frame
################################

# create a data frame


# create a data frame from existing vectors


################################
# Task 1
################################

# Create a dataframe called *co2.emissions* containing two columns: 
# *years* (2000, 2002, 2004, 2006, 2008, 2010) and 
# *emission* (2.7, 2.9, 4, 4.9, 5.3, 6.2).



################################
# Loading data frame from a file
################################

# reading a data frame from a CSV file


# print the number of rows


# print the number of columns


# str function


# summary function


# head and tail functions


# View function


# retrieve and change column names


# remove column duration


################################
# Subsetting
################################

# get an element from the row 3, column 1 


# get the third row


# get rows at positions from 3 to 5 (inclusive) 


# get rows at positions 3 and 6 


# get the second column


# get the Year column

# alternative:


# retrieve all songs released before year 1965

# alternative:


# retrieve all songs released before year 1965 with duration less than 150 seconds

# alternative:



################################
# Task 2
################################
# Print the number of songs from 1963 that last more than 2 minutes (120 seconds).



################################
# Plotting
################################

# create a plot for the given vectors 


# include ggplot2 library


# render a plot for the given data frame (columns Year and Duration)


# render a plot for the given data frame with points (scatter plot)


# add a column with song rating (1-5) by randomly sampling rating scores
# between 1 and 5 


# render a bar chart showing rating counts (= counts of songs with certain rating)


# render a bar chart for Year-Duration 


# transform Year to factor so that only years with songs
# are shown on the X-axis


# render a bar chart with custom title and axes labels  
# and separate color for each year


# render a bar chart showing the number of songs per year 
# (year is on the X-axis, the number of songs per year on the y-axis)



# render a line chart showing Year and Duration for the first five songs; 
# explore line and pont specific properties


################################
# Task 3
################################

# Create a line chart from the dataset *co2.emissions* (created in Task 1) 
# with the x-axis representing the years, and the y-axis representing the 
# values of CO2 emissions.
# Chart title should be "China CO2 Emissions" and y-axis should be 
# labelled "CO2 Emissions".




################################################################
# Homework - Complete interactive R tutorials with Swirl
################################################################
install.packages("swirl")
library("swirl")
swirl()