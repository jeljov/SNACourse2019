###############################
# Data sampling
###############################
  
# create vector x with values from 1 to 10


# create a sample of size 5 (without repetitions) from the vector x


# create two samples of size 20 from the vector x, where duplicates are allowed


# now, set the seed and again create two samples of size 20 from the vector x, 
# with duplicates allowed



###############################
# Matrices
###############################

# create a 2 x 4 matrix with values from 8 to 1, filled by rows


# get the first row


# get the element from row 1, column 2


# get number of rows


# get number of columns


# create two matrices of the same dimension


# add matrix2 to matrix1


# transpose a matrix


###############################
# Lists
###############################

# create a new list with attributes: passport, age, diplomatic


# get the 2nd element


# get the value of the 2nd element


# get the value of the age element

# alternative:


# get the list length


# add a new element (at the end of the list)


# add a new element after the 2nd element


# delete the 4th element


# concatinate two lists


# However, that's probably not what we wanted;
# Compare that to the following:


# take the 1st passenger data 


# take the passport of the 2nd passenger


# check if some data item is a list


# get names of the list elements


# get elements with the given name (e.g. 'age')



###############################
# Foreach loop
###############################

# print all numbers from 1 to 10 using for each loop


# print odd numbers from 1 to 10 using for each loop


# print elements of the traveler1 list


# use a double loop to print elements of a matrix 



###############################
# Task 1
###############################
# Create a 2 x 3 matrix with the following elements: 
# 3, 9, -1, 4, 2, 6 (by row). Print only the positive 
# values from the first row.



###############################
# Task 2
###############################
# Create a vector by sampling 15 values with replacement
# from the [1,100] range. Using the for each loop, compute 
# the number of odd and even numbers in the built vector.



###############################
## if-else
###############################

# use ifelse function to create a new list element called 'request' 
# with the value 'assistance required' if a traveler is younger 
# than 10 years, and 'no special requests' otherwise



########################################
# User-defined functions and apply
########################################

# Create a function that adds two numbers. 
# The default value for the second argument is 1



# create a function returning the absolute value of x. 
# Return the result using the return() function



##############################################################
# Applying a function over rows and columns in data frame
##############################################################

# load data from the "data/beatles_v2.csv" file


# check the structure of the read data


# get the number of characters in the song title "Yellow Submarine"


# get the number of characters in the titles of the first 10 songs


# compute the number of missing values for each attribute (column);
# consider NA and empty string ("") as missing values



###############################
# Working with tables
###############################

# create a contingency table of column Year values


# get the 4th element from the table


# sort the table in the descending order


# get the proportions table for the values of the Year column


# sort the proportions table in the descending order


# get the proportions table for the values of the Year column, 
# but limiting number of decimal points to 3


# create a contingency table for Top.50.Billboard vs. Year,
# using the data for the first 20 songs; allow for NA values


# alternative (using xtabs):


# again, we can compute proportions:

# along rows:

# along columns:



###############################
# Manipulating data frames
###############################

###############################
## Adding new rows and columns
###############################

# create a new column On_album and set FALSE for all songs


# create a new data frame with two columns with randomly sampled data:
# - Platinum - sample data from c(TRUE, FALSE)
# - Score - sample data from the 5:10 range
# the new data frame should have the same number of rows as the beatles data frame


# combine the two data frames


# get the first song


# add that song to the end of the data frame


# add the song after the 3rd song in the data frame 


# verify that the song is now in rows 1, 4, and the last row



###############################
## Removing columns and rows
###############################

# remove the attribute On_album


# remove columns Platinum (at index 10) and Score (at index 11)

# alternative:


# create a subset of the data frame without songs in rows 2, 4 and 6 


# create a subset of the data frame without songs in rows from 1 to 8 


##################################
## Updating column and row names
#################################

# get column names


# change name of the column at the index 6 to 'Song_genre'


# change name of the column 'Song_genre' to 'Genre'


# change row names to a string containing word 'song' and a song order number


# change back row names to a string containing order number


##################################
## Retrieving and changing values
##################################

# for the first five songs, get values of the Title and Album.debut attributes


# get titles of the songs from year 1964 not having McCartney as the only lead vocal


# get the songs from year 1964 not having McCartney as one of the lead vocals


# get the songs from year 1958, but only attributes Title and Album.debut


# create a vector of logical values denoting whether the attribute 
# Album.debut has a value or not
# Note: missing values are denoted here as empty strings ("")


# compute the number of missing ("") values for Album.debut  


# replace the empty value of the Album.debut attribute with NA


# compute the number of missing (NA) values for Album.debut  


# set the value back to empty string


###############################
## Saving a dataset
###############################

# save dataset to a CSV file, but without the row names (row numbers) column


# save R object for the next session into file "p2.RData"


# restore R object from the file "p2.RData" in the next session


###############################
# Task 3
###############################

# Create a new column in the *beatles* data frame called *Billboard.hit* 
# having TRUE for all songs that were in the Top 50 Billboard 
# (songs that have the Top.50.Billboard defined), 
# and FALSE for all other songs (not having this value set).


