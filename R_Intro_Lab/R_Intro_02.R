###############################
# Data sampling
###############################
  
# create a vector with values from 1 to 10
x <- 1:10

# create a sample of size 5 (without repetitions) from the vector x
sample(x, size = 5)

# create two samples of size 20 from the vector x, where duplicates are allowed
s1 <- sample(x, 20, replace = TRUE)
s1
s2 <- sample(x, 20, replace = TRUE)
s2

# now, set the seed and again create two samples of size 20 from the vector x, 
# with duplicates allowed (observe that the generated samples are now equal)
set.seed(1)
s1 <- sample(x, 20, replace = TRUE)
s1
set.seed(1)
s2 <- sample(x, 20, replace = TRUE)
s2

###############################
# Matrices
###############################

# create a 2 x 4 matrix with values from 8 to 1, filled by rows
a <- matrix(8:1, nrow = 2, ncol = 4, byrow = TRUE)
a

# get the first row
a[1, ]

# get the element from row 1, column 2
a[1,2]

# get number of rows
nrow(a)

# get number of columns
ncol(a)

# create two matrices of the same dimension
matrix1 <- matrix(c(3, 9, -1, 4), nrow = 2)
matrix1
matrix2 <- matrix(c(5, 2, 0, 9), nrow = 2)
matrix2

# add matrix2 to matrix1
matrix1 + matrix2

# transpose a matrix
t(matrix1)

###############################
# Lists
###############################

# create a new list with attributes: passport, age, diplomatic
traveler1 <- list(passport = "P123123", age = 34, diplomatic=TRUE)
traveler1

# get the 2nd element
traveler1[2]

# get the value of the 2nd element
traveler1[[2]]

# get the value of the age element
traveler1$age
# or
traveler1[['age']]

# get the list length
length(traveler1)

# add a new element (at the end of the list)
traveler1$name = 'Jim'
traveler1

# add a new element after the 2nd element
traveler1 <- append(traveler1, list(country = "AUS"), after=2)
traveler1
length(traveler1)

# delete the 4th element
traveler1[[4]] <- NULL
length(traveler1)
traveler1

# concatinate two lists
traveler2 <- list(passport = "P456456", age = 14, diplomatic = FALSE)
travelers <- c(traveler1, traveler2)
travelers

# However, that's probably not what we wanted;
# Compare that to the following:
passengers <- list(p1 = traveler1, p2 = traveler2)
passengers
# take the 1st passenger data 
passengers[[1]]
# take the passport of the 2nd passenger
passengers[[2]][['passport']]


# check if some data item is a list
is.list(traveler1)

# get names of the list elements
names(traveler1)

# get elements with the given name (e.g. 'age')
traveler1[names(traveler1) == "age"]


###############################
# Foreach loop
###############################

# print all numbers from 1 to 10 using for each loop
for (i in 1:10) {
  print(i)
}

# print odd numbers from 1 to 10 using for each loop
for (i in 1:10) {
  if (i %% 2 == 1) {
    print(paste(i,"is odd number"))
  }
}

# print elements of the traveler1 list
for(item in traveler1)
  print(item)


# use a double loop to print elements of a matrix 
for(i in 1:nrow(matrix1)){
  for (j in 1:ncol(matrix1)) {
      print(paste0(i, ". row, ", j, ". column: ", matrix1[i,j]))
  }
}


###############################
# Task 1
###############################
# Create a 2 x 3 matrix with the following elements: 
# 3, 9, -1, 4, 2, 6 (by row). Print only the positive 
# values from the first row.

# Answer:
matrix1 <- matrix(c(3, 9, -1, 4, 2, 6), nrow = 2)

for (i in matrix1[1,]) {
  if (i > 0) {
    print(i)
  }
}

###############################
# Task 2
###############################
# Create a vector by sampling 15 values with replacement
# from the [1,100] range. Using the for each loop, compute 
# the number of odd and even numbers in the built vector.

# Answer:
sample_values <- sample(x=1:100, size = 15, replace = TRUE)
cnt_even <- 0
cnt_odd <- 0
for (val in sample_values) {
  if(val %% 2 == 0)
    cnt_even = cnt_even + 1
  else
    cnt_odd = cnt_odd + 1
}
print(paste("Number of odd numbers:", cnt_odd))
print(paste("Number of even numbers:", cnt_even))


###############################
## if-else
###############################

# use ifelse function to create a new list element called 'request' 
# with the value 'assistance required' if a traveler is younger 
# than 10 years, and 'no special requests' otherwise
traveler1$request <- ifelse(test = traveler1$age < 10,
                            yes = "assistance required",
                            no = "no special requests")
traveler1


########################################
# User-defined functions and apply
########################################

# Create a function that adds two numbers. 
# The default value for the second argument is 1
add <- function(x, y = 1){
  x + y
}

add(2, 3)
add(2)


# create a function returning the absolute value of x. 
# Return the result using the return() function
my_abs <- function(x) {
  if (x >= 0) {
    return(x)
  }
  return(-x)
}

my_abs(5)
my_abs(-5)

##############################################################
# Applying a function over rows and columns in data frame
##############################################################

# assure that the current working directory is R_Intro_Lab
getwd()
setwd('R_Intro_Lab')

# load data from the "data/beatles_v2.csv" file
beatles <- read.csv("data/beatles_v2.csv", stringsAsFactors = FALSE)

# check the structure of the read data
str(beatles)

# get the number of characters in the song title "Yellow Submarine"
nchar("Yellow Submarine")

# get the number of characters in the titles of the first 10 songs
sapply(beatles$Title[1:10], FUN = function(x) nchar(x))
# or:
sapply(X = beatles$Title[1:10], FUN = nchar)


# compute the number of missing values for each attribute (column);
# consider NA and empty string ("") as missing values
apply(beatles, 2, function(x) sum(is.na(x) | x == ""))


###############################
# Working with tables
###############################

# create a contingency table of column Year values
year_counts <- table(beatles$Year)
year_counts

# get the 4th element from the table
year_counts[4]

# store the 4th element from the table in a variable
x <- year_counts[4]
x

# sort the table in the descending order
sort(year_counts, decreasing = T)

# get the proportions table for the values of the Year column
year_counts_prop <- prop.table(year_counts)
year_counts_prop

# sort the proportions table in the descending order
sort(year_counts_prop, decreasing = T)

# get the proportions table for the values of the Year column, 
# but limiting number of decimal points to 3
round(year_counts_prop, digits = 3)

# create a contingency table for Top.50.Billboard vs. Year,
# using the data for the first 20 songs; allow for NA values
first20 <- beatles[1:20,]
table(first20$Top.50.Billboard, first20$Year, useNA = 'ifany')

# alternative:
xtabs(~Top.50.Billboard + Year, first20, addNA = TRUE)

# again, we can compute proportions:
top50_per_year <- table(first20$Top.50.Billboard, first20$Year, useNA = 'ifany')
prop.table(top50_per_year)
# along rows:
prop.table(top50_per_year, margin = 1)
# along columns:
prop.table(top50_per_year, margin = 2)
round(prop.table(top50_per_year, margin = 2), digits = 4)

###############################
# Manipulating data frames
###############################

###############################
## Adding new rows and columns
###############################

# create a new column On_album and set FALSE for all songs
beatles$On_album <- FALSE
head(beatles)

# create a new data frame with two columns with randomly sampled data:
# - Platinum - sample data from c(TRUE, FALSE)
# - Score - sample data from the 5:10 range
# the new data frame should have the same number of rows as the beatles data frame
additional_columns <- data.frame(
  Platinum = sample(c(TRUE, FALSE), 310, replace = TRUE),
  Score = sample(5:10, 310, replace = TRUE)
)

# combine the two data frames
beatles <- cbind(beatles, additional_columns)
head(beatles)

# get the first song
first_song <- beatles[1, ]

# add that song to the end of the data frame
beatles <- rbind(beatles, first_song)
tail(beatles)

# add the song after the 3rd song in the data frame 
beatles <- rbind(beatles[1:3, ],
                           first_song, 
                           beatles[4:nrow(beatles), ])

first_song_positions <- c(1,4,nrow(beatles))
View(beatles[first_song_positions,])

###############################
## Removing columns and rows
###############################

# remove the attribute On_album
beatles$On_album <- NULL
names(beatles)

# remove columns Platinum (at index 10) and Score (at index 11)
beatles1 <- beatles[,-c(10, 11)]
names(beatles1)
# alternative:
beatles2 <- beatles[, !(names(beatles) %in% c('Platinum', 'Score'))]

# create a subset of the data frame without songs in rows 2, 4 and 6 
beatles3 <- beatles[-c(2, 4, 6), ]
head(beatles3)

# create a subset of the data frame without songs in rows from 1 to 8 
beatles4 <- beatles[-(1:8), ]
head(beatles4)

##################################
## Updating column and row names
#################################

# get column names
colnames(beatles)

# change name of the column at the index 6 to 'Song_genre'
colnames(beatles)[6] <- "Song_genre"
colnames(beatles)

# change name of the column 'Song_genre' to 'Genre'
genreIndex <- which(colnames(beatles) == "Song_genre")
colnames(beatles)[genreIndex] <- "Genre"
colnames(beatles)

# change row names to a string containing word 'song' and a song order number
rownames(beatles)[1:10]
rownames(beatles) <- paste("song", 1:nrow(beatles))
head(beatles)

# change back row names to a string containing order number
rownames(beatles) <- c(1:nrow(beatles))
head(beatles)

##################################
## Retrieving and changing values
##################################

# for the first five songs, get values of the Title and Album.debut attributes
first_songs <- beatles[1:5, c("Title", "Album.debut")]
first_songs


# get titles of the songs from year 1964 not having McCartney as the only lead vocal
indexes <- which((beatles$Year == "1964") & (beatles$Lead.vocal != "McCartney"))
selected_songs <- beatles[indexes, 'Title']
head(selected_songs)
# alternative:
selected_songs <- beatles[(beatles$Year == "1964") & 
                            (beatles$Lead.vocal != "McCartney"), 'Title']
head(selected_songs)


# get the songs from year 1964 not having McCartney as one of the lead vocals
grepl("McCartney", beatles$Lead.vocal, fixed = TRUE)
selected_songs2 <- beatles[(beatles$Year == "1964") & 
                            !grepl("McCartney", beatles$Lead.vocal, fixed = TRUE),] 
head(selected_songs2)


# get the songs from year 1958, but only attributes Title and Album.debut
songs_1958 <- subset(beatles, Year == 1958, c("Title", "Album.debut"))
head(songs_1958)


# create a vector of logical values denoting whether the attribute 
# Album.debut has a value or not 
# Note: missing values are denoted here as empty strings ("")
empty_album_debut <- beatles$Album.debut == ""
# compute the number of missing values for Album.debut  
sum(empty_album_debut)

# replace the empty value of the Album.debut attribute with NA
beatles$Album.debut[empty_album_debut] <- NA
# compute the number of missing (NA) values for Album.debut  
sum(is.na(beatles$Album.debut))

# set the value back to empty string
beatles$Album.debut[empty_album_debut] <- ""

###############################
## Saving a dataset
###############################

# save dataset to a CSV file, but without the row names (row numbers) column
write.csv(beatles, "data/p2.csv", row.names = FALSE)

# save R object for the next session into file "p2.RData"
saveRDS(beatles, "data/p2.RData")

# restore R object from the file "p2.RData" in the next session
p2 <- readRDS("data/p2.RData")
str(p2)

###############################
# Task 3
###############################

# Create a new column in the *beatles* data frame called *Billboard.hit* 
# having TRUE for all songs that were in the Top 50 Billboard 
# (songs that have the Top.50.Billboard defined), 
# and FALSE for all other songs (not having this value set).

# Answer:
beatles$Billboard.hit <- FALSE
beatles$Billboard.hit[!is.na(beatles$Top.50.Billboard)] <- TRUE
head(beatles)
