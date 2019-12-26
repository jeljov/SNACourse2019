###############################
# Vectors
###############################

# Numeric
x <- 41.5
x
x[1]
print(class(x))

# Integer
x <- 5L
x
x[1]
print(class(x))

# Character
x <- "Hello"
x
print(class(x))

# Logical (true/false)
x <- TRUE
x
print(class(x))
y <- TRUE
x + y
y <- FALSE
x + y

# Creating vector with more elements
v <- c(1.5, 5, 3)
v
v[1]

###############################
# Vector arithmetics
###############################

# (+, -, *, /, ^)
v + 1
-v
2*v + 2
v/2
v^2

# get the max value
v1 <- c(1, 2, 3, 4, 5)
max(v1)

# get the min value
min(v1)

# get the range of values
range(v1)

# calculate the sum of all elements
sum(v1)
z <- c(TRUE, TRUE, FALSE, TRUE)
sum(z)

# calculate the product of all elements
prod(v1)

################################
# Generating regular sequences
################################

# generate a sequence from 1 to 10
s1 <- 1:10
s1

# generate a sequence from 10 to 1
s2 <- 10:1
s2
rev(s2)

# generate a sequence from 3.2 to 4.7, with a step 0.2
seq(3.2, 4.7, by = 0.2)

################################
# Factor values
################################

# create a vector of character values and 
# convert it to a vector of factor values
v2 <- c("cold", "mild", "mild", "hot", "cold")
v2
temp <- factor(v2)
temp

# print all levels 
levels(temp)

# change the order of levels
temp2 <- factor(x=v2, levels = c('cold', 'mild', 'hot'))
levels(temp2)

# print the summary
summary(temp2)



################################
# Missing values
################################

# check which values are NAs
v3 <- c(1, NA, 3, 4, NA)
is.na(v3)
which(is.na(v))

# NaN - not a number; should not
# be mistaken with missing values
0/0


################################
# Data frame
################################

# create a data frame
data1 <- data.frame(
  days = c("Mon", "Tue", "Wen", "Thu", "Fri"), 
  temp = c(25, 28, 30, 29, 34))
data1

# create a data frame from existing vectors
data2 <- data.frame(v1, v2, v3)
data2

################################
# Task 1
################################

# Create a dataframe called *co2.emissions* containing two columns: 
# *years* (2000, 2002, 2004, 2006, 2008, 2010) and 
# *emission* (2.7, 2.9, 4, 4.9, 5.3, 6.2).

# Answer:
co2.emissions <- data.frame(
  year = c(2000, 2002, 2004, 2006, 2008, 2010),
  emission = c(2.7, 2.9, 4, 4.9, 5.3, 6.2)
)
co2.emissions

################################
# Loading data frame from a file
################################

getwd()
setwd('R_Intro_Lab')

# reading a data frame from a CSV file
beatles <- read.csv("data/beatles_v1.csv", stringsAsFactors = FALSE)
beatles

# print the number of rows
nrow(beatles)

# print the number of columns
ncol(beatles)

# str function
str(beatles)

# summary function
summary(beatles)

# head and tail functions
head(beatles)
tail(beatles)

# View function
View(beatles_df)

# retrieve and change column names
colnames(beatles)

beatles1 <- beatles
colnames(beatles1) <- c("song_name", "release_year", "duration")
str(beatles1)

# remove column duration
beatles1$duration <- NULL
str(beatles1)

################################
# Subsetting
################################

# get an element from the row 3, column 1 
song <- beatles[3, 1]
song

# get the third row
beatles.subset <- beatles[3,]
beatles.subset

# get rows at positions from 3 to 5 (inclusive) 
beatles.subset1 <- beatles[3:5,]
beatles.subset1

# get rows at positions 3 and 6 
beatles.subset2 <- beatles[c(3,6),]
beatles.subset2

# get the second column
years <- beatles[,2]
years

# get the Year column
years <- beatles$Year
years
# alternative:
years <- beatles[['Year']]
years

# retrieve all songs released before year 1965
songsBefore1965 <- beatles[beatles$Year < 1965,]
songsBefore1965
# alternative:
subset(beatles, Year < 1965)

# retrieve all songs released before year 1965 with duration less than 150 seconds
shortSongsBefore1965 <- beatles[beatles$Year < 1965 & beatles$Duration < 150,]
shortSongsBefore1965
# alternative:
subset(beatles, Year < 1965 & Duration < 150)


################################
# Task 2
################################
# Print the number of songs from 1963 that last more than 2 minutes (120 seconds).

# Answer:
nrow(beatles[beatles$Year == 1965 & beatles$Duration > 120,])

################################
# Plotting
################################

# create a plot for the given vectors 
plot(c(2, 8, 5), c(25, 10, 30))

# include ggplot2 library
install.packages("ggplot2")
library(ggplot2)

# render a plot for the given data frame (columns Year and Duration)
ggplot(beatles, aes(x=Year, y=Duration))

# render a plot for the given data frame with points
ggplot(beatles, aes(x=Year, y=Duration)) + geom_point()

# add a column with song rating (1-5) by randomly sampling rating scores
# between 1 and 5 
beatles$rating <- sample(x = 1:5, size = nrow(beatles), replace = TRUE)

# render a bar chart showing rating counts (= counts of songs with certain rating)
ggplot(beatles, aes(x=rating)) + geom_bar()

# render a bar chart for Year-Duration 
ggplot(beatles, aes(x=Year, y=Duration)) + 
  geom_col()

# transform Year to factor so that only years with songs
# are shown on the X-axis
beatles$Year <- factor(beatles$Year)
ggplot(beatles, aes(x=Year, y=Duration)) + 
  geom_col()

# render a bar chart with custom title and axes labels  
# and separate color for each year
ggplot(beatles, aes(x=Year, y=Duration, fill = Year)) + 
  geom_col(show.legend = FALSE) +
  labs(x="Song release years", y="Song duration",
       title="Duration of songs throughout the years")

# render a bar chart showing the number of songs per year 
# (year is on the X-axis, the number of songs per year on the y-axis)
ggplot(beatles, aes(x=Year, fill=Year)) + 
  geom_bar(show.legend = FALSE) +
  labs(x="Song release years", y="Number of songs", 
       title("Number of songs throughout the years")
       
       
# render a line chart showing Year and Duration for the first five songs; 
# explore line and pont specific properties
ggplot(beatles[1:5,], aes(x=Year, y=Duration, group = 1)) +
  geom_line(colour = "blue", linetype = "dotted", size = 1.5) + 
  geom_point(colour="blue", size = 4, shape = 'square') +
  labs(x="Song release years", y="Song duration",
       title="Duration of songs throughout the years")
       

################################
# Task 3
################################

# Create a line chart from the dataset *co2.emissions* (created in Task 1) 
# with the x-axis representing the years, and the y-axis representing the 
# values of CO2 emissions.
# Chart title should be "China CO2 Emissions" and y-axis should be 
# labelled "CO2 Emissions".

# Answer:
co2.emissions$year <- as.factor(co2.emissions$year)
ggplot(co2.emissions, aes(x = year, y = emission, group = 1)) +
  geom_line()  +
  labs(title="China CO2 Emissions", y="CO2 Emissions")

################################################################
# Homework - Complete interactive R tutorials with Swirl
################################################################
install.packages("swirl")
library("swirl")
swirl()