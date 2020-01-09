#################################################################
# This R script is partially based on the 'Introductory Lab' 
# from the Social Network Analysis course at Stanford University
# (available at: https://sna.stanford.edu/lab.php?l=1)
#################################################################
 

####################################################
# LAB 1 - Introductory Lab                                              
# The objectives of this lab are to:
# - introduce the *igraph* R package          
# - load and manage network data
# - create and explore graph objects
# - generate network visualizations
# - export the network (graph) data for use in  
#   subsequent analysis
####################################################
 
###
# 0. IGRAPH R PACKAGE 
###
 
# In this and all subsequent labs, we will use the *igraph* 
# R package, one of the most popular R packages for SNA.
# The official manual is available at: 
# http://cran.r-project.org/web/packages/igraph/igraph.pdf
# also at: http://igraph.org/r/
#
# There is also an excellent tutorial for R and igraph beginners
# available at: http://kateto.net/networks-r-igraph
# It starts with introduction (reminder) of basic R constructs 
# and then proceeds to cover igraph; it offers a detailed 
# coverage of igraph functionalities in an easy to follow, 
# step-by-step manner. 


# If you haven't used the *igraph* package before, you'll have  
# to instal it first:
# install.packages('igraph')

# This and the following labs use the current version of igraph
# which is 1.2.4.2. To check your version type:
packageVersion('igraph')

# The installation is done only once, but you'll have to load 
# the package each time you need to use it:
library(igraph) 
 
# Sometimes, different packages overlap in functionality and 
# cause unexpected behavior when both are loaded simultaneously.
# If you ever want to remove an existing library, use the 
# "detach" command:
#
# detach(package:igraph)


# Let's also install and load the *magrittr* package that allows 
# us to write a sequence of functions to be applied on the same
# data item as a pipeline (using the 'pipe' notation: %>%).
# This makes the code more readable
# install.packages('magrittr')
library(magrittr)
 
 
###
# 1. LOADING NETWORK DATA
###

# We will use David Krackhardt's High-tech Managers Networks dataset.
# The data were collected from 21 management personnel in a high-tech, 
# machine manufacturing firm to assess the effects of a recent 
# management intervention program.
# The dataset and its description are available at: 
# http://networkdata.ics.uci.edu/netdata/html/krackHighTech.html

# We will load edge lists for 3 networks with the same actors (nodes),
# but different kinds of ties among them:
# - advice ties - one actor (ego) tends to ask the other (alter) 
#   for advice
# - friendship ties - one actor (ego) has named the other actor
#   (alter) as a friend
# - reports-to ties - one actor (ego) reports to the other one
#   (alter)
# All three networks are directed.

# These edge lists are stored in tabular format in .txt files.
# To read data from those files, we will use the read.table() function.
# read.table() is a common R function for loading data from
# files in which values are in tabular format. The function loads
# the table into a data frame object. By default, R assumes that 
# the table has no header and the values are delimited by any white space; 
# these settings are fine for our purposes here.
#
# One handy aspect of R is that you can read in data from a URL 
# directly by referencing the URL in the read.table() function,
# as follows: 
advice_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-Advice.txt')
friendship_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-Friendship.txt')
reports_to_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-ReportsTo.txt')
 
# If the files you want to work with are on your local machine, 
# you can reference them by the path name:
# advice_data_frame <- read.table('data/Krack-High-Tec-edgelist-Advice.txt')
# friendship_data_frame <- read.table('data/Krack-High-Tec-edgelist-Friendship.txt')
# reports_to_data_frame <- read.table('data/Krack-High-Tec-edgelist-ReportsTo.txt')

# Let's see what these data frames look like
head(advice_data_frame, 10)
head(friendship_data_frame, n=10)
head(reports_to_data_frame, n=10)

# We can also examine the strucure of the data frame:
str(advice_data_frame)

# And look at the unique values of each column:
apply(advice_data_frame, 2, unique) 


# The attribute data for this lab are in a comma-separated-value
# (CSV) file. read.csv() loads a CSV file into a data frame object.
# In this case, we set header=T, to tell R that the first 
# row of data contains column names.
# We can load csv file from a URL:
attributes <- read.csv('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-Attributes.csv', header=T)

# Or we can read from a local file:
# attributes <- read.csv('data/Krack-High-Tec-Attributes.csv', header=T)

# Check the structure and content of the attributes data frame
str(attributes)

# The attributes set includes: 
# - the actors' age (in years), 
# - tenure or length of service (in years), 
# - level in the corporate hierarchy; this is coded as follows: 
#   1=CEO, 2 = Vice President, 3 = manager), 
# - department, which is coded 1,2,3,4 with the CEO in department 0 ie not in a department

summary(attributes)

# Note that the summaries for the attributes LEVEL and DEPT do not
# have much sense, as these are, in essence, factor variables, but
# we'll deal with this later.

# A few notes regarding loading data from other data storage formats:
# 1) To read an excel file into R, use, for example, 'readxl' package
#
# 2) With the 'foreign' package you can read a few other custom
# data types, such as SPSS or SAS files
# 
# 3) When data files are part of an R package you can read them as 
# follows:
# data(kracknets, package = "NetData")
# This is good to know as R packages often come with a number 
# of datasets that can be used for practicing. 
 
 
###
# 2. MERGING DATA
###

# Since the three loaded edge lists relate to the same actors,
# connected through 3 different kinds of relationships, for easier
# data management, we can merge the data from the 3 data frames 
# (corresponding to the 3 edge lists) into one data frame.
 
# For convenience, we can assign some more meaningful column names 
# to our newly imported data frames:
colnames(advice_data_frame) <- c('ego', 'alter', 'advice_tie')
head(advice_data_frame)
 
colnames(friendship_data_frame) <- c('ego', 'alter', 'friendship_tie')
head(friendship_data_frame)
 
colnames(reports_to_data_frame) <- c('ego', 'alter', 'reports_to_tie')
head(reports_to_data_frame)
 
# Now, merge the three data frames into a single data frame holding all the data.
# Note that the merge() function by default merges two data frames on their
# columns with the same names - here, those are the 'ego' and 'alter' columns
krack_full_data_frame <- merge(advice_data_frame, friendship_data_frame)
head(krack_full_data_frame)
krack_full_data_frame <- merge(krack_full_data_frame,reports_to_data_frame)
head(krack_full_data_frame)

str(krack_full_data_frame)
 
# Now let's move on to some data processing.

# Some actor pairs may not be connected at all. Let's check if / how many
# such cases we have:
subset(krack_full_data_frame, 
       (advice_tie == 0 & friendship_tie == 0 & reports_to_tie == 0)) %>% nrow()

# Reduce to non-zero edges so that the data frame contains only
# actual ties of some type
krack_full_nonzero_edges <- subset(krack_full_data_frame, 
	(advice_tie > 0 | friendship_tie > 0 | reports_to_tie > 0))
nrow(krack_full_nonzero_edges)


###
# 3. CREATING YOUR FIRST SOCIAL GRAPH
###

# Now we can import our data into a graph object using igraph. 
# Coercing the data into a graph object is what allows us to 
# perform network analysis.
krack_full <- graph_from_data_frame(krack_full_nonzero_edges) 
summary(krack_full)

# By default, graph_from_data_frame() treats the first two columns 
# of a data frame as an edge list and any remaining columns as 
# edge attributes. Thus, the 232 edges appearing in the summary()
# output refer to the 232 pairs of vertices that are joined by 
# any type of tie. The tie types themselves are listed as edge 
# attributes.

# To view the adjacency matrix of the graph:
krack_full %>% as_adjacency_matrix()

# The 'name' attribute should be of integer type so that
# vertices are numbered in a more natural order. 
# Let's change that:
krack_full <- delete_vertex_attr(krack_full, 'name')
V(krack_full)$name <- 1:vcount(krack_full)
krack_full %>% as_adjacency_matrix()

# Since we have node attributes, we can include them in the graph 
# when creating the graph object.
# To do that, we first need to add a column to the 'attributes' df 
# to represent the nodes' 'names', (ie. values to be used for the 
# node 'name' attribute in the graph). These names have to match 
# the nodes' names in the edge list (ie. values of the 'ego' and
# 'alter' columns in the krack_full_nonzero_edges df). Since in 
# our case nodes' names are simply the nodes' ordinal numbers, the
# new node_name column in the 'attributes' df will simply be:
attributes$node_name <- 1:nrow(attributes)

# Next, we need to reorder the columns, since the column with the node
# name has to be the first one:
str(attributes)
attributes <- attributes[,c(5,1:4)]
# Now, create the graph. 
# Check the documentatio of the graph_from_data_frame() f. to better
# understand the 'vertices' parameter. 
krack_full <- graph_from_data_frame(d = krack_full_nonzero_edges, 
                                       vertices = attributes) 
summary(krack_full)
# Note that we now have 'age,' 'tenure,' 'level', and 'dept'
# listed alongside 'name' as vertex attributes.

# Since the 'name' attribute is again given as character
# we'll replace its values with the corresponding integer
# values:
krack_full <- delete_vertex_attr(krack_full, 'name')
V(krack_full)$name <- 1:vcount(krack_full)

# We can get values for a given attribute for all of
# the actors in the network - for example:
vertex_attr(krack_full, 'AGE')
# Alternatively:
V(krack_full)$AGE
# where 'V' represents the set of nodes (vertices) of the graph
# whose name is given in the brackets

# Examine the actors' tenure attribute
vertex_attr(krack_full, 'TENURE')
# or
V(krack_full)$TENURE

# We can also get attribute values for a subset of nodes.
# For example, to get the tenure of only those who are 
# above 40 years of age:
vertex_attr(krack_full, 'TENURE', V(krack_full)[V(krack_full)$AGE > 40])
# or
V(krack_full)$TENURE[V(krack_full)$AGE > 40]

# Or, we can the age of employees in the largest department:
table(V(krack_full)$DEPT)
V(krack_full)$AGE[V(krack_full)$DEPT == 2]

 
# To get a vector of edge values for a specific type of tie, use the 
# edge_attr() function.
edge_attr(krack_full, 'advice_tie')
# Alternatively, it can be done as follows:
E(krack_full)$advice_tie
# where 'E' represents the set of edges of the graph
# whose name is given in the brackets

# We can examine the proportion of actor pairs that are 
# connected by the advice connection (out of all actor 
# pairs that are connected by any connection type):
E(krack_full)$advice_tie %>% table() %>% prop.table()

# Access the friendship ties
edge_attr(krack_full, 'friendship_tie')
# alternative:
E(krack_full)$friendship_tie
# The proportion of friendship relations 
# (among the three types of relations):
E(krack_full)$friendship_tie %>% table() %>% prop.table()

# And, to reports-to ties
edge_attr(krack_full, 'reports_to_tie')
# alternative:
E(krack_full)$reports_to_tie
# The proportion of reports-to relations:
E(krack_full)$reports_to_tie %>% table() %>% prop.table()

 
# If you would like to symmetrize the network, making all asymmetric
# (directed) ties symmetric (undirected), use the as.undirected() function
# (check the documentation for the 'mode' parameter): 
krack_full_undirect <- as.undirected(krack_full, mode='collapse')
summary(krack_full_undirect)

# Note that the resulting, undirected, network has no edge attributes.
# If we want to collapse the directed network and keep edge weights,
# we should set the "edge.attr.comb". For example, to sum the values
# of the friendship_tie attribute and ignore the other two tie attributes
# we set "edge.attr.comb" as follows:
krack_friend_undir_weighted <- as.undirected(krack_full, 
                                             mode='collapse', 
                                             edge.attr.comb = list(friendship_tie='sum', 'ignore'))

# Note: the list given as the value of the 'edge.attr.comb' parameter specifies that 
# the 'friendship_tie' attribute of the new (undirected) edge should be the sum of the 
# 'friendship_tie' attr. of the corresponding edges in the directed graph; and that the 
# rest of the attributes should be ignored (=dropped). 
# To learn more about setting these options, check the documentation for attribute.combination
?attribute.combination

summary(krack_friend_undir_weighted) 
E(krack_friend_undir_weighted)$friendship_tie
E(krack_friend_undir_weighted)$friendship_tie %>% table()
 

###
# 4. CREATING MORE GRAPHS
###

# Now, starting from the krack_full graph, we'll create a separate network 
# for each tie type

# advice network
krack_advice <- delete_edges(krack_full, 
                             E(krack_full)[E(krack_full)$advice_tie == 0])
summary(krack_advice)
# Note that this will not remove the other kinds of ties (friendship, reports_to).
# If two employees are connected not only through the advice tie, but also through
# other kinds of ties, those other kinds will be preserved. If you want to remove 
# those other kinds of ties, you need to remove the corresponding edge attributes
# using the delete_edge_attr() f.

# friendship network
krack_friendship <- delete_edges(krack_full, 
                                 E(krack_full)[E(krack_full)$friendship_tie == 0])
summary(krack_friendship)

# reports-to network
krack_reports_to <- delete_edges(krack_full, 
                                 E(krack_full)[E(krack_full)$reports_to_tie == 0])
summary(krack_reports_to)

# Note: an alternative way of creating a subgraph is by using the 
# subgraph.edges() function and specifying which edges are to be preserved.
# For example, to create the friendship network:
krack_friendship_2 <- subgraph.edges(krack_full, 
                                     eids = E(krack_full)[E(krack_full)$friendship_tie > 0])
summary(krack_friendship_2) 


###
# 5. VISUALIZING NETWORKS
###
 
# The igraph package allows us to use R's plot() function to generate 
# custom visualizations of our networks.

# R only lets us look at one plot at a time. To be able to compare
# multiple networks, that is, their plots, you may want to save 
# each plot in a separate PDF or image file. The code below shows 
# how to do that. Alternatively, to just create a plot, execute 
# the code between the pdf() / jpeg() function and dev.off().

# assure that you are in the "SNA_Intro_Labs" subdirectory
setwd('SNA_Intro_Labs')

# First, let's plot the network with all possible ties
jpeg("graphs/1.1_Krackhardt_Full.jpg")
plot(krack_full,
     edge.arrow.size=.3,  # reduce the size of arrows
     main="High-tech Managers Networks")
dev.off()
# Check the documentation of the jpeg() function to see
# how you can customise the image dimension, quality, ..

# For a full list of igraph plotting parameters see Section 4.1
# of the Network visualization in R tutorial:
# http://kateto.net/network-visualization

 
# This is a bit of a jumble, so let's look at the networks for
# single edge types
 
pdf("graphs/1.2_Krackhardt_Advice.pdf")
plot(krack_advice,
     edge.arrow.size=.3,
     main="High-tech Managers Advice Network")
dev.off()
# Check the documentation of the pdf() function to see
# how you can customise the generated PDF document
# (e.g. by setting font, paper size, title, etc)
 
jpeg("graphs/1.3_Krackhardt_Friendship.jpg")
plot(krack_friendship,
     edge.arrow.size=.3,
     main="High-tech Managers Friendship Network")
dev.off()

jpeg("graphs/1.4_Krackhardt_Reports.jpg")
plot(krack_reports_to,
     edge.arrow.size=.3,
     main="High-tech Managers Reports-to Network")
dev.off()
 
# We can optimize the layout by applying a layout 
# algorithm to the specific set of ties we care about. 
# The above graphs were plotted using the Fruchterman-Rheingold
# algorithm (the default one). Other options are described in 
# the igraph help page for "layout_" which can be accessed by 
# entering ?layout_
?layout_

# Now we'll use the layout generated for the Reports-to network
# to plot the friendship ties. The idea is to try to (visually) 
# explore how well friendship relations go along the official 
# organisational structure (reflected in the reports_to relation).

reports_to_layout <- layout_nicely(krack_reports_to)
jpeg("graphs/1.5_Krackhardt_Friendship_with_reports_layout.jpg")
plot(krack_friendship, 
     layout=reports_to_layout,
     edge.arrow.size=.3,
     main="High-tech Managers Friendship Network")
dev.off()
 

# Now let's color-code vertices by department 
# Check the possible values for the 'DEPT' attribute
unique(V(krack_full)$DEPT)
# Initiate the vector of node colors:
dept_vertex_colors <- V(krack_full)$DEPT + 1
# Select a color palette using the ColorBrewer (http://colorbrewer2.org) 
colors <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3')
# Associate department numbers with colors from the 'colors' palette
dept_vertex_colors <- sapply(dept_vertex_colors, function(x) colors[x])

jpeg("graphs/1.6_Krackhardt_Friendship_Color.jpg") 
plot(krack_friendship, 
    layout=reports_to_layout, 
    vertex.color=dept_vertex_colors, # setting node color
    vertex.label=V(krack_full)$LEVEL,   # using role (level) as the vertex label
    edge.arrow.size=.3,
    main="Friendship network\n(node color denotes department)")
dev.off() 


# Now let's make the vertex size proportional to the actors' tenure:
tenure_vertex_sizes = V(krack_full)$TENURE
summary(tenure_vertex_sizes)
# There is a large difference between the min and max values, which will not  
# 'translate' well into visual depiction of the graph. So, we will somewhat
# 'smooth' the difference:
tenure_vertex_sizes <- log(tenure_vertex_sizes)^2 + 7
summary(tenure_vertex_sizes)

jpeg("graphs/1.7_Krackhardt_Friendship_Vertex_Size.jpg") 
plot(krack_friendship, 
     layout=reports_to_layout, 
     vertex.color=dept_vertex_colors, 
     vertex.label=V(krack_full)$LEVEL,
     edge.arrow.size=.3,
     vertex.size=tenure_vertex_sizes, # setting the vertex size
     main="Friendship network\n(node color denotes department, size denotes tenure)")
dev.off() 


# We can also add visualization related attributes directly to a graph, 
# that is to the vertices and edges of the graph. Since such attributes
# serve only for visualizing a graph, we will first replicate the graph, 
# and add the visualisation attributes to the replicated one, thus avoiding
# to 'burden' the 'main' graph with visual details.
krack_full_viz <- krack_full

# As an example, let's visualise two different kinds of ties - friendship
# and reports-to - on the same graph, to further explore how these two 
# kinds of ties differ among the examined actors.

# Since we are not interested in the advice tie, we will remove that edge
# attribute from the graph we want to visualize
krack_full_viz <- delete_edge_attr(krack_full_viz, 'advice_tie')
summary(krack_full_viz)
# Add colors to edges based on their type (friendship, reports_to)
E(krack_full_viz)[ E(krack_full_viz)$friendship_tie==1 ]$color <- '#377eb8' # blue
E(krack_full_viz)[ E(krack_full_viz)$reports_to_tie==1 ]$color <- '#984ea3' # purple
# Increase the width of the reports_to edges
E(krack_full_viz)[ E(krack_full_viz)$friendship_tie==1 ]$width <- 1
E(krack_full_viz)[ E(krack_full_viz)$reports_to_tie==1 ]$width <- 2
E(krack_full_viz)$arrow.size <- .3 
V(krack_full_viz)$size <- tenure_vertex_sizes
V(krack_full_viz)$color <- '#fed98e' # yellow as the color for all vertices
V(krack_full_viz)$frame <- '#000000' # black as the color of the edge/frame of all vertices

# Since we have added the visualization related attributes
# to the graph object directly, we can visualize it without
# specifying any parameters to the plot() function
jpeg("graphs/1.8_Krackhardt_Overlayed_Ties.jpg")
plot(krack_full_viz, 
     layout=reports_to_layout)
dev.off() 
 

#############
# Task 1
#############

# Use network visualisations to explore the similarities and differences of 
# the advice and reports-to networks. To do that, follow a procedure similar to  
# the one used above for comparing / contrasting friendship and reports-to netwoks. 


 
###
# 5. EXPORT THE NETWORK
###
 
# The write.graph() function exports a graph object in various
# formats readable by other programs. For example, a graph can be
# exported in the 'graphml' format and imported in Gephi for 
# visualisation.
write.graph(krack_full, file='output/lab1/krack_full.graphml', format="graphml")
 
# For a more general file type (e.g., importable to Excel),
# use the "edgelist" format. Note that this file format will keep
# neither node nor tie attributes; only the edges will be stored.
write.graph(krack_full, file='output/lab1/krack_advice.txt', format="edgelist")

# We can also save graphs as RData files. This is the best option if we intend
# to further process graphs in R, since this format will keep all the specific
# characteristics of the graph objects that we have created.
saveRDS(krack_full, "output/lab1/krack_full.RData")
saveRDS(krack_advice, "output/lab1/krack_advice.RData")
saveRDS(krack_friendship, "output/lab1/krack_friendship.RData")
saveRDS(krack_reports_to, "output/lab1/krack_reports_to.RData")
