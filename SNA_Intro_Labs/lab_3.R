############################################################################
# This R script is partially based on the 'LAB 2: Methodological beginnings'
# from the Social Network Analysis course at Stanford University
# (available at: https://sna.stanford.edu/lab.php?l=2)
############################################################################


##############################################################
# 
# LAB 3
#
# The objective of this lab is to introduce measures of 
# connectivity and cohesion in a network, including:
# - reachability, shortest path, diameter
# - density, reciprocity, triadic census
# - transitivity (clustering coef.)
# - k-cores
# It also covers basic measures of homophily (assortativity) 
# and heterogenity.
#
##############################################################


### 
# 1. SET UP THE WORKSPACE
###

# Load the required libraries
library(igraph)
library(ggplot2)
library(tidyr)

# We will also use *visNetwork* R package to create interactive
# graph plots. For a brief introduction to visNetwork, see:
# - Online documentation: http://datastorm-open.github.io/visNetwork/
# - Section 6.2 of the 'Network visualization with R' tutorial, at:
#   http://kateto.net/network-visualization
# We will cover this visualization package in more detail in Lab 5.

# install.packages('visNetwork')
library(visNetwork)


# Load the data, that is, the networks we have created in Lab 1
getwd()
setwd("SNA_Intro_Labs")

data_dir = "output/lab1/"

krack_advice <- readRDS(paste0(data_dir, "krack_advice.RData"))
summary(krack_advice)

krack_friendship <- readRDS(paste0(data_dir, "krack_friendship.RData"))
summary(krack_friendship)

krack_reports_to <- readRDS(paste0(data_dir, "krack_reports_to.RData"))
summary(krack_reports_to)


### 
# 2. MEASURES OF CONNECTIVITY AND COHESION
###
  
#
# 2.1 Reachability
#

# The subcomponent function allows us to compute reachability for 
# an individual node. For example, reachability for node 1:
subcomponent(krack_advice, 1, mode = 'out')
subcomponent(krack_reports_to, 1, mode = 'out')
subcomponent(krack_reports_to, 1, mode = 'in')

# Note that the subcomponent function returns a vector of ids
# of vertices that are in the same component as the given vertex;
# so, the result includes the vertex itself.

# To get graph-wide statistics, we need a loop that will produce
# a matrix with reachability indicators (0/1) for each pair of 
# nodes in the graph

reachability <- function(g, m) {
	reach_mat = matrix(data = 0, nrow = vcount(g), ncol = vcount(g))
	for (i in 1:vcount(g)) {
		# reach_mat[i,] = 0
		node_i_reach <- subcomponent(g, i, mode = m)  
		for (alter in node_i_reach) 
		 	reach_mat[i, alter] = 1
	}
	return(reach_mat)
}

# Let's compare the advice and reports-to networks with
# respect to reachability

# Compute reachability matrices for the advice network,
# first for incoming links, than for outgoing edges:
reach_advice_in <- reachability(krack_advice, 'in')
View(reach_advice_in)
# It seems that in the advice network we have a perfect reachability.
# Let's check:
all(reach_advice_in == 1)
reach_advice_out <- reachability(krack_advice, 'out')
View(reach_advice_out)
all(reach_advice_out == 1)
# So, in advice network, each node is reachable from any other node
# We can further verify that by checking if the advice network is a
# strongly connected network:
is.connected(krack_advice, mode = 'strong')

# If our graph was larger, it would be difficult to determine the level of 
# reachability by observing the reachability matrix. 
# Instead, we can compute, for each node in a graph, the number of other 
# nodes reachable by the given node:
apply(reach_advice_in, 1, function(x) sum(x==1))
apply(reach_advice_out, 1, function(x) sum(x==1))
# As we saw above, every vertex is reachable from any other vertex

# Now, the same but for the reports-to network
reach_reports_to_in <- reachability(krack_reports_to, 'in')
View(reach_reports_to_in)
reach_reports_to_out <- reachability(krack_reports_to, 'out')
View(reach_reports_to_out)
# Far below the level of reachability observed in the advice network.
# Check for each vertex the number of reachable vertices in the network: 
apply(reach_reports_to_in, 1, function(x) sum(x==1))
apply(reach_reports_to_out, 1, function(x) sum(x==1))
# In general, very low reachability; only one - the central node - is 
# reachable by all other nodes

# To better understand these results, we can visualize the network.
# In particular, we will use the visNetwork package to create an 
# interactive visualization that allows for focusing on those 
# vertices we are interested in.

# First, prepare the graph data in the format required by visNetwork.
# As a minimum for graph visualization, visNetwork requires:
# - nodes data frame, with 'id' column (plus any additional columns with node attributes)
# - edges data.frame, with 'from' and 'to' columns (plus any additional columns with edge attributes)
reports_to_nodes <- data.frame(id=V(krack_reports_to)$name, stringsAsFactors = FALSE)
reports_to_edges <- data.frame(as_edgelist(krack_reports_to), stringsAsFactors = FALSE)
colnames(reports_to_edges) <- c('from', 'to')
# add arrows to display edge direction:
reports_to_edges$arrows <- "to"
# add labels to nodes
reports_to_nodes$label <- reports_to_nodes$id
# Finally, display the network
vis_net <- visNetwork(nodes = reports_to_nodes, 
                      edges = reports_to_edges, 
                      main="Reports-to network") 
vis_net
# Add highlighting to show reachable nodes
vis_net %>% 
  visOptions(highlightNearest = list(enabled=TRUE, degree=2, algorithm='hierarchical'))


# An average of reach for a network reveals what percent of the network 
# is connected in some way.
# For the advice network, we saw that there is a connection between 
# each pair of nodes in the network. However, in the reports-to network
# the level of reach is far lower:
sum(reach_reports_to_in)/(vcount(krack_reports_to)^2)
sum(reach_reports_to_out)/(vcount(krack_reports_to)^2)
# Only ~13% of the potential pairwise connections were realized


##
#
# TASK 1: 
# Compute the reachability for the friendship network. 
# Visualise this network and reachability of its nodes using
# the visNetwork package.
#
##


#
# 2.2 Shortest path (geodesic) and diameter
#

# Often we want to know path distances between actors in a network. 
# This is typically done by calculating geodesics, or shortest paths between
# each actor (node) pair. 
# By averaging geodesics for the entire network we get average distance in
# the network, which is a sort of cohesiveness score. 

# For example, we can compute shortest paths between each pair of nodes in
# the friendship network.
# First, let's compute shortest paths to each vertex
sp_friendship_in <- distances(krack_friendship, mode='in')
View(sp_friendship_in)
# Next, shortest paths from each vertex
sp_friendship_out <- distances(krack_friendship, mode='out')
View(sp_friendship_out)

# Inf values mean no connection between the corresponding two verices.
# Replace Inf values with NA
sp_friendship_in[is.infinite(sp_friendship_in)] <- NA
sp_friendship_out[is.infinite(sp_friendship_out)] <- NA

# Then, we can compute average shortest path for each node in the 
# friendship network
mean_sp_friendship_in <- apply(sp_friendship_in, 1, function(x) mean(x, na.rm = TRUE))
mean_sp_friendship_out <- apply(sp_friendship_out, 1, function(x) mean(x, na.rm = TRUE))
summary(mean_sp_friendship_in)
summary(mean_sp_friendship_out)


# Let's visualize these values to better understand them
# First, prepare the data
mean_sp_friendship_df <- data.frame(node_id=as.integer(V(krack_friendship)$name),
                                    sp_in = mean_sp_friendship_in,
                                    sp_out = mean_sp_friendship_out)
mean_sp_friendship_df_long <- pivot_longer(data = mean_sp_friendship_df, 
                                           cols = starts_with("sp"),
                                           names_to = "mode",
                                           names_ptypes = list(Mode=factor()),
                                           values_to = "SP")
head(mean_sp_friendship_df_long)
# Then, make a plot (bar plot)
ggplot(data = mean_sp_friendship_df_long,
       mapping = aes(x = node_id, y = SP, fill=mode)) +
  geom_col(position = 'dodge') +
  scale_fill_discrete(name='Kind of shortest path (SP)',
                      # breaks=c('sp_in', 'sp_out'),
                      labels=c('SP to the vertex', 'SP from the vertex')) +
  labs(x = 'Node ID', y = "Average Shortest Path") +
  scale_x_continuous(breaks = seq(1,21,1)) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  coord_flip()


# Let's now compute the diameter for the friendship network, that is, the 
# longest shortest path in the network:
diameter(krack_friendship)

# Note: since the frienship network is not connected, the diameter is computed
# on the largest connected component, that is, on the giant component.

# The notable difference between the value of the diameter (=5) and the average 
# shortest paths, suggests that we have some peripheral nodes in the network. 
# To get the nodes that are most apart, we can use the farthest_vertices() f.
farthest_vertices(krack_friendship)
# Take the vertices that are most apart:
most_apart <- farthest_vertices(krack_friendship)$vertices

# Get the shortest path (sequence of edges) that connect the two vertices
# that are most apart:
most_apart_path <- shortest_paths(krack_friendship, 
                                  from = most_apart[1], 
                                  to = most_apart[2], 
                                  output = 'epath')
most_apart_path
most_apart_path <- most_apart_path$epath[[1]]

# Let's plot the two most apart nodes and the shortest path that 
# connects them. 
# Define node color so that all the nodes are gold except the 
# two most distant ones which will be painted in red 
node_colors <- rep('gold', times=vcount(krack_friendship))
node_colors[most_apart] <- 'red3'
# Define edge color so that all edges are grey except those that
# connect the two most distant nodes - these will be red
edge_colors <- rep('grey', times=ecount(krack_friendship))
edge_colors[most_apart_path] <- 'red3'
# Define edge width (thickness) so that the width of all edges 
# is 1 except those that connect the two most distant nodes - their
# width will be 3.5
edge_width <- rep(1, times=ecount(krack_friendship))
edge_width[most_apart_path] <- 3.5
# Now, plot the network
plot(krack_friendship, 
     layout=layout_with_kk(krack_friendship), 
     vertex.color=node_colors, 
     edge.color=edge_colors,
     edge.width=edge_width,
     edge.arrow.size=.30,
     main="The longest geodesic in the friendship network")


##
#
# TASK 2: 
# Do the same kind of analysis for the advice network.
#
##


#
# 2.3 Density 
#

# The density of a graph is the ratio of the number of 
# edges in the graph and the number of possible edges
?edge_density

krack_list = list(advice=krack_advice, 
                  friendship=krack_friendship, 
                  reports_to=krack_reports_to)
lapply(krack_list, edge_density)

# Observe the difference in the density of the three
# examined networks

#
# 2.4 Dyad census and reciprocity
#

# Reciprocity represents the proportion of mutual (reciprocated) 
# connections in a directed graph. 
# To compute it, we can use the function that computes 
# dyad census, that is, the number of dyads (node pairs) with:
# - mutual connections ($mut)
# - non-mutual connections ($asym)
# - no connection at all ($null)
dc_friendship <- dyad_census(krack_friendship)
dc_friendship
# To get reciprocity:
(2*dc_friendship$mut)/ecount(krack_friendship)

# Note: since dc_friendship$mut is the number of node pairs with
# mutual connections, we multiply it by 2 to get the number of 
# connections (edges) between those pairs.

# We can also use the reciprocity function:
reciprocity(krack_friendship)

# Reciprocity is also defined as the (conditional) probability 
# that if A is connected to B, then B will be connected to A.
# In this interpretation, reciprocity is the following ratio:
dc_friendship$mut/(dc_friendship$mut + dc_friendship$asym)

# This can be also interpreted as the proportion of mutually
# connected node pairs out of all the pairs that are connected. 
# In this interpretation, reciprocity can be computed using 
# the reciprocity f. with the mode parameter set to 'ratio':
reciprocity(krack_friendship, mode = 'ratio')

# Compare the three networks with respect to reciprocity:
lapply(krack_list, reciprocity)
lapply(krack_list, reciprocity, mode = 'ratio')


#
# 2.6 Triad census
#

# Triad census is the number of different configurations of 3 vertices
# (triples) in a graph.
# See the figures in the 'resources' folder illustrating those different
# configurations and their labels - those are standard labels that are used in
# SNA analysis to refer to particular triple configurations. Their description
# is also given in the documentation of the triad_census() function:
?triad_census


# We'll first build a vector of labels for different triad types. 
# Then we'll combine this vector with the triad censuses for the different 
# networks.

census_labels = c('003',
                  '012',
                  '102',
                  '021D',
                  '021U',
                  '021C',
                  '111D',
                  '111U',
                  '030T',
                  '030C',
                  '201',
                  '120D',
                  '120U',
                  '120C',
                  '210',
                  '300')

triad_df <- data.frame(labels=census_labels,
                       advice=triad.census(krack_advice), 
                       friendship=triad.census(krack_friendship),
                       reports_to=triad.census(krack_reports_to))
triad_df

# we can also examine triad proportions:
triad_prop <- apply(triad_df[,-1], 2, function(x) (x/sum(x)) %>% round(digits = 3)) 
triad_prop_df <- as.data.frame(triad_prop)
triad_prop_df <- cbind(labels=census_labels, triad_prop_df)
triad_prop_df


##
#
# TASK 3: 
# (a) Identify the most dominant triad forms in each of the 3 examined networks. 
# (b) What do the identified triads tell us (for each network)? In other words, 
#     try to interpret the identified triad forms in the context of each network.
# (c) How do the networks differ with respect to their triad census?
#
##


#
# 2.5 Transitivity
#

# Transitivity implies that, if A is connected to B, and B is connected to C, 
# then A is connected to C, as well. In real networks, perfect transitivity is
# rarely present, partial transitivity is more typical: the fact that A is 
# connected to B and B is connected to C does not guarantee that A will be 
# related to C, but makes it much more likely; for example, the friend of my 
# friend is not necessarily my friend, but is far more likely to be my friend 
# than a randomly chosen member of the population.

# Transitivity is also known as the clustering coefficient. 
# It can be:
# - global: refers to the graph as a whole; it is defined as the ratio 
#           of the triangles and the connected triples in the graph
# - local: refers to an individual vertex; it is defined as the ratio 
#          of the triangles connected to the vertex and the triples 
#          centered on the vertex (see, e.g.: 
#          https://www.researchgate.net/profile/Ruggero_G_Bettinardi/publication/317033037/figure/fig51/AS:496043125952517@1495277305701/Figure-C4-Local-clustering-coefficient-Schematic-representation-of-how-to-compute-the_W640.jpg)

?transitivity
# Note: the transitivity function treats the input graph as undirected.

lapply(krack_list, transitivity, type = 'global')
# clearly, the highest (global) clustering is in the advice network

# We can also examine clustering at the local level:
friend_trans <- transitivity(krack_friendship, type = 'local', vids = V(krack_friendship))
# Note: even when local transitivity is computed for all the nodes, the 'vids' parameter 
# has to be explicitly specified 

friend_trans <- data.frame(node_id = 1:21,
                           trans= friend_trans)
friend_trans[order(friend_trans$trans, decreasing = TRUE),]
# when examining these results consider that the friendship network 
# is treated as an undirected network

# Let's visualise the undirected friendship network to better understand
# the obtained results. We'll use visNetwork as it allows for interaction
# with the network nodes

# First, transform the friendship network into undirected (unweighted) network
krack_friend_undirect <- as.undirected(krack_friendship, mode = 'collapse')
summary(krack_friend_undirect)
# Next, prepare the network data in the format required by visNetwork:
friend_undirect_nodes <- data.frame(id=V(krack_friend_undirect)$name, stringsAsFactors = FALSE)
friend_undirect_edges <- data.frame(as_edgelist(krack_friend_undirect), stringsAsFactors = FALSE)
colnames(friend_undirect_edges) <- c('from', 'to')
# add labels to nodes
friend_undirect_nodes$label <- friend_undirect_nodes$id
# Finally, display the network
visNetwork(nodes = friend_undirect_nodes, 
           edges = friend_undirect_edges, 
           main="Undirected friendship network") %>% 
  visOptions(highlightNearest = TRUE)



#
# 2.7 K-cores
#

# The k-core is the maximal subgraph in which every node has degree of at least k.
# In other words, it is a group of actors where each actor is directly connected
# to at least k other actors in the group.
# A node has coreness M if it belongs to a M-core but not to (M+1)-core.
# Coreness is used as a means of examining tightly connected groups in a network.

# The coreness function computes the coreness of each vertex in the network:
?coreness
friend_core_in <- coreness(krack_friendship, mode = 'in')
friend_core_in
table(friend_core_in)
friend_core_out <- coreness(krack_friendship, mode = 'out')
friend_core_out
table(friend_core_out)

# To better understand the coreness results, let's visualize them

# We will use a palette of colors from the *RColorBrewer* R package
# install.packages('RColorBrewer')
library(RColorBrewer)

# We will need as many colors in the palette as there are different 
# coreness values
n_colors <- length(unique(friend_core_in))

# To preview a particular color palette, use the function:
?display.brewer.pal
display.brewer.pal(n = n_colors, name = 'Blues')
# Then, create a palette:
core_colors <- brewer.pal(n = n_colors, name = 'Blues')

# Now, use the created palette to color nodes based on their in-coreness, 
# that is, their coreness based on their incoming edges
plot(krack_friendship,
     layout = layout_nicely(krack_friendship),
     vertex.color = core_colors[friend_core_in],
     edge.arrow.size = 0.2,
     main="Friendship network\n (node color denotes in-coreness)")

# Now, let's inspect the most densely connected part of the network - nodes
# that have the highest values for both in- and out-coreness.
# First, identify them:
friend_core <- which(friend_core_in == max(friend_core_in) & 
                       friend_core_out == max(friend_core_out))
# Then, extract the subgraph with those nodes only:
friend_core_net <- induced_subgraph(krack_friendship, 
                                    V(krack_friendship)[friend_core])
summary(friend_core_net)
edge_density(friend_core_net)
# finally, plot the densest subgraph:
plot(friend_core_net,
     layout = layout_nicely(friend_core_net),
     edge.arrow.size = 0.3,
     main="The densest subgraph of the friendship network")

##
#
# TASK 4: 
# Compute in- and out-coreness in the advice networks; then, identify
# and visualise the densest subgraph of the network.
# Compare the obtained results with those presented above for the 
# friendship network.
#
##


### 
# 3. HOMOPHILY AND HETEROGENEITY 
###

#
# 3.1 Homophily / Assortativity
#

# Homophily - also known as Assortativity or Assortative Mixing - can be defined 
# as the tendency of actors in a network to connect to other actors that are similar 
# to them in some way ("birds of feather flock together"). 
# Assortativity can be based on any attribute of a node, but often it is assortativity
# based on nodes' degree (number of direct connections) that is examined.

?assortativity

# The following function computes assortativity in the given network based on 
# age, department, tenure, and degree
compute_assortitivity <- function(g) {
  result = list()
  result$age <- assortativity(g, types1 = V(g)$AGE)
  result$dept <- assortativity_nominal(g, types = as.factor(V(g)$DEPT))
  result$tenure <- assortativity(g, types1 = V(g)$TENURE)
  result$degree <- assortativity_degree(g)
  result
}
# Note that for the DEPT attribute, assortativity is computed 
# in a different way - using assortativity_nominal() f. - since
# this attribute is, in essence, nominal

# Examine assortativity for the three kinds of networks 
# (homophily is indicated by positive values; the higher 
# the value, the higher the homophily)
lapply(krack_list, compute_assortitivity)

# Observe the differences in the attributes that have 
# higher assortativity values


#
# 3.2 Index of Qualitative Variation (IQV)
# 

# Another way to examine the presence of homophily in a network is
# to estimate the extent to which an actor's "associates" 
# (friend, advisor, manager) are heterogenous.
# To that end, we'll use a statistic called Index of Qualitative
# Variation (IQV). This is an implementation of Blau's Index of
# Heterogeneity, normalized so that perfect heterogeneity equals 1.

# We will use a function, get_iqvs, that takes a graph (the 1st input
# parameter) and computes the IQV statistic for the categorical attribute 
# given as the function's 2nd input parameter. 
# NOTE: this function assumes that categorical attribute has been 
# numerically coded to integer values that ascend sequentially from 0.
# The function is defined in the SNA_custom_functions script.
source('SNA_custom_functions.R')

# For this data set, we'll look at homophily across departments, 
# which is already coded 0-4, so no recoding is needed. 

advice_iqvs <- get_iqvs(krack_advice, 'DEPT')
summary(advice_iqvs)
# Mostly high values for IQV suggesting that employees are often connected, 
# through advice relation, to colleagues from different departments

friendship_iqvs <- get_iqvs(krack_friendship, 'DEPT')
summary(friendship_iqvs)
# The diversity is present but on a lower level than in the case of the 
# advice network
# Note: NA values in the results are due to the nodes not having
# alters (direct ties)

reports_to_iqvs <- get_iqvs(krack_reports_to, 'DEPT')
summary(reports_to_iqvs)
# According to the IQV values, there is a perfect homogeneity of
# reports-to connection with respect to the department attribute,
# that is, connections are always with alters from one (the same) 
# department.

# Now let's color-code vertices by department and compare the
# obtained IQV values with the department-colored plots 

# First, choose a qualitative palette so that we have one color for each department 
n_dept <- length(unique(V(krack_advice)$DEPT))
display.brewer.pal(n = n_dept, name = 'Set1')
dept_colors <- brewer.pal(n = n_dept, name = 'Set1')

# Then, use the palette to create plots
reports_to_layout <- layout_nicely(krack_reports_to)
plot(krack_reports_to, 
     layout=reports_to_layout, 
     vertex.color=dept_colors[V(krack_reports_to)$DEPT + 1], 
     edge.arrow.size=.25,
     main="Reports-to network\n(node color denotes department)")

plot(krack_advice, 
     layout=reports_to_layout, 
     vertex.color=dept_colors[V(krack_advice)$DEPT + 1], 
     edge.arrow.size=.25,
     main="Advice network\n(node color denotes department)")

plot(krack_friendship, 
     layout=reports_to_layout, 
     vertex.color=dept_colors[V(krack_friendship)$DEPT + 1], 
     edge.arrow.size=.25,
     main="Friendship network\n(node color denotes department)")

##
#
# TASK 5: 
# Numeric attributes can be discretized and thus turned into factors (that is,
# qualitative variables), so that the get_iqvs() function can be applied. 
# Try to compute and interpret IQV for the (discretized) 'age' attribute.
# To discretize values of a numeric attribute, you can use the cut() f.
?cut
# for an example, see this StackOverflow answer:
# https://stackoverflow.com/questions/22075592/creating-category-variables-from-numerical-variable-in-r
#
##


