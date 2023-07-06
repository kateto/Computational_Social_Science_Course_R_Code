
##================================================##
##                                                ##
##       Computational Social Science             ##
##       Doctoral Seminar (16:194:672)            ##
##       Spring 2023, Rutgers University          ## 
##       Katya Ognyanova, katya@ognyanova.net     ## 
##                                                ##
##================================================##




# ================  Social networks in R III   ================

# Packages we would need today:
# (remove # and run this if they are not already installed)  


 # install.packages("igraph")       # Work with networks
 # install.packages("statnet")      # Network hypotheses/models


# ================  ~~ Reading in the data  ================
    
# Set the working directory to the folder containing your files.
# In RStudio, you can do this through the menus by going to 
# "Session" -> "Set Working Directory" -> "To Source File Location"
# Or use a setwd() command:  setwd("C:/MyDataFolder") 

# You can download the network data used here from:
# http://kateto.net/css/network_data.zip

 
library("igraph")

# Dataset 1 is based on the structure of the well-known Zachary Karate Club graph.
# Simulated attributes include link type (financial or contract) and score (weight),
# as well as the person's name and the organizational unit they belong to.

nodes <- read.csv("./DATA/Network-1-NODES.csv", header=T, as.is=T)
links <- read.csv("./DATA/Network-1-EDGES.csv", header=T, as.is=T)

# Examine the data:
head(nodes)
head(links)
 
# Converting the data to an igraph object: 
net <- graph_from_data_frame(d=links, vertices=nodes, directed=TRUE) 

# Examine the resulting object:
class(net)
net 
 
# Removing self-loops from the graph:
net <- simplify(net, remove.multiple = F, remove.loops = T) 
  

# ================ Subgroups and communities ================


# ------~~ Components --------  


# Connected components are subgraphs of a network. Within a component, all nodes are 
# directly or indirectly connected. There are no links between components.

# A component is STRONGLY connected if for every pair of vertices A and B
# there is a path from A to B AND a path from B to A.
# A component is WEAKLY connected if for every pair of vertices A and B
# there is a path from A to B OR a path from B to A.

# Our graph has a single weakly connected component.
components(net, mode="weak")

# It does, however split into many strongly connected ones.
components(net, mode="strong")

# The components function returns component membership for nodes,
# component size ($csize), and number of components ($no).

# We can also find all nodes reachable from a single node A; 
# or all nodes that can reach A in our network:

subcomponent(net, V(net)[person=="Bruce Banner"], mode="out")
subcomponent(net, V(net)[person=="Bruce Banner"], mode="in")
subcomponent(net, V(net)[person=="Bruce Banner"], mode="all")


# ------~~ Communities --------  

# Graph community detection algorithms aim to identify groups that consist 
# of densely interconnected nodes with fewer connections across groups. 
# Many algorithms work by trying to optimize 'modularity' -- a measure of how
# well we've partitioned the graph. High modularity for a partitioning reflects 
# dense connections within communities and sparse connections across communities.
# Note that many of the community detection algorithms in igraph are going to 
# ignore link direction (or even throw errors if the input graph is directed).

# A number of community detection algorithms require undirected network.
# As we remember from last week, we can symmetrize our network by making
# all links in it be reciprocated.
netu <- as.undirected(net, mode="collapse") 


# Community detection based on edge betweenness (Newman-Girvan)
# High-betweenness edges are removed sequentially (recalculating at each step)
# and the best partitioning of the network is selected.
ceb <- cluster_edge_betweenness(netu) 
dendPlot(ceb, mode="hclust")
plot(ceb, netu) 

# Let's examine the community detection igraph object:
class(ceb)
length(ceb)     # number of communities
sizes(ceb)      # sizes of the communities
membership(ceb) # community membership for each node
crossing(ceb, net)   # boolean vector: TRUE for edges across communities
modularity(ceb) # a score showing how modular the graph partitioning is


# Community detection based on propagating labels
# Assigns node labels, randomizes, and replaces each vertex's label with
# the label that appears most frequently among neighbors. Repeated until
# each vertex has the most common label of its neighbors.
# Note: implemented for undirected networks only!
clp <- cluster_label_prop(netu, weights=E(netu)$score)
plot(clp, netu)

# Other community detection algorithm currently available in igraph include: 

# Community detection based on an optimization of modularity
co <- cluster_optimal(netu, weights=E(netu)$score)
plot(co, netu)

# Community detection based on greedy optimization of modularity
cfg <- cluster_fast_greedy(netu)
plot(cfg, netu)

# Community detection based on leading eigenvector of modularity matrix
cle <- cluster_leading_eigen(netu)
plot(cle, netu)

# Community detection trying to map information flow through the network
ci <- cluster_infomap(netu)
plot(ci, netu)

# Community detection based on short random walks
# (we assume those will tend to stay in the same community)
cwt <- cluster_walktrap(netu)
plot(cwt, netu)

# One way to compare the results of community detection is by 
# looking at the modularity scores: 
modularity(co) 
modularity(cwt)
 

# ================ Intro to networks in Statnet ================


#Just in case we have forgotten this earlier:
dev.off()
detach("package:igraph") 

# Load the statnet packages:
library("statnet")


# ------~~ Network objects --------  

# The ideas behind the 'statnet' libraries are fairly similar to those of 'igraph' -
# but a lot of the notation is different (a whole new set of parameter names!)
# Here is a quick example using our first network, Dataset 1.

# Our initial data:
head(links)
head(nodes)

# Convert the data into the network format used by statnet.
# Similar to how we did this in igraph: the first parameter
# is the link data, 'vertex.attr' are optional node attributes.
# matrix.type is "edgelist" for an edge list, and "adjacency" 
# in cases when our data is structured as a matrix (0/1 table)
# Remember to set the 'ignore.eval' to FALSE for weighted networks,
# otherwise the weights will be ignored by default.
snet <- network(links, vertex.attr=nodes, matrix.type="edgelist", 
                loops=T, multiple=F, ignore.eval = F)
snet

# We can access the edges, vertices, and the network matrix using:
snet[,]
snet %n% "net.name" <- "Organization Network" #  network attribute
snet %v% "unit"    # Node attribute
snet %e% "link"    # Edge attribute

# Create node (vertex) attribute col and store in it node colors based on the unit
snet %v% "col" <- c("tomato", "yellowgreen", "gold", "lightsteelblue")[snet %v% "unit_id"]

# Plot the network: 
plot(snet, vertex.cex=3, vertex.col="col", edge.col="gray50", arrowhead.cex=0)

# Help on plotting parameters: 
?plot.network


# ------~~ Basic metrics --------  

# Note that there are many different ways to calculate even basic network metrics
# (for example based on whether we focus on vertex pairs or edges; or how we treat
# nodes with no connections, known as 'isolates'). Do not be surprised therefore
# if some measures you get from 'statnet' differ slightly from the results in 'igraph'

# Network density
# mode is 'digraph' for directed, 'graph' for undirected networks.
gden(snet,  mode="digraph")

# Network reciprocity 
# edgewise: proportion edges that are reciprocated from all edges that are present
grecip(snet, measure="edgewise")

# Network transitivity 
# For directed graph, proportion of triads A->B->C where A->C
# For undirected graph, proportion closed triangles
gtrans(snet, mode="digraph")
 
# Triad census (how many of each type of triad):
triad.census(snet, mode="digraph")

# Degree centrality 
# gmode is 'digraph' or 'graph' for directed or undirected networks/
# cmode is 'indegree', 'outdegree', or 'freeman' (total degree)
degree(snet, gmode="digraph", cmode="indegree")  # indegree
degree(snet, gmode="digraph", cmode="outdegree") # outdegree 
degree(snet, gmode="digraph", cmode="freeman")   # total 

# Betweenness centrality 
betweenness(snet, gmode="graph")  

# Closeness centrality
closeness(snet, gmode="graph")   

# Centralization
# Centralization is calculated as a single function with FUN indicating which
# type of measure is to be used (e.g. closeness, degree, betweenness, etc.)
centralization(snet, FUN=degree, mode="graph")
centralization(snet, FUN=degree, mode="digraph", cmode="outdegree")
centralization(snet, FUN=betweenness, mode="graph")
centralization(snet, FUN=closeness, mode="graph")



# ================ Permutation tests ================

# Conditional Uniform Graph (CUG) tests for network measures

# When we examine network characteristics, it can be difficult to understand
# whether the scores we have obtained are relatively high or really low.
# One way to approach this question is to compare our network to other 
# graphs with similar properties (for example ones of the same size/density).
# Conditional uniform graph tests randomly draw graphs from a uniform distribution
# (one where each graph has an equal probability of being selected) and compare
# the metrics calculated for those graphs with those of our observed data.

# The function we can use to do that is cug.test().
# We can specify characteristics of the graph distribution using 'cmode'.
# The 'cmode' parameter lets us condition on size, density, or dyadic census.

# Compare how our graph compares in terms of its score on parameter FUN  
# to 1000 other graphs with the same size / density / dyad census.

my_fun <- function(my_net) { centralization(my_net, FUN=degree, mode="digraph", cmode="freeman") }
my_fun(snet)

centralization(snet, FUN=degree, mode="digraph", cmode="indegree")

# Conditioning on size only
ct1 <- cug.test(snet, FUN=my_fun , mode="digraph", cmode="size", reps = 1000) 
ct1
plot(ct1)

# Conditioning on number of edges
ct2 <- cug.test(snet, FUN=my_fun , mode="digraph", cmode="edges", reps = 1000)
ct2
plot(ct2)

# Conditioning on dyad census
ct3 <- cug.test(snet, FUN=my_fun , mode="digraph", cmode="dyad.census", reps = 1000)
ct3
plot(ct3)

# Pr(X>=Obs) value: out of 1000 networks, how many score as high or higher as ours?
# Pr(X<=Obs) value: out of 1000 networks, how many score as low or lower than ours?

# Looks like our network has betweenness centralization lower than most other
# graphs of the same size, density, and with similar proportions of dyad types.

# We can conduct similar test for other graph metrics as needed:
# E.g., is there more transitivity in our network than we can expect based on its density?

ct.tr <- cug.test(snet, gtrans, cmode="edges")  
ct.tr
plot(ct.tr)
 

# ================ Graph correlation and regression ================


# In the next few sections, we will use a new network dataset.
# This is the well known Padgett data -- it contains two networks
# describing marriage and business ties among Florentine families.

data(florentine) 

flobusiness # Business ties among families
flomarriage # Marital ties among families

plot(flomarriage)
plot(flobusiness)

 
# Correlation between marriage and business graphs:
gcor(flobusiness, flomarriage, mode="digraph")

# We can see the correlation, but again -- we want to evaluate 
# how likely it is that we'd see a correlation of this magnitude
# in networks with characteristics similar to our observed data.

# Note: We can't perform the usual tests of significance because
# they are premised on data with independent observations.
# In network data, observations are very much not independent
# and we can expect to see correlated errors.

# So, we use permutation tests instead. The Quadratic Assignment Procedure (QAP) 
# shuffles network rows and columns multiple times, calculates the desired 
# network metric on the resulting graphs, and compares it to our observed data.

# Graph correlation:
# The parameters g1=1 and g2=2 tell gcor to compare the first & second element
# of the network list that we have supplied as data here.
gcor(list(flomarriage,flobusiness),  g1=1, g2=2)

# QAP test of the correlation:
flo.qap <- qaptest(list(flomarriage,flobusiness), gcor, g1=1, g2=2, reps=1000)
plot(flo.qap)
flo.qap

# Our correlation is very high compared to that of the generated permutations!


# We can similarly use permutation tests for a linear regression on network variables.
# This test is known as MRQAP (multiple regression QAP)

# In the network regression, we need an outcome network: y and a set of 
# one or more predictor networks stored in x

# We can conduct a network regression of y on x using the netlm() function. 
# netlm takes a single matrix or network object as its first argument (the outcome) 
# and a stack of matrices (array containing all predictors) or a list of  network objects
# as its second argument.

net.mod <- netlm(y=flomarriage, x=flobusiness, reps=1000)
summary(net.mod)
 

# ================ Two-mode network example ================ 

# We may not get to this part of the code in class -- it is included
# here for those of you interested in this specific network type.
# Feel free to go over the code and reach out with questions!

# For this example, we will go back to the igraph package.

# We can represent two-mode data with an edge list that looks 
# exactly like the edge lists for a one-mode network.

# To make things a little more interesting here, we are going to use
# an 'incidence matrix'. That is a matrix where rows present one type
# of object, and the columns another type (e.g. people and companies).
# In this case, the rows are media outlets and the columns are people.
# Having '1' in the cell for row X column Y would tell us that 
# media X and person Y are connected (Y goes to X to get news).

nodes2 <- read.csv("./DATA/Network-2-NODES.csv", header=T, as.is=T)
links2 <- read.csv("./DATA/Network-2-EDGES.csv", header=T, row.names=1)
 
# We can see that links2 is a matrix for a two-mode network:
links2 <- as.matrix(links2)
dim(links2)

library(igraph)
 
# Create an igraph network object from the two-mode matrix: 
net2 <- graph_from_incidence_matrix(links2)
net2

# Igraph automatically generates a vertex attribute 'type' which shows
# which mode vertices belong to (first or second, media or people).
# The first mode has type TRUE, the second has type FALSE.
V(net2)$type

plot(net2,vertex.label=NA)


# This time we will make nodes look different based on their type.
# Media outlets are blue squares, audience nodes are orange circles:
V(net2)$color <- c("steel blue", "orange")[V(net2)$type+1]
V(net2)$shape <- c("square", "circle")[V(net2)$type+1]
 
plot(net2, vertex.label.color="white", 
     vertex.size=8, vertex.label=NA) 


# We can easily generate bipartite projections for the two-mode network:
# (Shared media sources among people; shared audience members among media)

net2.bp <- bipartite.projection(net2)

#Network of media sources only:
net2.bp$proj1
 
# Network of audience members only:
net2.bp$proj2


# Plot the two projected networks:
 
plot(net2.bp$proj1, vertex.label=NA)

plot(net2.bp$proj2, vertex.label=NA)
 
 
detach("package:igraph")



# ================ THE END ================



