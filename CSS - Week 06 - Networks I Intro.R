
##================================================##
##                                                ##
##       Computational Social Science             ##
##       Doctoral Seminar (16:194:672)            ##
##       Spring 2023, Rutgers University          ## 
##       Katya Ognyanova, katya@ognyanova.net     ## 
##                                                ##
##================================================##




# ================  Intro to networks in R   ================

# Packages we would need today:
# (remove # and run this if they are not already installed)  


 # install.packages("igraph")       #  Work with networks


# ================  ~~ Reading in network data  ================
    
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

nrow(nodes) # How many rows in the node data?
length(unique(nodes$id)) # How many unique nodes?

nrow(links) # How many rows in the edge data?
nrow(unique(links[,c("from", "to")])) # How many unique links?

# Converting the data to an igraph object:
# The graph_from_data_frame() function takes two data frames: 'd' and 'vertices'.
# 'd' describes the edges of the network - it should start with two columns 
# containing the source and target node IDs for each network tie.
# 'vertices' should start with a column of node IDs.
# Any additional columns in either data frame are interpreted as attributes.

net <- graph_from_data_frame(d=links, vertices=nodes, directed=TRUE) 

# If our network was stored as an adjacency matrix in a matrix object called 'm',
# we would transform it into an igraph object using graph_from_adjacency_matrix(m)
# If the network is valued, we would include a 'weighted=T' parameter.

# Examine the resulting object:
class(net)
net 

# First attempt to plot the graph:
plot(net) # not pretty!

# The description of the igraph object starts with up to four letters:
# 1. D or U, for a directed or undirected graph
# 2. N for a named graph (where nodes have a name attribute)
# 3. W for a weighted graph (where edges have a weight attribute)
# 4. B for a bipartite (two-mode) graph (where nodes have a type attribute)
#
# The two numbers that follow refer to the number of nodes and edges in the graph. 
# The description also lists graph, node & edge attributes, for example:
# (g/c) - graph-level character attribute
# (v/c) - vertex-level character attribute
# (e/n) - edge-level numeric attribute
# .... etc.


# If you need them, you can also extract an edge list 
# or a matrix back from the igraph networks.
as_edgelist(net, names=T)
as_adjacency_matrix(net, sparse=F) # Binary
as_adjacency_matrix(net, attr="score", sparse=F) # Valued

# Or you can get data frames describing nodes and edges:
as_data_frame(net, what="edges")
as_data_frame(net, what="vertices")

# You can also look at the network matrix directly:
net[1,]
net[5,7]

# Removing self-loops from the graph:
net <- simplify(net, remove.multiple = F, remove.loops = T) 

# Note that we could also use simplify to combine multiple edges
# between the same two nodes (though that may collapse different link types)



# ================  ~~  Vertex and edge attributes  ================
 

# We can access the nodes (vertices) and ties (edges) using V() and E()  
E(net)
V(net)

# We can access the node and edge attributes using the usual $ notation:
E(net)$link
V(net)$unit
V(net)$person

# We can also find specific nodes and edges by attribute: 
V(net)[person=="Peter Parker"]
E(net)[link=="financial"]

# Examine attributes:
vertex_attr(net) # Node
edge_attr(net)   # Edge   
graph_attr(net)  # Network

# Another way to set attributes:
# (you can similarly use set_edge_attr(), set_vertex_attr(), etc.)
net <- set_graph_attr(net, "name", "Organizational Network")
net <- set_graph_attr(net, "something", "A thing!")

graph_attr_names(net)
graph_attr(net)

graph_attr(net, "name") <- "Organization Network"
graph_attr(net, "something") <- "An Attribute"
graph_attr(net)

net <- delete_graph_attr(net, "something")
graph_attr(net)



# ================ Plotting network data  ================



#  -------~~ Colors and plots in R --------

# In most R functions, you can use named colors, hex, or rgb values:
plot(x=5, y=5, pch=19, cex=25, col="dark red")
plot(x=5, y=5, pch=19, cex=25, col="#557799")
plot(x=5, y=5, pch=19, cex=25, col=rgb(.25, .5, .3))

# Built-in named colors:
colors()

# Generate color gradients: 
# Note that colorRampPalette returns a *function* that we can use 
# to generate as many colors from that palette as we need.
palf <- colorRampPalette(c("gray70", "dark red")) 
plot(x=10:1, y=1:10, pch=19, cex=10, col=palf(10)) 

# General graphical parameters can be set with par()
# For example, parameter 'bg' sets the plot background color.
# Plot margins are set in par() with mar=c(bottom, left, top, right).
# We can also use par() to plot multiple figures.
# plot row by row: mfrow=c(number of rows, number of columns)
# plot column by column: mfcol=c(number of rows, number of columns)
?par

par(bg="gray", mfrow=c(1,2))
plot(x=5, y=5, pch=19, cex=25, col="dark red")
plot(x=5, y=5, pch=19, cex=25, col="#557799")

dev.off() # shuts off current graphical device



#  -------~~ Plotting parameters --------

plot(net) 

# Plotting with igraph: node options (starting with 'vertex.') and edge options
# (starting with 'edge.'). A list of options is included below.

?igraph.plotting

# ~~~~ NODES ~~~~ 
#
# vertex.color 	       Node color
# vertex.frame.color   Node border color
# vertex.shape 	       One of "none", "circle", "square", "csquare", "rectangle"
#                      "crectangle", "vrectangle", "pie", "raster", or "sphere"
# vertex.size 	       Size of the node (default is 15)
# vertex.size2 	       The second size of the node (e.g. for a rectangle)
# vertex.label 	       Character vector used to label the nodes
# vertex.label.color   Vertex label color
# vertex.label.family  Font family of the label (e.g."Times", "Helvetica")
# vertex.label.font	   Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
# vertex.label.cex     Font size (multiplication factor, device-dependent)
# vertex.label.dist	   Distance between the label and the vertex
# vertex.label.degree  The position of the label in relation to the vertex, where
#                      0 is right, "pi" is left, "pi/2" is below, and "-pi/2" is above
#
# ~~~~ EDGES ~~~~ 
#
# edge.color 	      Edge color
# edge.width 	      Edge width, defaults to 1
# edge.arrow.size   Arrow size, defaults to 1
# edge.arrow.width 	Arrow width, defaults to 1
# edge.arrow.mode 	Vector specifying whether edges should have arrows,
#                   possible values: 0 no arrow, 1 back, 2 forward, 3 both
# edge.curved 	    Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
# edge.lty 	        Line type, could be 0 or "blank", 1 or "solid", 2 or "dashed",
#                   3 or "dotted", 4 or "dotdash", 5 or "longdash", 6 or "twodash"
# edge.label 	      Character vector used to label edges
# edge.label.color 	Edge label color
# edge.label.family Font family of the label (e.g."Times", "Helvetica")
# edge.label.font 	Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
# edge.label.cex 	  Font size for edge labels
#
# ~~~~ OTHER ~~~~ 
#
# layout      The position of the nodes on the plot.
# margin      Empty space margins around the plot, vector with length 4
# frame       if TRUE, the plot will be framed
# main        If set, adds a title to the plot
# sub         If set, adds a subtitle to the plot 
# rescale     Whether to rescale coordinates to [-1,1]. Default is TRUE.


#  -------~~ Networks plots --------

# One way to specify plotting options is to include them
# as parameters in the plot() function, as we are doing below.

plot(net, edge.color="orange", edge.width=2, edge.curved=0, 
     edge.arrow.size=.2, vertex.color="lightsteelblue", 
     vertex.frame.color="black", vertex.size=9, 
     vertex.shape="square", vertex.label=NA) 

# Create node colors based on the unit each person belongs to:
colrs <- c("tomato", "yellowgreen", "gold", "lightsteelblue")
colrs <- colrs[V(net)$unit_id]
colrs

plot(net, edge.color="gray50", edge.width=1.5, edge.curved=.1, 
     edge.arrow.mode=0, vertex.size=10, vertex.shape="circle",
     vertex.color=colrs, vertex.frame.color="black",
     vertex.label=NA, layout=layout_with_kk) 

# Another way to set visual attributes is to add them to the igraph object.

# Add attributes to the nodes:
V(net)$color <- colrs

# Size the nodes based on tenure in the organization:
V(net)$size <- V(net)$tenure*0.9


# The labels are currently node IDs.
V(net)$label.color <- "black"
# Setting the labels to NA will render no labels:
V(net)$label <- NA

# Add attributes to the edges:
E(net)$color <- "gray80"
E(net)$arrow.mode <- 0
E(net)$curved <- 0
E(net)$width <- 2

# We can even set the network layout as a graph attribute:
graph_attr(net, "layout") <- layout_with_kk
plot(net) 


#  -------~~ Network layouts --------

# Network layouts are algorithms that return
# coordinates for each node in the network.

# You can set the layout in the plot function:
plot(net, layout=layout_randomly)

# Or calculate the vertex coordinates in advance:
l <- layout_in_circle(net)
# And then use those:
plot(net, layout=l)

# l simply stores the X and Y coordinates for the nodes in the graph. 
l

# Some available layouts in 'igraph' include:

# Randomly placed vertices
l <- layout_randomly(net)
plot(net, layout=l)

# Circle layout
# We can order the nodes by unit:
l <- layout_in_circle(net, order=order(V(net)$unit_id))
plot(net, layout=l)

# 3D sphere layout
l <- layout_on_sphere(net)
plot(net, layout=l)

# The Fruchterman-Reingold force-directed algorithm 
# Nice but slow, most often used in graphs smaller than ~1000 vertices. 
l <- layout_with_fr(net)
plot(net, layout=l)

# You will also notice that the layout is not deterministic - different runs 
# will result in slightly different configurations. Saving the layout in l
# allows us to get the exact same result multiple times.

par(mfrow=c(2,2)) # split the plot into 4, 2x2

plot(net, layout=layout_with_fr)
plot(net, layout=layout_with_fr)
plot(net, layout=l)
plot(net, layout=l)

dev.off() # clear the plot and the split

# Another popular force-directed algorithm that produces nice results for
# connected graphs is Kamada Kawai. Like Fruchterman Reingold, it attempts to 
# minimize the energy in a spring system.

l <- layout_with_kk(net)
plot(net, layout=l)

# Save the resulting plot as a file (those are also better rendered!)
pdf(file="My_Network_Vis.PDF")
plot(net, layout=l)
dev.off()


# ================ Network size and density ================


# Network size
# Number of vertices in the network:
vcount(net)
# Number of edges in the network:
ecount(net)

 
# Density
# The proportion of present edges from all possible edges
# Calculated as n*(n-1) where n is the number of nodes.
edge_density(net)
ecount(net)/(vcount(net)*(vcount(net)-1)) 

 
# ================ Node centrality ================


# DEGREE CENTRALITY
# The number of ties a node has (incoming, outgoing, or all)
# 'degree' has a mode of 'in' for in-degree, 'out' for out-degree,
# and 'all' or 'total' for total degree. 
degree(net, mode="all")   # all ties
degree(net, mode="in")    # incoming ties
degree(net, mode="out")   # outgoing ties

# Plot the network with node sizes based on the degree centrality: 
deg <- degree(net, mode="all")
plot(net, vertex.size=deg*3)

# Degree distribution (how many nodes of each degree are in the graph)
hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree")
plot(density(deg), main="Degree distribution")

# Note that degree centrality is based on just the number of ties.
# If we want to also consider the tie weights, we need to use
# a different function: strength()
str <- strength(net, mode="all", weight=E(net)$score)
str
plot(net, vertex.size=str/2)


# EIGENVECTOR CENTRALITY
# Eigenvector centrality is proportional to the sum of connection centralities.
# That means if two people have the same number of connections, the person whose
# ties are more central will get a higher eigenvector centrality score.
# Corresponds to values of the first eigenvector of the graph adjacency matrix
# We can use or ignore edge weights here.

eig <- eigen_centrality(net, directed=T, weights=NA)
eig$vector

# BETWEENNESS CENTRALITY 
# Betweenness indicates having a broker position, connecting other people and groups.
# High betweenness is associated with the ability to control the flow of resources.
# It is calculated based on the number of shortest paths connecting other dyads that
# go through a node (or through an edge - we can also calculate edge betweenness!)

bet <- betweenness(net, directed=T, weights=NA)
bet

ebet <- edge_betweenness(net, directed=T, weights=NA)
ebet

plot(net, edge.width=ebet/2)


# CLOSENESS CENTRALITY
# Closeness is based on a node's distance to others in the graph.
# High closeness indicates the person is in a good position to spread information.
# Calculated as inverse of the node's average geodesic distance to others in the graph.
clo <- closeness(net, mode="all", weights=NA) 
clo


# HUBS AND AUTHORITIES
# The hubs and authorities algorithm developed by Jon Kleinberg was initially used 
# to examine web pages. Hubs were expected to contain catalogues with a large number 
# of outgoing links; while authorities would get many incoming links from hubs, 
# presumably because of their high-quality relevant information. 
# So authorities have high # of incoming links from sites with high # of outgoing links.

hs <- hub_score(net, weights=NA)$vector
as <- authority_score(net, weights=NA)$vector

par(mfrow=c(1,2))
plot(net, vertex.size=hs*30, main="Hubs")
plot(net, vertex.size=as*30, main="Authorities")

dev.off()

 

# ================ Network centralization ================
 
# While centrality metrics examine the importance of specific nodes
# in a network, centralization scores characterize the entire graph.
# Network centralization is usually calculated by summing the differences 
# in centrality between the most central person in the network and everyone else.

# The centralization functions return "res" - vertex centrality, "centralization", 
# and "theoretical_max" - maximum centralization score for a graph of that size.

# Degree centralization (modes are "in", "out", or "all")
centr_degree(net, mode="all", normalized=F)
centr_degree(net, mode="in",  normalized=F)
centr_degree(net, mode="out", normalized=F)

# Results can be normalized (divided by the theoretical maximum) with normalized=T
centr_degree(net, mode="all", normalized=T)
centr_degree(net, mode="in",  normalized=T)
centr_degree(net, mode="out", normalized=T)

# Eigenvector centralization
centr_eigen(net, directed=T, normalized=T) 

# Betweenness centralization
centr_betw(net, directed=T, normalized=T)

# Closeness centralization
centr_clo(net, mode="all", normalized=T) 

 
 
# ================ THE END ================
  









