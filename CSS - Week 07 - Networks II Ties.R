
##================================================##
##                                                ##
##       Computational Social Science             ##
##       Doctoral Seminar (16:194:672)            ##
##       Spring 2023, Rutgers University          ## 
##       Katya Ognyanova, katya@ognyanova.net     ## 
##                                                ##
##================================================##




# ================  Social networks in R II   ================

# Packages we would need today:
# (remove # and run this if they are not already installed)  


# install.packages("igraph")       # Work with networks


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


# ================  ~~ Plotting in the data  ================

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
  
  
  # ================ ~~ Multiplex & valued ties ================
  
  net
  # The network we are currently working with is:
  #  -- Directed: links go from A to B, matrix may not be symmetric
  #  -- Valued: we have a numeric edge attribute representing weights
  #  -- Multiplex: we have two different relationship types between people
  
  
  # Our network is multiplex: it contains multiple types of ties. 
  plot(net, edge.color=c("dark red", "slategrey")[(E(net)$link=="offline")+1],
       edge.width=2, vertex.color="gray40")
  
  # Sometimes we want to extract separate networks for each type of tie.
  # One easy way to do it is to delete edges using the minus operator:  
  # (another way would be to use the function delete.edges())
  net.f <- net - E(net)[E(net)$link=="contract"]
  net.c <- net - E(net)[E(net)$link=="financial"]
  
  # See the two separately:
  par(mfrow=c(1,2))
  plot(net.f, vertex.color="orange", layout=layout_with_fr, main="Financial ties")
  plot(net.c, vertex.color="lightsteelblue2", layout=layout_with_fr, main="Contract ties")
  
  dev.off()
  
  # Make sure the nodes stay in the same place in both plots:
  par(mfrow=c(1,2))
  l <- layout_with_fr(net)
  plot(net.f, vertex.color="orange", layout=l, main="Financial ties")
  plot(net.c, vertex.color="lightsteelblue2", layout=l, main="Contract ties")
  
  dev.off()
  
  # Our network edges also have values stored in the 'score' edge attribute.
  # In many cases we may be particularly interested in the heaviest or
  # structurally most important ties. We may therefore want to keep those 
  # and drop the other less important edges in the graph.
  
  hist(links$score)
  mean(links$score)
  sd(links$score)
  
  # There are a number of sophisticated ways to extract key edges,
  # but one simple approach could be to keep edges with weight
  # one or two standard deviations over the mean.
  
  cut.off <- mean(links$score) + sd(links$score)
  net.sp <- delete_edges(net, E(net)[score<cut.off])
  plot(net.sp) 
  
  
  
  # Additional info:                                                                    
  #                                                                                                                                                                         #
  # Several R packages can help us identify the important links in a network.           
  #                                                                                      
  # For example, "disparityfilter" is an R package by Alessandro Bessi that implements  
  # a backbone extraction algorithm for directed or undirected weighted networks        
  # as described in this PNAS paper: https://doi.org/10.1073/pnas.0808904106            
  #                                                                                        
  # A new package from Zachary Neal called "backbone" will also let you                 
  # extract a backbone (significant edges) from weighted data, particularly             
  # from projections of bipartite graphs (we will discuss those next week)
  
  
  
  # ================  ~~ Network reciprocity ================ 
  
  # Reciprocity
  # The proportion of reciprocated ties (for a directed network).
  reciprocity(net)
  dyad_census(net) # Mutual, asymmetric, and null node pairs
  
  
  # Directed and undirected networks
  
  # We can treat the network as undirected ("symmetrize" it)
  # mode="mutual" only keeps reciprocated ties:
  u_net <- as.undirected(net, mode="mutual")
  plot(u_net)
  
  # mode="collapse" connects all linked nodes with an undirected tie:
  u_net <- as.undirected(net, mode="collapse")
  plot(u_net)
  u_net # Note that our edge attributes are gone!
  
  # As we collapse the edges, we can define the way to calculate the
  # new values of their attributes using parameter edge.attr.comb
  # options include "ignore" which drops the attribute, as well as
  # sum, prod, min, max, random, first, last, mean, median, and concat.
  u_net <- as.undirected(net, mode="collapse", 
                         edge.attr.comb=list(link="concat", score="sum",  arrow.mode="ignore", 
                                             color="ignore", curved="ignore", width="ignore") )
  plot(u_net) 
  u_net   
  
  # We can also make the undirected network directed:
  # Here, mode="mutual" makes two directed ties from every link:
  d_net <- as.directed(u_net, mode="mutual")
  plot(d_net)
  
  # What would the reciprocity of this network be?
  reciprocity(d_net)
  
  # Mode="arbitrary" selects a random link direction:
  d_net <- as.directed(u_net, mode="arbitrary")
  plot(d_net)
  
  
  
  # ================  ~~ Triadic closure ================ 
  
  
  plot(net)
  
  # Clustering coefficient
  # global - ratio of triangles (direction disregarded) to connected triples
  # local - ratio of triangles to connected triples each vertex is part of  
  transitivity(net, type="global")  # net is treated as an undirected network
  transitivity(as.undirected(net, mode="collapse")) # same as above
  
  # Calculate separately for each person in the network:
  transitivity(net, type="local", isolates="zero")
  
  triad_census(net) # for directed networks
  
  # Triad types (per Davis & Leinhardt):
  # 
  # 003  A, B, C, empty triad.
  # 012  A->B, C 
  # 102  A<->B, C  
  # 021D A<-B->C 
  # 021U A->B<-C 
  # 021C A->B->C
  # 111D A<->B<-C
  # 111U A<->B->C
  # 030T A->B<-C, A->C
  # 030C A<-B<-C, A->C.
  # 201  A<->B<->C.
  # 120D A<-B->C, A<->C.
  # 120U A->B<-C, A<->C.
  # 120C A->B->C, A<->C.
  # 210  A->B<->C, A<->C.
  # 300  A<->B<->C, A<->C, completely connected.
  
  
  
  # ================  ~~ Homophily / Assortativity ================
  
  
  # Homophily or assortativity is the tendency of nodes to connect to others
  # who are similar on some variable. The similarity could be on categorical 
  # node attributes, continuous node attributes, or even on network structure
  
  # igraph lets us examine this by using 'assortativity' scores which range
  # from -1 to 1; with high positive scores meaning similar nodes are connected
  # more often than we would expect by chance; and negative scores meaning they
  # are connected less often than we would expect by chance.
  
  # Assortativity for categorical variables (labels):
  # Are people from the same unit more likely to be connected?
  assortativity_nominal(net, V(net)$unit_id, directed=T)
  plot(net)
  
  # Assortativity for continuous variables
  # Are people with similar tenure more likely to be connected?
  assortativity(net, V(net)$tenure, directed=T)
  
  # Assortativity in node degrees:
  # Are people with high (low) degrees more likely to be connected?
  assortativity_degree(net, directed=T)
  
  
  
  
  # ================ THE END ================
  
  
  
  