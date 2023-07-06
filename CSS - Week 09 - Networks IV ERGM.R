
##================================================##
##                                                ##
##       Computational Social Science             ##
##       Doctoral Seminar (16:194:672)            ##
##       Spring 2023, Rutgers University          ## 
##       Katya Ognyanova, katya@ognyanova.net     ## 
##                                                ##
##================================================##



# ================  Social networks in R IV: ERGM ================

# Packages we would need today:
# (remove # and run this if they are not already installed)  

 
 # install.packages("statnet")      # Network hypotheses/models


# ================ ~~ Undirected network example ================

# Example of ERGM with a symmetric network: the Florentine families 

library(statnet)

# As is traditional in ergm tutorials, we'll start with the Padgett data 
# (Florentine marriage & business ties), included with the ergm package:

data(florentine) 

# The data contains two network objects - one with marital and another one
# with business relationships between Florentine families.

flobusiness

flomarriage

plot(flobusiness)

plot(flomarriage)

# Exponential random graph models - what terms can we use in a model?

help('ergm-terms')

# Let's estimate a simple  model which only examines density (edge term)
# Note that ergm() uses a formula, just like the lm() regression function.
# The format of the ergm command is ergm(YourNetwork ~ Signature1 + Signature2 + ...) 
# where YourNetwork can be a matrix or a network object.

flo.mar.1 <- ergm(flomarriage ~ edges)  		 
flo.mar.1
summary(flo.mar.1)

# We get a negative edge parameter since the network is rather sparse.
# The edge parameter here is based on probability of having a tie in our network.
# Specifically, it is the log odds of a tie, or log(#dyads-w-edge/#dyads-no-edge) )
# The network has 20 ties out of 120 possible ties. Let's calculate the log odds ourselves!
# Remember that an event with probability p has odds of p/(1-p) and log odds of log(p/(1-p)) 
# The reverse formula tells us an event with odds x has a probability of x/(1+x)

# Odds that a tie exists: 20 to 100 (20 ties exist, 100 do not of 120 possible ties) = 1/5 or 0.2
# Log odds - natural logarithm of the odds, (e to the power of ? will give us 1/5)
log(20/(120-20)) # We get -1.609, the same as the edge parameter in the erg model.
# Negative log odds tell us that the odds are less than 1, corresponding to probability <50%.
# The corresponding probability is .167 (16.7% is 20 out of 120 or 1/6). We can also calculate as:
exp(-1.609)/(1+exp(-1.609)) 



# Next, we look at a fancier model that includes triangles in addition to edges:
 
flo.mar.2 <- ergm(flomarriage ~ edges + triangles, # This is our formula
                  control=control.ergm(seed=0))    # Just for class work: ensures same results
flo.mar.2
summary(flo.mar.2)

# Note that we're using a stochastic algorithm - so you will get slightly different estimates
# if you rerun the model. Here we use seed=0 to make sure we'll get the same results every time
#
# The triangle coefficient here is not significant - so this is not a signature 
# likely to drive the network formation. What do the coefficients overall tell us?
# Conditional log-odds of a tie between two actors here =
# = -1.652*(change in the number of ties) + 0.148 * (change in the number of triangles)
# = -1.652*1 + 0.148*(change in the number of triangles)
# 
# if the tie will not add any triangles to the network, its log-odds = -1.652.
# if it will add one triangle to the network, its log-odds = -1.652 + 0.148 = -1.504
# if it will add two triangles to the network, its log-odds = -1.652 + 0.148*2 = -1.356
# The corresponding probabilities of these ties are 0.16, 0.18, and 0.20.
#
# There are a large number of other structural signatures you could add paramteters for.
# For instance 2-stars: kstar(2), 3-stars: kstar(3) isolates: isolates, etc. 
#
# Let's run a model checking whether edges in the Florentine business network are predicted by
# edges in the marriage network. To do that, we can use an edge covariate parameter edgecov()
# As in: ergm(MyNetwork ~ Signature1 + Signature2 + ... + edgecov(AnotherNetwork))

flo.mar.3 <- ergm(flobusiness ~ edges + edgecov(flomarriage))       
flo.mar.3
summary(flo.mar.3)

# The log odds of a business tie here would be -2.6 if there is no marriage tie,
# but -2.6 + 2.2 = -0.4 if there is a marriage tie.

# We can also use node attributes in an erg model.
# For the Florentine families, we have an attribute called "wealth" in the network object.

flomarriage %v% 'wealth'  # Family wealth (stored as a node attribute in the network object)
w.vec <- flomarriage %v% 'wealth' # if we wanted to store the wealth as a vector

# Let's plot the network with vertex size proportional to wealth:
gplot(flomarriage, vertex.cex=flomarriage %v% 'wealth'/25)	

# Let's test whether the edge probabilities are a function of wealth:
# Are wealthy families more likely to form ties? Use parameter nodecov(): 

flo.mar.4 <- ergm(flomarriage ~ edges + nodecov("wealth"))       
flo.mar.4
summary(flo.mar.4)

# Yes, there is a significant positive main effect for wealth (though small):
# - The p-value for the wealth parameter makes it significant at the .05 level.
# - It's positive, which means we see more of that configuration than we'd expect by chance.
# The log odds of a tie A <-> B here would be -2.6 + 0.1*(wealth of A + wealth of B)

# ================ ~~ Directed network example ================

# Example of ERGM for a directed network: Sampson Monastery 
# ERG model of a directed network - the "liking" relations between monks
# in Sampson's dataset at three different points in time.

data(samplk)

samplk1
samplk2
samplk3

plot(samplk3)

# Is there a statistically significant tendency for ties to be reciprocated?
# We will use the 'mutual' parameter below.

samp.mod.1 <- ergm(samplk3 ~ edges + mutual)	
summary(samp.mod.1)				

# Conditional log-odds of two actors forming a tie =
# = -2.15 * change in the number of ties + 2.3 * change in number of mutual dyads
# If adding the tie will not make a dyad reciprocal, its log-odds = -2.15
# if it will add a mutual dyad to the network, its log-odds = -2.15 + 2.3 = 0.15  



# ================ ~~ Node attribute example ================


# ERGM with node attributes: Faux Mesa High
# Faux mesa high is simulated data representing a high-school friendship network.
# Attributes for each node (student) include gender, race, and grade.
# This network is undirected.

data(faux.mesa.high)  			
fmh.net <- faux.mesa.high
plot(fmh.net)						
fmh.net

# Taking a look at gender 
fmh.net %v% 'Sex'
table(fmh.net %v% 'Sex')
plot(fmh.net, vertex.col='Sex')

# Taking a look at the grade of the students
fmh.net %v% 'Grade'
table(fmh.net %v% 'Grade')
plot(fmh.net, vertex.col='Grade') 

# Taking a look at the race of the students
fmh.net %v% 'Race'
table(fmh.net %v% 'Race')
plot(fmh.net, vertex.col='Race') 


# A simple model that includes just the edge (density) parameter:
fmh.mod.1 <- ergm(fmh.net ~ edges)
summary(fmh.mod.1)


# NODEMATCH 
# Are nodes with the same attribute levels more likely to be connected?
# Do high-school students tend to have friends of the same grade?

fmh.mod.2 <- ergm(fmh.net ~ edges + nodematch("Grade"))
summary(fmh.mod.2)

# We can add an attribute diff=T to nodematch to get a separate parameter for
# each level of the categorical variable. 
# Here, a separate parameter for each grade:

fmh.mod.3 <- ergm(fmh.net ~ edges + nodematch("Grade", diff=T))
summary(fmh.mod.3)


# How about gender and race?  

fmh.mod.4 <- ergm(fmh.net ~ edges + nodematch("Grade") + nodematch("Race") + nodematch("Sex"))
summary(fmh.mod.4)


# NODEMIX
# Nodemix will add a parameter for each combination of levels for the categorical variable.
# Let's look at the parameters for edges between students from different genders:

fmh.mod.5 <- ergm(fmh.net ~ edges + nodemix("Sex"))
summary(fmh.mod.5)

# How about mixing between students of different races?
fmh.mod.6 <- ergm(fmh.net ~ edges + nodemix("Race"))
summary(fmh.mod.6)

# Note that we got -Inf parameters in the model for some configurations 
# that is because they do not exist in the observed network at all.

table(fmh.net %v% "Race")  			# Check out race frequencies
mixingmatrix(fmh.net, "Race")   # Check out # of links between/within groups


# NODEFACTOR
# Main effect of a categorical attribute.
# Are some types of nodes more likely to form ties than others?
# For example, are boys forming friendship ties more actively than girls?

fmh.mod.7 <- ergm(fmh.net ~ edges + nodematch("Grade", diff = T) + nodefactor("Sex"))
summary(fmh.mod.7)

# Negative parameter for males means females are more actively forming friendships.

# NODECOV
# Main effect of a continuous attribute (we'll treat grade as continuous here).
# Are nodes with high levels on a continuous attribute more likely to form ties?
# Let's check if students with higher values on attribute "Grade" tend to form more friendships.

fmh.mod.8 <- ergm(fmh.net ~ edges + nodecov("Grade") + nodematch("Sex"))
summary(fmh.mod.8)

# Note that "nodecov" is the parameter version for undirected networks.
# For directed networks, we have two separate parameters we can use:
# "nodeicov" (for incoming links) and "nodeocov" (for outgoing links).
# So for example, we would use nodeicov("Grade") to examine whether 
# students in a higher grade are more likely to receive incoming links.
# Similarly "nodefactor" has directed versions "nodeifactor" & "nodeofactor".


# ABSDIFF
# For continuous attributes: are people more likely to be connected to others
# who have similar values on an attribute? Absdiff = abs(ValueNode1-ValueNode2)
# Here, are students more likely to have friends close to their own grade?
# (that is, links i->j are more likely for smaller values of abs(grade of i - grade of j))

fmh.mod.9 <- ergm(fmh.net ~ edges + absdiff("Grade") + nodematch("Sex"))
summary(fmh.mod.9)

# Result: the greater the difference in grade, the less likely nodes are to connect.


# ================ ~~ Network simulations ================

# Simulating networks based on a model 

# After we have estimated model coefficients, we can draw graphs from
# the probability distribution defined by those parameter values.
# If our model was good, the graphs we draw from this distribution
# should be similar to our observed data.

# Simulate 15 networks based on the fmh.mod.9 model:

fmh.mod.9.sim <- simulate(fmh.mod.9, nsim=15)
summary(fmh.mod.9.sim)

# All the simulated network are stored in the returned object:
class(fmh.mod.9.sim)

# We can access any of them and take a look at it:
fmh.mod.9.sim[[1]]



# Goodnes of Fit and diagnostics
#=============================================================#

# After estimating parameters for your mode, you want to know how well
# it fits the observed data.

# Let's check the goodness of fit for one of our initial models of the Padgett network.
# Check how well the degree distribution of the networks generated from our model
# match the degree distribution of the observed network:

summary(flo.mar.4) # Take a look at the model

# Goodness of fit for this model
# By default it examines degree distribution;
# edgewise shared partner (triadic closure statistic);
# and geodesic distance (shortest patsh between nodes)
gof.1 <- gof(flo.mar.4)
gof.1

plot(gof.1)

# Note that the p-values here tell us whether the networks generated by our model
# are significantly different from our observed data. So large p values are good news!
#
# Let's take a look at the degree distribution goodness of fit for our model.
# The resutls contain 1 row for each possible node degree (e.g. row 0 - number of isolates,
# row 1 - number of nodes with only 1 link, row 2 - # of nodes with 2 links, etc.)
# The first column contains the counts from the observed network, the other give staticstics
# from the simulated networks.  

# The fit is not bad - observed values are within the confidence interval (see the plot).
# P values are high (the observed & simulated values do not differ significantly). 


# We can check the goodness of fit with regard to other network statistics.
# For instance, different types of triads in the generated vs. observed network 
# That is to say, groups of three nodes with 0, with 1, 2, or 3 ties among them.

gof.2 <- gof(flo.mar.4, GOF=~triadcensus) 
gof.2
plot(gof.2)

 
# Model diagnostics (for MCMC)
# Information about the model that can help diagnose problems.
# Note we can't get (and don't need) these diagnostics for flo.mar.4 since
# it was not estimated using MCMC. This is because it was simple enough 
# (i.e. a dyadic independence model) that we did not need MCMC estimation.

mcmc.diagnostics(flo.mar.2)

# It's easier to go through the charts if we save in a PDF file:

pdf("flo_mar_model2.pdf")
mcmc.diagnostics(flo.mar.2)
dev.off()

# We can examine the diagnostics to see whether our model looks ok,
# check for model degeneracy, see if MCMC sample size and burn-in are large enough, etc. 
# Read more about interpreting model diagnostics in Section 7 of this tutorial:
# https://statnet.org/workshop-ergm/ergm_tutorial.html#7_Diagnostics:_troubleshooting_and_checking_for_model_degeneracy

 
# ================ THE END ================



