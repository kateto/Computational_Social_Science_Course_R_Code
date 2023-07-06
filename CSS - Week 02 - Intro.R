
##================================================##
##                                                ##
##       Computational Social Science             ##
##       Doctoral Seminar (16:194:672)            ##
##       Spring 2023, Rutgers University          ## 
##       Katya Ognyanova, katya@ognyanova.net     ## 
##                                                ##
##================================================##



# ================  Introduction to R ================


# Run any part of your code by selecting it an pressing Ctrl+Enter / Cmd+Enter
# Any part of the code starting with # is a comment 


#  -------~~ Assignments --------


# You can assign a value to an object using assign(),  "<-", or "=".
# If you are just starting out, use "<-", there is less chance of confusion with it.

x <- 3         # Assignment
x              # Evaluate the expression and print result

y <- 4         # Assignment
y + 5          # Evaluation, y remains 4

z <- x + 17*y  # Assignment
z              # Evaluation

rm(z)          # Remove z: deletes the object.
z              # Error!


# You can give the objects you create more helpful names.
# They should start with a letter or a period, and contain 
# letters, numbers, underscores, or periods.

my_dogs_age <- 5
my_dogs_age <- my_dogs_age + 1
my_dogs_age # Happy 6th birthday, dog!


#  -------~~ Value comparisons --------

# Comparisons return the logical constants TRUE or FALSE 

2==2  # Equality
2!=2  # Inequality; ! in R means "not"
x <= y # less than or equal: "<", ">", and ">=" also work


# You can force TRUE and FALSE to behave like numbers
# In that case, TRUE will be 1, and FALSE will be 0
TRUE + TRUE 
5*TRUE
5*FALSE 

# TRUE and FALSE can be abbreviated to T and F (capitalized!)
T==F
T>F
!T

#  ------~~ Special constants -------- 

# NA, NULL, Inf, -Inf, NaN

# NA - missing or undefined data
5 + NA      # When used in an expression, the result is generally NA
is.na(5+NA) # Check if missing?

# Note that is.na() above is an example of a function in R
# It uses the parameters listed in parentheses to compute and return a result

# NULL - an empty object, e.g. a null/empty list
10 + NULL     # use returns an empty object (length zero)
is.null(NULL) # check if NULL (is.null is another function)

# Inf and -Inf represent positive and negative infinity
# They can be returned by  mathematical operations like division of a number by zero:

5/0
is.finite(5/0) # Check if a number is finite

# NaN (Not a Number) - the result of an operation that cannot be reasonably defined 
0/0
is.nan(0/0)


#  ------~~ Vectors --------  

# We can create a vector with c(), a function which combines values
v1 <- c(1, 5, 11, 33)       # Numeric vector, length 4
v2 <- c("hello","world")    # Character vector, length 2 (a vector of strings)
v3 <- c(TRUE, TRUE, FALSE)  # Logical vector, length 3, same as c(T, T, F)

# Combining different types of elements in one vector will coerce the elements 
# to the least restrictive type:

v4 <- c(v1,v2,v3,"boo") 	# All elements turn into strings

# Other ways to create vectors:
v <- 1:7         # same as c(1,2,3,4,5,6,7)  
v <- rep(0, 77)  # repeat zero 77 times: v is a vector of 77 zeroes
v <- rep(1:3, times=2) # Repeat 1,2,3 twice  
v <- rep(1:10, each=2) # Repeat each element twice  
v <- seq(10,20,2) # sequence: numbers between 10 and 20, in jumps of 2  

length(v)        # check the length of the vector 

v1 <- 1:5         # 1,2,3,4,5
v2 <- rep(1,5)    # 1,1,1,1,1 

# Element-wise operations:
v1 + v2      # Element-wise addition
v1 + 1       # Add 1 to each element
v1 * 2       # Multiply each element by 2
v1 + c(1,2)  # Problem: c(1,2) is a vector of a different length

# Mathematical operations:
sum(v1)      # The sum of all elements
mean(v1)     # The average of all elements
sd(v1)       # The standard deviation
min(v1)      # the minimum value
max(v1)      # the maximum value
cor(v1,v1*5) # Correlation between v1 and v1*5 

# Logical operations:
v1 > 2       # Each element is compared to 2, returns logical vector
v1==v2       # Are corresponding elements equivalent, returns logical vector.
v1!=v2       # Are corresponding elements *not* equivalent? Same as !(v1==v2)
(v1>2) | (v2>0)   # the | is the logical operator OR, returns a vector
(v1>2) & (v2>0)   # the & is the logical operator AND, returns a vector

all(v1>3) # Function all() checks if something is true for ALL elements
any(v1>3) # Function any() checks if something is true for ANY elements

# Vector elements
v1[3]             # third element of v1
v1[2:4]           # elements 2, 3, 4 of v1
v1[c(1,3)]        # elements 1 and 3 - note that your indexes here are a vector
v1[c(T,T,F,F,F)]  # elements 1 and 2 - only returns the ones that are TRUE
v1[v1>3]          # v1>3 is a logical vector TRUE for elements >3
v1[-3]            # negative number: will return all elements *except* the third

# NOTE: If you are used to languages indexing from 0, R will surprise you by indexing from 1.

# To replace elements in a vector, assign them a new value
v1[5] <- 10
v1

# To add more elements to a vector, simply assign them values.
v1[6:10] <- 6:10

# We can even directly assign the vector a length:
length(v1) <- 15 # the last 5 elements are added as missing data: NA


#  ------~~ Factors --------

# Factors can be used to store categorical data.

home_state_v <- c("NJ", "CA", "NJ", "NY", "NY", "NY", "PA")         #vector
home_state_f <- factor(c("NJ", "CA", "NJ", "NY", "NY", "NY", "PA")) #factor
home_state_v
home_state_f

# R will identify the different levels of the factor - e.g. all distinct values. 
# The data is stored internally as integers - each number corresponding to a factor level.

levels(home_state_f)  # The levels (distinct values) of the factor (categorical variable)
levels(home_state_f) <- c("CA","NJ","NY","PA","TX","MA")
home_state_f

as.numeric(home_state_f)  # The factor as numeric values: 1 is CA, 2 is NJ, 3 is NY, 4 is PA
as.numeric(home_state_v)  # The character vector, however, can not be coerced to numeric values

as.character(home_state_f)  # convert factor to character vector
as.factor(home_state_v)     # convert vector to factor

# Sometimes, our categorical data can be ordered.
# For example, we may have a survey question asking people
# how they feel about puppies:

puppy_v <- c("love", "love", "hate", "like", "dislike", "love")
liking_levels <- c("love", "like","neutral", "dislike", "hate")

puppy_f <- factor(puppy_v, levels=liking_levels, ordered=TRUE)
puppy_f

# In the code above, we did two things: mentioned a possible level
# that was not in our data (neutral) and specified order


#  ------~~ Matrices & Arrays --------  

# A matrix is a vector with dimensions:
m <- rep(1, 20)   # A vector of 20 elements, all 1
dim(m) <- c(5,4)  # Dimensions set to 5 & 4, so m is now a 5x4 matrix

# Create a matrix using matrix():
m <- matrix(data=1, nrow=5, ncol=4)  # same matrix as above, 5x4, full of 1s
m <- matrix(1,5,4) 			             # same matrix as above
dim(m)                               # What are the dimensions of m?

# Functions (such as matrix above) often require that we provide some parameters.
# Here, for example, we have 'data', 'nrow' (number of rows), and 'ncol' (number of columns).
# If we do not mention the parameter names, they are assumed based on the given order --
# the first thing in the parentheses is expected to be data, the second rows, the third columns.

# Create a matrix by combining vectors:
m <- cbind(1:5, 5:1, 5:9)  # Bind 3 vectors as columns, 5x3 matrix
m <- rbind(1:5, 5:1, 5:9)  # Bind 3 vectors as rows, 3x5 matrix

# If the data we provide has fewer than 10x10=100 elements, it gets repeated
m <- matrix(1:10,10,10) 

# Select matrix elements: 
m[2,3]  # Matrix m, row 2, column 3 - a single cell
m[2,]   # The whole second row of m as a vector
m[,2]   # The whole second column of m as a vector
m[1:2,4:6] # submatrix: rows 1 and 2, columns 4, 5 and 6
m[-1,]     # all rows *except* the first one

m[1,]==m[,1]  # Are elements in row 1 equivalent to corresponding elements from column 1? 
m>3           # A logical matrix: TRUE for m elements >3, FALSE otherwise
m[m>3]        # Selects only TRUE elements - that is ones greater than 3


t(m)          # Transpose m  (switch rows and columns)   
m <- t(m)     # Assign m the transposed m
m %*% t(m)    # %*% does matrix multiplication
m * m         # * does element-wise multiplication

# Arrays: more than 2 dimensions
# Created with the array() function:

a <- array(data=1:18,dim=c(3,3,2)) # 3d with dimensions 3x3x2
a <- array(1:18,c(3,3,2))          # the same array


#  ------~~ Lists --------  

# Lists are collections of objects (e.g. of strings, vectors, matrices, other lists, etc.)

l1 <- list(boo=v1,foo=v2,moo=v3,zoo="Animals!")  # A list with four components
l2 <- list(v1,v2,v3,"Animals!")

l3 <- list() # empty list
l4 <- NULL   # null (empty) object

l1["boo"]      # Access boo: this returns a list.
l1[["boo"]]    # Access boo: this returns the numeric vector
l1[[1]]        # Returns the first component of the list, equivalent to above.
l1$boo         # Named elements can be accessed using the $ operator - equivalent to [[]]

# How can I tell if something is a list or not?
is.list(v1) # is this a list?
is.list(l1) # is this a list?
class(l1)   # what kind of object is this?

# Add more elements to a list:
l3[[1]] <- 11 # add an element to the empty list l3
l4[[3]] <- c(22, 23) # add a vector as element 3 in the empty list l4. 
# Since we added element 3, elements 1 & 2 will be generated and empty (NULL)
l1[[5]] <- "More elements!" # The list l1 had 4 elements, we're adding a 5th here.
l1[[8]] <- 1:11 # We added an 8th element, but not 6th or 7th. Those will be created empty (NULL)
l1$Something <- "A thing"  # Adds a ninth element - "A thing", named "Something"



#  ------~~ Data Frames --------  

# The data frame is a special kind of list used for storing our dataset tables.
# Think of rows as cases, columns as variables. Each column is a vector or factor.

# Creating a dataframe:

dfr1 <- data.frame( ID=1:4,
                    FirstName=c("John","Jim","Jane","Jill"),
                    Female=c(F,F,T,T), 
                    Age=c(22,33,44,55) )
dfr1

dfr1$ID        # Access the ID column of dfr1. 
class(dfr1$ID) # What type is that column?

dfr1$FirstName        # Access the FirstName column of dfr1. 
class(dfr1$FirstName) # What type is that column?

# Access elements of the data frame
dfr1[1,]   # First row, all columns
dfr1[,1]   # First column, all rows
dfr1$Age   # Age column, all rows
dfr1[1:2,3:4] # Rows 1 and 2, columns 3 and 4 - the gender and age of John & Jim
dfr1[c(1,3),] # Rows 1 and 3, all columns

# Find the names of everyone over the age of 30 in the data
dfr1[dfr1$Age>30, 2]

# Find the average age of all females in the data:
mean ( dfr1[dfr1$Female==TRUE, 4] )


#  ------~~ Flow Control --------

# if (condition) expr1 else expr2
x <- 5
y <- 10
if (x==0) y <- 0 else y <- y/x  
y

# To do several things if x equals 0, we can enclose the block of code
# that includes all of those things in {} like this:
if(x>0) {
  x <- 10
  y <- 42
}

# for (variable in sequence) expr
ASum <- 0
AProd <- 1
for (i in 1:x) {
  ASum <- ASum + i
  AProd <- AProd * i
}

ASum  # equivalent to sum(1:x)
AProd # equivalent to prod(1:x)

# while (condition) expr
while (x > 0) {
  print(x)
  x <- x-1 }

# repeat expr, use break to exit the loop
repeat { 
  print(x)
  x <- x+1
  if (x>10) break }


#  ------~~ R packages --------

# Much of the functionality of R is contained in packages.
# Packages group together related code and data that we can use.
# Anyone can create a package (I will show you how later on)
# "Official" versions of packages are stored in the CRAN repository.
# You can install those directly as long as you know the package name.

# For example: 
install.packages("igraph") 

# You can start using any package by loading it first:
library("igraph")          

# That allows you to access functions and data from that package.
# For example, graph.ring() is a function from the 'igraph' package.
# It creates a ring graph with a certain number of nodes (here 7)
g <- graph.ring(7)
plot(g)

# To get help on a package, try this:
package?igraph

# When you are done using a package, it is a good idea to let R know
# Detaching a package:
detach(package:igraph)    


#  ------~~ Read/write files --------

# Set the working folder where you want to read/write files:
# Note the forward slashes (as opposed to backslash in Windows paths)
setwd("C:/Classes/R CODE")

dfr1 <- data.frame( ID=1:4,
                    FirstName=c("John","Jim","Jane","Jill"),
                    Female=c(F,F,T,T), 
                    Age=c(22,33,44,55) )

# Save to a CSV file:
write.csv(dfr1, "my_data_frame.csv", row.names=F)

# Read the CSV file back:
dfr2 <- read.csv("my_data_frame.csv")
dfr2


#  ------~~ R troubleshooting --------

# While I generate many (and often very creative) errors in R, there are three
# simple things that will most often go wrong for me. Those include: 

# 1) Capitalization. R is case sensitive - a graph vertex named "Jack" is not the same
# as one named "jack". The function "rowSums" won't work as "rowsums" or "RowSums".
#
# 2) Object class. While many functions are willing to take anything you throw
# at them, some will still surprisingly require character vector or a factor instead of
# a numeric vector, or a matrix instead of a data frame. Functions will also occasionally
# return results in an unexpected format.
#
# 3) Package namespaces. Occasionally problems will arise when different packages
# contain functions with the same name. R may warn you about this by saying something
# like "The following object(s) are masked from 'package:igraph'" as you load a package.
# One way to deal with this is to call functions from a package explicitly using '::'.
# For instance, if function 'blah' is present in packages A and B, you can call
# A::blah and B::blah. In other cases the problem is more complicated, and you may
# have to load packages in certain order, or not use them together at all. 
 
# For more advanced troubleshooting, check out try(), tryCatch(), and debug().
?tryCatch







































