
##================================================##
##                                                ##
##       Computational Social Science             ##
##       Doctoral Seminar (16:194:672)            ##
##       Spring 2023, Rutgers University          ## 
##       Katya Ognyanova, katya@ognyanova.net     ## 
##                                                ##
##================================================##




# ================  Visualization in R  ================

# Packages we would need today:
# (remove # and run this if they are not already installed)  


# install.packages("ggplot2")    # Data visualization
# install.packages("dplyr")      # Data cleaning


# ================  Introduction to ggplot2  ================

# For (much more) detail on ggplot2 and data analysis, check out these books:
# "ggplot2: elegant graphics for data analysis"  https://ggplot2-book.org/ 
# "R for Data Science"  https://r4ds.had.co.nz/ 

library(ggplot2)
library(dplyr)

# We will use a popular car-related dataset included with the ggplot2 package.

# DISCLAIMER:
# Given my woeful ignorance about car models and fuel types, I am only reluctantly
# using this dataset because it is used in many function help files and online examples 
# you might see for ggplot. It may be helpful to understand it, though there are
# likely countless datasets out there that would be more interesting for all of us.
#
# The frequent use of this data as an example is alwo why I did not immpediately
# rename all columns to names that are more helpful (e.g. engine_size, fuel_type, etc.).


data(mpg)
head(mpg)

# Fuel efficiency data about cars released in 1999 and 2008.
# Some variables included in mpg...
# manufacturer: manufacturer name
# model: model name
# displ: engine size in liters
# year: year of manufacture
# cty: city miles per gallon
# hwy: highway miles per gallon
# fl: fuel type
# class: type of car
# drv: drivetrain, whatever that means, don't ask me, I have no idea.


# Each ggplot2 chart has three main components: 
# data, aes (aesthetics) and geom (geomertic objects)
# DATA: no surprise here, it's the data you want to visualize.
# AESTHETICS: how does your data map onto visual characteristics?
# GEOM OBJECTS: specific chart elements (lines, dots, bars, etc.)


# ================  Aesthetic mappings  ================

# Note the somewhat unusual format of ggplot() use.
# We create the plot with 'ggplot()', then add to it with '+'

ggplot(data = mpg) # empty plot!

# Let's plot car engine sizes by gas mileage on a highway:
# geom_point tells ggplot to add point to the plot with
# x coordinates engine size (displ), y coordinates mileage (hwy)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
 
# Here, the 'ggplot' function creates a blank chart.
# 'geom' adds a layer to the chart with a specific element (point, line, bar, etc.)
# 'aes'  links your data to specific visual properties (position, size, color, etc.)

# Each geom function we can use has a mapping specifying the x and y axis.
# We can add other aesthetics linking variables to color, size, etc.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# See the big-engine high-mileage outliers on the right?
# Now we know those are 2-seat sports cars of some sort.

# Use size instead of color:
# (ggplot warns us we're mapping size to a categorical variable)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# Use transparency instead of color, with parameter 'alpha'
# (alpha ranges from 0 or fully transparent, to 1 or fully opaque)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Use shape to specify fuel type.
# (For the sake of the example only; very tough to read!)
# ggplot uses only 6 shapes by default, stops plotting after.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = fl))

# We can also specify properties manually, not using our data
# Notice that those parameters are now added outside of aes()!  
# They change the appearance of the chart, but are NOT based on our data.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "darkred", size = 3, shape = 18)

# R has built-in shapes that you can specify by number. See the list of shapes at:
# https://cran.r-project.org/web/packages/ggplot2/vignettes/ggplot2-specs.html

# For elements that have outline and fill, we use 'color' for the outline colors, 
# but use 'fill' for the actual color of the element; 'stroke' is the outline width.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), 
             color = "darkred", fill="orange", size = 3, stroke = 2, shape = 22)

# If the variable we use to color the dots is continuous, ggplot will use a gradient:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = cyl), size = 2)

# Note that instead of specifying 'aes' mappings separately for each layer (geom),
# we can just add them to the main ggplot() function. That will make the mappings
# serve as the default for every layer that doesn't have its own aes().

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color=class)) + 
  geom_point(size=3)

# When we feel lazy, we can skip the "data" and "mapping" parameter names --
# we can usually count on R to recognize them based on their position.

ggplot(mpg, aes(x = displ, y = hwy, color=class)) + 
  geom_point(size=3)


# ================  Plot types  ================ 
 
# -------  ~~ Scatterplots  ------- 

# As we saw above, we can create those using 'geom_point':

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color=class)) + 
  geom_point(size=3)


# We can also fit a line to the data:

# Mileage on the highway by engine size in liters
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point( size = 2) + 
  geom_smooth(method='lm') # linear regression line: displ predicts hwy


ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point( size = 2) + 
  geom_smooth(method='loess') # local smooth regression



# -------  ~~ Line charts  ------- 
 

# Simple line
# Let's make up our own data for this example.
# (by now everyone is probably tired of car numbers)
yr <- 2000:2010
art <- c(1,4,6,7,12,14,9,5,4,3,0)
sci <- c(0,1,1,5,6,8,10,15,7,5,0)
budget <- data.frame(Year=yr, Art=art, Science = sci)
budget  

ggplot(budget, mapping = aes(x = Year, y = Art)) + 
  geom_line()


# Add a second layer with points to the data, and change colors/size
# Note that the Year axis doesn't look good -- we'll fix that later on.
ggplot(budget, aes(x = Year, y = Art)) +  
  geom_line(color="darkgray", linewidth = 1.5) + 
  geom_point(color="darkred", size = 3, alpha = 0.8)
  
# Add interval around our line
# We often want to show intervals around our data (e.g. a confidence interval)
# Here we'll use geom_ribbon and just set it to our data +/- 1.
ggplot(budget, aes(x = Year, y = Art)) + 
  geom_ribbon( aes(ymin = Art - 1, ymax = Art + 1), fill = "gray80") +
  geom_line(color="gray40", linewidth = 1) + 
  geom_point(color="darkred", size = 3, alpha = 0.5)

# Add both lines, art and science:
ggplot(budget) +  
  geom_line( aes(x = Year, y = Art), color="gray50", linewidth = 1.5) + 
  geom_line( aes(x = Year, y = Science), color="darkred", linewidth = 1.5)


# Do we have to add all the lines we want one by one? No, but we'd have to reshape our data.
# In many cases, ggplot will need to use the so-called 'long' data format (our data is 'wide').
# Below, reshape() changes our data format from wide to long by combining the separate columns
# we had for "Art" and "Science" into a single "Money" column. It also adds a "Subject" column
# where we specify if each of the money scores refers to our Art or Science budget.

budget2 <- reshape(budget, idvar="Year", varying=list(2:3), v.names = "Money",
                       timevar = "Subject", times=c("Art", "Science"), 
                       direction = "long")
budget2

ggplot(budget2,  aes(x = Year, y = Money, color = Subject)) +  
  geom_line (linewidth = 1.5)  


# And now back to our regularly scheduled programming: the boring car data.

# We are often going to need to do some calculations before visualizing.

# What is the average highway mileage by type of car (regardless of year)?
# First, we want to split our data in groups, one for each class of car.
# Then, we want to calculate average highway miles per gallon for each group.

mpg2 <- group_by(mpg, class)  # Group the mpg dataset by class
summarize(mpg2, annual_mpg = mean(hwy)) # Get average 'hwy' for each class


# How does the mileage for each class of car change from 1999 to 2008?
# We want to calculate average miles per gallon for car of each class in both years.
mpg3 <- group_by(mpg, year, class ) # Group the data by year and class
mpg3 <- summarize(mpg3, mileage= mean(hwy)) # Get average mileage for each
mpg3 # New dataset to use

# Line chart; lines represent change in mileage from 1999 to 2008
# Note how we've added two layers again -- both lines and points.
ggplot(mpg3, aes(x = year, y = mileage, color = class)) + 
  geom_line(linewidth = 1) +
  geom_point( size = 2)



# -------  ~~ Area charts  -------  


# We'll just  tweak our line chart, replacing geom_line with geom_density.
# stat = "identity" means to just use the raw data for the y axis (there are other options)
ggplot(budget2, aes(x = Year, y = Money, color = Subject, fill = Subject)) +  
  geom_density(stat = "identity", size = 1.5, alpha=0.4)


# -------  ~~ Bar charts  ------- 

# How many cars of each class?
ggplot(mpg, aes(x = class)) + 
  geom_bar()  

# And now horizontally: mapping class to the y axis.
# We'll also throw in outline and color.
ggplot(mpg, aes(y = class)) + 
  geom_bar(color="black", fill="olivedrab")  

# How about a stacked bar? 
# We can change the fill color based on a variable.
# Bars based on car type, fill based on fuel type used
ggplot(mpg, aes(x = class, fill = fl)) + 
  geom_bar(position = position_stack())

# Or, we can group the bars using 'group'
# position_dodge2 makes sure bars are next to each other
# the 'preserve' parameter makes the bars have equal width
ggplot(mpg, aes(x = class, fill=fl)) + 
  geom_bar(position = position_dodge2(preserve = "single"))

# What if we want our Y axis to be something other than counts?
# Average mileage for each car class, for example (we'll use mpg3). 
# To do that, we'll replace 'geom_bar' with 'geom_col' in this case:
ggplot(mpg3, aes(x = class, y = mileage, fill = year)) + 
  geom_col(position = position_dodge2(preserve = "single")) 

# We can use factor(year) to make sure it's treated as a categorical variable:
ggplot(mpg3, aes(x = class, y = mileage, fill = factor(year))) + 
  geom_col(position = position_dodge2(preserve = "single")) 


# -------  ~~ Box plots  ------- 

# Summary of the data showing maximum, minimum, and outliers (top and bottom)
# as well as the median (horizontal line) and interquartile range (box) 
# (interquartile range shows where the middle 50% of the data fall -- 
# e.g. for 1,2,3,4,5,6,7,8,9,10, it would be 3 to 8)

ggplot(mpg, aes(x = class, y = hwy, fill = class)) + 
  geom_boxplot()

# Horizontal
ggplot(mpg, aes(x = hwy, y = class, fill = class)) + 
  geom_boxplot()

# Separate boxplot for each year, mileage by class
ggplot(mpg, aes(x = class, y = hwy, fill = factor(year))) + 
  geom_boxplot()

# How do we add some other summary statistics like the 
# average highway mileage to our plot? 
# We can use stat_summary() and specify the function 
# we want calculated, here the mean.
 
ggplot(mpg, aes(x = class, y = hwy, fill = class)) + 
  geom_boxplot() +
  stat_summary(fun=mean, geom="point",  color="white")



# -------  ~~ Histograms  ------- 

# How many cars fall in each mileage range?
# 'bins' -- in how many bins to split the possible miles per gallon
# 'binwidth' -- the intervals for splitting miles per gallon 
ggplot(mpg, aes(x = hwy)) + 
  geom_histogram(bins = 20, color="white")

ggplot(mpg, aes(x = hwy)) + 
  geom_histogram(binwidth = 5, color="white")


# ================  Labels and styling  ================  


# -------  ~~ Themes  ------- 

# Settings for visual elements in ggplot can be stored as themes

ggplot(budget2, aes(x = Year, y = Money, color = Subject, fill = Subject)) +  
  geom_density(stat = "identity", size = 1.5, alpha=0.4) +
  theme_dark() #dark theme

ggplot(budget2, aes(x = Year, y = Money, color = Subject, fill = Subject)) +  
  geom_density(stat = "identity", size = 1.5, alpha=0.4) +
  theme_bw() #black and white theme

ggplot(budget2, aes(x = Year, y = Money, color = Subject, fill = Subject)) +  
  geom_density(stat = "identity", size = 1.5, alpha=0.4) +
  theme_classic()

ggplot(budget2, aes(x = Year, y = Money, color = Subject, fill = Subject)) +  
  geom_density(stat = "identity", size = 1.5, alpha=0.4) +
  theme_minimal()


# -------  ~~ Colors  ------- 

# So far we have been using the default ggplot2 color scheme.
# Let's select our own colors. We can select discrete colors 
# for lines with 'scale_color_manual' and fill with 'scale_fill_manual'

ggplot(budget2, aes(x = Year, y = Money, color = Subject, fill = Subject)) +  
  geom_density(stat = "identity", size = 1.5, alpha=0.4) +
  scale_color_manual(values = c("darkgray", "darkred")) + 
  scale_fill_manual(values = c("darkgray", "darkred")) + 
  theme_minimal()

# What about continuous variables where the color is based on a number?
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color=cyl)) + 
  geom_point(size=3) + 
  theme_minimal()

# In that case, we can use 'scale_color_gradient' and 'scale_fill_gradient'
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color=cyl)) + 
  geom_point(size=3) + 
  scale_color_gradient(low = "gray", high = "darkred") +
  theme_minimal()

# Sometimes, we might want to use a divergent gradient:
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color=hwy)) + 
  geom_point(size=3) + 
  scale_color_gradient2(low = "slateblue", mid = "gray", high = "darkred", midpoint = 25) +
  theme_minimal()

# Or just select a bunch of colors and make them into a gradient:
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color=hwy)) + 
  geom_point(size=3) + 
  scale_color_gradientn(colors=c("yellow", "orange", "red", "slateblue", "gray")) +
  theme_minimal() 


# We can even use pre-made palletes from the RColorBrewer package:
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color=class)) + 
  geom_point(size=3) + 
  scale_color_brewer(palette="Dark2") + 
  theme_minimal() 
  
# For the full list, see:
RColorBrewer::display.brewer.all()


# -------  ~~ Labels  ------- 

# Add labels
ggplot(budget2, aes(x = Year, y = Money, color = Subject, fill = Subject)) +  
  geom_density(stat = "identity", size = 1.5, alpha=0.4) +
  theme_bw() + 
  labs(title = "Budget for Art and Science Over Time",
       subtitle = "(2000-2010)",
       caption = "Data source: I made it up",
       tag = "Figure 1",
       x = "Year (2000-2010)",
       y = "Money in million dollars",
       fill = "Budget Area",
       color = "Budget Area")


# What if we want to add data labels though?
# We'll use a 'geom_label', where 'x' and 'y' specify label position
# and 'label' specifies what the text of the label should be
ggplot(budget, aes(x = Year, y = Art)) +  
  geom_col(size = 1.5, alpha=0.6) +
  geom_label(aes(label = Art)) +
  theme_bw() + 
  labs(title = "Budget for Art and Science Over Time",
       subtitle = "(2000-2010)")

# Nudge those labels a bit up by adding to 'y':
ggplot(budget, aes(x = Year, y = Art)) +  
  geom_col(size = 1.5, alpha=0.6) +
  geom_label(aes(label = Art, y = Art + 0.7)) +
  theme_bw() + 
  labs(title = "Budget for Art and Science Over Time",
       subtitle = "(2000-2010)")


# We can also specify a number of things about labels and other elements
# to modify the plot theme in a way that fits what we want to accomplish.
# Here is a quick example. For more details see https://ggplot2-book.org/polishing.html

ggplot(budget2, aes(x = Year, y = Money, fill=Subject)) + 
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("darkgray", "darkred")) + 
  labs(title = "Budget for Art and Science Over Time",
        subtitle = "(2000-2010)",
        caption = "Data source: I made it up") +
  theme( legend.position="top",                                            # Legent position
         panel.background = element_rect(fill = "oldlace"),                # Background
         panel.grid.major = element_line(color = "white",linewidth = 0.5), # grid color and width
         plot.margin=unit(c(0,1,0,1),"cm"),                                # Plot margins 
         plot.caption=element_text(size = 9, color="black"),               # Caption font     
         plot.subtitle=element_text(face = "italic", size = 12, color = "grey40"), # Subtitle
         plot.title=element_text(size = 18, face = "bold") )                       # Title



# -------  ~~ Scale values  ------- 

# How do we change the numeric values displayed on the x and y axis?
# We'll use 'scale_x_continuous' and 'scale_y_continuous' to do that.
# We can set the axis limits with 'limit = c(lower_limit, higher_limit)'
# The breaks (tick marks) are set with the 'breaks' parameter.
# We can also select what label to put at each break with 'labels'.

ggplot(budget2, aes(x = Year, y = Money, color = Subject, fill = Subject)) +  
  geom_line (linewidth = 1.5) +
  scale_color_manual(values = c("darkgray", "darkred")) +  
  scale_x_continuous(limits = c(1999,2011), breaks = 2000:2010) +
  scale_y_continuous(limits = c(0,15), breaks = c(5, 10, 15),
                     labels = c("Low", "Medium", "High")) +
  theme_minimal()


# In this dataset, we actually may want to treat x as categorical.
# We can specify properties for it using 'scale_x_discrete' 
# For example, we can change the names of the categories.
ggplot(budget, aes(x = factor(Year), y = Art)) +  
  geom_col(size = 1.5, fill = "olivedrab", alpha=0.7) + 
  scale_x_discrete(labels=c("Y2K", 2001:2009, "Twenty Ten") )+
  scale_y_continuous(limits = c(0,15), breaks = seq(2, 16, 2)) + 
  theme_bw() 
  

# -------  ~~ Multi-panel plots ------- 


# Make a plot with separate panels for art and science
# We can do that by adding 'facet_wrap' to our plot
# inside 'vars()', we will specify how we want to split 
# the plot panels (what variables to use to create panels)
# We can also specify how many rows / columns of panels we want

ggplot(budget2, aes(x = Year, y = Money, color = Subject, fill = Subject)) +  
  geom_line (linewidth = 1.5) +
  scale_color_manual(values = c("darkgray", "darkred")) +  
  scale_x_continuous(limits = c(1999,2011), breaks = 2000:2010) +
  scale_y_continuous(limits = c(0,15), breaks = c(5, 10, 15),
                     labels = c("Low", "Medium", "High")) +
  theme_minimal() + 
  facet_wrap( vars(Subject), nrow=1, ncol=2) 


# Same plot, 2 rows now:
ggplot(budget2, aes(x = Year, y = Money, color = Subject, fill = Subject)) +  
  geom_line (linewidth = 1.5) +
  scale_color_manual(values = c("darkgray", "darkred")) +  
  scale_x_continuous(limits = c(1999,2011), breaks = 2000:2010) +
  scale_y_continuous(limits = c(0,15), breaks = c(5, 10, 15),
                     labels = c("Low", "Medium", "High")) +
  theme_minimal() + 
  facet_wrap(vars(Subject), nrow=2, ncol=1) 


# We can also create a grid of panels defined by two variables.
# Below, we are plotting mileage (hwy) by engine size (displ)
# But we are creating a separate plot panel for each combination
# of the number of fuel type (fl) and year (year)

# We can do those as separate plots one after another with 'facet_wrap'
# Or place them in a common grid with 'facet_grid'

# Facet wrap, going in order plot by plot:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) + 
  theme_dark() +  
  facet_wrap(vars(year, fl))

# Face grid, columns by year, rows by fuel type
# Notice that the grid will show empty plots 
# (e.g. for some types of fuel missing in 1999)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) + 
  theme_dark() +  
  facet_grid(vars(fl), vars(year))



# ================  Save your plots  ================  

# We can store plots as R objects:

my_plot <- ggplot(budget2, aes(x = Year, y = Money, color = Subject, fill = Subject)) +  
            geom_line (linewidth = 1.5) +
            geom_density(stat = "identity", size = 1.5, alpha=0.4) +
            scale_color_manual(values = c("darkgray", "darkred")) +
            scale_fill_manual(values = c("darkgray", "darkred")) +
            scale_x_continuous(limits = c(1999,2011), breaks = 2000:2010) +
            scale_y_continuous(limits = c(0,15), breaks = c(5, 10, 15),
                               labels = c("Low", "Medium", "High")) +
            facet_wrap( vars(Subject), nrow=1, ncol=2)+
            theme_bw() + 
            labs(title = "Budget for Art and Science Over Time",
                 subtitle = "(2000-2010)",
                 caption = "Data source: I made it up",
                 x = "Year (2000-2010)",
                 y = "Money in million dollars",
                 fill = "Budget Area",
                 color = "Budget Area")

my_plot

# We can add to the plot:
my_plot + 
  labs(tag = "Figure 1")

# Save the plot as a file using ggsave
# If we don't specify a plot, ggsave will use the last one we created.
# The function can recognize several file extensions (pdf, png, jpeg, tiff, svg, etc.)

ggsave(plot = my_plot, filename = "My_plot.pdf", width =12, height =5, units = "in")

ggsave(plot = my_plot, filename = "My_plot.jpg", width =12, height =5, units = "in")


# ================ THE END ================



