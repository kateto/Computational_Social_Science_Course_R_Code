w
##================================================##
##                                                ##
##       Computational Social Science             ##
##       Doctoral Seminar (16:194:672)            ##
##       Spring 2023, Rutgers University          ## 
##       Katya Ognyanova, katya@ognyanova.net     ## 
##                                                ##
##================================================##



# ================    Survey data in R   ================
 
# Packages we would need today:
# (remove # and run this if they are not already installed)  
  
# install.packages("summarytools")  # get data descriptives
# install.packages("jtools")        # pretty regression tables
# install.packages("survey")        # work with survey data



# ================  ~~ Reading the data ================
 
# To take a look at survey data, we will work with a dataset adapted 
# from Pew Research Center's American Trends Panel, Wave 67, 05/2020
# Available online at www.pewresearch.org/american-trends-panel-datasets

# You can download the data and its description from: 
# http://kateto.net/css/pew_data.zip
 

# First we will read the data, which is stored in a CSV file.
# In their documentation, PEW note that "Refused" answers are coded as 99
# As we read in the data, we can tell R to replace 99 with NA (missing)

dat <- read.csv("DATA/pew_data.csv", na.strings = 99)

# Examine the data 
colnames(dat)  # variables in dat
dim(dat)       # dimensions of dat

# What type does each of the survey variables have in R?
# read.csv() chooses it based on the data it finds in each column.

# Using 'sapply()' as bellow will take our data and apply the same function
# to each variable/column in it, then combine the results (when possible):

sapply(dat, class) # What is the class of each variable/column in the data?
sapply(dat, is.numeric) # Is each variable/column numeric?

# remember that:
#    'integer' is used for whole numbers, 
#    'numeric' is used for  numbers with a decimal point
#    'character' is used for text


sapply(dat,is.integer) # are variables integer?
integer_columns <- sapply(dat,is.integer)
integer_columns

# Change all the integer columns to numeric 
# (some of the functions we use below don't like integer data)
dat[, integer_columns] <- sapply(dat[, integer_columns], as.numeric)  



#---------------------# ADDITIONAL INFORMATION #---------------------# 
#
#  Note that we could also easily read the data from SPSS format.
#  One way to do that is by using the package 'haven', like so:
#
#  install.packages("haven")         
#  library(haven) 
#  dat <- read_sav("DATA/pew_data.sav")
#
#-------------------------------------------------------------------# 



# ================  ~~ Data descriptives ================
 

# Some variables from our data include...
#
# sex:        1 Male, 2 Female
# age_cat:    1 Age 18-29, 2 Age 30-49, 3 Age 50-64, 4 Age 65+
# race_ethn:  1 White 2 Black 3 Hispanic 4 Other 5 Asian 
# party:      1 Republican, 2 Democrat, 3 Independent, 4 Other  
# edu:        1 Less than high school,  2 High school graduate,  3 Some college, no degree
#             4 Associate degree, 5 College graduate/some post-grad, 6 Postgraduate degree
# income:      family income, 9 categories from under 1 10K to 9 over $150K
# ideology:    ideology, 5 categories from 1 Very conservative to 5 Very liberal
# attend:      religious service attendance, 1 more than once a week to  6 never  
# covid_news:  following COVID news, 1 very closely to 4 not at all closely
# covid_trump: approval of Trump's COVID-19 response, 1 excellent to 4 poor 


# Let's examine our variables. 
# In base R, we can get frequencies like so:

table(dat$sex)              # N men & women
prop.table(table(dat$sex))  # Percent men & women


# There are, however, more convenient ways to do this!
# There is a package for everything in R:

library("summarytools")

freq(dat$sex) 
freq(dat$age_cat)  
freq(dat$race_ethn)   
freq(dat$region)   
freq(dat$party)   
freq(dat$edu)   

freq(dat$income)   
descr(dat$income)

freq(dat$ideology)   
descr(dat$ideology)

freq(dat$attend)   
descr(dat$attend)
 
freq(dat$covid_news)   
descr(dat$covid_news)

freq(dat$covid_trump)   
descr(dat$covid_trump)

# We can also examine crosstabs!
table(dat$party, dat$age_cat)

# Or, the prettier format of "summarytools" package:
ctable(factor(dat$party), factor(dat$age_cat))

# Note that we enclose the variables in factor() to tell R
# that they should be treated as categorical and not numbers.


# ================  ~~ Data recoding ================


# Let's recode some variables to make them more convenient to use:

dat$sex_r <- c("Male", "Female")[dat$sex]
freq(dat$sex_r)

# Notice what is happening here.
#
# First, we are creating a 2-element vector with c("Male","Female"). 
# Then, we use dat$sex to index the 2-element vector we created.
#
#  c("Male", "Female")[dat$sex] means:
#
#       element 1      element 2        from that vector, get me element number:
#      .--------.     .--------.          
#  c(  |  Male  |  ,  | Female |  )   [  2,1,1,2,2,2,2,2,2,1,1,1,1... etc.   ]
#      .--------.     .--------.    
#
# 
# dat$sex looks like this: 2,1,1,2,2,2,2,2,2,1,1,1,1..... etc. It has 10,642 elements.
# Each element is 1 or 2, where 1 means the respondent is a man, and 2 means a woman.
# R doesn't know that though -- it just sees a long vector consisting of 1s and 2s.
# With c("Male", "Female")[dat$sex], we say that every time we encounter a 1 in dat$sex, 
# we want to get the first element of c("Male","Female"), and every time we encounter 
# 2 in dat$sex, we want to get the second element of c("Male","Female").
# In all, we are creating a new vector that has "Male" where dat$sex had 1
# and "Female" where dat$sex had 2.


dat$age_r <- c("Age 18-29", "Age 30-49", "Age 50-64", "Age 65+")[dat$age_cat]
freq(dat$age_r)

dat$race_r <- c("White", "Black", "Hispanic", "Other", "Asian")[dat$race_ethn]
freq(dat$race_r)

dat$party_r <- c("Rep", "Dem", "Ind", "Oth")[dat$party]
freq(dat$party_r)

# What if we wanted to combine "Independents" and "Other" in the party variable?
# Well, we can just make element 3 and 4 below to be the same thing!
dat$party_r2 <- c("Rep", "Dem", "Ind/Oth", "Ind/Oth")[dat$party]
freq(dat$party_r2)


# Data crosstabs are now easier to understand:

# For each party, what % are in certain age groups?
ctable(dat$party_r, dat$age_r)

# For each age group, what % are in a particular party?
ctable(dat$age_r, dat$party_r)

# Some numerical variables in the data could also use recoding.

# For example, covid_news ranges 1 (follow closely) to 4 (not at all closely)
# We usually want our variables coded such that higher means more.
# So here, we'd like to reverse-code:

freq(dat$covid_news)
dat$covid_news_r <- 5 - dat$covid_news
freq(dat$covid_news_r)

# Similarly, the following variables represent approval of the COVID response
# for various actors. They range from 1 (excellent) to 4 (poor).
# Let's reverse them so that 1 is poor and 4 is excellent.

freq(dat$covid_trump)
dat$covid_trump_r <- 5 - dat$covid_trump
freq(dat$covid_trump)

freq(dat$covid_usa)
dat$covid_usa_r <- 5 - dat$covid_usa
freq(dat$covid_usa_r)

freq(dat$covid_china)
dat$covid_china_r  <- 5 - dat$covid_china
freq(dat$covid_china_r)



# ================  ~~ Data analysis ================


# Some basic tests we often perform with survey data...

# Correlations: 
# Is people's ideology associated with approval for Trump's covid response?
cor.test(dat$ideology, dat$covid_trump)

# R normally uses scientific notation, e.g. 2.546e-15
# If you are so inclined, you can ask it to stop doing that:
options(scipen=20) 

# One-sample t-test to check if mean ideology is significantly different from 3.00:
t.test(dat$ideology, mu=3)

# Paired samples t-test comparing mean approval for US and China's COVID-19 response:
t.test(dat$covid_usa_r, dat$covid_china_r, data=dat, paired=TRUE)
descr(dat$covid_usa_r)
descr(dat$covid_china_r)

# Independent samples t-test
# Are men and women different in terms of political ideology?
mean(dat$ideology[dat$sex_r=="Male"], na.rm=T)
mean(dat$ideology[dat$sex_r=="Female"], na.rm=T)

# independent samples t-test -- note the use of formula as its first parameter.
# Many statistical test functions in R can use a formula as a parameter.
# An object of class formula looks something like this: y ~ x1 + x2
# where y is a dependent variable, while x1 and x2 are independent variables.
# Notice also that in a formula, we use just the names of the columns (without dat$)
t.test(ideology ~ sex_r, data=dat)

# Regressions in R similarly use formulas to specify the desired model:
# Here the predicted variable is approval of USA's COVID-19 response,
# and the predictors include respondent sex, age, race, party, and ideology.
glm(covid_usa_r ~ sex_r + age_cat + race_r + edu + income + party_r2 + ideology, data=dat)

# We can store the regression output in an object.
# The built-in summary() function will show us a better summary of the results:
my_result <- glm(covid_usa_r ~ sex_r + age_cat + race_r + edu + income + party_r2 + ideology, data=dat)
summary(my_result)

# You will notice that by default, the generalized linear model (glm) function will  
# treat character variables as categorical. It will include in our model dummy variables 
# for all variable categories except one, which will serve as the reference category
# that all the rest are compared to.

# By default the last category is used as a reference. But what if we wanted
# to choose a different reference category? For example, we may want to drop
# "White" from the model and use it as our reference category for race.
# We can create a categorical variable (a so-called 'factor') and specify that.
dat$race_r_f <- factor(dat$race_r) # Create a factor (categorical variable)
dat$race_r_f  <- relevel(dat$race_r_f, ref="White") # specify reference category

# See how the regression output changes:
my_res <- glm(covid_usa_r ~ sex_r + age_cat + race_r_f + edu + income + party_r2 + ideology, data=dat)
summary(my_res)


# We can add more relevant variables from the data into our regression:
my_res_2 <- glm(covid_usa_r ~ sex_r + age_cat + race_r_f + edu + income + 
                party_r2 + ideology + covid_trump_r + covid_news_r, data=dat)
summary(my_res_2)


# As usual, there is an R package the will offer us
# better and more informative formatting:

library(jtools)

# Result table:
summ(my_res_2)

# Standardized coefficients:
summ(my_res_2, scale = T)

# Plot the regression results:
plot_summs(my_res_2)

# Plot the results from both models:
plot_summs(my_res, my_res_2)


# We can create more readable coefficient names to use here:
better_names <- c("Male"                      =  "sex_rMale",
                  "Age"                       =  "age_cat",
                  "Education"                 =  "edu",
                  "Income"                    =  "income",
                  "Race/Ethnicity: Asian"     =  "race_r_fAsian",
                  "Race/Ethnicity: Black"     =  "race_r_fBlack",
                  "Race/Ethnicity: Hispanic"  =  "race_r_fHispanic",         
                  "Race/Ethnicity: Other"     =  "race_r_fOther",
                  "Party: Republican"         =  "party_r2Rep",
                  "Party: Indep/Other"        =  "party_r2Ind/Oth",
                  "Ideology"                  =  "ideology",
                  "Trump COVID-19 approval"   =  "covid_trump_r",
                  "COVID-19 news use"         =  "covid_news_r"  ) 


# We can also use our own model names if we wish:
model_names <-  c("Model w/o Trump", "Model with Trump") 


plot_summs(my_res, my_res_2, coefs = better_names, model.names = model_names )
           
# Generate pretty tables for your papers:
export_summs(my_res, my_res_2, coefs = better_names, model.names = model_names)

# We can save the table in a pretty format:
export_summs(my_res, my_res_2, coefs = better_names, model.names = model_names,
             to.file = "HTML", file.name = "my_file.html")



#---------------------# ADDITIONAL INFORMATION #---------------------# 
#
#  Note that we can also use glm() for other types of regression 
#  by including an additional 'family' parameter, like so:
#  
#  glm( y ~ x1 + x2 + x3, data=dat, family = "binomial") # logistic regression
#  glm( y ~ x1 + x2 + x3, data=dat, family = "poisson")  # Poisson regression
#
#-------------------------------------------------------------------# 



# ================  ~~ Using survey weights ================

# As we will discuss in class, we often want to use survey weights 
# to improve the representativeness of our data.

# The pew survey data already includes weights. To apply them in a regression,
# we can simply add a parameter to the glm() function:

my_res_w <- glm(covid_usa_r ~ sex_r + age_cat + race_r_f + edu + income + 
                party_r2 + ideology + covid_trump_r + covid_news_r, 
                data=dat, weights=dat$weight )
summary(my_res_w)

summ(my_res_w)
plot_summs(my_res_2, my_res_w, coefs = better_names)


# But what if we want to do other types of analysis with weights?
# And what if we want to create our own weights? 
# Good news: there is a package for that too!

library(survey)

# We can create a "survey" object by using the svydesign() function.
# The 'ids' parameter specifies cluster IDs in a clustered design.
# When there are no clusters, we just put ids~1
# The 'data' parameter is the data frame we have been using
# the 'weights' parameter is the survey weight we want to use

# Create a survey object:
dat.w <- svydesign(ids=~1, data=dat, weights=dat$weight)

# Examine the weighted values for a variable:
svytable(~race_r, dat.w) # with weights
table(dat$race_r)        # no weights  

# Crosstabs:  
svytable(~race_r + sex_r, dat.w)  # with weights 
table(dat$race_r, dat$sex_r)      # no weights  

# Examine weighted mean and standard deviation for a variable
# the 'na.rm' parameter if true tells the functions used here 
# to ignore missing data if there is any of it present.
svymean(~ideology, dat.w, na.rm=TRUE)  # with weights
mean(dat$ideology, na.rm=TRUE)         # no weights  

svysd(~ideology, dat.w, na.rm=TRUE)  # with weights
sd(dat$ideology, na.rm=TRUE)         # no weights  

# We can use the function 'svyby' to get weighted means 
# by category -- e.g. weighted ideology mean for men/women:
svyby(~ideology, ~sex_r, dat.w, svymean, na.rm=T)

# We can do weighted t-tests:
svyttest(ideology~sex_r, dat.w) 
  
# And, as we did before, we can estimate regression models.

# Model 1: with weights, using the 'survey' package
svyglm(covid_usa_r ~ sex_r + age_cat + race_r_f + party_r2, design=dat.w) 

# Model 2: also with weights, but using glm() instead
glm(covid_usa_r ~ sex_r + age_cat + race_r_f + party_r2, data=dat, weights=dat$weight) 

# Model 3: using glm but no weights
glm(covid_usa_r ~ sex_r + age_cat + race_r_f + party_r2, data=dat)   

# We also can do other things, such as plot variable histograms: 
svyhist(~ideology, dat.w)  # with weights
hist(dat$ideology)         # no weights  



# ================  ~~ Creating survey weights ================


# Following-up class discussion on post-stratification weights,
# here is how we can generate our own weights.

# First, we need to select the variables we want to use for weighting.
# Question: how does our sample differ from the population in ways
# that affect our key research questions? (and what is the population?)

# Let's say we want to create weights based on gender and race.
# What are the distributions of gender and race in our population?
# We need to know that so we can adjust accordingly in our data.

# Looking up population data (see census.gov), we find that the US is
# 48% male and 52% female. We also see that we seem to have a total of
# 63% White, 17% Hispanic, 12% Black, 6% Asian, and 2% other Americans.

# How does our data compare to the general population?

freq(dat$sex_r) # we have slightly more women
freq(dat$race_r) # we have slightly more White and Hispanic respondents 

# Ok, let's create some weights to adjust for that. 

library(survey)

# Create a survey object with no weights specified:
dat.s <- svydesign(ids=~1, data=dat)

# Calculate how many people of each sex or race we would expect 
# to see in our data if it was distributed like the US population:

num_people <- nrow(dat) # number of people in our data
num_by_sex <- num_people * c(.48, .52)
num_by_race <- num_people * c(.63, .17, .12, .06, .02) 

num_people
num_by_sex
num_by_race

# Each of the following data frames has two columns: one with the name of the
# variable we will use to create weights, and a second column named "Freq". 
# The first column contains  all possible categories of that variable.
# The second gives us the number of people from that category we would have
# in the data if our sample followed the same proportions that we see in the 
# entire population (those are 'num_by_sex' and 'num_by_race' above)

sex_dist <- data.frame(sex_r = c("Male", "Female"),
                       Freq = num_by_sex)

race_dist <- data.frame(race_r = c("White","Hispanic", "Black","Asian", "Other"),
                        Freq = num_by_race )

sex_dist
race_dist

# Finally, we use the rake() function to calculate weights based on the
# population values for each of the variables we have decided to use.
# Note that we if we have any missing values in the sex or race data,
# we will get an error at this point.
# Here, 'sample.margins' is a list with formulas for all the weighing
# variables we are using. The 'population.margins' parameter contains
# the data frame we created above (one for each variable).
dat.s <-  rake(design = dat.s,
               sample.margins = list(~sex_r, ~race_r),
               population.margins = list(sex_dist, race_dist))

# We can extract the weight vector from our data if we need it:  
weights(dat.s)
summary(weights(dat.s))

# Occasionally, some generated weights may be too small or too large.
# We can prevent that problem by restricting them to some minimum/maximum values.
# We use trimWeights() with 'lower' set to the minimum and 'upper' to the maximum
# weight that we want to allow in our data.

dat.s <- trimWeights(dat.s, lower=0.3, upper=5,  strict=TRUE) 
summary(weights(dat.s))

# For a survey that should be nationally representative, we usually base our
# survey weights on at least the following variables: gender, age category,
# race, ethnicity, education, and region of the country. Additional variables 
# may be included depending on the topic and nature of the data. 

 

detach(package:survey)


































