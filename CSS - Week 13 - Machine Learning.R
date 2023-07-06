
##================================================##
##                                                ##
##       Computational Social Science             ##
##       Doctoral Seminar (16:194:672)            ##
##       Spring 2023, Rutgers University          ## 
##       Katya Ognyanova, katya@ognyanova.net     ## 
##                                                ##
##================================================##



# ================  Classification in R   ================

# Packages we would need today:
# (remove # and run this if they are not already installed)  


# install.packages("tidymodels")       #   Machine learning workflow
# install.packages("randomForest")     #   Used for Random Forest classifier
# install.packages("kknn")             #   Used for KNN classifier
# install.packages("DataExplorer")     #   Examining the data
# install.packages("imager")           #   Showing penguin pictures!


# ================  ~~ Examine the data    ================

library(tidymodels)
library(DataExplorer)    
library(imager)


# Today we'll use a dataset describing penguins who live on the Palmer Archipelago:
data(penguins)
penguins

# Rename variables to make life easier:
colnames(penguins) <- c("species", "island", 
                        "bill_length", "bill_depth", 
                        "flipper", "mass", "sex")
# Look at the data!
View(penguins)

# Look at the penguins!
penguin_pic <- load.image("https://www.bas.ac.uk/wp-content/uploads/2015/04/Penguin-heights-736x419.jpg")
plot(penguin_pic)
rm(penguin_pic)

beak_pic <- load.image("https://allisonhorst.github.io/palmerpenguins/reference/figures/culmen_depth.png")
plot(beak_pic)
rm(beak_pic)

# Check missing data
plot_missing(penguins)

# We wouldn't usually do that with real data, but for the sake of the example
# we'll remove missing data about our penguins (there is not much of it anyway)
dim(penguins)
penguins <- na.omit(penguins)
dim(penguins)
 
# Check numerical variable correlations
plot_correlation(penguins[,3:6])

# Check numeric variable distributions
plot_histogram(penguins)

# Plot mass by flipper length, colored by species
ggplot(penguins, aes(x=flipper, y=mass, color = species)) +
  geom_point(size=2, alpha=0.7) +
  scale_color_manual(values = c("darkorange","darkred","skyblue3")) +
  theme_bw()
 
# Plot mass by flipper length, split by species, colored by sex
ggplot(penguins, aes(x = flipper, y = mass)) +
  geom_point(aes(color = sex),size=2, alpha=0.7) +
  scale_color_manual(values = c("darkred", "deepskyblue4")) + 
  facet_wrap(~species)+ 
  theme_bw()   


# Plot bill length by bill depth, colored by species
ggplot(penguins, aes(x=bill_length, y=bill_depth, color = species)) +
  geom_point(size=2, alpha=0.7) +
  scale_color_manual(values = c("darkorange","darkred","skyblue3")) +
  theme_bw()

# Let's create a "long" version of our dataset for plotting:
# We are combining columns 3 to 6, creating two new columns
# The old variable names are now in "measure", the values in "score"
penguin_long <- pivot_longer(penguins, cols = 3:6,
                             names_to = "measure",
                             values_to = "score")
penguin_long

ggplot(penguin_long, aes(x=score, fill = species)) +
  geom_histogram(alpha = 0.7, bins=20, position = "identity") +
  scale_fill_manual(values = c("darkorange","darkred","skyblue4")) +
  facet_wrap(~ measure, scales = "free_x") + 
  theme_bw()   




# ================  ~~ Preprocess the data  ================

# Because splitting the data is random, we set a seed to make sure
# you do the same split and get the exact same results as I do
set.seed(111)

# Below, we are doing a simple random splitting of our dataset, with
# 70% of the data used for training and 30% witheld to test the model
# If we had a large class imbalance (lots of cases in some categories
# and very few in others), we could do stratified sampling, which would
# do the training/testing data split within each category and combine.
# For example, we could inclide "strata=species" below to split within species. 
penguins_split <- initial_split(penguins, prop = 0.7)
penguins_split

# Get our training and testing data based on the split:
penguins_train_data <- training(penguins_split)
penguins_test_data  <- testing(penguins_split)


# Preprocess the data for classification.
# The "tidymodel" package offers a way to do that using a 'recipe'.
# It specifies our outcome variable, predictor variables, as well as
# what we want to do with the data to filter and clean it for analysis.
# We can then "bake" different datasets using this recipe.

# First, we will specify the outcome variable and the predictors (features) we want to use
# We are using a formula, much like we did when we ran regression models.
penguins_recipe <- recipe(species ~ bill_length + bill_depth + flipper + mass + sex, 
                          data = penguins_train_data)

# Next, we'll add some data cleaning steps to our initial recipe:

# Remove any variables that have too much missing data. Our second parameter here
# tells the function which variables to use for this -- all predictors.
penguins_recipe <- step_filter_missing(penguins_recipe, all_predictors())

# Remove any variables that are too highly correlated. Apply to all numeric variables.
penguins_recipe <- step_corr(penguins_recipe,   all_numeric())

# Normalize the data so it has a mean of 0. Apply to numeric variables.
penguins_recipe <- step_center(penguins_recipe, all_numeric())

# Scale the data to have a standard deviation of 1. Apply to numeric variables.
penguins_recipe <- step_scale(penguins_recipe,  all_numeric())

# Calculate everything we need to execute the recipe (e.g. variable means, etc.)
penguins_recipe <- prep(penguins_recipe)

# Our recipe is all done!
penguins_recipe


# Apply the recipe to the training data
# You can use a shorter equivalent function: juice(penguins_recipe)
penguins_training <- bake(penguins_recipe, new_data=NULL)

# Apply the same recipe to our test data:
penguins_testing  <- bake(penguins_recipe, new_data=penguins_test_data)

# The new datasset are just our original data with the additional 
# steps which were described in the recipe applied to them.
penguins_training
penguins_testing


# ================  ~~ Training model  ================

# There are many classification techniques (e.g.  linear and logistic regression, 
# classification trees, random forests, k-nearest neighbors, support vector machines,
# neural networks, and others). We will not cover any of them in much detail today. 

# Here, we will try two classifiers: random forest and k nearest neighbors.

# Specify the RF model:
penguins_model_rf <- rand_forest(trees = 100)
penguins_model_rf <- set_mode(penguins_model_rf, "classification")
penguins_model_rf <- set_engine(penguins_model_rf, "randomForest")

# Train the model on the data:
penguins_fit_rf <- fit(penguins_model_rf, 
                       species ~ .,
                       data = penguins_training)
penguins_fit_rf

# In the formula avobe, species ~ . tells R to use as a predictor
# every available variable in the data other than the outcome (species)
# This saves us some time typing the full formula.

# Specify the KNN model:
penguins_model_knn <- nearest_neighbor(neighbors = 5)
penguins_model_knn <- set_mode(penguins_model_knn, "classification")
penguins_model_knn <- set_engine(penguins_model_knn, "kknn")

# Train the model on the data:
penguins_fit_knn <- fit(penguins_model_knn, 
                        species ~ .,
                        data = penguins_training)
penguins_fit_knn


# Notice that one thing we do not do here is examine how the model results
# change for different parmeter values like trees or neighbors above. 
# Typically we would want to tune the paramters to get the best
# possible model we can generate.


# ================  ~~ Data prediction  ================


# Predict the penguin species based using the trained model and training data:
penguins_predict_train_rf  <- predict(penguins_fit_rf, penguins_training)
penguins_predict_train_knn <- predict(penguins_fit_knn, penguins_training)

# Predict the penguin species based using the trained model and test data:
penguins_predict_test_rf  <- predict(penguins_fit_rf, penguins_testing)
penguins_predict_test_knn <- predict(penguins_fit_knn, penguins_testing)

# Add the predictions to our training dataset:
penguins_training$predict_rf  <- unlist(penguins_predict_train_rf)
penguins_training$predict_knn <- unlist(penguins_predict_train_knn) 
penguins_training

# Add the predictions to our test dataset (more interesting!):
penguins_testing$predict_rf  <- unlist(penguins_predict_test_rf)
penguins_testing$predict_knn <- unlist(penguins_predict_test_knn) 
penguins_testing

# We can also obtain predicted probabilities for each possible outcome:
penguins_prob_rf <- predict(penguins_fit_rf, penguins_testing, type="prob")
penguins_prob_rf

penguins_prob_knn <- predict(penguins_fit_knn, penguins_testing, type="prob")
penguins_prob_knn

# We can add these to our testing dataset as well:
names(penguins_prob_rf)  <- c("adelie_rf", "chinstrap_rf", "gentoo_rf")
penguins_testing <- cbind(penguins_testing, penguins_prob_rf)
  
names(penguins_prob_knn) <- c("adelie_knn", "chinstrap_knn", "gentoo_knn")
penguins_testing <- cbind(penguins_testing, penguins_prob_knn)


# ================  ~~ Data validation  ================

# A "confusion matrix" for our data shows us the actual and predicted values.
# Each column includes the cases that actually belong to a category.
# Each row shows what category the model predicted for those cases.
# (note that in some textbooks, rows and columns are reversed)

# Not surprisingly, the classifiers perfectly predict their training data:
conf_mat(penguins_training, truth = species, estimate = predict_rf)
conf_mat(penguins_training, truth = species, estimate = predict_knn)

# What we are interested in is how well they do on the witheld test data:
penguin_cm_rf  <- conf_mat(penguins_testing, truth = species, estimate = predict_rf)
penguin_cm_knn <- conf_mat(penguins_testing, truth = species, estimate = predict_knn)
penguin_cm_rf 
penguin_cm_knn 

# From the confusion matrix, we can obtain a number of model fit metrics:
summary(penguin_cm_rf)
summary(penguin_cm_knn)

# We can see that KNN has 100% accuracy on test data compared to 97% for RF.

# Some of the metrics we might be interested in include:
#
# accuracy: 
#   Percent of the cases that were predicted correctly
#   e.g. what percent of penguins were sorted in the right category?
#
# precision: 
#   Correctly predicted in category / Total predicted in category (correctly or not)
#   E.g. what percent of predicted Adelie penguins are really Adelie penguins? 
#
# sensitivity/recall: 
#  Correctly predicted in category / Total truly in that category (predicted or not)
#  E.g. what percent of real Adelie penguins were predicted to be Adelie penguins?
#
# f1-score/f-measure: harmonic mean of precision and recall
#   The score tries to evaluate balance of precision and recall
#
# specificity: Correctly predicted NOT in category / Total truly NOT in category
#   E.g. What percent of things we said were not Adelie were really not Adelie penguins?
#
# Kappa: Accuracy over and above the accuracy we can expect by chance
#  Values closer to 1 are preferable
#
#
# Depending on what we are predicting, we may prioritize different metrics. 
# For example, if we are categorizing sick people to treat, we may be more
# concerned about recall and less about precision (more concerned that we 
# identify everyone who is really sick, and less worried about false positives)
 


# ================  ~~ Workflow ================

# The tidymodel package allows us to create a workflow that
# combines our recipe and the training model.
# We can than use that workflow with different datasets.

penguins_workflow <- workflow() 
penguins_workflow <- add_model(penguins_workflow, penguins_model_knn)
penguins_workflow <- add_recipe(penguins_workflow, penguins_recipe)

penguins_work_fit <- fit(penguins_workflow, penguins_train_data)
penguins_work_fit

penguins_work_fit <- fit(penguins_workflow, penguins_test_data)
penguins_work_fit

# ================  ~~ Cross-validation ================

# Cross-validation involves splitting our data into training and testing
# datasets multiple times, then training and validating the model on each split.

# K-fold (or v-fold) cross-validation involves splitting the data into 
# k equal parts called "folds". We then use k-1 of the folds as our 
# training data, and the remaining one fold as the test data.
# We repeat that k times, independently training and testing the model.
# Our validation results are aggregated for all the k validations.
 
# We can use k-fold cross-validation to tune model paramters, 
# and/oror to validate the training model. 

# People often use k=5 or k=10 in cross-validation. 

# Cross-validation with 10-fold split:
penguins_val_split <- vfold_cv(penguins, v = 10)
penguins_val_split

# We can get each of the 10 as a separate split if we want to:
penguins_val_split$splits[[1]]


# Fit the Random Forest model for each of the 10 data splits:
penguins_val_fit <- fit_resamples(penguins_model_rf, 
                                  species ~ ., 
                                  resamples = penguins_val_split,
                                  control = control_resamples(save_pred = T))
penguins_val_fit
 

# Get predicted values:
penguins_val_predict <- collect_predictions(penguins_val_fit) 
penguins_val_predict

# Get the confusion matrix (averaged across resamples):
penguins_val_cm <- conf_mat_resampled(penguins_val_fit, tidy=F)
penguins_val_cm
 
# Get metrics from the averaged confusion matrix:
summary(penguins_val_cm)


# ================  ~~ Regression model ================

# Since we are already familiar with regression from statistics,
# regression machine learning models may be easier to understand.
# Instead of classifying data, we are predicting numerical values.

data(penguins) 
penguins <- na.omit(penguins)
colnames(penguins) <- c("species", "island", 
                        "bill_length", "bill_depth", 
                        "flipper", "mass", "sex")


# Here is a standard regression example:
my_regression <- lm(mass ~ species + flipper + sex + bill_length + bill_depth,
                    data=penguins)
summary(my_regression)


# Now a machine learning model predicting penguin mass:
penguins_split_reg <- initial_split(penguins, prop = 0.7)
penguins_split_reg

# Get our training and testing data:
penguins_train_reg <- training(penguins_split_reg)
penguins_test_reg  <- testing(penguins_split_reg)

# Preprocess the data:
penguins_recipe <- recipe(mass ~ species + flipper + sex + bill_length + bill_depth,
                          data = penguins_train_reg)
penguins_recipe <- step_corr(penguins_recipe,   all_numeric())
penguins_recipe <- step_center(penguins_recipe, all_numeric())
penguins_recipe <- step_scale(penguins_recipe,  all_numeric())
penguins_recipe <- step_dummy(penguins_recipe,  all_nominal())
penguins_recipe <- prep(penguins_recipe)

# And bake the recipe:
penguins_train_reg <- bake(penguins_recipe, new_data=NULL)
penguins_test_reg  <- bake(penguins_recipe, new_data=penguins_test_reg)

# We can specify a model that just uses linear regression:
penguins_model_reg_lm <- linear_reg() 
penguins_model_reg_lm <- set_mode(penguins_model_reg_lm, "regression")
penguins_model_reg_lm <- set_engine(penguins_model_reg_lm, "lm")

# Train the LM model on the data:
penguins_fit_reg_lm <- fit(penguins_model_reg_lm, 
                           mass ~ flipper + bill_length + bill_depth + 
                                  sex_male + species_Adelie + species_Chinstrap,
                           data = penguins_train_reg)
penguins_fit_reg_lm

# And a model that uses random forest:
penguins_model_reg_rf <- rand_forest(trees = 100)
penguins_model_reg_rf <- set_mode(penguins_model_reg_rf, "regression")
penguins_model_reg_rf <- set_engine(penguins_model_reg_rf, "randomForest")

# Train the RF model on the data:
penguins_fit_reg_rf <- fit(penguins_model_reg_rf, 
                           mass ~ flipper + bill_length + bill_depth + 
                                  sex_male + species_Adelie + species_Chinstrap,
                           data = penguins_train_reg)
penguins_fit_reg_rf


# Predict the penguin mass based using the trained model:
penguins_predict_reg_lm <- predict(penguins_fit_reg_lm, penguins_test_reg)
penguins_predict_reg_rf <- predict(penguins_fit_reg_rf, penguins_test_reg)
 
# Add the predictions to our test dataset:
penguins_test_reg$predict_lm  <- unlist(penguins_predict_reg_lm)
penguins_test_reg$predict_rf <- unlist(penguins_predict_reg_rf) 
penguins_test_reg

# Evaluate how well we did in our predictions:
metrics(penguins_test_reg, mass, predict_lm)
metrics(penguins_test_reg, mass, predict_rf)

# Here metrics include:
# R Squared: explained variance, higher is better
# Mean Absolute Error: average error between true and predicted values, lower is better
# Root Mean Squared Error: square root of the squared error, lower is better


# ================ THE END ================























 




